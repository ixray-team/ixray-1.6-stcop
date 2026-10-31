#include "stdafx.h"
#include "r4.h"
#include "r1_GlowManager.h"
#include "ResourceManager.h"
#include "FBasicVisual.h"
#include "../../xrEngine/Fmesh.h"
#include "../../xrEngine/xrLevel.h"
#include "../../xrEngine/x_ray.h"
#include "../../xrEngine/IGame_Persistent.h"
#include "../../xrCore/stream_reader.h"

#include "dxRenderDeviceRender.h"

#include "dx10BufferUtils.h"
#include "3DFluid/dx103DFluidVolume.h"

#include "FHierrarhyVisual.h"
#include "LegacyVertexElement.h"

namespace
{
	template<class T>
	T ReadAt(const u8* Source, u16 Offset)
	{
		T Value;
		memcpy(&Value, Source + Offset, sizeof(T));
		return Value;
	}

	Fvector UnpackColorVector(u32 Value)
	{
		return { float((Value >> 16) & 0xFF) / 127.5f - 1.0f, float((Value >> 8) & 0xFF) / 127.5f - 1.0f, float(Value & 0xFF) / 127.5f - 1.0f };
	}

	PackedVertex EncodeLevelVertex(const CRender::LevelVertexChunk& Chunk, const u8* Source)
	{
		PackedVertex Vertex = {};
		Vertex.P = ReadAt<Fvector>(Source, Chunk.Position);
		if (Chunk.Normal == CRender::LevelVertexChunk::Absent)
		{
			return Vertex;
		}
		const u32 N = ReadAt<u32>(Source, Chunk.Normal);
		const u32 T = ReadAt<u32>(Source, Chunk.Tangent);
		const u32 B = ReadAt<u32>(Source, Chunk.Binormal);
		VertexPacking::EncodeFrame(Vertex, UnpackColorVector(N), UnpackColorVector(T), UnpackColorVector(B));
		const u32 Hemi = N >> 24;
		if (Chunk.Tex0Short4)
		{
			const auto UV = ReadAt<std::array<s16, 4>>(Source, Chunk.Tex0);
			VertexPacking::EncodeUV(Vertex, UV[0] / 2048.0f, UV[1] / 2048.0f);
			Vertex.C = color_rgba(Hemi, Hemi, 255, 0);
			Vertex.X = u16(UV[2]);
			return Vertex;
		}
		const auto UV = ReadAt<std::array<s16, 2>>(Source, Chunk.Tex0);
		VertexPacking::EncodeUV(Vertex, (UV[0] + float(T >> 24) / 255.0f) / 1024.0f, (UV[1] + float(B >> 24) / 255.0f) / 1024.0f);
		if (Chunk.Color != CRender::LevelVertexChunk::Absent)
		{
			const u32 Color = ReadAt<u32>(Source, Chunk.Color);
			Vertex.C = color_rgba(Hemi, Color >> 24, 255, 0);
			Vertex.X = Color & 0x00FFFFFFu;
			return Vertex;
		}
		Vertex.C = color_rgba(Hemi, 0, 255, 0);
		Vertex.X = Chunk.Tex1 != CRender::LevelVertexChunk::Absent ? ReadAt<u32>(Source, Chunk.Tex1) : 0;
		return Vertex;
	}
}

#pragma warning(push)
#pragma warning(disable:4995)
#include <malloc.h>

#include "../../xrCore/FVFLegacy.h"

#include "../../xrCore/FormatParsers/LevelGeom/GeomIO.h"
using namespace FVF;

#pragma warning(pop)

void CRender::level_Load(IReader* fs)
{
	R_ASSERT						(0!=g_pGameLevel);
	R_ASSERT(!GraphMain.b_loaded);

	// Begin
	pApp->LoadBegin					();
	dxRenderDeviceRender::Instance().Resources->DeferredLoad(ps_r__common_flags.test(RFLAG_DD_TEX_LOAD));
	IReader*						chunk;

	o.dx11_disable_motion_vectors = MotionVectorsDisabled();

	// Shaders
	g_pGamePersistent->SetLoadStageTitle("st_loading_shaders");
	g_pGamePersistent->LoadTitle		();
	{
		chunk = fs->open_chunk		(fsL_SHADERS);
		R_ASSERT2					(chunk,"Level doesn't builded correctly.");
		u32 count = chunk->r_u32	();
		Shaders.resize				(count);
		for(u32 i=0; i<count; i++)	// skip first shader as "reserved" one
		{
			string512				n_sh,n_tlist;
			const char*			n		= (const char*)(chunk->pointer());
			chunk->skip_stringZ		();
			if (0==n[0])			continue;
			xr_strcpy					(n_sh,n);
			LPSTR			delim	= strchr(n_sh,'/');
			*delim					= 0;
			xr_strcpy					(n_tlist,delim+1);
			Shaders[i]				= dxRenderDeviceRender::Instance().Resources->Create(n_sh,n_tlist);
		}
		chunk->close();
	}

	// Components
	if (LightingModeIsStaticRender())
	{
		L_Projector = new CLightProjector();
		L_Shadows = new CLightShadows();
		L_Glows = new CGlowManager();
	}
	Wallmarks					= new CWallmarksEngine	();
	Details						= new CDetailManager	();

	if	(!g_dedicated_server)	{
		// VB,IB,SWI
		g_pGamePersistent->SetLoadStageTitle("st_loading_geometry");
		g_pGamePersistent->LoadTitle();
		
		{
			auto Geom = XRay::Geom::Read("$level$","level", ".geom");
			if (!I_ASSERT(Geom))
			{
				FATAL("Unable to load geometry file");
			}
		
			dxRenderDeviceRender::Instance().Resources->Evict();
			if (Geom->HasVBData())
			{
				LoadVertexBuffers(Geom->GetVBData(), false);
			}
			if (Geom->HasIBData())
			{
				LoadIndexBuffers(Geom->GetIBData(), false);
			}
			if (Geom->HasSWIData())
			{
				LoadSWIs(Geom->GetSWIData());
			}
		}
		
		//...and alternate/fast geometry
		if (LightingModeIsDynamicRender())
		{
			auto Geom = XRay::Geom::Read("$level$","level", ".geomx");
			if (!I_ASSERT(Geom))
			{
				FATAL("Unable to load geometry file");
			}
		
			dxRenderDeviceRender::Instance().Resources->Evict();
			if (Geom->HasVBData())
			{
				LoadVertexBuffers(Geom->GetVBData(), true);
			}
			if (Geom->HasIBData())
			{
				LoadIndexBuffers(Geom->GetIBData(), true);
			}
		}

		u64 TotalVertices = 0;
		u64 TotalIndices = 0;
		for (u32 Alternative = 0; Alternative < 2; ++Alternative)
		{
			for (const LevelVertexChunk& Chunk : LevelVertices[Alternative])
			{
				TotalVertices += Chunk.Count;
			}
			for (const xr_vector<u16>& Chunk : LevelIndices[Alternative])
			{
				TotalIndices += Chunk.size();
			}
		}
		Geometry.Reserve(u32(std::min<u64>(TotalVertices, u32(-1))), u32(std::min<u64>(TotalIndices, u32(-1))));

		// Visuals
		g_pGamePersistent->SetLoadStageTitle("st_loading_spatial_db");
		g_pGamePersistent->LoadTitle();
		chunk						= fs->open_chunk(fsL_VISUALS);
		LoadVisuals					(chunk);
		chunk->close				();

		for (u32 Alternative = 0; Alternative < 2; ++Alternative)
		{
			LevelVertices[Alternative] = {};
			LevelIndices[Alternative] = {};
		}
		LevelScratch = {};
		Geometry.Reserve(0, 0);

		// Details
		g_pGamePersistent->SetLoadStageTitle("st_loading_details");
		g_pGamePersistent->LoadTitle();
		Details->Load				();
	}

	// Sectors
	g_pGamePersistent->SetLoadStageTitle("st_loading_sectors_portals");
	g_pGamePersistent->LoadTitle();
	LoadSectors					(fs);

	// 3D Fluid
	Load3DFluid					();

	// HOM
	HOM.Load					();

	if (L_Glows)
	{
		IReader* glow_data = fs->open_chunk(fsL_GLOWS);
		if (glow_data)
		{
			if (glow_data->length()) L_Glows->Load(glow_data);
			glow_data->close();
		}
	}

	// Lights
	// pApp->LoadTitle			("Loading lights...");
	LoadLights					(fs);
	if (LightingModeIsDynamicRender())
	{
		LoadPuddles();
		LoadPlanars();
	}

	// End
	pApp->LoadEnd				();

	// sanity-clear
	GraphMain.lstLODgroups.clear			();
	GraphMain.mapLOD.clear				();

	// signal loaded
	GraphMain.b_loaded = true;
}

void CRender::LoadPuddles()
{
	if (g_pGameLevel->pLevel->section_exist("level_map"))
	{
		Fvector4 res2d = g_pGameLevel->pLevel->line_exist("level_map", "bound_rect") 
			? g_pGameLevel->pLevel->r_fvector4("level_map", "bound_rect") 
			: pGameIni->r_fvector4(g_pGameLevel->name(), "bound_rect");

		m_puddles_level_bound.lt.x = res2d.x;
		m_puddles_level_bound.lt.y = res2d.y;

		m_puddles_level_bound.rb.x = res2d.z;
		m_puddles_level_bound.rb.y = res2d.w;
	}
	else
	{
		Fbox puddles_level_bound = g_pGameLevel->ObjectSpace.GetBoundingVolume();

		m_puddles_level_bound.lt.x = puddles_level_bound.min.x;
		m_puddles_level_bound.lt.y = puddles_level_bound.min.z;

		m_puddles_level_bound.rb.x = puddles_level_bound.max.x;
		m_puddles_level_bound.rb.y = puddles_level_bound.max.z;
	}

	m_puddles_level_bound.rb.sub(m_puddles_level_bound.lt);

	m_puddles_level_bound.rb.x = m_puddles_level_bound.rb.x > 0.0f ? 1.0f / m_puddles_level_bound.rb.x : 0.0f;
	m_puddles_level_bound.rb.y = m_puddles_level_bound.rb.y > 0.0f ? -1.0f / m_puddles_level_bound.rb.y : 0.0f;

	m_puddles_level_bound.lt.mul(m_puddles_level_bound.rb);

	m_levels_puddles.resize(0);

	string_path ini_file;

	if(!FS.exist(ini_file, "$level$", "level.puddles"))
	{
		return;
	}

	CInifile ini(ini_file);
	CInifile::Root& sections = ini.sections();
	for(CInifile::Sect& sect : sections)
	{
		shared_str& sect_name = sect.Name;
		PuddleBase& m_puddle = m_levels_puddles.emplace_back();

		Fvector position = ini.r_fvector3(sect_name, "position");
		float max_height = ini.r_float(sect_name, "max_height");

		Fvector2 size_xz = ini.r_fvector2(sect_name, "size_xz");

		float rotX, rotY, rotZ = 0.0f;
		int rotArg = sscanf(ini.r_string(sect_name, "rotation"), "%0.3f, %0.3f, %0.3f", &rotX, &rotY, &rotZ);
		if (rotArg == 3)
		{
			m_puddle.m_world.rotateX(-rotX);
			m_puddle.m_world.rotateY(-rotY);
			m_puddle.m_world.rotateZ(-rotZ);
		}
		else
		{
			m_puddle.m_world.rotateY(rotX); // actually Y, just named X because it's the first one
		}

		m_puddle.m_world.mulB_43(Fmatrix().scale(size_xz.x, 1.0f, size_xz.y));

		m_puddle.m_world.translate_over(position);

		m_puddle.m_height = max_height;
		m_puddle.m_radius = size_xz.magnitude();
	}
}

void CRender::LoadPlanars()
{
	m_levels_planars.resize(0);

	string_path ini_file;

	if (!FS.exist(ini_file, "$level$", "level.planars"))
		return;

	CInifile ini(ini_file);
	CInifile::Root& sections = ini.sections();
	for (CInifile::Sect& sect : sections)
	{
		shared_str& sect_name = sect.Name;
		PlanarBase& planar = m_levels_planars.emplace_back();

		Fvector position = ini.r_fvector3(sect_name, "position");
		float influence = ini.r_float(sect_name, "influence");
		Fvector2 size_xz = ini.r_fvector2(sect_name, "size_xz");

		float rotX = 0.0f, rotY = 0.0f, rotZ = 0.0f;
		int rotArg = sscanf(ini.r_string(sect_name, "rotation"), "%0.3f, %0.3f, %0.3f", &rotX, &rotY, &rotZ);
		if (rotArg == 3)
		{
			planar.m_world.rotateX(-rotX);
			planar.m_world.rotateY(-rotY);
			planar.m_world.rotateZ(-rotZ);
		}
		else
		{
			planar.m_world.rotateY(rotX);
		}

		planar.m_world.mulB_43(Fmatrix().scale(size_xz.x, std::max(influence, EPS), size_xz.y));
		planar.m_world.translate_over(position);

		planar.m_influence = influence;
		planar.m_stiffness = ini.line_exist(sect_name, "stiffness") ? ini.r_float(sect_name, "stiffness") : 1.f;
		clamp(planar.m_stiffness, 0.f, 1.f);
		planar.m_radius = _sqrt(size_xz.x * size_xz.x + influence * influence + size_xz.y * size_xz.y);
	}
}

void CRender::level_Unload()
{
	if (0==g_pGameLevel)		return;
	if (!GraphMain.b_loaded)
	{
		return;
	}

	u32 I;

	// HOM
	HOM.Unload				();

	//*** Details
	Details->Unload			();

	//*** Sectors
	// 1.
	xr_delete				(rmPortals);
	pLastSector				= 0;
	pOutdoorSector			= 0;
	vLastCameraPos.set		(0,0,0);
	// 2.
	for (I=0; I<Sectors.size(); I++)	xr_delete(Sectors[I]);
	Sectors.clear			();
	// 3.
	for (I=0; I<Portals.size(); I++)	xr_delete(Portals[I]);
	Portals.clear			();

	//*** Lights
	// Glows.Unload			();
	Lights.Unload			();

	//*** Visuals
	for (I=0; I<Visuals.size(); I++)
	{
		Visuals[I]->Release();
		xr_delete(Visuals[I]);
	}
	Visuals.clear			();

	//*** SWI
	for (I=0; I<SWIs.size();I++)xr_free	(SWIs[I].sw);
	SWIs.clear				();

	//*** VB/IB
	Geometry.Trim();

	//*** Components
	xr_delete(L_Projector);
	xr_delete(L_Shadows);
	if (L_Glows) L_Glows->Unload();
	xr_delete(L_Glows);
	xr_delete					(Details);
	xr_delete					(Wallmarks);

	m_levels_puddles.resize(0);
	m_levels_planars.resize(0);

	//*** Shaders
	Shaders.clear();
	GraphMain.b_loaded = false;
}

void CRender::LoadVertexBuffers(IReaderBase& fs, bool _alternative)
{
	using XRay::Legacy::LEGACYVERTEXELEMENT9;
	xr_vector<LevelVertexChunk>& Chunks = LevelVertices[_alternative];
	Chunks.resize(fs.r_u32());
	for (LevelVertexChunk& Chunk : Chunks)
	{
		LEGACYVERTEXELEMENT9 Decl[XRay::Legacy::LEGACYMAXDECLLENGTH + 1];
		fs.r(Decl, sizeof(Decl));
		const u32 Length = u32(XRay::Legacy::GetDeclLength(Decl));
		fs.advance(int((Length + 1) * sizeof(LEGACYVERTEXELEMENT9)) - int(sizeof(Decl)));
		for (u32 Index = 0; Index < Length; ++Index)
		{
			const LEGACYVERTEXELEMENT9& Element = Decl[Index];
			const auto Expect = [&Element](u8 Type)
			{
				if (Element.Type != Type)
				{
					FATAL(make_string<const char*>("Level geometry: element usage %u index %u has unsupported type %u", Element.Usage, Element.UsageIndex, Element.Type));
				}
				return Element.Offset;
			};
			switch (Element.Usage)
			{
			case D3DDECLUSAGE_POSITION: Chunk.Position = Expect(D3DDECLTYPE_FLOAT3); break;
			case D3DDECLUSAGE_NORMAL: Chunk.Normal = Expect(D3DDECLTYPE_D3DCOLOR); break;
			case D3DDECLUSAGE_TANGENT: Chunk.Tangent = Expect(D3DDECLTYPE_D3DCOLOR); break;
			case D3DDECLUSAGE_BINORMAL: Chunk.Binormal = Expect(D3DDECLTYPE_D3DCOLOR); break;
			case D3DDECLUSAGE_COLOR: Chunk.Color = Expect(D3DDECLTYPE_D3DCOLOR); break;
			case D3DDECLUSAGE_TEXCOORD:
				if (Element.UsageIndex == 0)
				{
					Chunk.Tex0Short4 = Element.Type == D3DDECLTYPE_SHORT4;
					Chunk.Tex0 = Chunk.Tex0Short4 ? Element.Offset : Expect(D3DDECLTYPE_SHORT2);
				}
				else if (Element.UsageIndex == 1)
				{
					Chunk.Tex1 = Expect(D3DDECLTYPE_SHORT2);
				}
				break;
			}
		}
		const bool Shaded = Chunk.Normal != LevelVertexChunk::Absent;
		if (Chunk.Position == LevelVertexChunk::Absent || (Shaded && (Chunk.Tangent == LevelVertexChunk::Absent || Chunk.Binormal == LevelVertexChunk::Absent || Chunk.Tex0 == LevelVertexChunk::Absent)))
		{
			FATAL(make_string<const char*>("Level geometry: vertex declaration of %u elements has no POSITION or an incomplete normal frame", Length));
		}
		Chunk.Stride = u32(XRay::Legacy::ComputeVertexSize(Decl, 0));
		Chunk.Count = fs.r_u32();
		if (u64(Chunk.Count) * Chunk.Stride > u32(-1))
		{
			FATAL(make_string<const char*>("Level geometry: vertex buffer of %u vertices x %u bytes is too large", Chunk.Count, Chunk.Stride));
		}
		Chunk.Data.resize(size_t(Chunk.Count) * Chunk.Stride);
		fs.r(Chunk.Data.data(), u32(Chunk.Data.size()));
	}
}

GeometryAlloc CRender::UploadLevelMesh(bool Alternative, u32 VertexBuffer, u32 VertexBase, u32 VertexCount, u32 IndexBuffer, u32 IndexBase, u32 IndexCount)
{
	const xr_vector<LevelVertexChunk>& VertexChunks = LevelVertices[Alternative];
	const xr_vector<xr_vector<u16>>& IndexChunks = LevelIndices[Alternative];
	if (VertexBuffer >= VertexChunks.size() || IndexBuffer >= IndexChunks.size() || u64(VertexBase) + VertexCount > VertexChunks[VertexBuffer].Count || u64(IndexBase) + IndexCount > IndexChunks[IndexBuffer].size())
	{
		FATAL(make_string<const char*>("Level geometry: visual references vertices %u+%u of buffer %u and indices %u+%u of buffer %u out of range", VertexBase, VertexCount, VertexBuffer, IndexBase, IndexCount, IndexBuffer));
	}
	const LevelVertexChunk& Chunk = VertexChunks[VertexBuffer];
	LevelScratch.resize(VertexCount);
	for (u32 Index = 0; Index < VertexCount; ++Index)
	{
		LevelScratch[Index] = EncodeLevelVertex(Chunk, Chunk.Data.data() + size_t(VertexBase + Index) * Chunk.Stride);
	}
	const GeometryAlloc Block = Geometry.Alloc(VertexCount, IndexCount);
	Geometry.Upload(Block, 0, LevelScratch.data(), VertexCount);
	Geometry.UploadIndices(Block, 0, IndexChunks[IndexBuffer].data() + IndexBase, IndexCount);
	return Block;
}

void CRender::LoadIndexBuffers(IReaderBase& fs, bool _alternative)
{
	xr_vector<xr_vector<u16>>& Chunks = LevelIndices[_alternative];
	Chunks.resize(fs.r_u32());
	for (xr_vector<u16>& Chunk : Chunks)
	{
		Chunk.resize(fs.r_u32());
		fs.r(Chunk.data(), u32(Chunk.size() * sizeof(u16)));
	}
}

void CRender::LoadVisuals(IReader *fs)
{
	IReader*		chunk	= 0;
	u32			index	= 0;
	dxRender_Visual*		V		= 0;
	ogf_header		H;

	while ((chunk=fs->open_chunk(index))!=0)
	{
		chunk->r_chunk_safe			(OGF_HEADER,&H,sizeof(H));
		V = Models->Instance_Create	(H.type);
		V->Load(0,chunk,0);
		Visuals.push_back(V);

		chunk->close();
		index++;
	}
}

void CRender::LoadLights(IReader *fs)
{
	// lights
	Lights.Load	(fs);
	Lights.LoadHemi();
}

struct b_portal
{
	u16				sector_front;
	u16				sector_back;
	FixedVector<Fvector,6>	vertices;
};

void CRender::LoadSectors(IReader* fs)
{
	// allocate memory for portals
	u32 size = fs->find_chunk(fsL_PORTALS);
	R_ASSERT(0 == size % sizeof(b_portal));
	u32 count = size / sizeof(b_portal);
	Portals.resize(count);

	for (u32 c = 0; c < count; c++)
	{
		CPortal* portal = new CPortal();
		portal->index = c;
		Portals[c] = portal;
	}

	// load sectors
	IReader* S = fs->open_chunk(fsL_SECTORS);
	u32 crc = crc32(S->pointer(), S->length());

	for (u32 i = 0; ; i++)
	{
		IReader* P = S->open_chunk(i);
		if (0 == P) break;

		CSector* __S = new CSector();
		__S->index = (u32)Sectors.size();
		__S->load(*P);
		Sectors.push_back(__S);

		P->close();
	}
	S->close();

	// load portals
	if (count)
	{
		static CDB::Collector CL;
		CL.clear();
		CL.reserve(count * 4);
		fs->find_chunk(fsL_PORTALS);
		for (u32 i = 0; i < count; i++)
		{
			b_portal P;
			fs->r(&P, sizeof(P));
			CPortal* __P = (CPortal*)Portals[i];

			__P->Setup
			(
				P.vertices.begin(), P.vertices.size(),
				(CSector*)getSector(P.sector_front),
				(CSector*)getSector(P.sector_back)
			);

			for (u32 j = 2; j < P.vertices.size(); j++)
			{
				CL.add_face_packed_D(P.vertices[0], P.vertices[j - 1], P.vertices[j], u32(i));
			}
		}

		if (CL.getTS() < 2)
		{
			Fvector					v1, v2, v3;
			v1.set(-20000.f, -20000.f, -20000.f);
			v2.set(-20001.f, -20001.f, -20001.f);
			v3.set(-20002.f, -20002.f, -20002.f);
			CL.add_face_packed_D(v1, v2, v3, 0);
		}

		// Make cache
		string_path LevelName;
		xr_strconcat(LevelName, "level_cache\\", FS.get_path("$level$")->m_Add, "Portals.cache");
		IReader* pReaderCache = CDB::GetModelCache(LevelName, crc);

		// build portal model
		rmPortals = new CDB::MODEL();

		if (pReaderCache != nullptr)
		{
			rmPortals->build(CL.getV(), CL.getVS(), CL.getT(), CL.getTS(), nullptr, nullptr, pReaderCache, true);
		}
		else
		{
			IWriter* pWriterCache = FS.w_open("$app_data_root$", LevelName);
			pWriterCache->w_u32(CDB::CDB_MODEL_CACHE_VERSION);
			pWriterCache->w_u32(crc);
			rmPortals->build(CL.getV(), CL.getVS(), CL.getT(), CL.getTS(), nullptr, nullptr, pWriterCache, false);
		}
	}
	else
	{
		rmPortals = 0;
	}

	pLastSector = 0;

	// Search for default sector - assume "default" or "outdoor" sector is the largest one
	//. hack: need to know real outdoor sector
	CSector* largest_sector = 0;
	float largest_sector_vol = 0;
	for (u32 s = 0; s < Sectors.size(); s++)
	{
		CSector* S = (CSector*)Sectors[s];
		dxRender_Visual* V = S->root();
		float vol = V->vis.box.getvolume();
		if (vol > largest_sector_vol)
		{
			largest_sector_vol = vol;
			largest_sector = S;
		}
	}
	pOutdoorSector = largest_sector;
}

void CRender::LoadSWIs(IReaderBase& fs)
{
	// allocate memory for portals
	u32 item_count = fs.r_u32();
	for (auto& elem : SWIs)
	{
		xr_free(elem.sw);
	}
	SWIs.clear();
	SWIs.resize(item_count);
	for (auto& swi : SWIs){
		swi.reserved[0]	= fs.r_u32();	
		swi.reserved[1]	= fs.r_u32();	
		swi.reserved[2]	= fs.r_u32();	
		swi.reserved[3]	= fs.r_u32();	
		swi.count = fs.r_u32();
		VERIFY(nullptr==swi.sw);
		swi.sw = xr_alloc<FSlideWindow> (swi.count);
		fs.r(swi.sw,sizeof(FSlideWindow)*swi.count);
	}
}

void CRender::Load3DFluid()
{
	if (!RImplementation.o.volumetricfog)
		return;

	string_path fn_game;
	if ( FS.exist( fn_game, "$level$", "level.fog_vol" ) )
	{
		IReader *F	= FS.r_open( fn_game );
		u16 version	= F->r_u16();

		if(version == 3)
		{
			u32 cnt = F->r_u32();
			for(u32 i=0; i<cnt; ++i)
			{
				dx103DFluidVolume *pVolume = new dx103DFluidVolume();
				pVolume->Load("", F, 0);

				//	Attach to sector's static geometry
				CSector *pSector = (CSector*)detectSector(pVolume->getVisData().sphere.P);
				//	3DFluid volume must be in render sector
				VERIFY(pSector);

				dxRender_Visual* pRoot = pSector->root();
				//	Sector must have root
				VERIFY(pRoot);
				VERIFY(pRoot->getType() == MT_HIERRARHY);
				
				((FHierrarhyVisual*)pRoot)->children.push_back(pVolume);
			}
		}

		FS.r_close(F);
	}
}