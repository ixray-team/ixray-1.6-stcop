#include "stdafx.h"
#include "RenderThread.h"
#include "ixr.h"
#include "r1_GlowManager.h"
#include "ResourceManager.h"
#include "FBasicVisual.h"
#include "../../xrEngine/Fmesh.h"
#include "../../xrEngine/xrLevel.h"
#include "../../xrEngine/x_ray.h"
#include "../../xrEngine/IGame_Persistent.h"
#include "../../xrCore/stream_reader.h"

#include "dxRenderDeviceRender.h"


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

}

namespace LevelPacking
{
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

// What a loaded level holds. These numbers size the GPU scene tables.
static void LogSceneCounts(const xr_vector<dxRender_Visual*>& Visuals, size_t Sectors)
{
	u32 Types[32] = {};
	u32 Meshes = 0;
	u32 LargestMesh = 0;
	u64 Vertices = 0;
	u64 Indices = 0;
	u32 Passes = 0;
	xr_hash_set<Shader*> Shaders;
	xr_hash_set<STextureList*> Lists;
	xr_hash_set<CTexture*> Textures;
	for (dxRender_Visual* Visual : Visuals)
	{
		++Types[std::min<u32>(Visual->Type, 31)];
		if (const IRender_Mesh* Mesh = dynamic_cast<const IRender_Mesh*>(Visual))
		{
			++Meshes;
			Vertices += Mesh->vCount;
			Indices += u64(Mesh->dwPrimitives) * 3;
			LargestMesh = std::max(LargestMesh, Mesh->vCount);
		}

		Shader* Material = Visual->shader._get();
		if (!Material || !Shaders.insert(Material).second)
		{
			continue;
		}
		for (const ref_selement& Element : Material->E)
		{
			if (!Element._get())
			{
				continue;
			}
			for (u32 Pass = 0; Pass < Element->passes.size(); ++Pass)
			{
				STextureList* List = Element->passes[Pass]._get() ? Element->passes[Pass]->T._get() : nullptr;
				++Passes;
				if (!List || !Lists.insert(List).second)
				{
					continue;
				}
				for (const auto& [Stage, Texture] : *List)
				{
					if (Texture._get())
					{
						Textures.insert(Texture._get());
					}
				}
			}
		}
	}

	// Textures by their larger side, as a power of two; still loading ones have no surface yet
	u32 Sizes[14] = {};
	u32 Pending = 0;
	for (CTexture* Texture : Textures)
	{
		if (!Texture->pSurface)
		{
			++Pending;
			continue;
		}
		const u32 Side = std::max(Texture->pSurface->GetWidth(), Texture->pSurface->GetHeight());
		++Sizes[std::min<u32>(std::bit_width(std::max(Side, 1u)) - 1, 13)];
	}

	Msg("* [scene] visuals %u in %u sectors: meshes %u (normal %u, progressive %u, tree %u), hierarchy %u, LOD %u, MU LODs %u",
		u32(Visuals.size()), u32(Sectors), Meshes, Types[MT_NORMAL], Types[MT_PROGRESSIVE], Types[MT_TREE_ST] + Types[MT_TREE_PM], Types[MT_HIERRARHY], Types[MT_LOD], Types[MT_MESH_LODS]);
	Msg("* [scene] vertices %llu, indices %llu, largest mesh %u vertices", Vertices, Indices, LargestMesh);
	Msg("* [scene] shaders %u, passes %u, texture lists %u, textures %u (%u still loading)", u32(Shaders.size()), Passes, u32(Lists.size()), u32(Textures.size()), Pending);
	Msg("* [scene] textures by size: <=64 %u, 128 %u, 256 %u, 512 %u, 1024 %u, 2048 %u, 4096 %u, larger %u",
		Sizes[0] + Sizes[1] + Sizes[2] + Sizes[3] + Sizes[4] + Sizes[5] + Sizes[6], Sizes[7], Sizes[8], Sizes[9], Sizes[10], Sizes[11], Sizes[12], Sizes[13]);
}

void CRender::level_Load(IReader* fs)
{
	RenderThread::WaitIdle();
	PROF_EVENT(__FUNCTION__);
	ResetSunShadowCache();
	HeightfieldShadows.Destroy();

	if (Target)
	{
		Target->InvalidateSpotShadowCache();
		Target->ClearSpotShadowCache();
	}
	R_ASSERT						(0!=g_pGameLevel);
	R_ASSERT(!GraphMain.b_loaded);

	// The settings the level is built with take their stored values now and keep them until it unloads; the
	// shaders made before it are compiled again for them first
	VideoSettings::SetLevelLoaded(true);
	SyncShadingQuality();

	// Begin
	pApp->LoadBegin					();
	// The forward frame draws the level's textures from the scene's buckets: they wait for the scene to tell which
	// of them need a whole copy at all
	dxRenderDeviceRender::Instance().Resources->ForgetPending();
	dxRenderDeviceRender::Instance().Resources->DeferredLoad(true);
	IReader*						chunk;

	o.dx11_disable_motion_vectors = MotionVectorsDisabled();
	UpdateRainPuddleMode();
	if (o.ProceduralRainPuddles)
	{
		RainPuddleWater.Build(&g_pGameLevel->ObjectSpace);
	}
	else
	{
		RainPuddleWater.Destroy();
	}

	// ShadersLevel
	LogShaderTimes("the start of the level");
	g_pGamePersistent->SetLoadStageTitle("st_loading_shaders");
	g_pGamePersistent->LoadTitle		();
	{
		chunk = fs->open_chunk		(fsL_SHADERS);
		R_ASSERT2					(chunk,"Level doesn't builded correctly.");
		u32 count = chunk->r_u32	();
		ShadersLevel.resize				(count);

		xr_vector<std::pair<xr_string, xr_string>> Names(count);
		for(u32 i=0; i<count; i++)
		{
			const char*			n		= (const char*)(chunk->pointer());
			chunk->skip_stringZ		();
			if (0==n[0])			continue;
			const char*			delim	= strchr(n,'/');
			Names[i].first.assign	(n, size_t(delim - n));
			Names[i].second			= delim + 1;
		}
		chunk->close();

		xr_parallel_for(size_t(0), size_t(count), [this, &Names](size_t i)
		{
			if (Names[i].first.empty())
			{
				return;
			}

			ShadersLevel[i] = dxRenderDeviceRender::Instance().Resources->Create(Names[i].first.c_str(), Names[i].second.c_str());
		});
	}

	LogShaderTimes("the level's shaders");

	// Components
	if (g_RenderFeatures.SpriteGlows)
	{
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
				LoadVertexBuffers(Geom->GetVBData());
			}
			if (Geom->HasIBData())
			{
				LoadIndexBuffers(Geom->GetIBData());
			}
			if (Geom->HasSWIData())
			{
				LoadSWIs(Geom->GetSWIData());
			}
		}

		// Visuals
		g_pGamePersistent->SetLoadStageTitle("st_loading_spatial_db");
		g_pGamePersistent->LoadTitle();
		chunk						= fs->open_chunk(fsL_VISUALS);
		{
			PROF_EVENT("CRender::level_Load: visuals");
			LoadVisuals					(chunk);
		}
		chunk->close				();

		// Details
		g_pGamePersistent->SetLoadStageTitle("st_loading_details");
		g_pGamePersistent->LoadTitle();
		{
			PROF_EVENT("CRender::level_Load: details");
			Details->Load				();
		}
	}

	// Sectors
	g_pGamePersistent->SetLoadStageTitle("st_loading_sectors_portals");
	g_pGamePersistent->LoadTitle();
	{
		PROF_EVENT("CRender::level_Load: sectors");
		LoadSectors					(fs);
	}

	// HOM
	{
		PROF_EVENT("CRender::level_Load: HOM");
		HOM.Load					();
	}

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
	// The level's rectangle the rain puddles are laid out by comes with the authored puddles
	LoadPuddles();

	LogSceneCounts(Visuals, Sectors.size());
	xr_vector<GpuScene::GrassModel> Grass;

	for (const CDetail* Object : Details->objects)
	{
		Grass.push_back({ Object->shader._get(), Object->indices, Object->number_indices, Object->FarCount, Object->Alloc.VertexBase, Object->Alloc.Page });
	}

	Msg("* Game Loading Timer: %u ms at the scene's tables", loading_save_timer.GetElapsed_ms());
	Scene.Load(Visuals, Sectors, Grass);
	UploadLevelLeftovers();
	Msg("* Game Loading Timer: %u ms at the textures that waited", loading_save_timer.GetElapsed_ms());

	if (!ps_r__common_flags.test(RFLAG_DD_TEX_LOAD))
	{
		dxRenderDeviceRender::Instance().Resources->DeferredLoad(false);
		dxRenderDeviceRender::Instance().Resources->UploadPending();
	}

	Msg("* Game Loading Timer: %u ms at the end of the renderer's level", loading_save_timer.GetElapsed_ms());
	dxRenderDeviceRender::Instance().Resources->Manifest.Save();

	for (size_t Index = 0; Index < Grass.size(); ++Index)
	{
		Details->objects[Index]->SceneCluster = Grass[Index].Cluster;
	}

	// The scene was the last reader of the level's geometry chunks
	LevelVertices = {};
	LevelIndices = {};
	LevelMeshes = {};
	LevelMeshIds = {};
	LevelScratch = {};

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

		// Radians: one value turns the box about Y, three turn it about X, Y and Z
		float rotX = 0.0f, rotY = 0.0f, rotZ = 0.0f;
		const int rotArg = sscanf(ini.line_exist(sect_name, "rotation") ? ini.r_string(sect_name, "rotation") : "0", "%f , %f , %f", &rotX, &rotY, &rotZ);
		if (rotArg == 3)
		{
			m_puddle.m_world.setXYZi(rotX, rotY, rotZ);
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

void CRender::level_Unload()
{
	RenderThread::WaitIdle();
	// The pre-render stage runs in parallel with FrameMove, where a disconnect unloads the level.
	// It walks the sectors and static visuals deleted here.
	Device.PreRenderThread.Wait();
	ResetSunShadowCache();
	HeightfieldShadows.Destroy();
	if (0==g_pGameLevel)		return;
	if (!GraphMain.b_loaded)
	{
		return;
	}
	ForEachViewGraph([](R_dsgraph_structure& Graph)
	{
		Graph.r_dsgraph_clear_passes();
	});
	Scene.Unload();
	LampSeen.Destroy();
	VideoSettings::SetLevelLoaded(false);
	if (Target)
	{
		Target->InvalidateSpotShadowCache();
		Target->ClearSpotShadowCache();
	}
	if (o.ProceduralRainPuddles)
	{
		RainPuddleWater.Build();
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

	//*** Components
	if (L_Glows) L_Glows->Unload();
	xr_delete(L_Glows);
	xr_delete					(Details);
	xr_delete					(Wallmarks);

	//*** VB/IB
	Geometry.Trim();

	m_levels_puddles.resize(0);

	//*** ShadersLevel
	ShadersLevel.clear();
	ShadersShared.clear();
	GraphMain.b_loaded = false;
}

void CRender::LoadVertexBuffers(IReaderBase& fs)
{
	using XRay::Legacy::LEGACYVERTEXELEMENT9;
	xr_vector<LevelVertexChunk>& Chunks = LevelVertices;
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

u32 CRender::RegisterLevelMesh(u32 VertexBuffer, u32 VertexBase, u32 VertexCount, u32 IndexBuffer, u32 IndexBase, u32 IndexCount)
{
	if (VertexBuffer >= LevelVertices.size() || IndexBuffer >= LevelIndices.size() || u64(VertexBase) + VertexCount > LevelVertices[VertexBuffer].Count || u64(IndexBase) + IndexCount > LevelIndices[IndexBuffer].size())
	{
		FATAL(make_string<const char*>("Level geometry: visual references vertices %u+%u of buffer %u and indices %u+%u of buffer %u out of range", VertexBase, VertexCount, VertexBuffer, IndexBase, IndexCount, IndexBuffer));
	}
	const auto [Shared, Inserted] = LevelMeshIds.try_emplace({ VertexBuffer, VertexBase, VertexCount, IndexBuffer, IndexBase, IndexCount }, u32(LevelMeshes.size()));
	if (Inserted)
	{
		LevelMeshes.push_back({ Shared->first, {} });
	}
	return Shared->second;
}

void CRender::UploadLevelMesh(IRender_Mesh& Mesh)
{
	GeometryAlloc& Block = LevelMeshes[Mesh.LevelMesh].Block;
	if (Block.Ref)
	{
		Geometry.Retain(Block);
		Mesh.SetPulled(Block);
		return;
	}
	const xr_array<u32, 6>& Key = LevelMeshes[Mesh.LevelMesh].Key;
	const LevelVertexChunk& Chunk = LevelVertices[Key[0]];
	LevelScratch.resize(Key[2]);
	for (u32 Index = 0; Index < Key[2]; ++Index)
	{
		LevelScratch[Index] = LevelPacking::EncodeLevelVertex(Chunk, Chunk.Data.data() + size_t(Key[1] + Index) * Chunk.Stride);
	}
	Block = Geometry.Alloc(Key[2], Key[5]);
	Geometry.Upload(Block, 0, LevelScratch.data(), Key[2]);
	Geometry.UploadIndices(Block, 0, LevelIndices[Key[3]].data() + Key[4], Key[5]);
	Mesh.SetPulled(Block);
}

// The level's meshes are drawn from the scene's pages. Only what some view still draws from the heap gets a place in
// it: a visual the scene left to a graph, or every one where the scene drew none
void CRender::UploadLevelLeftovers()
{
	const bool IsScene = Scene.HasDepthBatch() && Scene.IsForwardBinned();
	xr_vector<IRender_Mesh*> Left;
	xr_vector<u8> IsCounted(LevelMeshes.size(), 0);
	u64 Vertices = 0;
	u64 Indices = 0;
	for (dxRender_Visual* Visual : Visuals)
	{
		IRender_Mesh* Mesh = dynamic_cast<IRender_Mesh*>(Visual);
		if (!Mesh || Mesh->LevelMesh == u32(-1))
		{
			continue;
		}
		Visual->IsPagesOnly = IsScene && Visual->IsDepthBatched && Visual->IsForwardBatched;
		if (Visual->IsPagesOnly)
		{
			continue;
		}
		Left.push_back(Mesh);
		if (!IsCounted[Mesh->LevelMesh])
		{
			IsCounted[Mesh->LevelMesh] = 1;
			Vertices += LevelMeshes[Mesh->LevelMesh].Key[2];
			Indices += LevelMeshes[Mesh->LevelMesh].Key[5] + 1;
		}
	}
	Geometry.Reserve(u32(std::min<u64>(Vertices, u32(-1))), u32(std::min<u64>(Indices, u32(-1))));
	for (IRender_Mesh* Mesh : Left)
	{
		UploadLevelMesh(*Mesh);
	}
	Geometry.Reserve(0, 0);
	Msg("* Level meshes in the heap: %u of %u visuals, %llu vertices of %u meshes", u32(Left.size()), u32(Visuals.size()), Vertices, u32(std::count(IsCounted.begin(), IsCounted.end(), u8(1))));
}

void CRender::LoadIndexBuffers(IReaderBase& fs)
{
	xr_vector<xr_vector<u16>>& Chunks = LevelIndices;
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

	while ((chunk=fs->open_chunk(index))!=nullptr)
	{
		chunk->r_chunk_safe			(OGF_HEADER,&H,sizeof(H));
		V = Models->Instance_Create	(H.type);
		xr_stack_string256 debug_name;
		if (auto data = chunk->open_chunk(OGF_DEBUG_DATA); data)
		{
			data->r_stringZ(debug_name);
			data->close();
		}
		V->Load(debug_name.empty() ? nullptr : debug_name.c_str(), chunk, 0);
		Visuals.push_back(V);

		chunk->close();
		index++;
	}
}

void CRender::LoadLights(IReader *fs)
{
	// lights
	Lights.Load	(fs);
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
