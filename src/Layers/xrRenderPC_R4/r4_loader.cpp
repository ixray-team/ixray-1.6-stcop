#include "stdafx.h"
#include "r4.h"
#include "../xrRender/ResourceManager.h"
#include "../xrRender/FBasicVisual.h"
#include "../../xrEngine/Fmesh.h"
#include "../../xrEngine/xrLevel.h"
#include "../../xrEngine/x_ray.h"
#include "../../xrEngine/IGame_Persistent.h"
#include "../../xrCore/stream_reader.h"

#include "../xrRender/dxRenderDeviceRender.h"

#include "../xrRenderDX10/dx10BufferUtils.h"
#include "../xrRenderDX10/3DFluid/dx103DFluidVolume.h"

#include "../xrRender/FHierrarhyVisual.h"

#pragma warning(push)
#pragma warning(disable:4995)
#include <malloc.h>

#include <FlexibleVertexFormat.h>

#include "../../xrCore/FormatParsers/LevelGeom/GeomIO.h"
using namespace FVF;

#pragma warning(pop)

void CRender::level_Load(IReader* fs)
{
	R_ASSERT						(0!=g_pGameLevel);
	R_ASSERT						(!b_loaded);

	// Begin
	pApp->LoadBegin					();
	dxRenderDeviceRender::Instance().Resources->DeferredLoad	(TRUE);
	IReader*						chunk;

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
			LPCSTR			n		= LPCSTR(chunk->pointer());
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
			LoadVertexBuffers(Geom->GetVBData(), false);
			LoadIndexBuffers(Geom->GetIBData(), false);
			LoadSWIs(Geom->GetSWIData());
		}
		
		//...and alternate/fast geometry
		{
			auto Geom = XRay::Geom::Read("$level$","level", ".geomx");
			if (!I_ASSERT(Geom))
			{
				FATAL("Unable to load geometry file");
			}
		
			dxRenderDeviceRender::Instance().Resources->Evict();
			LoadVertexBuffers(Geom->GetVBData(), true);
			LoadIndexBuffers(Geom->GetIBData(), true);
		}

		// Visuals
		g_pGamePersistent->SetLoadStageTitle("st_loading_spatial_db");
		g_pGamePersistent->LoadTitle();
		chunk						= fs->open_chunk(fsL_VISUALS);
		LoadVisuals					(chunk);
		chunk->close				();

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

	// Lights
	// pApp->LoadTitle			("Loading lights...");
	LoadLights					(fs);
	LoadPuddles					();

	// End
	pApp->LoadEnd				();

	// sanity-clear
	lstLODs.clear				();
	lstLODgroups.clear			();
	mapLOD.clear				();

	// signal loaded
	b_loaded					= TRUE	;
}

void CRender::LoadPuddles()
{
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
		float rotation = ini.r_float(sect_name, "rotation");

		m_puddle.m_world.rotateY(rotation);
		m_puddle.m_world.mulB_43(Fmatrix().scale(size_xz.x, 1.0f, size_xz.y));

		m_puddle.m_world.translate_over(position);

		m_puddle.m_height = max_height;
		m_puddle.m_radius = size_xz.magnitude();
	}
}

void CRender::level_Unload()
{
	if (0==g_pGameLevel)		return;
	if (!b_loaded)				return;

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
	for (I=0; I<nVB.size(); I++)	_RELEASE(nVB[I]);
	for (I=0; I<xVB.size(); I++)	_RELEASE(xVB[I]);
	nVB.clear(); xVB.clear();
	for (I=0; I<nIB.size(); I++)	_RELEASE(nIB[I]);
	for (I=0; I<xIB.size(); I++)	_RELEASE(xIB[I]);
	nIB.clear(); xIB.clear();
	nDC.clear(); xDC.clear();

	//*** Components
	xr_delete					(Details);
	xr_delete					(Wallmarks);

	m_levels_puddles.resize(0);

	//*** Shaders
	Shaders.clear();
	b_loaded					= FALSE;
}

void CRender::LoadVertexBuffers(IReaderBase& fs, bool _alternative)
{
	xr_vector<VertexDeclarator> &_DC	= _alternative?xDC:nDC;
	xr_vector<IRHIBuffer*>		&_VB	= _alternative?xVB:nVB;
	
	// Vertex buffers
	u32 count = fs.r_u32();
	_DC.resize(count);
	_VB.resize(count);

	ReadVBChunk(_VB, _DC, count, fs);
}

void CRender::LoadIndexBuffers(IReaderBase& fs, bool _alternative)
{
	xr_vector<IRHIBuffer*>& _IB	= _alternative?xIB:nIB;

	// Index buffers
	u32 count = fs.r_u32();
	_IB.resize(count);
	for (u32 i=0; i<count; i++)
	{
		u32 iCount = fs.r_u32();

		//	TODO: DX10: Check fragmentation.
		//	Check if buffer is less then 2048 kb
		BYTE* pData = xr_alloc<BYTE>(iCount*2);
		fs.r(pData,iCount*2);
		RHIUtils::CreateIndexBuffer(&_IB[i], pData, iCount*2);
		xr_free(pData);
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
	svector<Fvector,6>	vertices;
};

void CRender::LoadSectors(IReader* fs)
{
	// allocate memory for portals
	u32 size = fs->find_chunk(fsL_PORTALS);
	R_ASSERT(0 == size % sizeof(b_portal));
	u32 count = size / sizeof(b_portal);
	Portals.resize(count);

	for (u32 c = 0; c < count; c++)
		Portals[c] = new CPortal();

	// load sectors
	IReader* S = fs->open_chunk(fsL_SECTORS);
	u32 crc = crc32(S->pointer(), S->length());

	for (u32 i = 0; ; i++)
	{
		IReader* P = S->open_chunk(i);
		if (0 == P) break;

		CSector* __S = new CSector();
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