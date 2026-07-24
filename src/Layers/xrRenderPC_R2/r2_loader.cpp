#include "stdafx.h"
#include "r2.h"
#include "Layers/xrRender/ResourceManager.h"
#include "Layers/xrRender/FBasicVisual.h"
#include "xrEngine/FmeshRender.h"
#include "xrEngine/xrLevel.h"
#include "xrEngine/x_ray.h"
#include "xrEngine/IGame_Persistent.h"

#include "Layers/xrRender/dxRenderDeviceRender.h"

#pragma warning(push)
#pragma warning(disable:4995)

#include "xrCore/FormatParsers/LevelGeom/GeomIO.h"
#include "xrCore/Collision/override/Model.h"
using namespace FVF;

#pragma warning(pop)

void CRender::level_Load(IReader* fs)
{
	R_ASSERT						(0!=g_pGameLevel);
	R_ASSERT						(!b_loaded);

	// Begin
	pApp->LoadBegin					();
	dxRenderDeviceRender::Instance().Resources->DeferredLoad(ps_r__common_flags.test(RFLAG_DD_TEX_LOAD));
	IReader*						chunk;

	// Shaders
	g_pGamePersistent->SetLoadStageTitle		("st_loading_shaders");
	g_pGamePersistent->LoadTitle		();
	{
		chunk = fs->open_chunk		(fsL_SHADERS);
		R_ASSERT2					(chunk,"Level doesn't builded correctly.");
		u32 count = chunk->r_u32	();
		ShadersLevel.resize				(count);
		for(u32 i=0; i<count; i++)	// skip first shader as "reserved" one
		{
			string512				n_sh,n_tlist;
			str_c			n		= str_c(chunk->pointer());
			chunk->skip_stringZ		();
			if (0==n[0])			continue;
			xr_strcpy					(n_sh,n);
			LPSTR			delim	= strchr(n_sh,'/');
			*delim					= 0;
			xr_strcpy					(n_tlist,delim+1);
			ShadersLevel[i]				= dxRenderDeviceRender::Instance().Resources->Create(n_sh,n_tlist);
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
		{
			auto Geom = XRay::Geom::Read("$level$","level", ".geomx");
			if (!I_ASSERT(Geom))
			{
				FATAL("Unable to load fast geometry file");
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

	// HOM
	HOM.Load					();

	// Lights
	// pApp->LoadTitle			("Loading lights...");
	LoadLights					(fs);

	// End
	pApp->LoadEnd				();

	// sanity-clear
	lstLODs.clear				();
	lstLODgroups.clear			();
	mapLOD.clear				();

	// signal loaded
	b_loaded					= true	;
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
	for (auto& elem : Visuals)
	{
		elem->Release();
		xr_delete(elem);
	}
	Visuals.clear			();

	//*** SWI
	for (auto& elem : nGlobalData.SWIs)
	{
		xr_free(elem.sw);
	}
	nGlobalData.SWIs.clear();

	//*** VB/IB
	for (auto& elem : nGlobalData.VB)
	{
		_RELEASE(elem);
	}

	for (auto& elem : xGlobalData.VB)	
	{
		_RELEASE(elem);
	}
	nGlobalData.VB.clear(); xGlobalData.VB.clear();

	for (auto& elem : nGlobalData.IB)	
	{
		_RELEASE(elem);
	}

	for (auto& elem : xGlobalData.IB)	
	{
		_RELEASE(elem);
	}

	nGlobalData.IB.clear(); xGlobalData.IB.clear();
	nGlobalData.DCL.clear(); xGlobalData.DCL.clear();

	//*** Components
	xr_delete					(Details);
	xr_delete					(Wallmarks);

	//*** Shaders
	ShadersLevel.clear();
	ShadersShared.clear();
	b_loaded					= false;
/*	
	Models->ClearPool( true );
	Visuals.clear_and_free();
	dxRenderDeviceRender::Instance().Resources->Dump(false);
	static int unload_counter = 0;
	Msg("The Level Unloaded.======================== %d", ++unload_counter);
*/
}

void CRender::LoadVertexBuffers(IReaderBase& fs, bool _alternative)
{

	xr_vector<VertexDeclarator>& _DC = _alternative ? xGlobalData.DCL : nGlobalData.DCL;
	xr_vector<IRHIBuffer*>& _VB = _alternative ? xGlobalData.VB : nGlobalData.VB;

	// Vertex buffers
	{
		// Use DX9-style declarators
		u32 count = fs.r_u32();
		_DC.resize(count);
		_VB.resize(count);
		ReadVBChunk(_VB, _DC, count, fs);
	}
}

void CRender::LoadIndexBuffers(IReaderBase& fs, bool _alternative)
{
	xr_vector<IRHIBuffer*>& _IB = _alternative ? xGlobalData.IB : nGlobalData.IB;
	ReadIBChunk(_IB, fs);
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
		xr_stack_string256 debug_name;
		if (auto data = chunk->open_chunk(OGF_DEBUG_DATA); data)
		{
			data->r_stringZ(debug_name);
			data->close();
		}
		V->Load(debug_name.empty() ? nullptr : debug_name.c_str(),chunk,0);
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

void CRender::LoadSectors(IReader* fs) {
	// allocate memory for portals
	u32 size = fs->find_chunk(fsL_PORTALS); 
	R_ASSERT(0==size%sizeof(b_portal));
	u32 count = size/sizeof(b_portal);
	Portals.resize	(count);
	for (u32 c=0; c<count; c++)
		Portals[c]	= new CPortal ();

	// load sectors
	IReader* S = fs->open_chunk(fsL_SECTORS);
	u32 crc = crc32(S->pointer(), S->length());

	for (u32 i = 0; ; i++)
	{
		IReader* P = S->open_chunk(i);
		if (0 == P) break;

		CSector* __S		= new CSector ();
		__S->load			(*P);
		Sectors.push_back	(__S);

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

		// build portal model
		rmPortals = new CDB::MODEL();
		rmPortals->verts = CL.verts;
		rmPortals->tris = CL.faces;
		rmPortals->build_simple();
	}
	else
	{
		rmPortals = nullptr;
	}

	pLastSector = nullptr;

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
	ReadSWIsChunk(nGlobalData.SWIs, fs);
}
