#include "stdafx.h"
#include "../xrRender/FBasicVisual.h"
#include "../../xrEngine/Fmesh.h"
#include "../../xrEngine/xrLevel.h"
#include "../../xrEngine/x_ray.h"
#include "../../xrEngine/IGame_Persistent.h"
#include "../../xrCore/stream_reader.h"

#include "../xrRender/dxRenderDeviceRender.h"

#pragma warning(push)
#pragma warning(disable:4995)

#include <FlexibleVertexFormat.h>

#include "../../xrCore/FormatParsers/LevelGeom/GeomIO.h"
#include "src/xrCore/SharedMaterialLibrary.h"
using namespace FVF;

#pragma warning(pop)

void CRender::level_Load(IReader *fs)
{
	R_ASSERT			(0!=g_pGameLevel);
	R_ASSERT			(!b_loaded);

	// Begin
	pApp->LoadBegin					();
	dxRenderDeviceRender::Instance().Resources->DeferredLoad(ps_r__common_flags.test(RFLAG_DD_TEX_LOAD));
	IReader*						chunk;

	// Shaders
	g_pGamePersistent->SetLoadStageTitle("st_loading_shaders");
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

	L_Shadows					= new CLightShadows		();
	L_Projector					= new CLightProjector	();
	L_DB						= new CLight_DB			();
	L_Glows						= new CGlowManager		();
	Wallmarks					= new CWallmarksEngine	();
	Details						= new CDetailManager	();

	rmFar						();
	rmNormal					();

	marker						= 0;

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
	//pApp->LoadTitle				("Loading lights...");
	LoadLights					(fs);

	// End
	pApp->LoadEnd				();
	b_loaded					= true	;
}

void CRender::level_Unload		()
{
	if (0==g_pGameLevel)		return;
	if (!b_loaded)				return;

	u32							I;

	// HOM
	HOM.Unload					();

	//*** Details
	Details->Unload				();

	//*** Sectors
	// 1.
	xr_delete					(rmPortals);
	pLastSector					= 0;
	pOutdoorSector				= 0;
	vLastCameraPos.set			(flt_max,flt_max,flt_max);
	uLastLTRACK					= 0;

	// 2.
	for (I=0; I<Sectors.size(); I++)	xr_delete(Sectors[I]);
	Sectors.clear();
	// 3.
	for (I=0; I<Portals.size(); I++)	xr_delete(Portals[I]);
	Portals.clear();

	//*** Lights
	L_Glows->Unload				();
	L_DB->Unload				();

	//*** Visuals
	for (I=0; I<Visuals.size(); I++)	{
		Visuals[I]->Release();
		xr_delete(Visuals[I]);
	}
	Visuals.clear();

	//*** SWI
	for (I=0; I<SWIs.size();I++)xr_free	(SWIs[I].sw);
	SWIs.clear					();

	//*** VB/IB
	for (I=0; I<VB.size(); I++)	_RELEASE(VB[I]);
	for (I=0; I<IB.size(); I++)	_RELEASE(IB[I]);
	DCL.clear();
	VB.clear();
	IB.clear();

	//*** Components
	xr_delete					(Details);
	xr_delete					(Wallmarks);
	xr_delete					(L_Glows);
	xr_delete					(L_DB);
	xr_delete					(L_Projector);
	xr_delete					(L_Shadows);

	//*** Shaders
	ShadersLevel.clear();
	ShadersShared.clear();

	b_loaded					= false;
}

void CRender::LoadVertexBuffers(IReaderBase& fs)
{
	// Vertex buffers
	u32 count				= fs.r_u32();
	DCL.resize				(count);
	VB.resize				(count);

	ReadVBChunk(VB, DCL, count, fs);
}

void CRender::LoadIndexBuffers(IReaderBase& fs)
{

	// Index buffers
	u32 count = fs.r_u32();
	IB.resize(count);

	for (u32 i = 0; i < count; i++)
	{
		u32 iCount = fs.r_u32();

		// ��������� ����� ��� ������ ��������
		std::vector<u16> tmpData(iCount);
		fs.r(tmpData.data(), iCount * sizeof(u16));

		RHIBufferDesc ibDesc{};
		ibDesc.Size = iCount * sizeof(u16);
		ibDesc.Type = ERHI_BUFFER_TYPE::INDEX;
		ibDesc.Usage = ERHI_USAGE::USAGE_DEFAULT;
		ibDesc.CPUAccessFlags = 0;

		RHIBufferSubresource ibInit{};
		ibInit.pSysMem = tmpData.data();

		IB[i] = GRHI->CreateBuffer(ibDesc, &ibInit);
	}
}

void CRender::LoadVisuals(IReader *fs)
{
	IReader*		chunk	= 0;
	u32				index	= 0;
	dxRender_Visual*	V		= 0;
	ogf_header		H;

	while ((chunk=fs->open_chunk(index))!=0)
	{
		chunk->r_chunk_safe			(OGF_HEADER,&H,sizeof(H));
		V = Models->Instance_Create	(H.type);
		V->Load						(0,chunk,0);
		Visuals.push_back			(V);

		chunk->close();
		index++;
	}
}

void CRender::LoadLights(IReader *fs)
{
	// lights
	L_DB->Load		(fs);

	// glows
	IReader			*chunk = fs->open_chunk(fsL_GLOWS);
	if (chunk == nullptr)
	{
		return;
	}

	L_Glows->Load	(chunk);
	chunk->close	();
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

	if (!g_dedicated_server)
	{
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
}

void CRender::LoadSWIs(IReaderBase& fs)
{
	// allocate memory for portals
	u32 item_count		= fs.r_u32();	

	xr_vector<FSlideWindowItem>::iterator it	= SWIs.begin();
	xr_vector<FSlideWindowItem>::iterator it_e	= SWIs.end();

	for(;it!=it_e;++it)
		xr_free( (*it).sw );

	SWIs.clear();
	SWIs.resize(item_count);

	for (u32 c=0; c<item_count; c++){
		FSlideWindowItem& swi = SWIs[c];
		swi.reserved[0]	= fs.r_u32();	
		swi.reserved[1]	= fs.r_u32();	
		swi.reserved[2]	= fs.r_u32();	
		swi.reserved[3]	= fs.r_u32();	
		swi.count		= fs.r_u32();	
		VERIFY			(nullptr==swi.sw);
		swi.sw			= xr_alloc<FSlideWindow> (swi.count);
		fs.r			(swi.sw,sizeof(FSlideWindow)*swi.count);
	}
}
