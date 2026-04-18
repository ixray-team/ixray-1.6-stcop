#include "stdafx.h"

#include "xr_area.h"
#define ENGINE_API
#include "../xrEngine/xr_object.h"
//#include "../xrEngine/xrLevel.h"
#include "../xrEngine/xr_collide_form.h"

using namespace	collide;

//----------------------------------------------------------------------
// Class	: CObjectSpace
// Purpose	: stores space slots
//----------------------------------------------------------------------
CObjectSpace::CObjectSpace()
#ifdef DEBUG
	: m_pRender(0)
#endif
{
#ifdef DEBUG
	if (RenderFactory)
		m_pRender = new FactoryPtr<IObjectSpaceRender>();
#endif
	m_BoundingVolume.invalidate();
}

//----------------------------------------------------------------------
CObjectSpace::~CObjectSpace()
{
#ifdef DEBUG
	xr_delete(m_pRender);
#endif
}
//----------------------------------------------------------------------

//----------------------------------------------------------------------
void CObjectSpace::Load(CDB::build_callback build_callback)
{
	
	Load("$level$", "level", build_callback);
}

void CObjectSpace::Load(const char* initial, const char* fname, CDB::build_callback build_callback, bool NotFromLevel)
{
	xr_string Filename;
	auto CFormData = XRay::CForm::Read(initial, fname);
	if (!I_ASSERT(CFormData))
	{
		FATAL("Attempt to load level with invalid collision data!");
		return;
	}

	xr_stack_string_path LevelName;
	auto LevelPath = FS.get_path("$level$")->m_Add;
	IReader* pReaderCache = nullptr;

	if (LevelPath != nullptr)
	{
		LevelName.append("level_cache\\");
		LevelName.append(LevelPath);
		LevelName.append("cform.cache");
		pReaderCache = CDB::GetModelCache(LevelName, CFormData->GetFileHash());
	}
	
	if (pReaderCache != nullptr)
	{
		// Just restore
		Create(*CFormData, build_callback, pReaderCache, true);
	}
	else
	{
		IWriter* pWriterCache = FS.w_open("$app_data_root$", LevelName.c_str());
		pWriterCache->w_u32(CFormData->GetFileHash());
		Create(*CFormData, build_callback, pWriterCache, false);
	}
	
	/*IReader* F = FS.r_open(path, fname);
	R_ASSERT(F);
	Load(F, build_callback);*/
}

/*void CObjectSpace::Load(IReader* F, CDB::build_callback build_callback)
{
	hdrCFORM H;

	// Cache for cform
	string_path LevelName = {};
	u32 crc = crc32(F->pointer(), F->length());
	auto LevelPath = FS.get_path("$level$")->m_Add;
	IReader* pReaderCache = nullptr;

	if (LevelPath != nullptr)
	{
		xr_strconcat(LevelName, "level_cache\\", LevelPath, "cform.cache"); 
		pReaderCache = CDB::GetModelCache(LevelName, crc);
	}

	F->r(&H, sizeof(hdrCFORM));
	Fvector* verts = (Fvector*)F->pointer();
	CDB::TRI* tris = (CDB::TRI*)(verts + H.vertcount);
	
	if (pReaderCache != nullptr)
	{
		// Just restore
		Create(verts, tris, H, build_callback, pReaderCache, true);
	}
	else
	{
		IWriter* pWriterCache = FS.w_open("$app_data_root$", LevelName);
		pWriterCache->w_u32(crc);
		Create(verts, tris, H, build_callback, pWriterCache, false);
	}
	
	FS.r_close(F);
}*/

/*void CObjectSpace::Create(Fvector* verts, CDB::TRI* tris, const hdrCFORM& H, CDB::build_callback build_callback, void* pRW, bool RWMode)
{
	R_ASSERT(CFORM_CURRENT_VERSION == H.version);
	Static.build(verts, H.vertcount, tris, H.facecount, build_callback, nullptr, pRW, RWMode);

	m_BoundingVolume.set(H.aabb);

	g_SpatialSpace->initialize(m_BoundingVolume);
	g_SpatialSpacePhysic->initialize(m_BoundingVolume);
}*/

void CObjectSpace::Create(const XRay::CForm::IFormat& Data, CDB::build_callback build_callback, void* pRW, bool RWMode)
{
	auto& H = Data.GetHeader();
	switch (H.version)
	{
	case CFormVersions::Vanilla:
	case CFormVersions::VanillaChunked:
		{
			xr_vector<Fvector>& Verts = Static.get_verts();
			xr_vector<CDB::TRI>& Tris = Static.get_tris();
			Data.GetStaticGeom(Verts, Tris);
			Static.build(Verts.data(), Verts.size(), Tris.data(), Tris.size(), build_callback, nullptr, pRW, RWMode);
			break;
		}
	default:
		{
			FATAL("Invalid CForm version!");
		}
	}
	
	m_BoundingVolume.set(H.aabb);

	g_SpatialSpace->initialize(m_BoundingVolume);
	g_SpatialSpacePhysic->initialize(m_BoundingVolume);
}

//----------------------------------------------------------------------
#ifdef DEBUG
void CObjectSpace::dbgRender()
{
	(*m_pRender)->dbgRender();
}
/*
void CObjectSpace::dbgRender()
{
	R_ASSERT(bDebug);

	RCache.set_Shader(sh_debug);
	for (u32 i=0; i<q_debug.boxes.size(); i++)
	{
		Fobb&		obb		= q_debug.boxes[i];
		Fmatrix		X,S,R;
		obb.xform_get(X);
		RCache.dbg_DrawOBB(X,obb.m_halfsize,color_xrgb(255,0,0));
		S.scale		(obb.m_halfsize);
		R.mul		(X,S);
		RCache.dbg_DrawEllipse(R,color_xrgb(0,0,255));
	}
	q_debug.boxes.clear();

	for (i=0; i<dbg_S.size(); i++)
	{
		std::pair<Fsphere,u32>& P = dbg_S[i];
		Fsphere&	S = P.first;
		Fmatrix		M;
		M.scale		(S.R,S.R,S.R);
		M.translate_over(S.P);
		RCache.dbg_DrawEllipse(M,P.second);
	}
	dbg_S.clear();
}
*/
#endif
