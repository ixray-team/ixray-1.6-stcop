#include "stdafx.h"

#include "xr_area.h"
#include "../xrEngine/xr_object.h"
#include "../xrEngine/xrLevel.h"
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
		m_pRender = CNEW(FactoryPtr<IObjectSpaceRender>)();
#endif
	m_BoundingVolume.invalidate();
}

//----------------------------------------------------------------------
CObjectSpace::~CObjectSpace	( )
{
#ifdef DEBUG
	//sh_debug.destroy			();
	CDELETE(m_pRender);
#endif
}
//----------------------------------------------------------------------

//----------------------------------------------------------------------
int CObjectSpace::GetNearest		( xr_vector<ISpatial*>& q_spatial, xr_vector<CObject*>&	q_nearest, const Fvector &point, float range, CObject* ignore_object )
{
	q_spatial.clear();
	// Query objects
	q_nearest.clear();
	Fsphere				Q;	Q.set	(point,range);
	Fvector				B;	B.set	(range,range,range);
	g_SpatialSpace->q_box(q_spatial,0,STYPE_COLLIDEABLE,point,B);

	// Iterate
	xr_vector<ISpatial*>::iterator	it	= q_spatial.begin	();
	xr_vector<ISpatial*>::iterator	end	= q_spatial.end		();
	for (; it!=end; it++)		{
		CObject* O				= (*it)->dcast_CObject		();
		if (0==O)				continue;
		if (O==ignore_object)	continue;
		Fsphere mS				= { O->spatial.sphere.P, O->spatial.sphere.R	};
		if (Q.intersect(mS))	q_nearest.push_back(O);
	}

	return (int)q_nearest.size();
}
//----------------------------------------------------------------------
IC int CObjectSpace::GetNearest(xr_vector<CObject*>& q_nearest, ICollisionForm* obj, float range)
{
	CObject* O = obj->Owner();
	return GetNearest(q_nearest, O->spatial.sphere.P, range + O->spatial.sphere.R, O);
}

//----------------------------------------------------------------------
void CObjectSpace::Load(CDB::build_callback build_callback)
{
	Load("$level$", "level.cform", build_callback);
}

void CObjectSpace::Load(LPCSTR path, LPCSTR fname, CDB::build_callback build_callback)
{
	IReader* F = FS.r_open(path, fname);
	R_ASSERT(F);
	Load(F, build_callback);
}

void CObjectSpace::Load(IReader* F, CDB::build_callback build_callback)
{
	hdrCFORM H;

	// Cache for cform
	string_path LevelName;
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
		if (pReaderCache != nullptr)
		{
			FS.r_close(pReaderCache);
		}

		IWriter* pWriterCache = FS.w_open("$app_data_root$", LevelName);
		pWriterCache->w_u32(crc);
		Create(verts, tris, H, build_callback, pWriterCache, false);
	}
	
	FS.r_close(F);
}

void CObjectSpace::Create(Fvector* verts, CDB::TRI* tris, const hdrCFORM& H, CDB::build_callback build_callback, void* pRW, bool RWMode)
{
	R_ASSERT(CFORM_CURRENT_VERSION == H.version);
	Static.build(verts, H.vertcount, tris, H.facecount, build_callback, nullptr, pRW, RWMode);

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
