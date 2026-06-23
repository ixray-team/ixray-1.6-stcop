#include "stdafx.h"

#include "xr_area.h"
#define ENGINE_API
#include "../xrEngine/xr_object.h"
//#include "../xrEngine/xrLevel.h"
#include "../xrEngine/xr_collide_form.h"
#include "override/Model.h"

using namespace	collide;

//----------------------------------------------------------------------
// Class	: CObjectSpace
// Purpose	: stores space slots
//----------------------------------------------------------------------
CObjectSpace::CObjectSpace()
#ifdef DEBUG
	: m_pRender(nullptr)
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
		
	Create(*CFormData, build_callback);
}

void CObjectSpace::Create(const XRay::CForm::IFormat& Data, CDB::build_callback build_callback)
{
	Data.ReadData(Static, build_callback, nullptr);
	
	m_BoundingVolume.set(Data.GetHeader().aabb);

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
