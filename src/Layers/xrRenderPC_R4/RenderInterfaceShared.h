#pragma once
#include "LegacyVertexElement.h"

IRenderVisual* CRender::getVisual(int id)
{
	VERIFY(id<int(Visuals.size()));
	return Visuals[id];
}

ref_shader CRender::getShader(int id)
{
	VERIFY(id<int(Shaders.size()));
	return Shaders[id];
}

IRender_Portal* CRender::getPortal(int id)
{
	VERIFY(id<int(Portals.size()));
	return Portals[id];
}

IRender_Sector* CRender::getSector(int id)
{
	if (id >= 0 && id<int(Sectors.size()))
		return Sectors[id];

	return nullptr;
}

IRender_Sector* CRender::getSectorActive()
{
	return pLastSector;
}

void CRender::models_Prefetch()
{
	Models->Prefetch();
}

void CRender::models_Clear(bool b_complete)
{
	Models->ClearPool(b_complete);
}

bool CRender::occ_visible(vis_data& P)
{
	if (Device.IsEditorMode())
	{
		return occ_visible(P.box);
	}
	return HOM.visible(P);
}

bool CRender::occ_visible(sPoly& P)
{
	if (Device.IsEditorMode())
	{
		return ViewBase.testPolyInside(P);
	}
	return HOM.visible(P);
}

bool CRender::occ_visible(Fbox& P)
{
	if (Device.IsEditorMode())
	{
		u32 Mask = 0xff;
		if (!ViewBase.testAABB(P.data(), Mask))
		{
			return false;
		}

		return GraphMain.View != nullptr || !EditorOcclusionCulling || EditorHiZ.IsVisible(P);
	}
	return HOM.visible(P);
}

void CRender::flush()
{
	GraphMain.r_dsgraph_render_graph(0);
}

void CRender::add_SkeletonWallmark(intrusive_ptr<CSkeletonWallmark> wm)
{
	if (Wallmarks == nullptr)
	{
		return;
	}
	Wallmarks->AddSkeletonWallmark(wm);
}

void CRender::add_SkeletonWallmark(const Fmatrix* xf, CKinematics* obj, ref_shader& sh, const Fvector& start, const Fvector& dir, float size)
{
	if (Wallmarks == nullptr)
	{
		return;
	}
	Wallmarks->AddSkeletonWallmark(xf, obj, sh, start, dir, size);
}

void CRender::add_SkeletonWallmark(const Fmatrix* xf, IKinematics* obj, IWallMarkArray* pArray, const Fvector& start, const Fvector& dir, float size)
{
	dxWallMarkArray* pWMA = (dxWallMarkArray*)pArray;
	ref_shader* pShader = pWMA->dxGenerateWallmark();
	if (pShader)
	{
		add_SkeletonWallmark(xf, (CKinematics*)obj, *pShader, start, dir, size);
	}
}

void CRender::clear_static_wallmarks()
{
	if (Wallmarks == nullptr)
	{
		return;
	}
	Wallmarks->clear();
}

void CRender::add_Occluder(Fbox2& bb_screenspace)
{
	if (Device.IsEditorMode())
	{
		return;
	}
	HOM.occlude(bb_screenspace);
}

void CRender::add_StaticWallmark(ref_shader& S, const Fvector& P, float s, CDB::TRI* T, Fvector* verts, bool UseCameraDirection)
{
	if (T->suppress_wm)
	{
		return;
	}

	VERIFY2(_valid(P) && _valid(s) && T && verts && (s > EPS_L), "Invalid static wallmark params");
	Wallmarks->AddStaticWallmark(T, verts, P, &*S, s, Flags8(StaticWallmarkHandle::flTimeToLive), UseCameraDirection);
}

void CRender::add_StaticWallmark(IWallMarkArray* pArray, const Fvector& P, float s, CDB::TRI* T, Fvector* V, bool UseCameraDirection)
{
	dxWallMarkArray* pWMA = (dxWallMarkArray*)pArray;
	ref_shader* pShader = pWMA->dxGenerateWallmark();
	if (pShader)
	{
		add_StaticWallmark(*pShader, P, s, T, V, UseCameraDirection);
	}
}

StaticWallmarkHandle::WallmarkHandlePtr CRender::add_DynamicWallmark(const wm_shader& S, const Fvector& P, float w, float h, float r, CDB::TRI* T, Fvector* V)
{
	if (T->suppress_wm)
	{
		R_ASSERT2(!T->suppress_wm, "Unable to add dynamic wallmark!");
		return nullptr;
	}

	VERIFY2(_valid(P), "Invalid dynamic wallmark position");
	VERIFY2(_valid(w) && (w > EPS_L), "Invalid dynamic wallmark width");
	VERIFY2(_valid(h) && (h > EPS_L), "Invalid dynamic wallmark height");
	VERIFY2(_valid(r), "Invalid dynamic wallmark rotation");
	VERIFY2(T && V, "Invalid static wallmark params");

	dxUIShader* pShader = (dxUIShader*)&*S;
	auto wm = Wallmarks->AddStaticWallmark(T, V, P, pShader->hShader, w, h, r, Flags8(StaticWallmarkHandle::flHandler | StaticWallmarkHandle::flForceSpawn));
	VERIFY(wm);

	return wm->handler;
}

void CRender::add_StaticWallmark(const wm_shader& S, const Fvector& P, float s, CDB::TRI* T, Fvector* V)
{
	dxUIShader* pShader = (dxUIShader*)&*S;
	add_StaticWallmark(pShader->hShader, P, s, T, V);
}

IRender_Target* CRender::getTarget()
{
	return Target;
}

void CRender::add_Visual(IRenderVisual* V, bool Ignore, void* graph)
{
	TargetGraph(graph).add_leafs_Dynamic((dxRender_Visual*)V, Ignore);
}

void CRender::rmNear()
{
	IRender_Target* T = getTarget();
	const float W = T ? (float)T->get_width() : (float)Device.TargetWidth;
	const float H = T ? (float)T->get_height() : (float)Device.TargetHeight;
	RHIViewport VP = { 0, 0, W, H, 0, 0.02f };
	RImplementation.Imm().SetViewport(VP);
}

void CRender::rmFar()
{
	IRender_Target* T = getTarget();
	const float W = T ? (float)T->get_width() : (float)Device.TargetWidth;
	const float H = T ? (float)T->get_height() : (float)Device.TargetHeight;
	RHIViewport VP = { 0, 0, W, H, 0.99999f, 1.f };
	RImplementation.Imm().SetViewport(VP);
}

void CRender::rmNormal()
{
	IRender_Target* T = getTarget();
	const float W = T ? (float)T->get_width() : (float)Device.TargetWidth;
	const float H = T ? (float)T->get_height() : (float)Device.TargetHeight;
	RHIViewport VP = { 0, 0, W, H, 0, 1.f };
	RImplementation.Imm().SetViewport(VP);
}
