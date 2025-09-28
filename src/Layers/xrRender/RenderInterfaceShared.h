#pragma once

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

void CRender::models_Clear(BOOL b_complete)
{
	Models->ClearPool(b_complete);
}

BOOL CRender::occ_visible(vis_data& P)
{
	return HOM.visible(P);
}

BOOL CRender::occ_visible(sPoly& P)
{
	return HOM.visible(P);
}

BOOL CRender::occ_visible(Fbox& P)
{
	return HOM.visible(P);
}

void CRender::flush()
{
	r_dsgraph_render_graph(0);
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
	HOM.occlude(bb_screenspace);
}

void CRender::add_StaticWallmark(ref_shader& S, const Fvector& P, float s, CDB::TRI* T, Fvector* verts, bool UseCameraDirection)
{
	if (T->suppress_wm)
	{
		return;
	}

	VERIFY2(_valid(P) && _valid(s) && T && verts && (s > EPS_L), "Invalid static wallmark params");
	Wallmarks->AddStaticWallmark(T, verts, P, &*S, s, UseCameraDirection);
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

void CRender::add_StaticWallmark(const wm_shader& S, const Fvector& P, float s, CDB::TRI* T, Fvector* V)
{
	dxUIShader* pShader = (dxUIShader*)&*S;
	add_StaticWallmark(pShader->hShader, P, s, T, V);
}

IRender_Target* CRender::getTarget()
{
	return Target;
}

void CRender::add_Visual(IRenderVisual* V)
{
	add_leafs_Dynamic((dxRender_Visual*)V);
}

void CRender::add_Geometry(IRenderVisual* V)
{
	add_Static((dxRender_Visual*)V, View->getMask());
}

void CRender::rmNear()
{
	IRender_Target* T = getTarget();
	RHIViewport VP = { 0, 0, (float)T->get_width(), (float)T->get_height(), 0, 0.02f };
	GRHI->SetViewport(VP);
}

void CRender::rmFar()
{
	IRender_Target* T = getTarget();
	RHIViewport VP = { 0, 0, (float)T->get_width(),(float)T->get_height(), 0.99999f, 1.f };
	GRHI->SetViewport(VP);
}

void CRender::rmNormal()
{
	IRender_Target* T = getTarget();
	RHIViewport VP = { 0, 0, (float)T->get_width(), (float)T->get_height(), 0, 1.f };
	GRHI->SetViewport(VP);
}