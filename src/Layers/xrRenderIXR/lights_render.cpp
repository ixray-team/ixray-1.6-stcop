#include "stdafx.h"
#include "../../xrEngine/xr_object.h"

GraphView CRender::MakeLampView(const light& L) const
{
	GraphView View = MakeGraphView(PHASE_SMAP);
	View.SetSubspace(L.SpatialComponent->sector, L.X.S.combine, L.position);
	View.PassMask[1] = ps_r2_ls_flags_ext.test(R4FLAG_TSHDOWS);
	return View;
}

void CRender::TraverseShadowView(R_dsgraph_structure& Graph) const
{
	Graph.Traverse();

	if (Scene.IsShadowBinned(Graph))
	{
		return;
	}

	Graph.SkipDepthBatched = Scene.HasDepthBatch();
	Graph.CaptureStatic();
	Graph.SortStatic();
	Graph.SkipDepthBatched = false;
}

// A lamp part's map in its place in the atlas, see light::X.S and light::vis.smap_ID; returns whether it has a map to
// read. One drawn whole has the level as a batch and the statics the batch leaves in its graph; a decor lamp casts its
// decor objects alone. What moves is cast out of the frame's table with the other parts of the page (QueueLampTile)
bool CRender::RenderLampShadow(GfxCmdList& Cmd, CRenderTarget::SpotShadowPart& Part, u32 Index)
{
	if (!Target->CheckSpotShadowGraph() || Part.IsEmpty)
	{
		++stats.s_finalclip;
		return false;
	}

	phase = PHASE_SMAP;

	if (!Part.IsDirect)
	{
		const bool IsDrawn = Target->DrawSpotShadow(Cmd, Part, Index);
		++(IsDrawn ? stats.s_merged : stats.s_finalclip);
		return IsDrawn;
	}

	light& L = *Part.Light;
	const GraphView View = MakeLampView(L);
	const bool HasLevel = View.Sector && !Part.IsDecor;
	GraphPass.Begin(View);

	// The part's portals were traversed by LampCollect: walked again only for the statics a level leaves to the graph
	if (HasLevel && !Scene.IsShadowBinned(GraphPass))
	{
		PROF_EVENT("LampPortals");
		TraverseShadowView(GraphPass);
	}

	Scene.InsertLampLeftovers(GraphPass, Index);
	++stats.s_merged;
	Target->phase_smap_spot(Cmd, &L);

	if (HasLevel)
	{
		PROF_EVENT("LampDepthBatch");
		const u16 Page = L.vis.smap_ID;
		const RHIViewport Area = { float(L.X.S.posX), float(L.X.S.posY), float(L.X.S.size), float(L.X.S.size), 0.0f, 1.0f };
		GpuScene::DepthDraw Draw = { Target->SmapPage(Page), Page % CRenderTarget::SmapPages, &Area };
		Draw.Eye = &L.position;
		Draw.Sectors = LampTiles[Index].Sectors;
		Draw.IsSpot = true;
		Scene.DrawDepth(Cmd, GraphPass, Draw);
	}

	const bool IsNear = L.SpatialComponent->sphere.P.distance_to_sqr(RFrame.vCameraPosition) <= _sqr(40.f + L.SpatialComponent->sphere.R);
	DrawLampGraph(Cmd, L, !Part.IsDecor && !Part.IsUnseen && IsNear, ShadowKind::Lamp);
	QueueLampTile(Cmd, Part, Index);
	return true;
}

// The editors have no sectors and no table of moving objects: their scene is submitted into the part's graph, which draws
// all of it
bool CRender::RenderEditorLampShadow(GfxCmdList& Cmd, light& L)
{
	if (!Target->CheckSpotShadowGraph())
	{
		++stats.s_finalclip;
		return false;
	}

	phase = PHASE_SMAP;
	GraphPass.Begin(MakeLampView(L));
	EditorRenderSubspace(Cmd, GraphPass);

	if (!GraphPass.HasPasses(0) && !GraphPass.HasPasses(1) && GraphPass.mapSorted.empty())
	{
		++stats.s_finalclip;
		return false;
	}

	++stats.s_merged;
	Target->phase_smap_spot(Cmd, &L);
	DrawLampGraph(Cmd, L, L.SpatialComponent->sphere.P.distance_to_sqr(RFrame.vCameraPosition) <= _sqr(40.f + L.SpatialComponent->sphere.R), ShadowKind::Lamp);
	return true;
}

// What a part's graph holds, its grass near the eye, then the translucent casters the batch left: depth alone, as the rest
void CRender::DrawLampGraph(GfxCmdList& Cmd, light& L, bool HasGrass, ShadowKind Kind)
{
	const u16 Page = L.vis.smap_ID;

	Scene.CountGraph(Kind, GraphPass);
	Cmd.SetObjectTransform(Fidentity);
	Cmd.SetView(L.X.S.view, L.X.S.project);

	if (GraphPass.HasPasses(0))
	{
		PROF_EVENT("LampGraph");
		GraphPass.r_dsgraph_render_graph(0);
	}

	// Only a lamp whose reach holds grass: a cull of every blade a part is what the grass of a lamp costs
	if (HasGrass && ps_r2_ls_flags.test(R2FLAG_LIGHTS_DETAILS) && psDeviceFlags.is(rsDetails) && GetDetails() && GetDetails()->HasGrassNear(L.SpatialComponent->sphere.P, L.SpatialComponent->sphere.R))
	{
		PROF_EVENT("LampGrass");
		const RHIViewport Area = { float(L.X.S.posX), float(L.X.S.posY), float(L.X.S.size), float(L.X.S.size), 0.0f, 1.0f };
		GpuScene::DepthDraw Draw = { Target->SmapPage(Page), Page % CRenderTarget::SmapPages, &Area };
		Draw.IsSpot = true;
		GetDetails()->hw_Render(Cmd, &L.X.S.combine, &Draw);
	}

	if (GraphPass.HasPasses(1) || GraphPass.mapSorted.size())
	{
		PROF_EVENT("LampTranslucent");
		GraphPass.r_dsgraph_render_graph(1);
		GraphPass.r_dsgraph_render_sorted();
	}
}

void CRender::QueueLampTile(GfxCmdList& Cmd, const CRenderTarget::SpotShadowPart& Part, u32 Index)
{
	if (!Part.HasCasters)
	{
		return;
	}

	const light& L = *Part.Light;
	const u16 Page = L.vis.smap_ID;
	const RHIViewport Area = { float(L.X.S.posX), float(L.X.S.posY), float(L.X.S.size), float(L.X.S.size), 0.0f, 1.0f };

	if (!Part.IsDecor)
	{
		Scene.QueueTile(Cmd, Target->SmapPage(Page), Page % CRenderTarget::SmapPages, Area, LampTiles[Index]);
		return;
	}

	// A decor lamp casts each of its decor objects alone, a tile each
	GpuTileView View = LampTiles[Index];

	for (CObject* Object : L.decor_object)
	{
		View.Object = Object && !Object->getDestroy() ? Scene.ObjectOf(Object->dcast_Renderable()) : SCENE_OBJECT_NONE;

		if (View.Object != SCENE_OBJECT_NONE)
		{
			Scene.QueueTile(Cmd, Target->SmapPage(Page), Page % CRenderTarget::SmapPages, Area, View);
		}
	}
}
