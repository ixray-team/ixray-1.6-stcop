#include "stdafx.h"

#include "../../xrEngine/Render.h"
#include "../../xrEngine/IRenderable.h"
#include "../../xrEngine/IGame_Persistent.h"
#include "../../xrEngine/Environment.h"
#include "../../xrEngine/CustomHUD.h"
#include "../../xrEngine/xr_object.h"

#include "FBasicVisual.h"
#include "CHudInitializer.h"
#include "SkeletonCustom.h"
#include "SVGStorage.h"

using namespace		R_dsgraph;

extern float		r_ssaDISCARD;
extern float		r_ssaDONTSORT;
extern float		r_ssaHZBvsTEX;
extern float		r_ssaGLOD_start,	r_ssaGLOD_end;

ICF float calcLOD	(float ssa/*fDistSq*/, float R)
{
	return _sqrt(clampr((ssa - r_ssaGLOD_end)/(r_ssaGLOD_start-r_ssaGLOD_end),0.f,1.f));
}

template <class T>
static void SortPackets(xr_vector<T>& List)
{
	const auto Less = [](const T& A, const T& B) { return A.Key < B.Key; };
	if (!std::is_sorted(List.begin(), List.end(), Less))
	{
		std::sort(List.begin(), List.end(), Less);
	}
}

static void BindPass(GfxCmdList& Cmd, SPass& Pass)
{
	Cmd.SetVS(Pass.vs._get());
	Cmd.SetShader(Pass.gs->gs, ERHI_SHADER_TYPE::GS);
	Cmd.SetShader(Pass.ps->ps, ERHI_SHADER_TYPE::PS);
	Cmd.SetShader(Pass.hs->sh, ERHI_SHADER_TYPE::HS);
	Cmd.SetShader(Pass.ds->sh, ERHI_SHADER_TYPE::DS);
	Cmd.SetConstantTable(Pass.constants._get());
	Cmd.SetStates(Pass.state->state);
	Cmd.SetTextures(Pass.T._get());
}

void R_dsgraph_structure::r_dsgraph_clear_passes()
{
	for (u32 i = 0; i < 2; ++i)
	{
		StaticPasses[i].clear();
		DynamicPasses[i].clear();
	}
	r_dsgraph_clear_aux();
}

void R_dsgraph_structure::r_dsgraph_clear_aux()
{
	mapSorted.clear();
	mapDistort.clear();
	mapEmissive.clear();
	mapLOD.clear();
	mapWmark.clear();
	lstLODgroups.clear();
}

void R_dsgraph_structure::r_dsgraph_render_graph(u32 _priority, bool _clear)
{
	PROF_EVENT("r_dsgraph_render_graph");
	CScopeTimer Timer(Device.Statistic->RenderDUMP);
	GfxCmdList& Cmd = *Backend;

	{
		PROF_EVENT("NORMAL_SHADER_PASSES");
		Cmd.SetObjectTransform(Fidentity);
		StaticList& List = StaticPasses[_priority];
		SortPackets(List);
		SPass* Bound = nullptr;
		for (StaticPacket& Packet : List)
		{
			if (Packet.Pass != Bound)
			{
				Bound = Packet.Pass;
				BindPass(Cmd, *Bound);
				RImplementation.apply_lmaterial(Cmd);
			}
			VERIFY(!SpotShadowCachedCasters || !SpotShadowCachedCasters->contains(Packet.Visual));
			if (SpotShadowPass && (!SpotShadowBuild || !SpotShadowBuild->IsDepthCacheBuild))
			{
				++SpotShadowStaticItems;
			}
			float LOD = SpotShadowBuild ? 1.f : calcLOD(Packet.Ssa, Packet.Radius);
			Cmd.Constants.SetLOD(LOD);
			if (Packet.Geometry)
			{
				Cmd.SetGeometry(Packet.Geometry);
				Cmd.DrawIndexed(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, Packet.IndexCount, Packet.StartIndex, Packet.BaseVertex);
			}
			else
			{
				Packet.Visual->Render(Cmd, LOD);
			}
		}
		if (_clear)
		{
			List.clear();
		}
	}

	{
		PROF_EVENT("MATRIX_SHADER_PASSES");
		DynamicList& List = DynamicPasses[_priority];
		SortPackets(List);
		SPass* Bound = nullptr;
		for (DynamicPacket& Packet : List)
		{
			if (Packet.Pass != Bound)
			{
				Bound = Packet.Pass;
				BindPass(Cmd, *Bound);
			}
			VERIFY(!SpotShadowCachedCasters || !SpotShadowCachedCasters->contains(Packet.Visual));
			if (Packet.Visual->shader == nullptr)
			{
				continue;
			}
			Cmd.SetObjectTransform(Packet.Matrix);
			if (SpotShadowPass)
			{
				++SpotShadowDynamicItems;
			}
			RImplementation.apply_object(Cmd, Packet.Object);
			RImplementation.apply_lmaterial(Cmd);

			float LOD = calcLOD(Packet.Ssa, Packet.Visual->vis.sphere.R);
			// Distance lets tessellation fade out with it (see CShaderConstants::SetLOD).
			const float tess_dist = Device.vCameraPosition.distance_to(Packet.Matrix.c) - Packet.Visual->vis.sphere.R;
			Cmd.Constants.SetLOD(LOD, tess_dist);
			Packet.Visual->Render(Cmd, LOD);
		}
		if (_clear)
		{
			List.clear();
		}
	}
}

ICF void RenderNode(const R_dsgraph_structure& Graph, GfxCmdList& Cmd, DynamicPacket& N, bool emissive = false)
{
	dxRender_Visual* V = N.Visual;

	VERIFY(V && V->shader._get());
	Cmd.SetPass(N.Element);

	const auto* Settings = Graph.val_bUI ? Device.m_pRender->GetUIIconLighting() : nullptr;
	Hlsl::float4& Tint = Cmd.Constants.Write<CBUIIcon>().ui_emissive_tint;
	if (Settings)
	{
		Tint.set(Settings->Color.x * Settings->Intensity, Settings->Color.y * Settings->Intensity, Settings->Color.z * Settings->Intensity, 1.0f);
	}
	else
	{
		Tint.set(1.0f, 1.0f, 1.0f, 1.0f);
	}

	if (emissive)
	{
		Cmd.SetBlend(true, D3DBLEND_ONE, D3DBLEND_ONE);
		Cmd.SetColorWriteEnable(D3DCOLORWRITEENABLE_RED | D3DCOLORWRITEENABLE_GREEN | D3DCOLORWRITEENABLE_BLUE);
	}
	else if (Graph.val_bUI && N.Element->flags.bEmissive)
	{
		Cmd.SetColorWriteEnable();
		Cmd.SetDepthWrite(true);
	}

	if (V->dcast_ParticleCustom())
	{
		V->Render(Cmd, 0);
		return;
	}

	Cmd.SetObjectTransform(N.Matrix);

	RImplementation.apply_object(Cmd, N.Object);
	RImplementation.apply_lmaterial(Cmd);

	V->Render(Cmd, calcLOD(N.Ssa, V->vis.sphere.R));
}

ICF void RenderMap(const R_dsgraph_structure& Graph, GfxCmdList& Cmd, DynamicList& Map, bool emissive = false)
{
	for (DynamicPacket& Node : Map)
	{
		RenderNode(Graph, Cmd, Node, emissive);
	}

	Map.clear();
}

ICF void RenderBackToFront(const R_dsgraph_structure& Graph, GfxCmdList& Cmd, DynamicList& Map)
{
	SortPackets(Map);
	RenderMap(Graph, Cmd, Map);
}

void R_dsgraph_structure::r_dsgraph_render_ui()
{
	GfxCmdList& Cmd = *Backend;
	RenderMap(*this, Cmd, mapUI);
}

void R_dsgraph_structure::r_dsgraph_render_sorted_ui()
{
	GfxCmdList& Cmd = *Backend;
	RenderMap(*this, Cmd, mapUIEmissive);

	RenderBackToFront(*this, Cmd, mapUISorted);
}

void R_dsgraph_structure::r_dsgraph_render_hud()
{
	PROF_EVENT("r_dsgraph_render_hud");
	GfxCmdList& Cmd = *Backend;
	CHudInitializer initalizer(Cmd, true, true);

	RenderMap(*this, Cmd, mapHUD);

}

void R_dsgraph_structure::r_dsgraph_render_hud_ui()
{
	PROF_EVENT("r_dsgraph_render_hud_ui");
	VERIFY(g_hud && g_hud->RenderActiveItemUIQuery());

	GfxCmdList& Cmd = *Backend;
	CHudInitializer initalizer(Cmd, true, true);


	g_hud->RenderActiveItemUI();
}

void R_dsgraph_structure::r_dsgraph_render_sorted(bool render_hud)
{
	PROF_EVENT("r_dsgraph_render_sorted");
	GfxCmdList& Cmd = *Backend;

	RenderBackToFront(*this, Cmd, mapSorted);

	if (render_hud)
	{
		r_dsgraph_render_sorted_hud();
	}
}

void R_dsgraph_structure::r_dsgraph_render_sorted_hud()
{
	PROF_EVENT("r_dsgraph_render_sorted_hud");

	GfxCmdList& Cmd = *Backend;
	CHudInitializer initalizer(Cmd, true, true);
	auto velocity_target = Cmd.GetRenderTarget(1);
	Cmd.SetRenderTarget(nullptr, 1);
	RenderMap(*this, Cmd, mapHUDEmissive, true);
	Cmd.SetRenderTarget(velocity_target, 1);

	if (g_hud && g_hud->RenderActiveItemUIQuery())
	{
		r_dsgraph_render_hud_ui();
	}

	RenderBackToFront(*this, Cmd, mapHUDSorted);
}

void R_dsgraph_structure::r_dsgraph_render_emissive()
{
	PROF_EVENT("r_dsgraph_render_emissive");
	GfxCmdList& Cmd = *Backend;

	RenderMap(*this, Cmd, mapEmissive);
}

void R_dsgraph_structure::r_dsgraph_render_scope()
{
	GPU_EVENT(SCOPE_BUFFER_RENDER);
	GfxCmdList& Cmd = *Backend;
	RImplementation.Target->LensDofFactor = mapHUDScopeMask.size() > 0 && Device.hudViewportData.isRenderActive ? clampr(Device.hudViewportData.renderZoomRotateFactor, 0.0f, 1.0f) : 0.0f;
	RImplementation.Target->copy_position();

	RImplementation.Target->u_setrt(Cmd, RImplementation.Target->rt_DofScopeMask, nullptr, RDepth);

	CHudInitializer initalizer(Cmd, true);
	RenderMap(*this, Cmd, mapHUDScopeMask);
}

void R_dsgraph_structure::r_dsgraph_render_wmarks()
{
	PROF_EVENT("r_dsgraph_render_wmarks");
	GfxCmdList& Cmd = *Backend;

	RenderMap(*this, Cmd, mapWmark);
}

void R_dsgraph_structure::r_dsgraph_render_distort()
{
	PROF_EVENT("r_dsgraph_render_distort");
	GfxCmdList& Cmd = *Backend;
	RenderMap(*this, Cmd, mapDistort);

	CHudInitializer initalizer(Cmd, true, true);
	RenderMap(*this, Cmd, mapHUDDistort);
}

void R_dsgraph_structure::add_Static(IRenderVisual* Visual, CFrustum& Frustum, u32 Planes)
{
	CFrustum* SavedView = View;
	View = &Frustum;
	add_Static((dxRender_Visual*)Visual, Planes);
	View = SavedView;
}

void R_dsgraph_structure::add_Dynamic(IRenderVisual* Visual, Fmatrix* Xform, bool IgnoreOptimize, bool Force)
{
	val_pTransform = Xform;
	add_leafs_Dynamic((dxRender_Visual*)Visual, IgnoreOptimize, Force);
}

void R_dsgraph_structure::set_Object(IRenderable* O)
{
	RImplementation.set_Object(O, this);
}

void R_dsgraph_structure::Begin(const GraphView& NewView)
{
	ViewParams = NewView;
	OwnsView = true;
	PortalTraverser.ViewportNear = NewView.ViewportNear;
	PortalTraverser.prepare_local_clips((u32)RImplementation.Sectors.size(), (u32)RImplementation.Portals.size());

	private_visuals.clear();
	val_pObject = nullptr;
	val_pTransform = &Fidentity;
	val_bHUD = false;
	val_bUI = false;
	val_bInvisible = false;
	r_dsgraph_clear_passes();
	r_pmask(NewView.PassMask[0], NewView.PassMask[1], NewView.PassMask[2]);
}

void R_dsgraph_structure::Traverse()
{
	View = &ViewParams.Frustum;
	PortalTraverser.traverse(ViewParams.Sector, ViewParams.Frustum, ViewParams.Origin, ViewParams.Transform, ViewParams.TraverseOptions);
}

void R_dsgraph_structure::CaptureStatic()
{
	PROF_EVENT("add_static");
	if (psDeviceFlags.test(rsDrawStatic))
	{
		for (IRender_Sector* Item : PortalTraverser.r_sectors)
		{
			CSector* Sector = (CSector*)Item;
			for (CFrustum& Frustum : PortalTraverser.local_frustums[Sector->index])
			{
				View = &Frustum;
				add_Static(Sector->root(), Frustum.getMask());
			}
		}
	}
	View = nullptr;
}

void R_dsgraph_structure::Build(const GraphView& NewView)
{
	PROF_EVENT("Build graph");
	Begin(NewView);
	Traverse();
	CaptureStatic();
	SortStatic();
}

void R_dsgraph_structure::SortStatic()
{
	SortPackets(StaticPasses[0]);
	SortPackets(StaticPasses[1]);
}

void R_dsgraph_structure::TakeStatic(R_dsgraph_structure& From)
{
	const auto Move = [](auto& To, auto& List, bool Front = false)
	{
		To.insert(Front ? To.begin() : To.end(), List.begin(), List.end());
		List.clear();
	};
	Move(StaticPasses[0], From.StaticPasses[0]);
	Move(StaticPasses[1], From.StaticPasses[1]);
	Move(mapSorted, From.mapSorted);
	Move(mapLOD, From.mapLOD);
	// Statics were inserted before dynamics; these lists draw in insertion order
	Move(mapDistort, From.mapDistort, true);
	Move(mapEmissive, From.mapEmissive, true);
	Move(mapWmark, From.mapWmark, true);
}

void R_dsgraph_structure::BuildFull(const GraphView& NewView, CObject* Ignore)
{
	PROF_EVENT("Build graph");
	Begin(NewView);
	if (!NewView.Sector)
	{
		return;
	}

	Traverse();
	CollectDynamic();
	CaptureStatic();
	if (psDeviceFlags.test(rsDrawDynamic))
	{
		CaptureDynamic(Ignore);
	}
}

void R_dsgraph_structure::CollectDynamic()
{
	if (psDeviceFlags.test(rsDrawDynamic))
	{
		View = &ViewParams.Frustum;
		CollectSubspaceDynamic();
		View = nullptr;
	}
	else
	{
		SubspaceDynamic.clear();
	}
}

void R_dsgraph_structure::CaptureDynamic(CObject* Ignore)
{
	PROF_EVENT("add_dynamic")
	RImplementation.set_Object(0, this);

	for (const SubspaceItem& Item : SubspaceDynamic)
	{
		View = Item.Frustum;
		if (Ignore && Ignore->dcast_Renderable() == Item.Renderable)
		{
			continue;
		}

		if (ViewPhase() != CRender::PHASE_SMAP)
		{
			RImplementation.set_Object(Item.Renderable, this);
		}

		Item.Renderable->renderable_Render(this);
	}

	RImplementation.set_Object(0, this);
	View = nullptr;
}

void R_dsgraph_structure::CollectSubspaceDynamic()
{
	PROF_EVENT("collect_dynamic");
	SubspaceDynamic.clear();

	// Traverse object database
	g_SpatialSpace->q_frustum
	(
		lstRenderables,
		ISpatial_DB::O_ORDERED,
		ESPATIAL_TYPE::RENDERABLE | ESPATIAL_TYPE::RENDERABLESHADOW,
		*View
	);

	const CFrustum& MainView = RImplementation.ViewBase;

	// Determine visibility for dynamic part of scene
	for (u32 o_it=0; o_it<lstRenderables.size(); o_it++)
	{
		ISpatial*	spatial		= lstRenderables[o_it].get();
		spatial->spatial_updatesector();
		CSector*	sector		= (CSector*)spatial->sector;
		if	(0==sector)										continue;	// disassociated from S/P structure
		if	(!PortalTraverser.IsVisited(sector->index))		continue;	// inactive (untouched) sector
		for (CFrustum& view : PortalTraverser.local_frustums[sector->index])
		{
			if (!view.testSphere_dirty(spatial->sphere.P,spatial->sphere.R))	continue;

			// renderable
			IRenderable* renderable = spatial->dcast_Renderable();
			if (0 == renderable)				continue;					// unknown, but renderable object (r1_glow???)
			if (ViewParams.CalculateSkeletons && Device.vCameraPosition.distance_to_sqr(renderable->renderable.xform.c)<=10000.f)
			{
				// Shadow casters, and objects the main view did not calculate
				const bool ShadowCaster = (spatial->type & ESPATIAL_TYPE::RENDERABLESHADOW) != ESPATIAL_TYPE::NONE;
				const bool OutsideMainView = (spatial->type & ESPATIAL_TYPE::RENDERABLE) != ESPATIAL_TYPE::NONE &&
					!MainView.testSphere_dirty(spatial->sphere.P, spatial->sphere.R);
				if (ShadowCaster || OutsideMainView)
				{
					if (CKinematics* pKin = PCKinematics((dxRender_Visual*)renderable->renderable.visual))
					{
						pKin->CalculateBones(true);
					}
				}
			}
			SubspaceDynamic.push_back({ renderable, &view });
		}
	}
}

#include "FHierrarhyVisual.h"
#include "../../xrEngine/Fmesh.h"
#include "FLOD.h"

void	R_dsgraph_structure::r_dsgraph_render_R1_box	(IRender_Sector* _S, Fbox& BB, int sh)
{
	GfxCmdList&	Cmd			= *Backend;
	CSector*	S			= (CSector*)_S;
	lstVisuals.clear		();
	lstVisuals.push_back	(S->root());
	
	for (u32 test=0; test<lstVisuals.size(); test++)
	{
		dxRender_Visual*	V		= 	lstVisuals[test];
		
		// Visual is 100% visible - simply add it
		xr_vector<dxRender_Visual*>::iterator I,E;	// it may be usefull for 'hierrarhy' visuals
		
		switch (V->Type) {
		case MT_HIERRARHY:
			{
				// Add all children
				FHierrarhyVisual* pV = (FHierrarhyVisual*)V;
				I = pV->children.begin	();
				E = pV->children.end		();
				for (; I!=E; I++)		{
					dxRender_Visual* T			= *I;
					if (BB.intersect(T->vis.box))	lstVisuals.push_back(T);
				}
			}
			break;
		case MT_SKELETON_ANIM:
		case MT_SKELETON_RIGID:
			{
				// Add all children	(s)
				CKinematics * pV		= (CKinematics*)V;
				pV->CalculateBones		(true);
				I = pV->children.begin	();
				E = pV->children.end		();
				for (; I!=E; I++)		{
					dxRender_Visual* T				= *I;
					if (BB.intersect(T->vis.box))	lstVisuals.push_back(T);
				}
			}
			break;
		case MT_LOD:
			{
				FLOD		* pV		=	(FLOD*) V;
				I = pV->children.begin		();
				E = pV->children.end		();
				for (; I!=E; I++)		{
					dxRender_Visual* T				= *I;
					if (BB.intersect(T->vis.box))	lstVisuals.push_back(T);
				}
			}
			break;
		case MT_LOD1:
			{
				FLOD		* pV		=	(FLOD*) V;
				I = pV->children.begin		();
				E = pV->children.end		();
				for (; I!=E; I++)		{
					dxRender_Visual* T				= *I;
					if (BB.intersect(T->vis.box))	lstVisuals.push_back(T);
				}
			}
			break;
		case MT_LOD2:
			{
				FLOD		* pV		=	(FLOD*) V;
				I = pV->children.begin		();
				E = pV->children.end		();
				for (; I!=E; I++)		{
					dxRender_Visual* T				= *I;
					if (BB.intersect(T->vis.box))	lstVisuals.push_back(T);
				}
			}
			break;
		case MT_LOD3:
			{
				FLOD		* pV		=	(FLOD*) V;
				I = pV->children.begin		();
				E = pV->children.end		();
				for (; I!=E; I++)		{
					dxRender_Visual* T				= *I;
					if (BB.intersect(T->vis.box))	lstVisuals.push_back(T);
				}
			}
			break;
		case MT_LOD4:
			{
				FLOD		* pV		=	(FLOD*) V;
				I = pV->children.begin		();
				E = pV->children.end		();
				for (; I!=E; I++)		{
					dxRender_Visual* T				= *I;
					if (BB.intersect(T->vis.box))	lstVisuals.push_back(T);
				}
			}
			break;
		default:
			{
				// Renderable visual
				ShaderElement* E_	= V->shader->E[sh]._get();
				if (E_ && !(E_->flags.bDistort))
				{
					for (u32 pass=0; pass<E_->passes.size(); pass++)
					{
						Cmd.SetPass			(E_,pass);
						V->Render					(Cmd, -1.f);
					}
				}
			}
			break;
		}
	}
}
