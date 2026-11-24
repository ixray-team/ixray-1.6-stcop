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

void R_dsgraph_structure::CountPacket() const
{
	const R_dsgraph_structure* Faces = RImplementation.GraphReflection.data();
	const u32 Kind = this >= Faces && this < Faces + RImplementation.GraphReflection.size() ? 2 : ViewPhase() == CRender::PHASE_SMAP ? 1 : 0;
	PacketsDrawn[Kind].fetch_add(1, std::memory_order_relaxed);
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
	mapLOD.clear();
	lstLODgroups.clear();
}

void R_dsgraph_structure::r_dsgraph_render_graph(u32 _priority, bool _clear, u32 PassIndex, u32 OnlyStage, u32 SkipStage)
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
			if (PassIndex != u32(-1) && u32(Packet.Key >> 60) != PassIndex)
			{
				continue;
			}

			// A pass with a priority of its own belongs to a stage of the frame
			if ((OnlyStage != u32(-1) && Packet.Pass->iPriority != OnlyStage) || Packet.Pass->iPriority == SkipStage)
			{
				continue;
			}

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
			CountPacket();
			const float LOD = SpotShadowBuild || ViewParams.FullDetail ? 1.f : calcLOD(Packet.Ssa, Packet.Radius);
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
			if (PassIndex != u32(-1) && u32(Packet.Key >> 60) != PassIndex)
			{
				continue;
			}

			// A pass with a priority of its own belongs to a stage of the frame
			if ((OnlyStage != u32(-1) && Packet.Pass->iPriority != OnlyStage) || Packet.Pass->iPriority == SkipStage)
			{
				continue;
			}

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
			RImplementation.apply_object(Cmd, Packet.Track);
			RImplementation.apply_lmaterial(Cmd);

			CountPacket();
			const float LOD = ViewParams.FullDetail ? 1.f : calcLOD(Packet.Ssa, Packet.Visual->vis.sphere.R);
			Packet.Visual->Render(Cmd, LOD);
		}
		if (_clear)
		{
			List.clear();
		}
	}
}

ICF void RenderNode(const R_dsgraph_structure& Graph, GfxCmdList& Cmd, DynamicPacket& N)
{
	dxRender_Visual* V = N.Visual;

	VERIFY(V && V->shader._get());

	// An element of the forward level frame may be its depth, then its colour: both are drawn here, or the one
	// the graph asks for; an element with one pass counts as colour
	const u32 PassCount = u32(N.Element->passes.size());

	if (PassCount > 1 && Graph.PassOnly == u32(-1))
	{
		for (u32 PassIdx = 0; PassIdx + 1 < PassCount; ++PassIdx)
		{
			Cmd.SetPass(N.Element, PassIdx);
			Cmd.SetObjectTransform(N.Matrix);
			RImplementation.apply_object(Cmd, N.Track);
			RImplementation.apply_lmaterial(Cmd);
			Graph.CountPacket();
			V->Render(Cmd, Graph.ViewParams.FullDetail ? 1.f : calcLOD(N.Ssa, V->vis.sphere.R));
		}
	}

	if (((Graph.PassOnly == 0 || Graph.PassOnly == 3) && PassCount < 2) || (Graph.PassOnly == 2 && PassCount > 1))
	{
		return;
	}

	Cmd.SetPass(N.Element, Graph.PassOnly == 0 ? 0 : PassCount - 1);
	Graph.CountPacket();

	// Of r_dsgraph_render_sorted (editors, shadow views): the main view's walks take their particle effects out first
	if (V->dcast_ParticleCustom())
	{
		V->Render(Cmd, 0);
		return;
	}

	Cmd.SetObjectTransform(N.Matrix);

	RImplementation.apply_object(Cmd, N.Track);
	RImplementation.apply_lmaterial(Cmd);

	V->Render(Cmd, Graph.ViewParams.FullDetail ? 1.f : calcLOD(N.Ssa, V->vis.sphere.R));
}

ICF void RenderMap(const R_dsgraph_structure& Graph, GfxCmdList& Cmd, DynamicList& Map)
{
	for (DynamicPacket& Node : Map)
	{
		RenderNode(Graph, Cmd, Node);
	}

	Map.clear();
}

ICF void RenderBackToFront(const R_dsgraph_structure& Graph, GfxCmdList& Cmd, DynamicList& Map)
{
	SortPackets(Map);
	RenderMap(Graph, Cmd, Map);
}

void R_dsgraph_structure::r_dsgraph_render_hud(u32 PassIndex)
{
	PROF_EVENT("r_dsgraph_render_hud");
	GfxCmdList& Cmd = *Backend;
	CHudInitializer initalizer(Cmd, true, true);
	PassOnly = PassIndex;

	for (DynamicPacket& Node : mapHUD)
	{
		RenderNode(*this, Cmd, Node);
	}

	if (PassIndex != 0 && PassIndex != 2)
	{
		mapHUD.clear();
	}

	PassOnly = u32(-1);
}

void R_dsgraph_structure::r_dsgraph_render_sorted()
{
	PROF_EVENT("r_dsgraph_render_sorted");
	GfxCmdList& Cmd = *Backend;

	RenderBackToFront(*this, Cmd, mapSorted);
}

void R_dsgraph_structure::r_dsgraph_render_node(DynamicPacket& Node)
{
	RenderNode(*this, *Backend, Node);
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
	val_bInvisible = false;
	r_dsgraph_clear_passes();
	r_pmask(NewView.PassMask[0], NewView.PassMask[1], NewView.PassMask[2]);
}

void R_dsgraph_structure::Traverse()
{
	PROF_EVENT("Traverse");

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
			const xr_vector<dxRender_Visual*>* Leftover = SkipDepthBatched ? RImplementation.Scene.GetLeftover(Sector->index, !pmask[1] && !pmask[2]) : nullptr;
			for (CFrustum& Frustum : PortalTraverser.local_frustums[Sector->index])
			{
				View = &Frustum;
				if (!Leftover)
				{
					add_Static(Sector->root(), Frustum.getMask());
					continue;
				}
				for (dxRender_Visual* Visual : *Leftover)
				{
					add_Static(Visual, Frustum.getMask());
				}
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
}
