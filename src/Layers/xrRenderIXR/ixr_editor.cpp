/**************************************************************************************
* Copyright (C) 2026 IXRay-Team
* IX-Ray Renderer
***************************************************************************************
* Source code is licensed under the following terms:
*
* "Team" means the maintainers of the official IX-Ray repository.
*
* 1. IX-Ray Team License
*    The Team may copy, modify, merge, publish and distribute this Software
*    and its documentation, in source and binary form, without restriction.
*
* 2. Public Access License
*    Non-exclusive rights granted to everyone else.
*
*    Permitted rights:
*      a. Read and clone the source code.
*      b. Fork the repository and modify the source code for the sole purpose
*         of proposing the change to the official repository. By proposing it,
*         the author grants the Team a worldwide, perpetual, irrevocable,
*         royalty-free right to use, modify, relicense and distribute it.
*      c. Redistribute binaries built and published by the Team, unmodified
*         and together with this notice.
*
* Usage of this Software beyond the rights granted above is strictly prohibited,
* including distribution of the source code or of any modified version of it and
* distribution of binaries not built by the Team.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND. THE COPYRIGHT
* HOLDERS ARE NOT LIABLE FOR ANY CLAIM OR DAMAGE ARISING FROM IT.
*
* The above copyright notice and this license text must be included in all
* copies or substantial portions of the Software.
**************************************************************************************/
#include "stdafx.h"

#include "EditorDrawContext.h"
#include "EditorRenderHost.h"
#include "EditorDeviceFlags.h"
#include "RenderScope.h"
#include "SkeletonCustom.h"
#include "dxRenderDeviceRender.h"
#include "WallmarksEngine.h"
#include "../../xrEngine/CustomHUD.h"
#include "../../xrEngine/IGame_Level.h"
#include "../../xrEngine/IGame_Persistent.h"
#include "../../xrEngine/Environment.h"
#include "../../../shaders/common/debug_view.hlsli"

namespace
{
	// Editors do not track lighting per object: full hemisphere and sun for everything
	class EditorObjectSpecific : public IRender_ObjectSpecific
	{
	public:
		void force_mode(u32 mode) override {}
		float get_luminocity() override { return 1.f; }
		float get_luminocity_hemi() override { return 1.f; }
		float* get_luminocity_hemi_cube() override
		{
			static float Cube[8] = {};
			return Cube;
		}
	};

	// Editor scene code may still issue direct draws while it submits; keep them away from the
	// pipeline targets.
	class SubmitScope
	{
	public:
		SubmitScope(GfxCmdList& Cmd)
			: Targets(Cmd, nullptr)
		{
			SavedPass = GEditorDraw.Pass;
			GEditorDraw.Pass = EEditorDrawPass::Submit;
		}

		~SubmitScope()
		{
			GEditorDraw.Pass = SavedPass;
		}

	private:
		RenderScope Targets;
		EEditorDrawPass SavedPass = EEditorDrawPass::Immediate;
	};
}

CDetailManager* CRender::GetDetails() const
{
	if (Details != nullptr)
	{
		return Details;
	}

	return Device.IsEditorMode() && g_EditorRenderHost ? g_EditorRenderHost->GetSceneDetails() : nullptr;
}

IRender_ObjectSpecific* CreateEditorObjectSpecific()
{
	return new EditorObjectSpecific();
}

namespace
{
	// Unlit editor view: the scene of base textures into the final target
	class CBlender_EditorUnlit final : public IBlender
	{
	public:
		CBlender_EditorUnlit()
		{
			description.CLS = 0;
		}

		const char* getComment() override
		{
			return "INTERNAL: editor unlit view";
		}

		bool canBeDetailed() override
		{
			return false;
		}

		bool canBeLMAPped() override
		{
			return false;
		}

		void Compile(CBlender_Compile& C) override
		{
			IBlender::Compile(C);

			C.r_Pass("stub_fullscreen_triangle", "editor_unlit", false, false, false);
			C.r_dx10Texture("s_image", r2_RT_generic2);
			C.r_End();
		}
	};

	// Scene depth into the viewport-sized overlay depth (no color, depth always written)
	class CBlender_EditorDepthUpscale final : public IBlender
	{
	public:
		CBlender_EditorDepthUpscale()
		{
			description.CLS = 0;
		}

		const char* getComment() override
		{
			return "INTERNAL: editor overlay depth";
		}

		bool canBeDetailed() override
		{
			return false;
		}

		bool canBeLMAPped() override
		{
			return false;
		}

		void Compile(CBlender_Compile& C) override
		{
			IBlender::Compile(C);

			C.r_Pass("stub_fullscreen_triangle", "editor_depth_upscale", false, false, true);
			C.r_dx10Texture("s_position", r2_RT_P);
			C.r_End();
		}
	};
}

//---------------------------------------------------------------------------
// Unlit view

void CRender::EditorCreate()
{
	EditorUnlitBlender = new CBlender_EditorUnlit();
	EditorUnlitShader.create(EditorUnlitBlender);

	EditorDepthUpscaleBlender = new CBlender_EditorDepthUpscale();
	EditorDepthUpscaleShader.create(EditorDepthUpscaleBlender);

	// The game loads them with the level; the editors play levels without it (PIE)
	Wallmarks = new CWallmarksEngine();
}

void CRender::EditorDestroy()
{
	EditorHiZ.Destroy();

	for (light* Light : EditorLights)
	{
		xr_delete(Light);
	}
	EditorLights.clear();

	xr_delete(Wallmarks);
	EditorUnlitShader.destroy();
	xr_delete(EditorUnlitBlender);
	EditorDepthUpscaleShader.destroy();
	xr_delete(EditorDepthUpscaleBlender);
}

bool CRender::EditorIsLit() const
{
	if (!psDeviceFlags.test(rsEnvironment) || g_pGamePersistent == nullptr)
	{
		return false;
	}

	const CEnvironment& Environment = g_pGamePersistent->Environment();
	return Environment.CurrentEnv != nullptr && Environment.Current[0] != nullptr && Environment.Current[1] != nullptr;
}

void CRender::EditorRenderUnlit(GfxCmdList& Cmd)
{
	ps_r_taa_jitter.set(0.f, 0.f, -1.f);
	ps_r_taa_jitter_full.set(ps_r_taa_jitter);
	Cmd.Constants.UpdateScreen();

	ViewBase.CreateFromMatrix(RFrame.mFullTransform, FRUSTUM_P_LRTB + FRUSTUM_P_FAR);
	GraphMain.View = nullptr;

	if (g_EditorRenderHost)
	{
		g_EditorRenderHost->PrepareScene();
	}

	// The unlit image has no distortion pass: keep distortive visuals out of mapDistort
	o.distortion = false;

	GraphMain.r_pmask(true, true);
	phase = PHASE_NORMAL;
	render_main(Cmd);

	// Every surface shows its base texture, in the scene's encoding, over the editor background colour; what blends
	// blends by its alpha
	const float Decode = ps_r_shading_quality >= 1 ? 2.2f : 1.f;
	const float ClearColor[4] = { std::pow(EditorClearColor.r, Decode), std::pow(EditorClearColor.g, Decode), std::pow(EditorClearColor.b, Decode), EditorClearColor.a };
	Cmd.Constants.Write<CBFrame>().debug_view.set(float(DebugView_Base), 0.f, 0.f, 0.f);

	Target->u_setrt(Cmd, Target->rt_Generic_2, nullptr, RDepth);
	Cmd.ClearTarget(Target->rt_Generic_2->pRT, ClearColor);
	Cmd.ClearDepthStencil(RDepth, ERHI_CLEAR_TARGET::DEPTH | ERHI_CLEAR_TARGET::STENCIL, 1.f, 0);
	rmNormal();
	Cmd.SetObjectTransform(Fidentity);
	Cmd.SetStencil(false);
	Cmd.SetColorWriteEnable();

	{
		CEditorFillModeScope FillModeScope(Cmd, true);
		GraphMain.r_dsgraph_render_hud();
		GraphMain.r_dsgraph_render_graph(0);

		if (CDetailManager* SceneDetails = GetDetails())
		{
			SceneDetails->Render(Cmd);
		}

		GraphMain.r_dsgraph_render_lods(true, true);
		GraphMain.r_dsgraph_render_graph(1);
	}

	RenderForwardOit(Cmd, Target->rt_Generic_2);

	{
		CEditorFillModeScope FillModeScope(Cmd, true);
		GraphMain.r_dsgraph_render_sorted();
	}

	Cmd.Constants.Write<CBFrame>().debug_view.set(float(ps_r_debug_view), 0.f, 0.f, 0.f);

	if (EditorOcclusionCulling)
	{
		Target->copy_position();
		EditorHiZ.Build(Cmd);
	}

	// Into a target of the scene size, upscaled to the viewport at the end
	const u32 Width = (u32)GfxCmdList::GetWidth();
	const u32 Height = (u32)GfxCmdList::GetHeight();

	Target->u_setrt(Cmd, Width, Height, Target->rt_Generic_0->pRT, nullptr, nullptr, nullptr, nullptr);
	rmNormal();

	Cmd.SetCullMode(ERHI_CULLMODE::NONE);
	Cmd.SetStencil(FALSE);
	Cmd.SetColorWriteEnable();
	Cmd.SetPass(EditorUnlitShader->E[0]);
	DrawFullscreen(Cmd, 3);

	Target->u_setrt(Cmd, Width, Height, Target->rt_Generic_0->pRT, nullptr, nullptr, nullptr, RDepth);
	rmNormal();
	Cmd.SetObjectTransform(Fidentity);
	Cmd.SetView(RFrame.mView, RFrame.mProject);
	Cmd.SetCullMode(ERHI_CULLMODE::BACK);

	if (g_EditorRenderHost)
	{
		g_EditorRenderHost->DrawSceneHelpers();
	}

	GraphMain.mapHUDSorted.clear();

	GraphMain.r_pmask(true, false);

	Target->PhaseEditorScale(Cmd);
}

//---------------------------------------------------------------------------
// Lifecycle

void CRender::Initialize()
{
	PSLibrary.OnCreate();
}

void CRender::ShutDown()
{
	PSLibrary.OnDestroy();
}

void CRender::OnDeviceCreate()
{
	Models = new CModelPool();
}

void CRender::OnDeviceDestroy()
{
	xr_delete(Models);
}

//---------------------------------------------------------------------------
// Models

void CRender::model_Render(IRenderVisual* V, const Fmatrix& Transform, int Priority, bool StrictB2F, float LOD)
{
	if (GEditorDraw.IsOverlayPass())
	{
		return;
	}

	if (GEditorDraw.IsSubmitPass())
	{
		// The scene graph sorts by shader priority itself: submit once, from the base priority
		if (Priority == 1 && !StrictB2F)
		{
			model_RenderSingle(V, Transform, LOD);
		}
		return;
	}

	Models->Render(Imm(), (dxRender_Visual*)V, Transform, Priority, StrictB2F, LOD);
}

void CRender::model_RenderSingle(IRenderVisual* V, const Fmatrix& Transform, float LOD)
{
	if (GEditorDraw.IsOverlayPass())
	{
		return;
	}

	if (GEditorDraw.IsSubmitPass())
	{
		if (IKinematics* Kinematics = PKinematics(V))
		{
			Kinematics->CalculateBones(TRUE);
		}

		Fmatrix SubmitTransform = Transform;
		EditorGraph->add_Dynamic(V, &SubmitTransform, true);
		return;
	}

	Models->RenderSingle(Imm(), (dxRender_Visual*)V, Transform, LOD);
}

//---------------------------------------------------------------------------
// Scene collection

void CRender::EditorRenderMain(GfxCmdList& Cmd)
{
	BeginMainCollect();
	EditorCollect(Cmd, false);
}

void CRender::EditorRenderSubspace(GfxCmdList& Cmd, R_dsgraph_structure& Graph)
{
	const CFrustum SavedView = ViewBase;
	ViewBase = Graph.ViewParams.Frustum;
	EditorGraph = &Graph;

	EditorCollect(Cmd, true);

	EditorGraph = &GraphMain;
	ViewBase = SavedView;
}

void CRender::EditorSyncLights()
{
	if (g_EditorRenderHost == nullptr)
	{
		return;
	}

	const u32 Total = g_EditorRenderHost->CollectEditorLights(nullptr, 0);
	if (Total == 0)
	{
		return;
	}

	// Pool grows to the scene size and is reused across frames. The lights are never registered in the spatial
	// database: they reach the frame through SurfaceLights alone
	while (EditorLights.size() < Total)
	{
		EditorLights.push_back(new light());
	}

	static xr_vector<EditorLightDescriptor> Scratch;
	if (Scratch.size() < Total)
	{
		Scratch.resize(Total);
	}

	const u32 Written = g_EditorRenderHost->CollectEditorLights(Scratch.data(), Total);
	const float FogDistance = RFrame.Env().fog_distance;

	for (u32 Index = 0; Index < Written; ++Index)
	{
		const EditorLightDescriptor& Descriptor = Scratch[Index];
		if (Descriptor.Range < 0.1f)
		{
			continue;
		}

		light* Light = EditorLights[Index];
		const bool IsSpot = Descriptor.Type == (u32)IRender_Light::SPOT;
		Light->set_type(IsSpot ? IRender_Light::SPOT : IRender_Light::POINT);
		// -noshadows disables every shadowed light
		Light->set_shadow(Descriptor.CastShadow && EditorLightShadows && !o.noshadows);
		Light->set_hud_mode(false);
		Light->set_position(Descriptor.Position);
		if (IsSpot)
		{
			Fvector Right = { 1.f, 0.f, 0.f };
			if (std::abs(Right.dotproduct(Descriptor.Direction)) > 0.99f)
			{
				Right.set(0.f, 0.f, 1.f);
			}
			Light->set_rotation(Descriptor.Direction, Right);
			Light->set_cone(std::min(Descriptor.Cone, deg2rad(120.f)));
		}
		Light->set_range(Descriptor.Range);
		Light->set_color(Descriptor.Color);
		Light->set_attenuation_params(Descriptor.Attenuation0, Descriptor.Attenuation1, Descriptor.Attenuation2, 1.f);

		// Made before the light's own is let go, so a texture it keeps is not unloaded in between
		ref_texture Projector;
		if (IsSpot && Descriptor.Texture.size())
		{
			Projector.create(Descriptor.Texture.c_str());
		}
		Light->Projector = Projector;

		// Same visibility rules as the spatial lights
		if (!ViewBase.testSphere_dirty(Light->SpatialComponent->sphere.P, Light->SpatialComponent->sphere.R))
		{
			continue;
		}
		if (RFrame.vCameraPosition.distance_to_sqr(Light->SpatialComponent->sphere.P) > _sqr(FogDistance))
		{
			continue;
		}

		// A shadowed light fades out with its size on the screen (UploadSurfaceLights): the faded ones are left out, as the
		// spatial ones are
		if (Light->get_LOD() <= EPS_L)
		{
			continue;
		}

		// Several editor viewports render within one Device.dwFrame: the transforms light::xform_calc keeps for the
		// frame are made anew, the six shadowed faces of a point light's too
		Light->m_xform_frame = Device.dwFrame + 1;
		for (light* Face : Light->omnipart)
		{
			if (Face != nullptr)
			{
				Face->m_xform_frame = Device.dwFrame + 1;
			}
		}

		// Active for the frame's light lists only: the pool stays out of the spatial database
		Light->flags.bActive = true;
		SurfaceLights.push_back(Light);
	}
}

void CRender::EditorCollect(GfxCmdList& Cmd, bool Shadows)
{
	R_dsgraph_structure& Graph = *EditorGraph;
	set_Object(nullptr, &Graph);
	// Normal (static) items and details apply the material without an object
	EditorApplyHemi();

	// The editors control visibility themselves (their device flags have no rsDraw* bits)
	if (g_EditorRenderHost)
	{
		SubmitScope Scope(Cmd);
		if (Shadows)
		{
			g_EditorRenderHost->SubmitShadowCasters();
		}
		else
		{
			g_EditorRenderHost->SubmitScene();
		}
	}

	// The scene's placed lights light the frame with the spatial ones
	const bool HasLamps = !Shadows && EditorIsLit();

	if (HasLamps && EditorDynamicLights)
	{
		EditorSyncLights();
	}

	// HUD of the played level (PIE)
	if (!Shadows && g_pGameLevel != nullptr && g_hud != nullptr && phase == PHASE_NORMAL)
	{
		g_hud->Render_Last(&Graph);
	}

	// Not every editor creates the spatial database (the Actor Editor has none)
	if (g_SpatialSpace == nullptr)
	{
		return;
	}

	const ESPATIAL_TYPE Types = Shadows
		? (ESPATIAL_TYPE::RENDERABLE | ESPATIAL_TYPE::RENDERABLESHADOW)
		: (ESPATIAL_TYPE::RENDERABLE | ESPATIAL_TYPE::PARTICLE | ESPATIAL_TYPE::LIGHTSOURCE);

	g_SpatialSpace->q_frustum(Graph.lstRenderablesMain, ISpatial_DB::O_ORDERED, Types, ViewBase);

	for (ISpatialShared& SpatialShared : Graph.lstRenderablesMain)
	{
		ISpatial* Spatial = SpatialShared.get();
		if (Spatial == nullptr)
		{
			continue;
		}

		if ((Spatial->type & ESPATIAL_TYPE::LIGHTSOURCE) != ESPATIAL_TYPE::NONE)
		{
			if (HasLamps)
			{
				light* L = (light*)Spatial->dcast_Light();
				if (L && L->get_LOD() > EPS_L)
				{
					Lights.add_light(L);
				}
			}
			continue;
		}

		IRenderable* Renderable = Spatial->dcast_Renderable();
		if (Renderable == nullptr)
		{
			continue;
		}

		// Particles do not cast shadows
		if ((Spatial->type & ESPATIAL_TYPE::PARTICLE) != ESPATIAL_TYPE::NONE && Shadows)
		{
			continue;
		}

		if (IKinematics* Kinematics = PKinematics(Renderable->renderable.visual))
		{
			Kinematics->CalculateBones(TRUE);
		}

		set_Object(Renderable, &Graph);
		Renderable->renderable_Render(&Graph);
		set_Object(nullptr, &Graph);
	}

	Graph.lstRenderablesMain.clear();
}

//---------------------------------------------------------------------------
// Output

IRHIDepthStencilView* CRender::EditorOverlayDepth(GfxCmdList& Cmd)
{
	if (!Target->rt_EditorOverlayDepth)
	{
		return RDepth;
	}

	Target->copy_position();

	IRHIDepthStencilView* Depth = Target->rt_EditorOverlayDepth->pZRT;
	Target->u_setrt(Cmd, Target->get_target_width(), Target->get_target_height(), nullptr, nullptr, nullptr, nullptr, Depth);
	rmNormal();

	Cmd.SetCullMode(ERHI_CULLMODE::NONE);
	Cmd.SetStencil(FALSE);
	Cmd.SetPass(EditorDepthUpscaleShader->E[0]);
	DrawFullscreen(Cmd, 3);

	return Depth;
}

void CRender::EditorBeginOverlay()
{
	GfxCmdList& Cmd = Imm();
	IRHIDepthStencilView* Depth = EditorOverlayDepth(Cmd);

	ps_r_taa_jitter.set(0.f, 0.f, -1.f);
	ps_r_taa_jitter_full.set(ps_r_taa_jitter);
	Cmd.Constants.UpdateScreen();

	Target->u_setrt(Cmd, Target->get_target_width(), Target->get_target_height(), Target->rt_BackbufferLUT->pRT, nullptr, nullptr, nullptr, Depth);
	rmNormal();

	Cmd.SetObjectTransform(Fidentity);
	Cmd.SetView(RFrame.mView, RFrame.mProject);

	Cmd.SetStencil(FALSE);
	Cmd.SetColorWriteEnable();
	Cmd.SetCullMode(ERHI_CULLMODE::BACK);
}

void CRender::EditorPresent(IRHIRenderTargetView* Destination)
{
	Target->PhaseGammaApply(Imm(), Destination);
}

void CRender::EditorEndFrame()
{
	NriPasses.EndFrame();
	Dynamic.EndFrame();
	Scene.EndFrame();

	if (Target)
	{
		Target->CreateSpotShadowDepths();
	}
}
