#include "stdafx.h"

#include "EditorDrawContext.h"
#include "EditorRenderHost.h"
#include "EditorDeviceFlags.h"
#include "SkeletonCustom.h"
#include "dxRenderDeviceRender.h"
#include "WallmarksEngine.h"
#include "../../xrEngine/CustomHUD.h"
#include "../../xrEngine/IGame_Level.h"
#include "../../xrEngine/IGame_Persistent.h"
#include "../../xrEngine/Environment.h"

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
		SubmitScope()
		{
			for (u32 Index = 0; Index < 4; ++Index)
			{
				SavedRT[Index] = RCache.get_RT(Index);
				RCache.set_RT(nullptr, Index);
			}

			SavedZB = GRHI->GetDepthStencilView();
			GRHI->SetDepthStencilView(nullptr);

			SavedPass = GEditorDraw.Pass;
			GEditorDraw.Pass = EEditorDrawPass::Submit;
		}

		~SubmitScope()
		{
			GEditorDraw.Pass = SavedPass;

			for (u32 Index = 0; Index < 4; ++Index)
			{
				RCache.set_RT(SavedRT[Index], Index);
			}
			GRHI->SetDepthStencilView(SavedZB);
		}

	private:
		IRHIRenderTargetView* SavedRT[4] = {};
		IRHIDepthStencilView* SavedZB = nullptr;
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
	// Unlit editor view: G-buffer albedo into the final target
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
			C.r_dx10Texture("s_image", r2_RT_albedo);
			C.r_dx10Sampler("smp_nofilter");
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
			C.r_dx10Sampler("smp_nofilter");
			C.r_End();
		}
	};
}

namespace
{
	// x: 1 in the unlit view (no weather): forward shaders that depend on the environment
	// (water) show their base texture instead
	class CEditorParamsBinder final : public RHIShaderConstant::Setup
	{
	public:
		void setup(RHIShaderConstant* C) override
		{
			RCache.set_c(C, RImplementation.EditorIsLit() ? 0.f : 1.f, 0.f, 0.f, 0.f);
		}
	};

	CEditorParamsBinder EditorParamsBinder;
}

//---------------------------------------------------------------------------
// Unlit view

void CRender::EditorCreate()
{
	dxRenderDeviceRender::Instance().Resources->RegisterConstantSetup("editor_params", &EditorParamsBinder);

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

void CRender::EditorRenderUnlit()
{
	ps_r_taa_jitter.set(0.f, 0.f, -1.f);
	ps_r_taa_jitter_full.set(ps_r_taa_jitter);

	ViewBase.CreateFromMatrix(Device.mFullTransform, FRUSTUM_P_LRTB + FRUSTUM_P_FAR);
	View = nullptr;

	if (g_EditorRenderHost)
	{
		g_EditorRenderHost->PrepareScene();
	}

	// G-buffer of the opaque scene; the albedo starts with the editor background colour, so the
	// whole albedo is the final unlit image
	float ClearColor[4] = { EditorClearColor.r, EditorClearColor.g, EditorClearColor.b, EditorClearColor.a };

	Target->phase_scene_prepare();
	GRHI->ClearTarget(Target->rt_Color->pRT, ClearColor);

	// The unlit image has no distortion pass: keep distortive visuals out of mapDistort,
	// like the deferred path does before its main calc
	o.distortion = false;

	r_pmask(true, false, true);
	phase = PHASE_NORMAL;
	render_main(true);
	r_pmask(true, false);

	rmNormal();
	{
		CEditorFillModeScope FillModeScope(true);
		Target->phase_scene_begin();
		r_dsgraph_render_hud();
		Target->phase_scene_begin();
		r_dsgraph_render_graph(0);
		if (CDetailManager* SceneDetails = GetDetails())
		{
			SceneDetails->Render();
		}
		Target->phase_scene_end();
	}

	if (EditorOcclusionCulling)
	{
		Target->copy_position();
		EditorHiZ.Build();
	}

	// Albedo into a target of the scene size, upscaled to the viewport at the end
	const u32 Width = (u32)RCache.get_width();
	const u32 Height = (u32)RCache.get_height();

	Target->u_setrt(Width, Height, Target->rt_Generic_0->pRT, nullptr, nullptr, nullptr, nullptr);
	rmNormal();

	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
	RCache.set_Stencil(FALSE);
	RCache.set_ColorWriteEnable();
	RCache.set_Element(EditorUnlitShader->E[0]);
	RCache.set_Geometry(Target->FSTriangleGeom);
	RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, 3, 0, 1);

	// Forward geometry (blended surfaces, particles)
	r_pmask(false, true);
	phase = PHASE_NORMAL;
	render_main(false);

	Target->u_setrt(Width, Height, Target->rt_Generic_0->pRT, nullptr, nullptr, nullptr, RDepth);
	rmNormal();
	RCache.set_xform_world(Fidentity);
	RCache.set_xform_view(Device.mView);
	RCache.set_xform_project(Device.mProject);
	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::BACK);

	// Helpers under the blended geometry
	if (g_EditorRenderHost)
	{
		g_EditorRenderHost->DrawSceneHelpers();
	}

	{
		CEditorFillModeScope FillModeScope(true);
		r_dsgraph_render_graph(1);
		r_dsgraph_render_sorted(false);
	}

	mapEmissive.clear();
	mapWmark.clear();
	mapHUDEmissive.clear();
	mapHUDSorted.clear();

	r_pmask(true, false);

	Target->PhaseEditorScale();
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

	Models->Render((dxRender_Visual*)V, Transform, Priority, StrictB2F, LOD);
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
		set_Transform(SubmitTransform);
		add_Visual(V, true);
		return;
	}

	Models->RenderSingle((dxRender_Visual*)V, Transform, LOD);
}

//---------------------------------------------------------------------------
// Scene collection

void CRender::EditorRenderMain(bool Deferred)
{
	marker++;
	EditorCollect(Deferred, false);
}

void CRender::EditorRenderSubspace(CFrustum& Frustum)
{
	marker++;

	CFrustum SavedView = ViewBase;
	ViewBase = Frustum;
	View = &ViewBase;

	EditorCollect(true, true);

	ViewBase = SavedView;
	View = nullptr;
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

	// Pool grows to the scene size and is reused across frames. The lights stay
	// inactive (never registered in the spatial DB), so missing entries cost nothing.
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
	const float FogDistance = g_pGamePersistent->Environment().CurrentEnv->fog_distance;

	for (u32 Index = 0; Index < Written; ++Index)
	{
		const EditorLightDescriptor& Descriptor = Scratch[Index];
		if (Descriptor.Range < 0.1f)
		{
			continue;
		}

		light* Light = EditorLights[Index];
		Light->set_type(Descriptor.Type == (u32)IRender_Light::SPOT ? IRender_Light::SPOT : IRender_Light::POINT);
		// Same rule as CLight_DB::add_light: -noshadows disables every shadowed light
		Light->set_shadow(Descriptor.CastShadow && EditorLightShadows && !o.noshadows);
		// Hardware occlusion queries drop lights hidden behind geometry, as in the game
		// (light::vis_prepare / vis_update). The point light faces inherit it in export_.
		Light->set_occq_mode(true);
		Light->set_hud_mode(false);
		Light->set_position(Descriptor.Position);
		if (Descriptor.Type == (u32)IRender_Light::SPOT)
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

		// Same visibility rules as the spatial lights (CLight_DB::add_light).
		if (!ViewBase.testSphere_dirty(Light->SpatialComponent->sphere.P, Light->SpatialComponent->sphere.R))
		{
			continue;
		}
		if (Device.vCameraPosition.distance_to_sqr(Light->SpatialComponent->sphere.P) > _sqr(FogDistance))
		{
			continue;
		}

		// Shadowed lights fade out with the screen size (accum multiplies the colour by the LOD):
		// skip the faded ones like the spatial lights do
		if (Light->get_LOD() <= EPS_L)
		{
			continue;
		}

		// Several editor viewports render within one Device.dwFrame, so the per-frame
		// caches of CLight_DB::add_light and light::xform_calc are bypassed explicitly.
		Light->m_xform_frame = Device.dwFrame + 1;
		// The 6 shadowed faces of a point light recompute their xforms in export_
		for (light* Face : Light->omnipart)
		{
			if (Face != nullptr)
			{
				Face->m_xform_frame = Device.dwFrame + 1;
			}
		}
		Light->export_(Lights.package);
	}
}

void CRender::EditorCollect(bool Deferred, bool Shadows)
{
	set_Object(nullptr);
	// Normal (static) items and details apply the material without an object
	EditorApplyHemi();

	// The editors control visibility themselves (their device flags have no rsDraw* bits)
	if (g_EditorRenderHost)
	{
		SubmitScope Scope;
		if (Shadows)
		{
			g_EditorRenderHost->SubmitShadowCasters();
		}
		else
		{
			g_EditorRenderHost->SubmitScene();
		}
	}

	// Placed editor lights join the game lighting pipeline with the spatial ones
	if (Deferred && !Shadows && EditorDynamicLights && EditorIsLit())
	{
		EditorSyncLights();
	}

	// HUD of the played level (PIE)
	if (!Shadows && g_pGameLevel != nullptr && g_hud != nullptr && phase == PHASE_NORMAL)
	{
		g_hud->Render_Last();
	}

	// Not every editor creates the spatial database (the Actor Editor has none)
	if (g_SpatialSpace == nullptr)
	{
		return;
	}

	const ESPATIAL_TYPE Types = Shadows
		? (ESPATIAL_TYPE::RENDERABLE | ESPATIAL_TYPE::RENDERABLESHADOW)
		: (ESPATIAL_TYPE::RENDERABLE | ESPATIAL_TYPE::PARTICLE | ESPATIAL_TYPE::LIGHTSOURCE);

	g_SpatialSpace->q_frustum(lstRenderablesMain, ISpatial_DB::O_ORDERED, Types, ViewBase);

	for (ISpatialShared& SpatialShared : lstRenderablesMain)
	{
		ISpatial* Spatial = SpatialShared.get();
		if (Spatial == nullptr)
		{
			continue;
		}

		if ((Spatial->type & ESPATIAL_TYPE::LIGHTSOURCE) != ESPATIAL_TYPE::NONE)
		{
			if (Deferred && !Shadows && EditorIsLit())
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

		// Particles are forward only and do not cast shadows
		if ((Spatial->type & ESPATIAL_TYPE::PARTICLE) != ESPATIAL_TYPE::NONE && (Deferred || Shadows))
		{
			continue;
		}

		if (IKinematics* Kinematics = PKinematics(Renderable->renderable.visual))
		{
			Kinematics->CalculateBones(TRUE);
		}

		set_Object(Renderable);
		Renderable->renderable_Render();
		set_Object(nullptr);
	}

	lstRenderablesMain.clear();
}

//---------------------------------------------------------------------------
// Output

IRHIDepthStencilView* CRender::EditorOverlayDepth()
{
	if (!Target->rt_EditorOverlayDepth)
	{
		return RDepth;
	}

	Target->copy_position();

	IRHIDepthStencilView* Depth = Target->rt_EditorOverlayDepth->pZRT;
	Target->u_setrt(Target->get_target_width(), Target->get_target_height(), nullptr, nullptr, nullptr, nullptr, Depth);
	rmNormal();

	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
	RCache.set_Stencil(FALSE);
	RCache.set_Element(EditorDepthUpscaleShader->E[0]);
	RCache.set_Geometry(Target->FSTriangleGeom);
	RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, 3, 0, 1);

	return Depth;
}

void CRender::EditorBeginOverlay()
{
	IRHIDepthStencilView* Depth = EditorOverlayDepth();

	ps_r_taa_jitter.set(0.f, 0.f, -1.f);
	ps_r_taa_jitter_full.set(ps_r_taa_jitter);

	Target->u_setrt(Target->get_target_width(), Target->get_target_height(), Target->rt_BackbufferLUT->pRT, nullptr, nullptr, nullptr, Depth);
	rmNormal();

	RCache.set_xform_world(Fidentity);
	RCache.set_xform_view(Device.mView);
	RCache.set_xform_project(Device.mProject);

	RCache.set_Stencil(FALSE);
	RCache.set_ColorWriteEnable();
	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::BACK);
}

void CRender::EditorPresent(IRHIRenderTargetView* Destination)
{
	Target->PhaseGammaApply(Destination);
}
