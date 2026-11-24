#include "stdafx.h"

// What the distortion target holds where nothing pushes the picture: the value its readers take away. Half would
// push every pixel by a part of a texel and blur the frame
static constexpr float NoDistortion[4] = { 127.0f / 255.0f, 127.0f / 255.0f, 127.0f / 255.0f, 127.0f / 255.0f };
#include "r1_GlowManager.h"
#include "StaticTraversalStats.h"
#include "../../xrEngine/IGame_Actor.h"

#include "../../xrEngine/IGame_Persistent.h"
#include "FBasicVisual.h"
#include "../../xrEngine/CustomHUD.h"
#include "../../xrEngine/xr_object.h"

#include "EditorDrawContext.h"

#include "OverlayAPI/FSR3Wrapper.h"

#include "../../xrEngine/GameFont.h"
#include "../../xrEngine/x_ray.h"
#include "SkeletonCustom.h"
#include "CHudInitializer.h"
#include "EditorRenderHost.h"
#include "RenderThread.h"
#include "../../xrEngine/Environment.h"
#include "dxLensFlareRender.h"
#include "dxRainRender.h"
#include "dxThunderboltRender.h"
#include "../../xrEngine/IGame_Actor.h"

void CRender::BeginMainCollect()
{
	GraphMain.private_visuals.clear();
	// The editors move the camera for their subspaces
	if (Device.IsEditorMode())
	{
		return;
	}

	GraphMain.ViewParams = MakeGraphView(phase);
	GraphMain.ViewParams.Distortion = o.distortion;
	GraphMain.ViewParams.TrackStats = true;
	GraphMain.OwnsView = true;
}

void CRender::render_main	(GfxCmdList& Cmd)
{
	GPU_EVENT(render_main);
	if (Device.IsEditorMode())
	{
		EditorRenderMain(Cmd);
		return;
	}
//	Msg						("---begin");
	BeginMainCollect();
	Scene.BeginObjects(GraphMain);
	bool dont_test_sectors = Sectors.size() <= 1;

	// Calculate sector(s) and their objects
	if (pLastSector)		
	{
		//!!!
		//!!! BECAUSE OF PARALLEL HOM RENDERING TRY TO DELAY ACCESS TO HOM AS MUCH AS POSSIBLE
		//!!!
		// Traverse object database
		g_SpatialSpace->q_frustum
		(
			GraphMain.lstRenderablesMain,
		ISpatial_DB::O_ORDERED,
		ESPATIAL_TYPE::RENDERABLE | ESPATIAL_TYPE::RENDERABLESHADOW | ESPATIAL_TYPE::PARTICLE | ESPATIAL_TYPE::LIGHTSOURCE,
		ViewBase);//nearest sorting

		// Determine visibility for dynamic part of scene
		set_Object							(0);
		u32 uID_LTRACK						= 0xffffffff;
		if (phase==CRender::PHASE_NORMAL)
		{
			// update light-vis for current entity / actor
			if (CObject* O = g_pGameLevel->CurrentViewEntity())
			{
				if(!O->getDestroy())
				{
					if (CROS_impl*	R = (CROS_impl*)O->ROS())
					{
						R->update(O);
						R->update_smooth(O);
					}
				}
			}

			if (GraphMain.lstRenderablesMain.size())
			{
				uLastLTRACK	++;
				uID_LTRACK = uLastLTRACK % GraphMain.lstRenderablesMain.size();

				// update light-vis for selected entity
				// track lighting environment
				if (IRenderable* renderable = (IRenderable*)GraphMain.lstRenderablesMain[uID_LTRACK]->dcast_Renderable())
				{
					if (CROS_impl* T = (CROS_impl*)renderable->renderable_ROS())
						T->update(renderable);
				}
			}
		}

		// Traverse sector/portal structure
		if (!dont_test_sectors)
		{
			GraphMain.PortalTraverser.ViewportNear = RFrame.fViewportNear;
			GraphMain.PortalTraverser.prepare_local_clips((u32)Sectors.size(), (u32)Portals.size());
			GraphMain.PortalTraverser.traverse
				(
				pLastSector,
				ViewBase,
				vLastCameraPos,
				RFrame.mFullTransform,
				CPortalTraverser::VQ_HOM + CPortalTraverser::VQ_SSA + CPortalTraverser::VQ_FADE
				//. disabled scissoring (HW.Caps.bScissor?CPortalTraverser::VQ_SCISSOR:0)	// generate scissoring info
				);
		}

		BeginMainStatic(dont_test_sectors);
		Scene.CollectObjects(GraphMain);

		if (g_pGameLevel && psDeviceFlags.test(rsDrawDynamic) && (phase==PHASE_NORMAL))
		{
			PROF_EVENT("Render HUD");
			g_hud->Render_Last(&Scene.Objects());		// HUD
		}
		EndMainStatic();
	}
	else
	{
		set_Object(0);
		if (g_pGameLevel && psDeviceFlags.test(rsDrawDynamic) && (phase == PHASE_NORMAL))
		{
			PROF_EVENT("Render HUD");
			g_hud->Render_Last(&Scene.Objects());		// HUD
		}
	}
}

// When the scene draws the level's statics (GpuScene::IsMainListed) the few it leaves are collected into GraphMain
// here, before the moving objects; otherwise the sectors are walked on a worker into GraphMainStatic until EndMainStatic
void CRender::BeginMainStatic(bool dont_test_sectors)
{
	if (Scene.IsMainListed())
	{
		GraphMain.val_pTransform = &Fidentity;
		CollectMainStatic(GraphMain, dont_test_sectors);
		return;
	}

	R_dsgraph_structure& Graph = GraphMainStatic;
	Graph.ViewParams = GraphMain.ViewParams;
	Graph.OwnsView = true;
	Graph.CollectForwardTogether = GraphMain.CollectForwardTogether;
	Graph.r_pmask(GraphMain.pmask[0], GraphMain.pmask[1], GraphMain.pmask[2]);
	Graph.val_pTransform = &Fidentity;
	Graph.private_visuals.clear();
	MainStaticTask.run([this, dont_test_sectors]() { CollectMainStatic(GraphMainStatic, dont_test_sectors); });
}

void CRender::EndMainStatic()
{
	if (Scene.IsMainListed())
	{
		return;
	}

	PROF_EVENT("join add_static")
	MainStaticTask.wait();
	GraphMain.TakeStatic(GraphMainStatic);
}

// Reads the portal traversal of GraphMain and the occlusion map, writes only Graph and the occlusion test frames of the
// level's visuals, which no other view tests
void CRender::CollectMainStatic(R_dsgraph_structure& Graph, bool dont_test_sectors)
{
	PROF_EVENT("add_static")
	if (!psDeviceFlags.test(rsDrawStatic))
	{
		return;
	}

	const auto Collect = [this, &Graph](CSector* Sector, CFrustum& Frustum)
	{
		Graph.View = &Frustum;
		const xr_vector<dxRender_Visual*>* Own = Scene.GetMainStatics(Sector->index);

		if (!Own)
		{
			Graph.add_Static(Sector->root(), Frustum.getMask());
			return;
		}

		for (dxRender_Visual* Visual : *Own)
		{
			Graph.add_Static(Visual, Frustum.getMask());
		}
	};

	if (dont_test_sectors)
	{
		Collect((CSector*)Sectors[0], ViewBase);
	}
	else if (psDeviceFlags.test(rsClearBB))
	{
		for (auto visual : Visuals)
		{
			Graph.r_dsgraph_insert_static(visual);
		}
	}
	else
	{
		const bool CaptureStats = Graph.ViewParams.Phase == CRender::PHASE_NORMAL && g_StaticTraversalStats.BeginFrame();
		for (IRender_Sector* Item : GraphMain.PortalTraverser.r_sectors)
		{
			CSector* sector = (CSector*)Item;
			g_StaticTraversalStats.BeginSector(sector);
			for (CFrustum& Frustum : GraphMain.PortalTraverser.local_frustums[sector->index])
			{
				Collect(sector, Frustum);
			}
			g_StaticTraversalStats.EndSector();
		}
		if (CaptureStats)
		{
			g_StaticTraversalStats.EndFrame();
		}
	}
	Graph.View = nullptr;
	Graph.SortStatic();
}

void CRender::render_menu(GfxCmdList& Cmd) {
	GPU_EVENT(render_menu);
	//	Globals
	Cmd.SetCullMode(ERHI_CULLMODE::BACK);
	Cmd.SetStencil(false);
	Cmd.SetColorWriteEnable();

	// Main Render
	{
		Target->u_setrt(Cmd, Target->rt_Generic, 0, 0, 0);		// LDR RT
		rmNormal();

		PlayedScene ? Recorder.Play(PlayedScene->PostMain) : g_pGamePersistent->OnRenderPPUI_main();	// PP-UI
	}

	// Distort
	{
		Target->u_setrt(Cmd, Target->rt_Generic_1, 0, 0, 0);		// Now RT is a distortion mask
		rmNormal();
		Cmd.ClearTarget(Target->rt_Generic_1->pRT, NoDistortion);
		PlayedScene ? Recorder.Play(PlayedScene->PostDistort) : g_pGamePersistent->OnRenderPPUI_PP();	// PP-UI
	}

	// Actual Display
	Target->u_setrt(Cmd, (u32)Cmd.GetTargetWidth(), (u32)Cmd.GetTargetHeight(), RImplementation.Target->rt_BackbufferLUT->pRT, nullptr, nullptr, nullptr);
	rmNormal();

	Cmd.SetPass(Target->s_menu);
	Cmd.SetGeometry(Target->g_menu);

	Fvector2 p0, p1;
	u32 Offset;
	constexpr auto C = color_rgba(255, 255, 255, 255);
	float	_w = Cmd.GetTargetWidth();
	float	_h = Cmd.GetTargetHeight();
	float	d_Z = EPS_S;
	float	d_W = 1.f;

	p0.set(.5f / _w, .5f / _h);
	p1.set((_w + .5f) / _w, (_h + .5f) / _h);

	FVF::TL* pv = (FVF::TL*)Dynamic.Lock(4, EVertexFormat::TL, Offset);

	pv->set(EPS, float(_h + EPS), d_Z, d_W, C, p0.x, p1.y);	pv++;
	pv->set(EPS, EPS, d_Z, d_W, C, p0.x, p0.y);	pv++;
	pv->set(float(_w + EPS), float(_h + EPS), d_Z, d_W, C, p1.x, p1.y);	pv++;
	pv->set(float(_w + EPS), EPS, d_Z, d_W, C, p1.x, p0.y);	pv++;

	Dynamic.Unlock(4);
	Dynamic.DrawQuads(Cmd, 1, Offset);
}

#include "../../Include/xrRender/UIRender.h"

// The icon's picture stays in rt_ui_color at its place (the interface's scissor, doubled) from the frame that drew it:
// it is drawn again only when its key changes or another icon was drawn over that place
namespace
{
// The camera an icon is drawn with, for the length of its draw: the caller's view and projection, the eye at the origin
// looking along Z. The frame's own camera comes back after it
struct IconCamera
{
	IconCamera(const Fmatrix& View, const Fmatrix& Project)
	{
		RFrame.vCameraTop.set(Fidentity.j);
		RFrame.vCameraRight.set(Fidentity.i);
		RFrame.vCameraDirection.set(Fidentity.k);
		RFrame.vCameraPosition.set(Fidentity.c);
		RFrame.mView = View;
		RFrame.mProject = Project;
		Device.m_pRender->SetCacheXform(RFrame.mView, RFrame.mProject);
	}

	~IconCamera()
	{
		RFrame.vCameraPosition.set(RFrame.vCameraPosition_saved);
		RFrame.vCameraDirection.set(RFrame.vCameraDirection_saved);
		RFrame.vCameraRight.set(RFrame.vCameraRight_saved);
		RFrame.vCameraTop.set(RFrame.vCameraTop_saved);
		RFrame.mView = RFrame.mView_saved;
		RFrame.mProject = RFrame.mProject_saved;
		Device.m_pRender->SetCacheXform(RFrame.mView, RFrame.mProject);
	}
};
} // namespace

void CRender::RenderUI(xr_span<const UIIconVisual> Visuals, const Fmatrix& View, const Fmatrix& Project, Fcolor* color)
{
	const Irect Scissor = UIRender->GetScissor();

	if (DrawRecorder::IsRecording())
	{
		Recorder.Call([this, Kept = xr_vector<UIIconVisual>(Visuals.begin(), Visuals.end()), View, Project, Tint = color ? *color : Fcolor(), HasTint = color != nullptr, Scissor]() mutable
		{
			DrawIcon(Kept, View, Project, HasTint ? &Tint : nullptr, Scissor);
		});
		return;
	}

	DrawIcon(Visuals, View, Project, color, Scissor);
}

void CRender::DrawIcon(xr_span<const UIIconVisual> Visuals, const Fmatrix& View, const Fmatrix& Project, Fcolor* color, Irect Scissor)
{
	const IconCamera Camera(View, Project);
	GfxCmdList& Cmd = Imm();

	ps_r_taa_jitter.set(0, 0, -1);
	ps_r_taa_jitter_full.set(ps_r_taa_jitter);
	Cmd.Constants.UpdateScreen();

	Irect Place = Scissor;
	Place.mul(2, 2);
	const u32 Key = Scene.IconKey(Visuals);

	if (!Scene.IsIconKept(Place, Key))
	{
		// Read before the icon's target, twice the size, becomes the current one
		const u32 Width = Target->get_width();
		const u32 Height = Target->get_height();
		auto pRT_0 = Cmd.GetRenderTarget(0);
		auto pRT_1 = Cmd.GetRenderTarget(1);
		auto pRT_2 = Cmd.GetRenderTarget(2);
		auto pRT_3 = Cmd.GetRenderTarget(3);
		auto pZRT = Cmd.GetDepthStencil();

		Cmd.ClearDepthStencil(Target->rt_ui_depth->pZRT, ERHI_CLEAR_TARGET::DEPTH, 1.0f, 0L);
		Target->u_setrt(Cmd, Target->rt_ui_color, NULL, Target->rt_ui_depth->pZRT);
		Recorder.SetScissor(&Place);
		rmNormal();
		Target->phase_ui_postprocess_clear(Cmd);

		const UIIconLighting* Light = Device.m_pRender->GetUIIconLighting();
		Cmd.Constants.Write<CBUIIcon>().ui_emissive_tint.set(Light ? Light->Color.x * Light->Intensity : 1.0f, Light ? Light->Color.y * Light->Intensity : 1.0f, Light ? Light->Color.z * Light->Intensity : 1.0f, 1.0f);
		Scene.DrawIcon(Cmd, Visuals, Place, Key, Target->rt_ui_color->pRT, Target->rt_ui_depth->pZRT);

		Target->u_setrt(Cmd, Width, Height, pRT_0, pRT_1, pRT_2, pRT_3, pZRT);
		Recorder.SetScissor(&Scissor);
	}

	RImplementation.rmNormal();
	Target->phase_ui_postprocess(Cmd, color);
}

CRender::SurfaceParams CRender::BeginUIImage()
{
	// The item interface's target: its frame has read it by the time the tools draw, and Render clears it first
	CRT& Image = *Target->rt_ui_pda;
	const bool IsFirst = UIImageFrame != Device.dwFrame;
	UIImageFrame = Device.dwFrame;

	// The target is switched where the picture's draws are: with them in the list when the frame is recorded, or they
	// are played into whatever the frame draws to by then
	auto Begin = [this, IsFirst]
	{
		GfxCmdList& Cmd = Imm();

		for (u32 Index = 0; Index < 4; ++Index)
		{
			UIImageTargets[Index] = Cmd.GetRenderTarget(Index);
		}

		UIImageDepth = Cmd.GetDepthStencil();
		UIImageWidth = Target->get_width();
		UIImageHeight = Target->get_height();

		if (IsFirst)
		{
			Cmd.ClearTarget(Target->rt_ui_pda->pRT, ERTColor::Transparent);
		}

		Target->u_setrt(Cmd, Target->rt_ui_pda, 0, 0);
		rmNormal();
	};

	if (DrawRecorder::IsRecording())
	{
		Recorder.Call(Begin);
	}
	else
	{
		Begin();
	}

	return { float(Image.dwWidth), float(Image.dwHeight), RHIUtils::ImGui::Image(Image.pSurface) };
}

void CRender::EndUIImage()
{
	if (DrawRecorder::IsRecording())
	{
		Recorder.Call([this] { EndUIImage(); });
		return;
	}

	GfxCmdList& Cmd = Imm();
	Target->u_setrt(Cmd, UIImageWidth, UIImageHeight, UIImageTargets[0], UIImageTargets[1], UIImageTargets[2], UIImageTargets[3], UIImageDepth);
	Cmd.ApplyRenderTargets();
}

void CRender::Render()
{
	if (DrawRecorder::IsRecording())
	{
		RecordRender();
		return;
	}

	// A recorded scene is drawn with what its frame began with: the device may be the next frame's by now
	if (!PlayedScene)
	{
		RFrame.Sync();
	}

	GPU_EVENT(CRender_Render);
	VERIFY(0 == GraphMain.mapDistort.size() + GraphMain.mapHUDDistort.size());
	GfxCmdList& Cmd = Imm();

	// The editors render their scene without a level, HUD or menu
	const bool EditorMode = Device.IsEditorMode();

	if (!EditorMode)
	{
		bool _menu_pp = PlayedScene ? PlayedScene->Path == 1 : g_pGamePersistent ? g_pGamePersistent->OnRenderPPUI_query() : false;

		if (_menu_pp)
		{
			render_menu(Cmd);
			return;
		};

		IMainMenu* pMainMenu = g_pGamePersistent ? g_pGamePersistent->m_pMainMenu : 0;
		bool bMenu = pMainMenu ? pMainMenu->CanSkipSceneRendering() : false;
		bMenu |= !(g_pGameLevel && g_hud);
		bMenu = PlayedScene ? PlayedScene->Path == 2 : bMenu;

		if (bMenu)
		{
			Target->u_setrt(Cmd, (u32)Cmd.GetTargetWidth(), (u32)Cmd.GetTargetHeight(), RImplementation.Target->rt_BackbufferLUT->pRT, nullptr, nullptr, nullptr);
			return;
		}
	}

	if(m_bFirstFrameAfterReset)
	{
		xrRender_apply_tf();
		m_bFirstFrameAfterReset = false;

		// The editors resize their targets often and redraw on demand: no skipped frame
		if (!EditorMode)
		{
			return;
		}
	}

	// The editors run no graph collection of the engine's frame (BeginGraphCollect): the forward frame's settings are
	// taken here
	if (EditorMode)
	{
		SyncShadingQuality();
	}

	// Depth of a recent frame for the occlusion culling of this one
	if (EditorMode && EditorOcclusionCulling)
	{
		EditorHiZ.Fetch();
	}

	if (EditorMode && !EditorIsLit())
	{
		EditorRenderUnlit(Cmd);
		return;
	}

	RImplementation.o.distortion = false;

	// Editors and frames the pre-render stage could not foresee collect on this thread
	if (g_RenderFeatures.SunShadows && (EditorMode || SunCollectFrame != RFrame.dwFrame))
	{
		prepare_sun_collect(RFrame.dwFrame);
	}

	// Reflections can sample the captures before this frame's sun pass.
	Cmd.Constants.UpdateSunShadows();

	if (g_RenderFeatures.MotionVectors && ((g_RenderFeatures.Upscaling && ps_r_scale_mode > 1) || ps_r2_aa_type == 3))
	{
		s32 jitterPhaseCount = g_Fsr3Wrapper.GetJitterPhaseCount(static_cast<u32>(Cmd.GetWidth()), static_cast<u32>(Cmd.GetTargetWidth()));
		g_Fsr3Wrapper.GetJitterOffset(ps_r_taa_jitter_full.x, ps_r_taa_jitter_full.y, RFrame.dwFrame, jitterPhaseCount);

		ps_r_taa_jitter_full = ps_r_taa_jitter_full.mul(ps_r_taa_jitter_scale);

		ps_r_taa_jitter.x = 2.0f * ps_r_taa_jitter_full.x / Cmd.GetWidth();
		ps_r_taa_jitter.y = -2.0f * ps_r_taa_jitter_full.y / Cmd.GetHeight();
		ps_r_taa_jitter.z = float(RFrame.dwFrame % jitterPhaseCount) / float(jitterPhaseCount) + EPS;
		Cmd.Constants.UpdateScreen();
	}
	else
	{
		ps_r_taa_jitter.set(0, 0, -1);
		ps_r_taa_jitter_full.set(ps_r_taa_jitter);
		Cmd.Constants.UpdateScreen();
	}

	Device.ScopePostprocessActive = false;
	// Shared item UI is rendered once before any world or HUD passes.
	Cmd.ClearTarget(Target->rt_ui_pda->pRT, ERTColor::Transparent);
	if (PlayedScene ? PlayedScene->ItemUi.IsRecorded : GActorInterface != nullptr)
	{
		GPU_EVENT(ItemUI);
		Target->u_setrt(Cmd, Target->rt_ui_pda, 0, 0);
		rmNormal();

		PlayedScene ? Recorder.Play(PlayedScene->ItemUi) : GActorInterface->RenderItemUI();
	}

	RenderForwardLevel(Cmd);
}

// The rest of the frame: the level is drawn forward, everything goes lit into the colour target. Each pass asks for the
// feature it needs
// The game's thread asked for the scene while it records its draws: the render thread gets the call, and the game makes
// here the draws the scene's frame would have called it for, by the checks Render makes on its way to each
void CRender::RecordRender()
{
	if (RecordedSceneCount == RecordedScenes.size())
	{
		RecordedScenes.push_back(xr_make_unique<SceneDraws>());
	}

	SceneDraws& Draws = *RecordedScenes[RecordedSceneCount++];
	Draws.ItemUi.Clear();
	Draws.ActiveItemUi.Clear();
	Draws.PostMain.Clear();
	Draws.PostDistort.Clear();

	Recorder.Call([this, Played = &Draws]
	{
		PlayedScene = Played;
		Render();
		PlayedScene = nullptr;
	});

	const auto Record = [](DrawRecorder::List& Into, auto&& Draw)
	{
		DrawRecorder::Scope Open(Into);
		Draw();
	};

	const auto RecordPost = [&]
	{
		Record(Draws.PostMain, [] { g_pGamePersistent->OnRenderPPUI_main(); });
		Record(Draws.PostDistort, [] { g_pGamePersistent->OnRenderPPUI_PP(); });
	};

	Draws.Path = 0;

	if (g_pGamePersistent && g_pGamePersistent->OnRenderPPUI_query())
	{
		Draws.Path = 1;
		RecordPost();
		return;
	}

	IMainMenu* Menu = g_pGamePersistent ? g_pGamePersistent->m_pMainMenu : nullptr;

	if ((Menu && Menu->CanSkipSceneRendering()) || !g_pGameLevel || !g_hud)
	{
		Draws.Path = 2;
		return;
	}

	if (m_bFirstFrameAfterReset)
	{
		return;
	}

	if (GActorInterface)
	{
		Record(Draws.ItemUi, [] { GActorInterface->RenderItemUI(); });
	}

	// The game's code reads the device's camera while it draws the HUD's item: the HUD's, as CHudInitializer gives it
	if (g_hud->RenderActiveItemUIQuery())
	{
		const Fmatrix View = Device.mView;
		const Fmatrix Project = Device.mProject;
		const Fmatrix Full = Device.mFullTransform;
		Device.mView.set(Device.mView_hud);
		Device.mProject.set(Device.mProject_hud);
		Device.mFullTransform.mul(Device.mProject, Device.mView);
		Record(Draws.ActiveItemUi, [] { g_hud->RenderActiveItemUI(); });
		Device.mView.set(View);
		Device.mProject.set(Project);
		Device.mFullTransform.set(Full);
	}

	RecordPost();
}

// With r_render_thread the level's collect runs ahead of the frame, on the thread that hands the frame over: the
// checks are those Render makes before it draws a level
void CRender::CollectFrame()
{
	IMainMenu* Menu = g_pGamePersistent ? g_pGamePersistent->m_pMainMenu : nullptr;

	if (Device.IsEditorMode() || !g_pGameLevel || !g_hud || m_bFirstFrameAfterReset || (g_pGamePersistent && g_pGamePersistent->OnRenderPPUI_query()) ||
		(Menu && Menu->CanSkipSceneRendering()))
	{
		return;
	}

	PROF_EVENT("CollectFrame");
	Calculate();

	if (g_RenderFeatures.SunShadows && SunCollectFrame != RFrame.dwFrame)
	{
		prepare_sun_collect(RFrame.dwFrame);
	}

	CollectForwardLevel();
	CollectedFrame = RFrame.dwFrame;
}

// What the level frame reads of the game's objects: the main view's visibility, its moving objects with their poses,
// the lights they bring
void CRender::CollectForwardLevel()
{
	o.distortion = o.distortion_enabled;
	vis_intersect = false;
	phase = PHASE_NORMAL;
	ViewBase.CreateFromMatrix(RFrame.mFullTransform, FRUSTUM_P_LRTB + FRUSTUM_P_FAR);
	GraphMain.View = nullptr;

	if (Device.IsEditorMode() && g_EditorRenderHost)
	{
		g_EditorRenderHost->PrepareScene();
	}

	{
		PROF_EVENT("OcclusionMap");
		HOM.Enable();
		HOM.Render(ViewBase);
	}

	BeginFarSunCascades();
	GraphMain.r_pmask(true, true);
	SurfaceLights.clear();

	{
		PROF_EVENT("CollectMain");
		render_main(Imm());
		Scene.BuildObjects(GraphMain);
	}

	// The nearest lights first: the frame has room for 64 shadow maps and 63 lights, and in the order the lights were
	// found a lamp at arm's length went without its shadow while lamps thirty metres off had six maps each
	std::stable_sort(SurfaceLights.begin(), SurfaceLights.end(), [](const light* Left, const light* Right)
	{
		return Left->position.distance_to(RFrame.vCameraPosition) - Left->range < Right->position.distance_to(RFrame.vCameraPosition) - Right->range;
	});

	TakeWeatherEffects();

	if (g_RenderFeatures.SpriteGlows && L_Glows)
	{
		L_Glows->Collect();
	}

	// What casts in the lamps' maps and the cascades: the lamps' collect takes the cascades' with it
	CollectForwardLampShadows();

	if (SunCascadeDrawnCount)
	{
		CollectShadowCasters();
	}

	Skinning.Capture();
}

// What the frame draws of the flares, the bolt and the rain, as the game's frame left them: after the view's frustum,
// which culls the rain's drops
void CRender::TakeWeatherEffects()
{
	if (!g_pGamePersistent || !g_pGamePersistent->pEnvironment || (!g_pGameLevel && !Device.IsEditorMode()))
	{
		return;
	}

	CEnvironment& Environment = *g_pGamePersistent->pEnvironment;
	dxLensFlareRender::Take(*Environment.eff_LensFlare);
	dxThunderboltRender::Take(*Environment.eff_Thunderbolt);
	dxRainRender::Take(*Environment.eff_Rain);
}

// The sky and its clouds, from the frame's copy of the weather
void CRender::DrawSky()
{
	if (!g_pGameLevel && !Device.IsEditorMode())
	{
		return;
	}

	CEnvironment& Environment = g_pGamePersistent->Environment();
	Environment.m_pRender->RenderSky(Environment);

	if (!fis_zero(RFrame.Env().clouds_color.w, EPS_L))
	{
		Environment.m_pRender->RenderClouds(Environment);
	}
}

void CRender::RenderForwardLevel(GfxCmdList& Cmd)
{
	GPU_EVENT(FORWARD_LEVEL_FRAME);
	const bool EditorMode = Device.IsEditorMode();
	CDetailManager* const SceneDetails = GetDetails();

	if (EditorMode || CollectedFrame != RFrame.dwFrame)
	{
		CollectForwardLevel();
	}

	Cmd.SetObjectTransform(Fidentity);

	// The surface shaders read the rain's map: it is drawn before any of them
	RenderRainMap(Cmd);

	RenderForwardLampShadows(Cmd);

	{
		GPU_EVENT(SurfaceLights);
		UploadSurfaceLights(Cmd);
	}

	{
		PROF_EVENT("ForwardDecals");
		BuildForwardDecals(Cmd);
	}

	RenderFarSunCascade(Cmd);
	HeightfieldShadows.Render(Cmd);

	// The cube of what is around the eye, drawn with this frame's lights and sun cascades and without the jitter.
	// Its faces cull the scene's clusters into the buffers the main view's cull fills next
	if (HasReflectionCube() && !EditorMode)
	{
		GPU_EVENT(OffscreenReflections);
		BindRainTextures(Cmd);
		const Fvector3 Jitter = ps_r_taa_jitter;
		const Fvector3 JitterFull = ps_r_taa_jitter_full;
		ps_r_taa_jitter.set(0, 0, -1);
		ps_r_taa_jitter_full.set(ps_r_taa_jitter);
		Cmd.Constants.UpdateScreen();
		render_reflections(Cmd);
		ps_r_taa_jitter = Jitter;
		ps_r_taa_jitter_full = JitterFull;
		Cmd.Constants.UpdateScreen();
		Cmd.SetObjectTransform(Fidentity);
		Target->u_setrt(Cmd, Target->rt_Generic_0, nullptr, RDepth);
		rmNormal();
		Cmd.SetStencil(false);
		Cmd.SetColorWriteEnable();
	}

	{
		PROF_EVENT("ForwardCull");
		Scene.UploadModels(Cmd);
		Scene.CullForward(GraphMain);
	}

	BeginForwardMsaa();
	Cmd.SetObjectTransform(Fidentity);
	Target->u_setrt(Cmd, Target->rt_Generic_0, nullptr, RDepth);
	Cmd.ClearTarget(Target->rt_Generic_0->pRT);
	Cmd.ClearDepthStencil(RDepth, ERHI_CLEAR_TARGET::DEPTH | ERHI_CLEAR_TARGET::STENCIL, 1.f, 0);
	rmNormal();
	Cmd.SetStencil(false);
	Cmd.SetColorWriteEnable();

	BindRainTextures(Cmd);

	if (!ForwardOcclusion())
	{
		RenderSkyBehind(Cmd);
	}

	if (ForwardOcclusion())
	{
		{
			GPU_EVENT(ScenePrepass);
			CEditorFillModeScope FillModeScope(Cmd, EditorMode);

			if (ForwardVelocityPass())
			{
				const ref_rt& Velocity = Target->rt_VelocityMs ? Target->rt_VelocityMs : Target->rt_Velocity;
				const float Still[4] = {};
				Cmd.ClearTarget(Velocity->pRT, Still);
				// What the depth pass does not draw must not keep a mark of a model from an earlier frame
				Cmd.ClearTarget((Target->rt_ThinMs ? Target->rt_ThinMs : Target->rt_Thin)->pRT, Still);
				Target->u_setrt(Cmd, Target->rt_Generic_0, Target->rt_ThinMs ? Target->rt_ThinMs : Target->rt_Thin, Velocity, RDepth);
			}
			else if (ForwardOcclusionNormals())
			{
				Target->u_setrt(Cmd, Target->rt_Generic_0, Target->rt_ThinMs ? Target->rt_ThinMs : Target->rt_Thin, RDepth);
			}

			{
				PROF_EVENT("ForwardDepthGraph");
				GraphMain.r_dsgraph_render_hud(0);
				GraphMain.r_dsgraph_render_graph(0, false, 0);
			}

			{
				const u32 Count = ForwardVelocityPass() ? 3 : ForwardOcclusionNormals() ? 2 : 1;
				// Under multisampling the frame draws into the multisampled ones
				IRHIRenderTargetView* Views[3] = { Target->rt_Generic_0->pRT, Count > 1 ? (Target->rt_ThinMs ? Target->rt_ThinMs : Target->rt_Thin)->pRT : nullptr, Count > 2 ? (Target->rt_VelocityMs ? Target->rt_VelocityMs : Target->rt_Velocity)->pRT : nullptr };
				// The level as ids, shaded once a pixel after everything opaque: its bins leave the passes below
				const bool IsIds = Scene.IsVisibilityFrame();
				// 2048: nor the models, whose depth pass keeps their surfaces for the same pass to light
				const u32 Rest = IsIds ? 512u | 2048u : 0u;

				if (IsIds)
				{
					const float None[4] = {};
					Cmd.ClearTarget(Target->rt_Ids->pRT, None);
					Cmd.ClearTarget(Target->rt_Surface->pRT, None);
					Cmd.ClearTarget(Target->rt_Thin->pRT, None);
					Scene.DrawForward(Cmd, &Target->rt_Ids->pRT, 1, RDepth, false, 1 | 256);
				}

				Scene.DrawForward(Cmd, Views, Count, RDepth, false, 1 | Rest);
				// The models and the grass, whose depth pass keeps their surfaces for the frame to light
				IRHIRenderTargetView* Kept[4];
				std::copy(Views, Views + Count, Kept);
				Kept[Count] = Target->rt_Surface->pRT;
				const auto DrawKept = [&](u32 Part)
				{
					return IsIds && Scene.DrawForward(Cmd, Kept, Count + 1, RDepth, false, Part | 1024);
				};
				// The grass keeps nothing else: a pixel of it is written many times over
				const auto DrawGrassKept = [&]
				{
					return IsIds && Scene.DrawForward(Cmd, &Target->rt_Surface->pRT, 1, RDepth, false, 4 | 1024);
				};
				DrawKept(1);

				// r_aref_occlusion 4: the grass hides the grass. It is culled here, against the pyramid the frame before
				// left, which holds that frame's whole depth with its grass
				const bool IsGrassHiding = ps_r_aref_occlusion == 4;

				if (IsGrassHiding && SceneDetails)
				{
					ForwardPassOnly = 0;
					SceneDetails->Render(Cmd);
				}

				// What the frame before did not show is drawn where the depth so far does not hide it.
				// r_aref_occlusion 3 and 4: where it mostly hides it, for everything the pyramid is asked about
				Scene.BuildHiZ(Cmd, RDepth, ps_r_aref_occlusion >= 3);

				// r_aref_occlusion: what is cut out by its texture hides the grass too. The level's second part is
				// drawn before the grass is culled, and the pyramid is built again with its depth: a texel of it
				// hides only where every pixel under it was drawn, so a crown with holes hides little
				if (ps_r_aref_occlusion == 1 || ps_r_aref_occlusion == 2)
				{
					if (Scene.CullForward(GraphMain, true))
					{
						if (IsIds)
						{
							Scene.DrawForward(Cmd, &Target->rt_Ids->pRT, 1, RDepth, false, 2 | 256);
						}

						Scene.DrawForward(Cmd, Views, Count, RDepth, false, 2 | Rest);
						DrawKept(2);
						Scene.BuildHiZ(Cmd, RDepth, ps_r_aref_occlusion > 1);
					}

					if (SceneDetails)
					{
						ForwardPassOnly = 0;
						SceneDetails->Render(Cmd);
					}

					if (Scene.IsForwardGrassReady())
					{
						if (!DrawGrassKept())
						{
							Scene.DrawForward(Cmd, Views, Count, RDepth, false, 4);
						}
					}
				}
				else
				{
					// The grass after the pyramid: the scene culls it here, against that depth too, and draws what it
					// finds with the level's second part
					if (SceneDetails && !IsGrassHiding)
					{
						ForwardPassOnly = 0;
						SceneDetails->Render(Cmd);
					}

					if (Scene.CullForward(GraphMain, true) || Scene.IsForwardGrassReady())
					{
						if (IsIds)
						{
							Scene.DrawForward(Cmd, &Target->rt_Ids->pRT, 1, RDepth, false, 2 | 256);
						}

						Scene.DrawForward(Cmd, Views, Count, RDepth, false, 2 | Rest);
						DrawKept(2);
						DrawGrassKept();
					}

					if (IsGrassHiding)
					{
						Scene.BuildHiZ(Cmd, RDepth, true);
					}
				}
			}
		}

		RenderForwardOcclusion(Cmd);

		// The editors cull the objects of their next frame by the depth the occlusion has just copied
		if (EditorMode && EditorOcclusionCulling)
		{
			EditorHiZ.Build(Cmd);
		}

		// Before any colour of the frame: what stands in front of the level is drawn over it by its depth
		if (Scene.IsVisibilityFrame())
		{
			GPU_EVENT(SceneLight);
			// A surface is read or written, not both: nothing of the pass stays bound as a target
			Target->u_setrt(Cmd, Target->rt_Generic_0->ViewWidth(), Target->rt_Generic_0->ViewHeight(), nullptr, nullptr, nullptr, nullptr);
			Cmd.SetView(RFrame.mView, RFrame.mProject);
			Scene.Light(Cmd, Target->rt_Surface->pSurface, Target->rt_Position->pSurface, Target->rt_Thin->pSurface, ForwardReflections() ? Target->SslrOutput()->pSurface : nullptr,
				Target->rt_SkyBehind ? Target->rt_SkyBehind->pSurface : nullptr, Target->rt_Generic_0->pSurface);
			Target->u_setrt(Cmd, Target->rt_Generic_0, nullptr, RDepth);
		}

		{
			GPU_EVENT(ForwardHud);
			CEditorFillModeScope FillModeScope(Cmd, EditorMode);
			BindForwardDecals(Cmd);
			GraphMain.r_dsgraph_render_hud(1);
		}

		Cmd.SetObjectTransform(Fidentity);

		{
			GPU_EVENT(ForwardOpaque);
			CEditorFillModeScope FillModeScope(Cmd, EditorMode);
			GraphMain.r_dsgraph_render_graph(0, true, 1);
			Scene.DrawForward(Cmd, &Target->rt_Generic_0->pRT, 1, RDepth, true, 3 | (Scene.IsVisibilityFrame() ? 512u | 2048u : 0u));

			// The models' HUD parts: the rest of them the frame has lit already
			if (Scene.IsVisibilityFrame())
			{
				Scene.DrawForward(Cmd, &Target->rt_Generic_0->pRT, 1, RDepth, true, 1 | 1024);
			}
		}

		if (SceneDetails)
		{
			GPU_EVENT(ForwardGrass);
			CEditorFillModeScope FillModeScope(Cmd, EditorMode);
			ForwardPassOnly = 1;
			SceneDetails->hw_Render(Cmd);
		}

		ForwardPassOnly = u32(-1);
	}
	else
	{
		{
			GPU_EVENT(ForwardHud);
			CEditorFillModeScope FillModeScope(Cmd, EditorMode);
			BindForwardDecals(Cmd);
			GraphMain.r_dsgraph_render_hud();
		}

		{
			GPU_EVENT(ForwardOpaque);
			CEditorFillModeScope FillModeScope(Cmd, EditorMode);
			GraphMain.r_dsgraph_render_graph(0);

			// Under r_msaa what is cut out by its texture draws the samples it covers into the depth first, its colour
			// then where the depth is equal, as the graph draws it
			const bool IsSplit = Target->ForwardWindowDepth != nullptr;

			if (IsSplit)
			{
				Scene.DrawForward(Cmd, &Target->rt_Generic_0->pRT, 1, RDepth, false);
			}

			Scene.DrawForward(Cmd, &Target->rt_Generic_0->pRT, 1, RDepth, true);
			Scene.BuildHiZ(Cmd, RDepth);

			if (SceneDetails)
			{
				GPU_EVENT(ForwardGrass);
				SceneDetails->Render(Cmd);
			}

			// Without a pyramid the cull has no second part: the grass alone is left to draw
			const u32 Rest = Scene.CullForward(GraphMain, true) ? 2u : 4u;

			if (Rest == 2 || Scene.IsForwardGrassReady())
			{
				if (IsSplit)
				{
					Scene.DrawForward(Cmd, &Target->rt_Generic_0->pRT, 1, RDepth, false, Rest);
				}

				Scene.DrawForward(Cmd, &Target->rt_Generic_0->pRT, 1, RDepth, true, Rest);
			}
		}

		// A multisampled depth is resolved only at the end of the frame: the editors' culling then keeps the frame before's
		if (EditorMode && EditorOcclusionCulling && !Target->ForwardWindowDepth)
		{
			Target->copy_position();
			EditorHiZ.Build(Cmd);
			Target->u_setrt(Cmd, Target->rt_Generic_0, nullptr, RDepth);
		}
	}

	{
		GPU_EVENT(ForwardLods);
		CEditorFillModeScope FillModeScope(Cmd, EditorMode);
		// The scene's cards, then those it cannot draw from the graph
		Scene.CullCards(GraphMain);
		Scene.DrawForward(Cmd, &Target->rt_Generic_0->pRT, 1, RDepth, true, 8);
		GraphMain.r_dsgraph_render_lods(true, false);
	}

	{
		GPU_EVENT(Sky);
		DrawSky();
	}

	Target->u_setrt(Cmd, Target->rt_Generic_0, nullptr, RDepth);
	DetailLayers_RenderBrush3D(Cmd);

	if (Wallmarks)
	{
		GPU_EVENT(Wallmarks);
		Wallmarks->Render(Cmd);
	}

	Scene.CountGraphPasses(GraphMain);

	{
		GPU_EVENT(ForwardOpaqueLate);
		CEditorFillModeScope FillModeScope(Cmd, EditorMode);
		Scene.DrawForward(Cmd, &Target->rt_Generic_0->pRT, 1, RDepth, true, 16);
		GraphMain.r_dsgraph_render_lods(false, true);
		// The scene's blended statics (water without WBOIT, wallmarks, light planes), then what the graph holds of the
		// second priority: it drew those statics there by pass, never by distance
		Scene.DrawForward(Cmd, &Target->rt_Generic_0->pRT, 1, RDepth, true, 64);
		GraphMain.r_dsgraph_render_graph(1);
		RenderForwardPuddles(Cmd);
	}

	HOM.Enable();
	GraphMain.PortalTraverser.fade_render(Cmd);

	// Editor helpers under the blended geometry
	if (EditorMode && g_EditorRenderHost)
	{
		Target->u_setrt(Cmd, Target->rt_Generic_0, nullptr, RDepth);
		g_EditorRenderHost->DrawSceneHelpers();
	}

	RenderForwardOit(Cmd, Target->rt_Generic_0);

	{
		GPU_EVENT(ForwardSorted);
		CEditorFillModeScope FillModeScope(Cmd, EditorMode);
		Scene.DrawSorted(Cmd, GraphMain, &Target->rt_Generic_0->pRT, 1, RDepth, false);
	}

	{
		GPU_EVENT(EnvironmentLast);
		dxRainRender::Draw();
		dxThunderboltRender::Draw();
	}

	CopyForwardLensScene(Cmd);

	{
		GPU_EVENT(HudSorted);

		if (PlayedScene ? PlayedScene->ActiveItemUi.IsRecorded : g_hud && g_hud->RenderActiveItemUIQuery())
		{
			PROF_EVENT("RenderActiveItemUI");
			CHudInitializer Hud(Cmd, true, true);
			PlayedScene ? Recorder.Play(PlayedScene->ActiveItemUi) : g_hud->RenderActiveItemUI();
		}

		Scene.DrawSorted(Cmd, GraphMain, &Target->rt_Generic_0->pRT, 1, RDepth, false, true);
	}

	if (g_RenderFeatures.SpriteGlows && L_Glows)
	{
		GPU_EVENT(SpriteGlows);
		L_Glows->Render(Cmd);
	}

	// With the frame's depth, as the frame that shades a pixel once had it: the bullets' tracers are drawn here, and
	// with none bound they showed through walls
	Target->u_setrt(Cmd, Target->rt_Generic_0, nullptr, RDepth);
	PlayedScene ? Recorder.Play(PlayedScene->PostMain) : g_pGamePersistent->OnRenderPPUI_main();

	Target->u_setrt(Cmd, Target->rt_Generic_1, nullptr, RDepth);
	Cmd.ClearTarget(Target->rt_Generic_1->pRT, NoDistortion);

	// Nothing distorts most frames: the pass that pushes the picture by the mask is then a copy of it
	const bool IsDistorted = !GraphMain.mapDistort.empty() || !GraphMain.mapHUDDistort.empty() || !g_pGameLevel || (g_pGamePersistent->m_pMainMenu && g_pGamePersistent->m_pMainMenu->IsActive());

	{
		GPU_EVENT(render_distort_objects);
		Scene.DrawDistortion(Cmd, GraphMain, Target->rt_Generic_1->pRT, RDepth, false);
		PlayedScene ? Recorder.Play(PlayedScene->PostDistort) : g_pGamePersistent->OnRenderPPUI_PP();
	}

	EndForwardMsaa(Cmd);
	Target->ForwardVelocity(Cmd);
	Target->ForwardVolumetrics(Cmd);

	// What the lens of a scope shows stays sharp under the depth of field: its mask over the plain depth, which it
	// clears where the lens is; the mask reads the depth as it was
	Cmd.ClearTarget(Target->rt_DofScopeMask->pRT, ERTColor::Transparent);

	if (o.DofCompiled && !GraphMain.mapHUDScopeMask.empty())
	{
		GPU_EVENT(SCOPE_BUFFER_RENDER);
		Target->LensDofFactor = Device.hudViewportData.isRenderActive ? clampr(Device.hudViewportData.renderZoomRotateFactor, 0.0f, 1.0f) : 0.0f;
		Target->copy_position();
		Target->u_setrt(Cmd, Target->rt_DofScopeMask, nullptr, RDepth);
		Scene.DrawDistortion(Cmd, GraphMain, Target->rt_DofScopeMask->pRT, RDepth, true);
	}

	GraphMain.mapHUDScopeMask.clear();

	if (IsDistorted)
	{
		GPU_EVENT(DistortCombine);
		Target->u_setrt(Cmd, Target->rt_Accumulator, nullptr, nullptr);
		Cmd.SetPass(Target->s_r1_distort->E[0]);
		DrawFullscreen(Cmd, 3);
		Cmd.CopySurface(Target->rt_Generic_0->pSurface, Target->rt_Accumulator->pSurface);
	}
	Target->phase_post(Cmd);
}
