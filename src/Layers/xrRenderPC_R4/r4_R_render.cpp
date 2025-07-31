#include "stdafx.h"

#include "../../xrEngine/IGame_Persistent.h"
#include "../xrRender/FBasicVisual.h"
#include "../../xrEngine/CustomHUD.h"
#include "../../xrEngine/xr_object.h"

#include "../../xrParticles/ParticlesAsyncManager.h"

#include "../xrRender/QueryHelper.h"

#include "OverlayAPI\FSR2Wrapper.h"

#include "../../xrEngine/GameFont.h"
#include "../../xrEngine/x_ray.h"
#include "../xrRender/SkeletonCustom.h"
static	float	CalcSSADynamic				(const Fvector& C, float R)
{
    Fvector4 v_res1, v_res2;
    Device.mFullTransform.transform(v_res1, C);
    Device.mFullTransform.transform(v_res2, Fvector(C).mad(Device.vCameraRight, R));
	return	v_res1.sub(v_res2).magnitude();
}
constexpr float base_fov = 67.f;

// Aproximate, adjusted by fov, distance from camera to position (For right work when looking though binoculars and scopes)
static float GetDistFromCamera(const Fvector& from_position)
{
	float distance = Device.vCameraPosition.distance_to(from_position);
	float fov_K = base_fov / Device.fFOV;
	float adjusted_distane = distance / fov_K;

	return adjusted_distane;
}

void CRender::render_main	(bool deffered, bool zfill)
{
	GPU_EVENT(render_main);
//	Msg						("---begin");
	marker					++;
	bool dont_test_sectors = Sectors.size() <= 1;

	// Calculate sector(s) and their objects
	if (pLastSector)		
	{
		//!!!
		//!!! BECAUSE OF PARALLEL HOM RENDERING TRY TO DELAY ACCESS TO HOM AS MUCH AS POSSIBLE
		//!!!
		if(deffered)
		{
			// Traverse object database
			g_SpatialSpace->q_frustum
			(
			lstRenderablesMain,
			ISpatial_DB::O_ORDERED,
			STYPE_RENDERABLE + STYPE_RENDERABLESHADOW + STYPE_PARTICLE + STYPE_LIGHTSOURCE,
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
							R->update(O);
					}
				}

				if (lstRenderablesMain.size())
				{
					uLastLTRACK	++;
					uID_LTRACK = uLastLTRACK%lstRenderablesMain.size();

					// update light-vis for selected entity
					// track lighting environment
					if (IRenderable* renderable = (IRenderable*)lstRenderablesMain[uID_LTRACK]->dcast_Renderable())
					{
						if (CROS_impl* T = (CROS_impl*)renderable->renderable_ROS())
							T->update(renderable);
					}
				}
			}
		}
		Fmatrix mftrans;
		if(zfill)
		{
			Fmatrix m_project;
			m_project.build_projection(
				deg2rad(Device.fFOV/* *Device.fASPECT*/), 
				Device.fASPECT, Device.fViewportNear,
				ps_r2_zfill * g_pGamePersistent->Environment().CurrentEnv->far_plane);
			mftrans.mul(m_project,Device.mView);
		}
		else
			mftrans = Device.mFullTransform;
		// Traverse sector/portal structure
		if (!dont_test_sectors)
		{
			PortalTraverser.traverse	
				(
				pLastSector,
				ViewBase,
				Device.vCameraPosition,
				mftrans,
				CPortalTraverser::VQ_HOM + CPortalTraverser::VQ_SSA + CPortalTraverser::VQ_FADE
				//. disabled scissoring (HW.Caps.bScissor?CPortalTraverser::VQ_SCISSOR:0)	// generate scissoring info
				);
		}
		// Determine visibility for static geometry hierrarhy
		if(psDeviceFlags.test(rsDrawStatic))
		{
			PROF_EVENT("add_static")

			if (dont_test_sectors)
			{
				CSector*	sector		= (CSector*)Sectors[0];
				set_Frustum			(&ViewBase);
				add_Geometry		(sector->root());
			}
			else
			{
				for (u32 s_it=0; s_it<PortalTraverser.r_sectors.size(); s_it++)
				{
					CSector*	sector		= (CSector*)PortalTraverser.r_sectors[s_it];
					dxRender_Visual*	root	= sector->root();
					for (u32 v_it=0; v_it<sector->r_frustums.size(); v_it++)	{
						set_Frustum			(&(sector->r_frustums[v_it]));
						add_Geometry		(root);
					}
				}
			}
		}
		PROF_EVENT("add_dynamic")
		// Traverse frustums
		for (u32 o_it=0; o_it<lstRenderablesMain.size(); o_it++)
		{
			ISpatial*	spatial	= lstRenderablesMain[o_it].get();
			if	(0==spatial) continue; spatial->spatial_updatesector();
			CSector* sector = (CSector*)spatial->spatial.sector;
			if	(0==sector) continue;

			if ((spatial->spatial.type & STYPE_LIGHTSOURCE) && deffered)
			{
				// hud lightsource
				if(light* L = (light*)(spatial->dcast_Light()))
				{
					if(L->flags.bHudMode)
					{
						Lights.add_light(L);
						continue;
					}
				}
			}

			if(!HOM.visible(spatial->spatial.sphere)) continue;

			if ((spatial->spatial.type & STYPE_LIGHTSOURCE) && deffered)
			{
				// lightsource
				if(light* L = (light*)(spatial->dcast_Light()))
				{
					if (L->get_LOD()>EPS_L&&!L->flags.bHudMode)
						Lights.add_light(L);
				}
				continue;
			}
			if (dont_test_sectors)
			{
				if (spatial->spatial.type & STYPE_RENDERABLE && psDeviceFlags.test(rsDrawDynamic))
				{
					// renderable
					if (IRenderable* renderable = spatial->dcast_Renderable())
					{
						if (Device.vCameraPosition.distance_to_sqr(spatial->spatial.sphere.P) < _sqr(g_pGamePersistent->Environment().CurrentEnv->fog_distance))
						{
							if (CalcSSADynamic(spatial->spatial.sphere.P, spatial->spatial.sphere.R) > spatial->spatial.ssa_dyn_factor && GetDistFromCamera(spatial->spatial.sphere.P) < spatial->spatial.ssa_d_cam)
							{
								if (deffered)
								{
									CKinematics* pKin = (CKinematics*)renderable->renderable.visual;
									if (pKin)
									{
										pKin->CalculateBones(TRUE);
										pKin->CalculateWallmarks();
										//dbg_text_renderer(spatial->spatial.sphere.P);
									}
								}
								if (spatial->spatial.sphere.R > 1.f)
								{
									// Rendering
									set_Object(renderable);
									renderable->renderable_Render();
									set_Object(0);
								}
							}
							if (spatial->spatial.sphere.R <= 1.f)
							{
								// Rendering
								set_Object(renderable);
								renderable->renderable_Render();
								set_Object(0);
							}
						}
					}
				}

				if (spatial->spatial.type & STYPE_PARTICLE && !deffered)
				{
					// renderable
					if	(IRenderable* renderable = spatial->dcast_Renderable())
					{
						// Rendering
						set_Object						(renderable);
						renderable->renderable_Render();
						set_Object						(0);
					}
				}
			}
			else
			{
				if	(PortalTraverser.i_marker != sector->r_marker)	continue;	// inactive (untouched) sector
				for (u32 v_it=0; v_it<sector->r_frustums.size(); v_it++)
				{
					CFrustum&	view	= sector->r_frustums[v_it];
					if (!view.testSphere_dirty(spatial->spatial.sphere.P,spatial->spatial.sphere.R))	continue;

					if (spatial->spatial.type & STYPE_RENDERABLE && psDeviceFlags.test(rsDrawDynamic))
					{
						// renderable
						if	(IRenderable* renderable = spatial->dcast_Renderable())
						{
							if(Device.vCameraPosition.distance_to_sqr(spatial->spatial.sphere.P)<_sqr(g_pGamePersistent->Environment().CurrentEnv->fog_distance))
							{
								if(CalcSSADynamic(spatial->spatial.sphere.P,spatial->spatial.sphere.R)>spatial->spatial.ssa_dyn_factor&&GetDistFromCamera(spatial->spatial.sphere.P)<spatial->spatial.ssa_d_cam)
								{
									if(deffered)
									{
										CKinematics* pKin = (CKinematics*)renderable->renderable.visual;
										if(pKin)
										{
											pKin->CalculateBones(TRUE);
											pKin->CalculateWallmarks();
											//dbg_text_renderer(spatial->spatial.sphere.P);
										}
									}
									if(spatial->spatial.sphere.R>1.f)
									{
										// Rendering
										set_Object						(renderable);
										renderable->renderable_Render();
										set_Object						(0);
									}
								}
								if(spatial->spatial.sphere.R<=1.f)
								{
									// Rendering
									set_Object						(renderable);
									renderable->renderable_Render();
									set_Object						(0);
								}
							}
						}
					}

					if (spatial->spatial.type & STYPE_PARTICLE && !deffered)
					{
						// renderable
						if	(IRenderable* renderable = spatial->dcast_Renderable())
						{
							// Rendering
							set_Object						(renderable);
							renderable->renderable_Render();
							set_Object						(0);
						}
					}
				}
			}
		}
		if (g_pGameLevel && psDeviceFlags.test(rsDrawDynamic) && (phase==PHASE_NORMAL))	
		{
			PROF_EVENT("Render HUD");
			g_hud->Render_Last();		// HUD
		}
	}
	else
	{
		set_Object(0);
		if (g_pGameLevel && psDeviceFlags.test(rsDrawDynamic) && (phase == PHASE_NORMAL))
		{
			PROF_EVENT("Render HUD");
			g_hud->Render_Last();		// HUD
		}
	}
}

void CRender::render_menu() {
	GPU_EVENT(render_menu);
	//	Globals
	RCache.set_CullMode(CULL_CCW);
	RCache.set_Stencil(FALSE);
	RCache.set_ColorWriteEnable();

	// Main Render
	{
		Target->u_setrt(Target->rt_Generic, 0, 0, 0);		// LDR RT
		rmNormal();

		g_pGamePersistent->OnRenderPPUI_main();	// PP-UI
	}

	// Distort
	{
		FLOAT ColorRGBA[4] = {127.0f / 255.0f, 127.0f / 255.0f, 0.0f, 127.0f / 255.0f};
		Target->u_setrt(Target->rt_Generic_1, 0, 0, 0);		// Now RT is a distortion mask
		rmNormal();
		RContext->ClearRenderTargetView(Target->rt_Generic_1->pRT, ColorRGBA);
		g_pGamePersistent->OnRenderPPUI_PP();	// PP-UI
	}

	// Actual Display
	Target->u_setrt((u32)RCache.get_target_width(), (u32)RCache.get_target_height(), RTarget, nullptr, nullptr, nullptr);
	rmNormal();

	RCache.set_Shader(Target->s_menu);
	RCache.set_Geometry(Target->g_menu);

	Fvector2 p0, p1;
	u32 Offset;
	constexpr auto C = color_rgba(255, 255, 255, 255);
	float	_w = RCache.get_target_width();
	float	_h = RCache.get_target_height();
	float	d_Z = EPS_S;
	float	d_W = 1.f;

	p0.set(.5f / _w, .5f / _h);
	p1.set((_w + .5f) / _w, (_h + .5f) / _h);

	FVF::TL* pv = (FVF::TL*)RCache.Vertex.Lock(4, Target->g_menu->vb_stride, Offset);

	pv->set(EPS, float(_h + EPS), d_Z, d_W, C, p0.x, p1.y);	pv++;
	pv->set(EPS, EPS, d_Z, d_W, C, p0.x, p0.y);	pv++;
	pv->set(float(_w + EPS), float(_h + EPS), d_Z, d_W, C, p1.x, p1.y);	pv++;
	pv->set(float(_w + EPS), EPS, d_Z, d_W, C, p1.x, p0.y);	pv++;

	RCache.Vertex.Unlock(4, Target->g_menu->vb_stride);
	RCache.Render(D3DPT_TRIANGLELIST, Offset, 0, 4, 0, 2);
}

Fvector3 ps_r_taa_jitter_full = {0,0,0};

extern u32 g_r;
bool is_render_cubemap = false;

void CRender::Render()
{
	GPU_EVENT(CRender_Render);

	g_r						= 1;
	VERIFY					(0==mapDistort.size() + mapHUDDistort.size());

//	rmNormal();

	bool	_menu_pp		= g_pGamePersistent?g_pGamePersistent->OnRenderPPUI_query():false;
	if (_menu_pp)			{
		render_menu			()	;
		return					;
	};

	IMainMenu*	pMainMenu = g_pGamePersistent?g_pGamePersistent->m_pMainMenu:0;
	bool	bMenu = pMainMenu?pMainMenu->CanSkipSceneRendering():false;

	if (!(g_pGameLevel && g_hud) || bMenu) {
		Target->u_setrt((u32)RCache.get_target_width(), (u32)RCache.get_target_height(), RTarget, nullptr, nullptr, nullptr);
		return;
	}

	if(m_bFirstFrameAfterReset)
	{
		xrRender_apply_tf();
		m_bFirstFrameAfterReset = false;
		return;
	}

	if(RImplementation.o.offscreen_reflecitons && pLastSector) {
		GPU_EVENT(FORWARD_REFLECTIONS);

		is_render_cubemap = true;
		static Fmatrix cProj{}, cView{}, cTrans{};
		static Fvector cmNorm[6]{}, cmDir[6]{};

		auto& CurrentEnv = *g_pGamePersistent->Environment().CurrentEnv;
		u32 RefSize = Target->rt_Reflection->dwSize;

		Fvector4 fog_color4 = {
			CurrentEnv.fog_color.x / (1.0f + CurrentEnv.fog_color.x),
			CurrentEnv.fog_color.y / (1.0f + CurrentEnv.fog_color.y),
			CurrentEnv.fog_color.z / (1.0f + CurrentEnv.fog_color.z),
			CurrentEnv.far_plane
		};

		cProj.build_projection(PI_DIV_2 + 0.002f, 1.0f,
			Device.fViewportNear, CurrentEnv.far_plane * ps_r4_vslr_distance);

		cmDir[2].mul(Device.vCameraTop, +1.0f);
		cmDir[3].mul(Device.vCameraTop, -1.0f);

		cmNorm[2].mul(Device.vCameraDirection, -1.0f);
		cmNorm[3].mul(Device.vCameraDirection, +1.0f);

		cmDir[0].mul(Device.vCameraRight, +1.0f);
		cmDir[1].mul(Device.vCameraRight, -1.0f);

		cmNorm[0].mul(Device.vCameraTop, +1.0f);
		cmNorm[1].mul(Device.vCameraTop, +1.0f);

		cmDir[4].mul(Device.vCameraDirection, +1.0f);
		cmDir[5].mul(Device.vCameraDirection, -1.0f);

		cmNorm[4].mul(Device.vCameraTop, +1.0f);
		cmNorm[5].mul(Device.vCameraTop, +1.0f);

		RCache.set_xform_project(cProj);

		phase = PHASE_REFLECT;

		HOM.Disable();
		r_pmask(true, false, true);

		ps_r_taa_jitter.set(0, 0, -1);
		ps_r_taa_jitter_full.set(ps_r_taa_jitter);
		
		Fvector PointPos = Device.vCameraPosition;
		RContext->CopyResource(Target->rt_Reflection_temp->pSurface, Target->rt_Reflection->pSurface);

		for(auto i = 0; i < 6; ++i) {
			GPU_EVENT(FORWARD_REFLECTION_SIDE);

			cView.build_camera_dir(PointPos, cmDir[i], cmNorm[i]);
			cTrans.mul(cProj, cView);

			r_dsgraph_render_subspace(pLastSector, cTrans, PointPos, FALSE, FALSE);

			RCache.set_xform_view(cView);
			mapWmark.clear();

			RContext->ClearRenderTargetView(Target->rt_Reflection->pRT[i], (FLOAT*)&fog_color4);
			RContext->ClearDepthStencilView(Target->rt_Depth->pZRT, D3D_CLEAR_DEPTH, 1.0f, 0);

			Target->u_setrt(RefSize, RefSize,
				Target->rt_Reflection->pRT[i], NULL, NULL, Target->rt_Depth->pZRT);
			
			RImplementation.rmNormal();

			RCache.set_Stencil(FALSE);
			RCache.set_ColorWriteEnable();

			r_dsgraph_render_graph(0);
		}

		RContext->GenerateMips(Target->rt_Reflection->pTexture->get_SRView());

		RCache.set_xform_project(Device.mProject);
		RCache.set_xform_view(Device.mView);

		is_render_cubemap = false;
		phase = PHASE_NORMAL;
	}

	if(ps_r_scale_mode > 1 || ps_r2_aa_type == 3)
	{
		int32_t jitterPhaseCount = ffxFsr2GetJitterPhaseCount((int32_t)RCache.get_width(), (int32_t)RCache.get_target_width());
		ffxFsr2GetJitterOffset(&ps_r_taa_jitter_full.x, &ps_r_taa_jitter_full.y, Device.dwFrame, jitterPhaseCount);

		ps_r_taa_jitter_full = ps_r_taa_jitter_full.mul(ps_r_taa_jitter_scale);

		ps_r_taa_jitter.x = 2.0f * ps_r_taa_jitter_full.x / RCache.get_width();
		ps_r_taa_jitter.y = -2.0f * ps_r_taa_jitter_full.y / RCache.get_height();
		ps_r_taa_jitter.z = float(Device.dwFrame % jitterPhaseCount) / float(jitterPhaseCount) + EPS;
	}
	else
	{
		ps_r_taa_jitter.set(0, 0, -1);
		ps_r_taa_jitter_full.set(ps_r_taa_jitter);
	}

	Target->u_setrt(Target->rt_Generic_0, Target->rt_Velocity, 0, 0);

	FLOAT ColorRGBA[4] = { 0.0f, 0.0f, 0.0f, 0.0f };
	RContext->ClearRenderTargetView(Target->rt_Generic_0->pRT, ColorRGBA);
	RContext->ClearRenderTargetView(Target->rt_Velocity->pRT, ColorRGBA);

	RCache.set_CullMode(CULL_NONE);
	RCache.set_Stencil(FALSE);

	g_pGamePersistent->Environment().RenderSky();

	RImplementation.o.distortion = FALSE;
	Fcolor sun_color = ((light*)Lights.sun_adapted._get())->color;
	bool bSUN = !o.sunstatic && u_diffuse2s(sun_color) > EPS;

	RCache.set_xform_world(Fidentity);

	ViewBase.CreateFromMatrix(Device.mFullTransform, FRUSTUM_P_LRTB + FRUSTUM_P_FAR);
	View = 0;

	if(!ps_r2_ls_flags.test(R2FLAG_EXP_MT_CALC)) {
		HOM.Enable();
		HOM.Render(ViewBase);
	}
	
	//******* Z-prefill calc - DEFERRER RENDERER
	{
		GPU_EVENT(DEFER_ZPREFILL);
		if (ps_r2_ls_flags.test(R2FLAG_ZFILL))
		{
			Device.Statistic->RenderCALC.Begin();
			r_pmask(true, false);	// enable priority "0"
			phase = PHASE_SMAP;
			render_main(false, true);
			r_pmask(true, false);	// disable priority "1"
			Device.Statistic->RenderCALC.End();

			// flush
			Target->phase_scene_prepare();
			RCache.set_ColorWriteEnable(FALSE);
			r_dsgraph_render_graph(0);
			RCache.set_ColorWriteEnable();
		}
		else
		{
			Target->phase_scene_prepare();
		}
	}

	//******* Main calc - DEFERRER RENDERER
	// Main calc
	Device.Statistic->RenderCALC.Begin			();
	r_pmask										(true,false,true);	// enable priority "0",+ capture wmarks
	phase										= PHASE_NORMAL;

	render_main									(true);
	r_pmask										(true,false);	// disable priority "1"
	Device.Statistic->RenderCALC.End			();

	BOOL	split_the_scene_to_minimize_wait		= FALSE;
	if (ps_r2_ls_flags.test(R2FLAG_EXP_SPLIT_SCENE))	split_the_scene_to_minimize_wait=TRUE;

	if (mapHUDScopeMask.size() > 0) {
		split_the_scene_to_minimize_wait = FALSE;
	}

	rmNormal();

	Target->u_setrt((u32)RCache.get_width(), (u32)RCache.get_height(), nullptr, nullptr, nullptr, RDepth);

	//******* Main render :: PART-0	-- first
	if (!split_the_scene_to_minimize_wait)
	{
		GPU_EVENT(DEFER_PART0_NO_SPLIT);
		// level, DO NOT SPLIT
		Target->phase_scene_begin				();
		r_dsgraph_render_hud					();
		r_dsgraph_render_scope					();
		Target->phase_scene_begin				();
		r_dsgraph_render_graph					(0);
		r_dsgraph_render_lods					(true,true);
		if(Details)	Details->Render				();
		Target->phase_scene_end					();
	} 
	else 
	{
		GPU_EVENT(DEFER_PART0_SPLIT);
		// level, SPLIT
		Target->phase_scene_begin				();
		r_dsgraph_render_graph					(0);
		Target->disable_aniso					();
	}

	{
		GPU_EVENT(DEFER_TEST_LIGHT_VIS);
		if(Lights.package.v_point.empty()&&Lights.package.v_spot.empty()&&Lights.package.v_shadowed.empty())
			HWOCC.occq_refresh();
		else
		{

			//******* Occlusion testing of volume-limited light-sources
			Target->phase_occq							();
			LP_normal.clear								();
			LP_pending.clear							();
			{
				// perform tests
				size_t	count = 0;
				light_Package&	LP	= Lights.package;

				// stats
				stats.l_shadowed	= (u32)LP.v_shadowed.size();
				stats.l_unshadowed	= (u32)(LP.v_point.size() + LP.v_spot.size());
				stats.l_total		= stats.l_shadowed + stats.l_unshadowed;

				// perform tests
				count = std::max(count, LP.v_point.size());
				count = std::max(count, LP.v_spot.size());
				count = std::max(count, LP.v_shadowed.size());
				for (size_t it = 0; it < count; it++)	{
					if (it<LP.v_point.size())		{
						light*	L			= LP.v_point	[it];
						if(L->flags.bOccq&&!L->flags.bHudMode)
						{
							L->vis_prepare		();
							if (L->vis.pending)	LP_pending.v_point.push_back	(L);
							else				LP_normal.v_point.push_back		(L);
						}
						else
							LP_normal.v_point.push_back		(L);
					}
					if (it<LP.v_spot.size())		{
						light*	L			= LP.v_spot		[it];
						if(L->flags.bOccq&&!L->flags.bHudMode)
						{
							L->vis_prepare		();
							if (L->vis.pending)	LP_pending.v_spot.push_back		(L);
							else				LP_normal.v_spot.push_back		(L);
						}
						else
							LP_normal.v_spot.push_back		(L);
					}
					if (it<LP.v_shadowed.size())	{
						light*	L			= LP.v_shadowed	[it];
						if(L->flags.bOccq&&!L->flags.bHudMode)
						{
							L->vis_prepare		();
							if (L->vis.pending)	LP_pending.v_shadowed.push_back	(L);
							else				LP_normal.v_shadowed.push_back	(L);
						}
						else
							LP_normal.v_shadowed.push_back	(L);
					}
				}
			}
			LP_normal.sort							();
			LP_pending.sort							();
		}
	}

   //******* Main render :: PART-1 (second)
	if (split_the_scene_to_minimize_wait)	
	{
		GPU_EVENT(DEFER_PART1_SPLIT);
		
		// level
		Target->phase_scene_begin				();
		r_dsgraph_render_hud					();
		r_dsgraph_render_lods					(true,true);
		if(Details)	Details->Render				();
		Target->phase_scene_end					();
	}

	{
		GPU_EVENT(ZBUFFER_COPY);
		RCache.set_ZB(NULL);

		ID3D11Resource* res{};
		RDepth->GetResource(&res);

		RContext->CopyResource(Target->rt_Position->pSurface, res);
		_RELEASE(res);
	}

	// Wall marks
	if(Wallmarks)	
	{
		GPU_EVENT(DEFER_WALLMARKS);
		Target->phase_wallmarks					();
		g_r										= 0;
		Wallmarks->Render						();				// wallmarks has priority as normal geometry
	}

	// Update incremental shadowmap-visibility solver
	{
		GPU_EVENT(DEFER_FLUSH_OCCLUSION);
		u32 it=0;
		for (it=0; it<Lights_LastFrame.size(); it++)	{
			if (0==Lights_LastFrame[it])	continue	;
			try {
				Lights_LastFrame[it]->svis.flushoccq()	;
			} catch (...)
			{
				Msg	("! Failed to flush-OCCq on light [%d] %X",it,*(u32*)(&Lights_LastFrame[it]));
			}
		}
		Lights_LastFrame.clear	();
	}

	//	TODO: DX10: Implement DX10 rain.
	if (ps_r2_ls_flags.test(R3FLAG_DYN_WET_SURF))
	{
		GPU_EVENT(DEFER_RAIN);
		render_rain();
	}

	rmNormal();

	// Directional light - fucking sun
	if (bSUN)	
	{
		GPU_EVENT(DEFER_SUN);
		RImplementation.stats.l_visible		++;
		render_sun_cascades();
		Target->increment_light_marker();
	}

	phase = PHASE_NORMAL;

	{
		GPU_EVENT(DEFER_SELF_ILLUM);
		Target->phase_accumulator			();
		// Render emissive geometry, stencil - write 0x0 at pixel pos
		RCache.set_xform_project			(Device.mProject); 
		RCache.set_xform_view				(Device.mView);
		// Stencil - write 0x1 at pixel pos - 
		RCache.set_Stencil(TRUE, D3DCMP_ALWAYS, 0x01, 0xff, 0xff, D3DSTENCILOP_KEEP, D3DSTENCILOP_REPLACE, D3DSTENCILOP_KEEP);
		//RCache.set_Stencil				(TRUE,D3DCMP_ALWAYS,0x00,0xff,0xff,D3DSTENCILOP_KEEP,D3DSTENCILOP_REPLACE,D3DSTENCILOP_KEEP);
		RCache.set_CullMode					(CULL_CCW);
		RCache.set_ColorWriteEnable			();
		RImplementation.r_dsgraph_render_emissive();
	}

	if(g_hud && g_hud->RenderActiveItemUIQuery()) {
		Target->phase_accumulator();
		r_dsgraph_render_hud_ui();
	}

	// Lighting, non dependant on OCCQ
	{
		GPU_EVENT(DEFER_LIGHT_NO_OCCQ);
		Target->phase_accumulator				();
		render_lights							(LP_normal);
	}

	// Lighting, dependant on OCCQ
	{
		GPU_EVENT(DEFER_LIGHT_OCCQ);
		render_lights							(LP_pending);
	}

	phase = PHASE_NORMAL;

	// Postprocess
	{
		GPU_EVENT(DEFER_LIGHT_COMBINE);
		Target->phase_combine					();
	}

	VERIFY	(0==mapDistort.size() + mapHUDDistort.size());

	//HWOCC.occq_stats();
}

void CRender::render_forward				()
{
	VERIFY	(0==mapDistort.size() + mapHUDDistort.size());
	RImplementation.o.distortion				= RImplementation.o.distortion_enabled;	// enable distorion

	//******* Main render - second order geometry (the one, that doesn't support deffering)
	//.todo: should be done inside "combine" with estimation of of luminance, tone-mapping, etc.
	{
		// level
		r_pmask									(false,true);			// enable priority "1"
		phase									= PHASE_NORMAL;
		render_main								(false);//
		//	Igor: we don't want to render old lods on next frame.
		mapLOD.clear							();
		CParticlesAsync::Wait();
		r_dsgraph_render_graph					(1)	;					// normal level, secondary priority
		PortalTraverser.fade_render				()	;					// faded-portals
		r_dsgraph_render_sorted					(false)	;					// strict-sorted geoms
		g_pGamePersistent->Environment().RenderLast()	;					// rain/thunder-bolts

		RContext->CopyResource(Target->rt_Accumulator->pSurface, Target->rt_Generic_0->pSurface);
		r_dsgraph_render_sorted_hud();
	}

	RImplementation.o.distortion				= FALSE;				// disable distorion
}
