#include "stdafx.h"

#include "../../xrEngine/igame_persistent.h"
#include "../xrRender/FBasicVisual.h"
#include "../../xrEngine/customhud.h"
#include "../../xrEngine/xr_object.h"

#include "../xrRender/QueryHelper.h"

#include "FSR2Wrapper.h"

#include "../../xrEngine/gamefont.h"
#include "../../xrEngine/x_ray.h"
#include "../xrRender/SkeletonCustom.h"
#include "../../xrEngine/IGame_Actor.h"
static	float	CalcSSADynamic				(const Fvector& C, float R)
{
    Fvector4 v_res1, v_res2;
    Device.mFullTransform.transform(v_res1, C);
    Device.mFullTransform.transform(v_res2, Fvector(C).mad(Device.vCameraRight, R));
	return	v_res1.sub(v_res2).magnitude();
}
constexpr float base_fov = 67.f;
static float GetDistFromCamera(const Fvector& from_position)
	// Aproximate, adjusted by fov, distance from camera to position (For right work when looking though binoculars and scopes)
{
	float distance = Device.vCameraPosition.distance_to(from_position);
	float fov_K = base_fov / Device.fFOV;
	float adjusted_distane = distance / fov_K;

	return adjusted_distane;
}

//static void dbg_text_renderer(const Fvector& pos, u32 color = color_rgba(0,255,100,255), shared_str str = "+")
//{
//    Fvector4		v_res;
//    Device.mFullTransform.transform(v_res, pos);
//
//    float x = (1.f + v_res.x) / 2.f * (Device.Width);
//    float y = (1.f - v_res.y) / 2.f * (Device.Height);
//
//    if (v_res.z < 0 || v_res.w < 0)
//        return;
//
//    if (v_res.x < -1.f || v_res.x > 1.f || v_res.y < -1.f || v_res.y>1.f)
//        return;
//
//	g_FontManager->pFontSystem->SetAligment(CGameFont::alCenter);
//	g_FontManager->pFontSystem->SetColor(color);
//	g_FontManager->pFontSystem->Out(x, y, "%s", str.c_str());
//}

void CRender::render_main	(bool deffered, bool zfill)
{
	PIX_EVENT(render_main);
//	Msg						("---begin");
	marker					++;
	bool dont_test_sectors = Sectors.size() <= 1;
	// Calculate sector(s) and their objects
	if (pLastSector)		{
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
				ViewBase
				);

			// (almost) Exact sorting order (front-to-back)
			std::sort(lstRenderablesMain.begin(), lstRenderablesMain.end(), [](ISpatial* _1, ISpatial* _2) {
			float d1 = _1->spatial.sphere.P.distance_to_sqr(Device.vCameraPosition);
			float d2 = _2->spatial.sphere.P.distance_to_sqr(Device.vCameraPosition);
			return d1 < d2;
			});

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
				Device.fASPECT, VIEWPORT_NEAR, 
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
			PROF_EVENT("static geometry hierrarhy");

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

		// Traverse frustums
		for (u32 o_it=0; o_it<lstRenderablesMain.size(); o_it++)
		{
			ISpatial*	spatial	= lstRenderablesMain[o_it];
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

			Fbox sp_box;
			sp_box.setb(spatial->spatial.sphere.P,Fvector().set(spatial->spatial.sphere.R, spatial->spatial.sphere.R, spatial->spatial.sphere.R));
			HOM.Enable();
			if(!HOM.visible(sp_box)) continue;

			if ((spatial->spatial.type & STYPE_LIGHTSOURCE) && deffered)
			{
				// lightsource
				if(light* L = (light*)(spatial->dcast_Light()))
				{
					if (L->get_LOD()>EPS_L&&!L->flags.bHudMode)
					{
						
						if(dont_test_sectors)
							Lights.add_light(L);
						else
						{
							if(Sectors.size()>1)
							{
								if(L->b_need_detect_sectors)
								{
									L->get_sectors();
									L->b_need_detect_sectors = false;
								}
							}
							for (u32 s_it = 0; s_it < L->m_sectors.size(); s_it++)
							{
								CSector* sector_ = (CSector*)L->m_sectors[s_it];
								if(PortalTraverser.i_marker == sector_->r_marker)
								{
									Lights.add_light(L);
									break;
								}
							}
						}
					}
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
	PIX_EVENT(render_menu);
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
void CRender::Render		()
{
	PIX_EVENT(CRender_Render);

	g_r						= 1;
	VERIFY					(0==mapDistort.size());

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

	if(ps_r_scale_mode > 1) {
		int32_t jitterPhaseCount = ffxFsr2GetJitterPhaseCount((int32_t)RCache.get_width(), (int32_t)RCache.get_target_width());
		ffxFsr2GetJitterOffset(&ps_r_taa_jitter_full.x, &ps_r_taa_jitter_full.y, Device.dwFrame, jitterPhaseCount);

		ps_r_taa_jitter_full = ps_r_taa_jitter_full.mul(ps_r_taa_jitter_scale);

		ps_r_taa_jitter.x = 2.0f * ps_r_taa_jitter_full.x / RCache.get_width();
		ps_r_taa_jitter.y = -2.0f * ps_r_taa_jitter_full.y / RCache.get_height();
		ps_r_taa_jitter.z = float(Device.dwFrame % jitterPhaseCount) / float(jitterPhaseCount) + EPS;
	}
	else {
		ps_r_taa_jitter.set(0, 0, -1);
		ps_r_taa_jitter_full.set(ps_r_taa_jitter);
	}

	if(g_pIGameActor) {
		Target->u_setrt(Target->rt_ui_pda, 0, 0);
		g_pIGameActor->RenderItemUI();
	}

	Target->u_setrt(Target->rt_Generic_0, Target->rt_Velocity, 0, 0);

	FLOAT ColorRGBA[4] = { 0.0f, 0.0f, 0.0f, 0.0f };
	RContext->ClearRenderTargetView(Target->rt_Generic_0->pRT, ColorRGBA);
	RContext->ClearRenderTargetView(Target->rt_Velocity->pRT, ColorRGBA);

	RCache.set_CullMode(CULL_NONE);
	RCache.set_Stencil(FALSE);

	g_pGamePersistent->Environment().RenderSky();

	// Configure
	RImplementation.o.distortion				= FALSE;		// disable distorion
	Fcolor					sun_color			= ((light*)Lights.sun_adapted._get())->color;
	bool					bSUN				= !o.sunstatic && (u_diffuse2s(sun_color.r,sun_color.g,sun_color.b)>EPS);

	// Msg						("sstatic: %s, sun: %s",o.sunstatic?;"true":"false", bSUN?"true":"false");

	RCache.set_xform_world(Fidentity);

	// HOM
	ViewBase.CreateFromMatrix					(Device.mFullTransform, FRUSTUM_P_LRTB + FRUSTUM_P_FAR);
	View										= 0;
	if (!ps_r2_ls_flags.test(R2FLAG_EXP_MT_CALC))	{
		HOM.Enable									();
		HOM.Render									(ViewBase);
	}

	//******* Z-prefill calc - DEFERRER RENDERER
	if (ps_r2_ls_flags.test(R2FLAG_ZFILL))		
	{
		PIX_EVENT(DEFER_Z_FILL);
		Device.Statistic->RenderCALC.Begin			();
		r_pmask										(true,false);	// enable priority "0"
		set_Recorder								(nullptr)		;
		phase										= PHASE_SMAP;
		render_main									(false,true)	;
		r_pmask										(true,false);	// disable priority "1"
		Device.Statistic->RenderCALC.End				( )			;

		// flush
		Target->phase_scene_prepare					();
		RCache.set_ColorWriteEnable					(FALSE);
		r_dsgraph_render_graph						(0);
		RCache.set_ColorWriteEnable					( );
	} 
	else 
	{
		Target->phase_scene_prepare					();
	}

	//*******
	// Sync point
	Device.Statistic->RenderDUMP_Wait_S.Begin	();
	if (1)
	{
		CTimer	T;							T.Start	();
		BOOL	result						= FALSE;
		HRESULT	hr							= S_FALSE;
		//while	((hr=q_sync_point[q_sync_count]->GetData	(&result,sizeof(result),D3DGETDATA_FLUSH))==S_FALSE) {
		while	((hr=GetData (q_sync_point[q_sync_count], &result,sizeof(result)))==S_FALSE) 
		{
			if (!SwitchToThread())			Sleep(ps_r2_wait_sleep);
			if (T.GetElapsed_ms() > 500)	{
				result	= FALSE;
				break;
			}
		}
	}
	Device.Statistic->RenderDUMP_Wait_S.End		();
	q_sync_count								= (q_sync_count+1) % 2;
	//CHK_DX										(q_sync_point[q_sync_count]->Issue(D3DISSUE_END));
	CHK_DX										(EndQuery(q_sync_point[q_sync_count]));

	//******* Main calc - DEFERRER RENDERER
	// Main calc
	Device.Statistic->RenderCALC.Begin			();
	r_pmask										(true,false,true);	// enable priority "0",+ capture wmarks
	if (bSUN)									set_Recorder	(&main_coarse_structure);
	else										set_Recorder	(nullptr);
	phase										= PHASE_NORMAL;
	render_main									(true);
	set_Recorder								(nullptr);
	r_pmask										(true,false);	// disable priority "1"
	Device.Statistic->RenderCALC.End			();

	BOOL	split_the_scene_to_minimize_wait		= FALSE;
	if (ps_r2_ls_flags.test(R2FLAG_EXP_SPLIT_SCENE))	split_the_scene_to_minimize_wait=TRUE;

	rmNormal();

	// Landshaft phase 
	Target->u_setrt((u32)RCache.get_width(), (u32)RCache.get_height(), nullptr, nullptr, nullptr, RDepth);
	r_dsgraph_render_landscape(0, false);

	IGame_Level::BonesCalcGroup.wait();

	//******* Main render :: PART-0	-- first
	if (!split_the_scene_to_minimize_wait)
	{
		PIX_EVENT(DEFER_PART0_NO_SPLIT);
		// level, DO NOT SPLIT
		Target->phase_scene_begin				();
		r_dsgraph_render_hud					();
		r_dsgraph_render_graph					(0);
		r_dsgraph_render_lods					(true,true);
		if(Details)	Details->Render				();
		r_dsgraph_render_landscape				(1, true);
		Target->phase_scene_end					();
	} 
	else 
	{
		PIX_EVENT(DEFER_PART0_SPLIT);
		// level, SPLIT
		Target->phase_scene_begin				();
		r_dsgraph_render_graph					(0);
		r_dsgraph_render_landscape				(1, true);
		Target->disable_aniso					();
	}

	if(Lights.package.v_point.empty()&&Lights.package.v_spot.empty()&&Lights.package.v_shadowed.empty())
		HWOCC.occq_refresh();
	else
	{
		PIX_EVENT(DEFER_TEST_LIGHT_VIS);
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

   //******* Main render :: PART-1 (second)
	if (split_the_scene_to_minimize_wait)	
	{
		PIX_EVENT(DEFER_PART1_SPLIT);
		
		// level
		Target->phase_scene_begin				();
		r_dsgraph_render_hud					();
		r_dsgraph_render_lods					(true,true);
		if(Details)	Details->Render				();
		Target->phase_scene_end					();
	}

	{
		PIX_EVENT(ZBUFFER_COPY);
		RCache.set_ZB(NULL);
		ID3D11Resource* res;
		RDepth->GetResource(&res);
	
		RContext->CopyResource(Target->rt_Position->pSurface, res);
		_RELEASE(res);
	}

	// Wall marks
	if(Wallmarks)	
	{
		PIX_EVENT(DEFER_WALLMARKS);
		Target->phase_wallmarks					();
		g_r										= 0;
		Wallmarks->Render						();				// wallmarks has priority as normal geometry
	}

	//	TODO: DX10: Implement DX10 rain.
	if (ps_r2_ls_flags.test(R3FLAG_DYN_WET_SURF))
	{
		PIX_EVENT(DEFER_RAIN);
		render_rain();
	}

	rmNormal();

	// Directional light - fucking sun
	if (bSUN)	
	{
		PIX_EVENT(DEFER_SUN);
		RImplementation.stats.l_visible		++;
		render_sun_cascades();
		Target->increment_light_marker();
	}

	{
		PIX_EVENT(DEFER_SELF_ILLUM);
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
		PIX_EVENT(DEFER_LIGHT_NO_OCCQ);
		Target->phase_accumulator				();
		HOM.Disable								();
		render_lights							(LP_normal);
	}

	// Lighting, dependant on OCCQ
	{
		PIX_EVENT(DEFER_LIGHT_OCCQ);
		render_lights							(LP_pending);
	}

	// Postprocess
	{
		PIX_EVENT(DEFER_LIGHT_COMBINE);
		Target->phase_combine					();
	}

	VERIFY	(0==mapDistort.size());

	//HWOCC.occq_stats();
}

void CRender::render_forward				()
{
	VERIFY	(0==mapDistort.size());
	RImplementation.o.distortion				= RImplementation.o.distortion_enabled;	// enable distorion

	//******* Main render - second order geometry (the one, that doesn't support deffering)
	//.todo: should be done inside "combine" with estimation of of luminance, tone-mapping, etc.
	{
		HOM.Enable();
		// level
		r_pmask									(false,true);			// enable priority "1"
		phase									= PHASE_NORMAL;
		render_main								(false);//
		//	Igor: we don't want to render old lods on next frame.
		mapLOD.clear							();
		r_dsgraph_render_graph					(1)	;					// normal level, secondary priority
		PortalTraverser.fade_render				()	;					// faded-portals
		r_dsgraph_render_sorted					()	;					// strict-sorted geoms
		g_pGamePersistent->Environment().RenderLast()	;					// rain/thunder-bolts
	}

	RImplementation.o.distortion				= FALSE;				// disable distorion
}
