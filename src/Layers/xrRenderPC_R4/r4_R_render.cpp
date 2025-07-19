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
//static	float	CalcSSADynamic				(const Fvector& C, float R)
//{
//    Fvector4 v_res1, v_res2;
//    Device.mFullTransform.transform(v_res1, C);
//    Device.mFullTransform.transform(v_res2, Fvector(C).mad(Device.vCameraRight, R));
//	return	v_res1.sub(v_res2).magnitude();
//}
//constexpr float base_fov = 67.f;
//static float GetDistFromCamera(const Fvector& from_position)
//	// Aproximate, adjusted by fov, distance from camera to position (For right work when looking though binoculars and scopes)
//{
//	float distance = Device.vCameraPosition.distance_to(from_position);
//	float fov_K = base_fov / Device.fFOV;
//	float adjusted_distane = distance / fov_K;
//
//	return adjusted_distane;
//}

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

void CRender::render_menu()
{
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
	Target->u_setrt((u32)RCache.get_target_width(), (u32)RCache.get_target_height(), RImplementation.Target->rt_BackbufferLUT->pRT, nullptr, nullptr, nullptr);
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

//	rmNormal();

	bool	_menu_pp		= g_pGamePersistent?g_pGamePersistent->OnRenderPPUI_query():false;
	if (_menu_pp)			{
		render_menu			()	;
		return					;
	};

	IMainMenu*	pMainMenu = g_pGamePersistent?g_pGamePersistent->m_pMainMenu:0;
	bool	bMenu = pMainMenu?pMainMenu->CanSkipSceneRendering():false;

	if (!(g_pGameLevel && g_hud) || bMenu) {
		Target->u_setrt((u32)RCache.get_target_width(), (u32)RCache.get_target_height(), RImplementation.Target->rt_BackbufferLUT->pRT, nullptr, nullptr, nullptr);
		return;
	}

	if(m_bFirstFrameAfterReset)
	{
		for (light* L : v_all_lights)//critical!!!
			L->m_moving_frames = 0;

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

		ps_r_taa_jitter.set(0, 0, -1);
		ps_r_taa_jitter_full.set(ps_r_taa_jitter);
		
		Fvector PointPos = Device.vCameraPosition;
		RContext->CopyResource(Target->rt_Reflection_temp->pSurface, Target->rt_Reflection->pSurface);

		for(auto i = 0; i < 6; ++i) {
			GPU_EVENT(FORWARD_REFLECTION_SIDE);

			cView.build_camera_dir(PointPos, cmDir[i], cmNorm[i]);
			cTrans.mul(cProj, cView);

			CFrustum F;
			F.CreateFromMatrix(cTrans, FRUSTUM_P_ALL);

			GMBase.traverse(pLastSector, F, PointPos, cTrans);
			GMBase.r_dsgraph_capture();

			RCache.set_xform_view(cView);
			GMBase.RGraph.mapStaticSorted.Wmark.clear(); GMBase.RGraph.mapDynamicSorted.Wmark.clear();

			RContext->ClearRenderTargetView(Target->rt_Reflection->pRT[i], (FLOAT*)&fog_color4);
			RContext->ClearDepthStencilView(Target->rt_Depth->pZRT, D3D_CLEAR_DEPTH, 1.0f, 0);

			Target->u_setrt(RefSize, RefSize,
				Target->rt_Reflection->pRT[i], NULL, NULL, Target->rt_Depth->pZRT);
			
			RImplementation.rmNormal();

			RCache.set_Stencil(FALSE);
			RCache.set_ColorWriteEnable();

			GMBase.r_dsgraph_render_graph(0);
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

	if(!ps_r2_ls_flags.test(R2FLAG_EXP_MT_CALC)) {
		HOM.Enable();
		HOM.Render(ViewBase);
	}
	
	Target->phase_scene_prepare();
	//******* Main calc - DEFERRER RENDERER
	phase = PHASE_NORMAL;

	rmNormal();

	Target->u_setrt((u32)RCache.get_width(), (u32)RCache.get_height(), nullptr, nullptr, nullptr, RDepth);

	//******* Main render :: PART-0	-- first
	{
		GPU_EVENT(DEFER_PART0_SPLIT);
		// level, SPLIT
		Target->phase_scene_begin();
		GMBase.traverse(RImplementation.pLastSector, ViewBase, Device.vCameraPosition, Device.mFullTransform);

		GMBase.r_dsgraph_capture_hud();
		GMBase.r_dsgraph_render_hud();

		GMBase.r_dsgraph_capture_static();
		GMBase.r_dsgraph_render_static(0);
		Target->disable_aniso();
	}

	{
		GPU_EVENT(DEFER_TEST_LIGHT_VIS);

		//******* Occlusion testing of volume-limited light-sources
		Target->phase_occq();
		LP_normal.clear();
		LP_pending.clear();
		GMBase.r_dsgraph_capture_lights();
	}

   //******* Main render :: PART-1 (second)
	{
		GPU_EVENT(DEFER_PART1_SPLIT);
		
		// level
		Target->phase_scene_begin				();
		GMBase.r_dsgraph_capture_dynamic();
		GMBase.r_dsgraph_render_dynamic(0);
		GMBase.r_dsgraph_render_lods(true,true);
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

	static bool UseWinterPass = EngineExternal()[EEngineExternalRender::UseDynamicSnowMask];
	if(UseWinterPass)
	{
		GPU_EVENT(PhaseWinter);

		RContext->CopyResource(Target->rt_NormalTemp->pSurface, Target->rt_Normal->pSurface);
		RContext->CopyResource(Target->rt_SurfaceTemp->pSurface, Target->rt_Surface->pSurface);

		Target->phase_scene_begin();
		RCache.set_ZB(nullptr);

		RCache.set_ColorWriteEnable(D3DCOLORWRITEENABLE_RED | D3DCOLORWRITEENABLE_GREEN | D3DCOLORWRITEENABLE_BLUE);
		Target->PhaseWinter();

		RCache.set_ColorWriteEnable();
	}

	// Wall marks
	if(Wallmarks)	
	{
		GPU_EVENT(DEFER_WALLMARKS);
		Target->phase_wallmarks					();
		g_r										= 0;
		Wallmarks->Render						();				// wallmarks has priority as normal geometry
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
		RCache.set_CullMode(CULL_CCW);
		RCache.set_ColorWriteEnable();
		GMBase.r_dsgraph_render_emissive();
	}

	if(g_hud && g_hud->RenderActiveItemUIQuery()) {
		Target->phase_accumulator();
		GMBase.r_dsgraph_render_hud_ui();
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
}
#include "../xrRender/CHudInitializer.h"
void CRender::render_forward				()
{
	RImplementation.o.distortion				= RImplementation.o.distortion_enabled;	// enable distorion

	//******* Main render - second order geometry (the one, that doesn't support deffering)
	//.todo: should be done inside "combine" with estimation of of luminance, tone-mapping, etc.
	{
		// level
		phase									= PHASE_NORMAL;
		//	Igor: we don't want to render old lods on next frame.
		GMBase.r_dsgraph_render_static(1);					// normal level, secondary priority
		CParticlesAsync::Wait();
		GMBase.r_dsgraph_render_dynamic(1);
		GMBase.fade_render();					// faded-portals
		GMBase.r_dsgraph_render_sorted(false);					// strict-sorted geoms
		g_pGamePersistent->Environment().RenderLast();					// rain/thunder-bolts
		GMBase.r_dsgraph_render_sorted_hud();
	}

	RImplementation.o.distortion				= FALSE;				// disable distorion
}
