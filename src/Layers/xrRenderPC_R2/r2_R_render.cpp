#include "stdafx.h"
#include "../../xrEngine/IGame_Persistent.h"
#include "../xrRender/FBasicVisual.h"
#include "../../xrEngine/CustomHUD.h"
#include "../../xrEngine/xr_object.h"
#include "../xrRender/SkeletonCustom.h"
#include "../../xrParticles/ParticlesAsyncManager.h"

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
static void dbg_text_renderer(const Fvector& pos, u32 color = color_rgba(0,255,100,255), shared_str str = "+")
{
    Fvector4		v_res;
    Device.mFullTransform.transform(v_res, pos);

    float x = (1.f + v_res.x) / 2.f * (Device.Width);
    float y = (1.f - v_res.y) / 2.f * (Device.Height);

    if (v_res.z < 0 || v_res.w < 0)
        return;

    if (v_res.x < -1.f || v_res.x > 1.f || v_res.y < -1.f || v_res.y>1.f)
        return;

	g_FontManager->pFontSystem->SetAligment(CGameFont::alCenter);
	g_FontManager->pFontSystem->SetColor(color);
	g_FontManager->pFontSystem->Out(x, y, "%s", str.c_str());
}

void CRender::render_menu	()
{
	//	Globals
	RCache.set_CullMode				(CULL_CCW);
	RCache.set_Stencil				(FALSE);
	RCache.set_ColorWriteEnable		();

	// Main Render
	{
		Target->u_setrt						(Target->rt_Generic_0,0,0,RDepth);		// LDR RT
		g_pGamePersistent->OnRenderPPUI_main()	;	// PP-UI
	}
	// Distort
	{
		Target->u_setrt						(Target->rt_Generic_1,0,0,RDepth);		// Now RT is a distortion mask
		CHK_DX(RDevice->Clear			( 0L, nullptr, D3DCLEAR_TARGET, color_rgba(127,127,0,127), 1.0f, 0L));
		g_pGamePersistent->OnRenderPPUI_PP	()	;	// PP-UI
	}

	// Actual Display
	Target->u_setrt					( (u32)RCache.get_width(),(u32)RCache.get_height(),RTarget,nullptr,nullptr,RDepth);
	RCache.set_Shader				( Target->s_menu	);
	RCache.set_Geometry				( Target->g_menu	);

	Fvector2						p0,p1;
	u32								Offset;
	u32		C						= color_rgba	(255,255,255,255);
	float	_w						= float(RCache.get_width());
	float	_h						= float(RCache.get_height());
	float	d_Z						= EPS_S;
	float	d_W						= 1.f;
	p0.set							(.5f/_w, .5f/_h);
	p1.set							((_w+.5f)/_w, (_h+.5f)/_h );

	FVF::TL* pv						= (FVF::TL*) RCache.Vertex.Lock	(4,Target->g_menu->vb_stride,Offset);
	pv->set							(EPS,			float(_h+EPS),	d_Z,	d_W, C, p0.x, p1.y);	pv++;
	pv->set							(EPS,			EPS,			d_Z,	d_W, C, p0.x, p0.y);	pv++;
	pv->set							(float(_w+EPS),	float(_h+EPS),	d_Z,	d_W, C, p1.x, p1.y);	pv++;
	pv->set							(float(_w+EPS),	EPS,			d_Z,	d_W, C, p1.x, p0.y);	pv++;
	RCache.Vertex.Unlock			(4,Target->g_menu->vb_stride);
	RCache.Render					(D3DPT_TRIANGLELIST,Offset,0,4,0,2);
}

extern u32 g_r;
void CRender::Render()
{
	g_r						= 1;
	bool	_menu_pp		= g_pGamePersistent?g_pGamePersistent->OnRenderPPUI_query():false;
	if (_menu_pp)			{
		render_menu			()	;
		return					;
	};

	IMainMenu*	pMainMenu = g_pGamePersistent?g_pGamePersistent->m_pMainMenu:0;
	bool	bMenu = pMainMenu?pMainMenu->CanSkipSceneRendering():false;

	if (!(g_pGameLevel && g_hud) || bMenu) {
		Target->u_setrt((u32)RCache.get_width(), (u32)RCache.get_height(), RTarget, nullptr, nullptr, RDepth);
		return;
	}

	if( m_bFirstFrameAfterReset )
	{
		for (light* L : v_all_lights)//critical!!!
			L->m_moving_frames = 0;

		m_bFirstFrameAfterReset = false;
		return;
	}

//.	VERIFY					(g_pGameLevel && g_pGameLevel->pHUD);

	// Configure
	RImplementation.o.distortion				= FALSE;		// disable distorion
	Fcolor					sun_color			= ((light*)Lights.sun_adapted._get())->color;
	bool					bSUN				= !o.sunstatic && (u_diffuse2s(sun_color.r,sun_color.g,sun_color.b)>EPS);
	// Msg						("sstatic: %s, sun: %s",o.sunstatic?"true":"false", bSUN?"true":"false");

	// HOM
	ViewBase.CreateFromMatrix					(Device.mFullTransform, FRUSTUM_P_LRTB + FRUSTUM_P_FAR);
	if (!ps_r2_ls_flags.test(R2FLAG_EXP_MT_CALC))	{
		HOM.Enable									();
		HOM.Render									(ViewBase);
	}



	//******* Z-prefill calc - DEFERRER RENDERER
	Target->phase_scene_prepare();

	//******* Main calc - DEFERRER RENDERER
	// Main calc
	Device.Statistic->RenderCALC.Begin			();
	phase										= PHASE_NORMAL;
	GMBase.traverse(pLastSector, ViewBase, Device.vCameraPosition, Device.mFullTransform);
	GMBase.r_dsgraph_capture(false, true);
	GMBase.r_dsgraph_capture_hud();
	Device.Statistic->RenderCALC.End			();


	//******* Main render :: PART-0	-- first
	{
		// level, SPLIT
		Target->phase_scene_begin();
		GMBase.r_dsgraph_render_graph(0);
		Target->disable_aniso();
	}

	//******* Occlusion testing of volume-limited light-sources

	{
		Target->phase_occq();
		LP_normal.clear();
		LP_pending.clear();
		GMBase.r_dsgraph_capture_lights();
	}
	//******* Main render :: PART-1 (second)
	{
		// level, SPLIT2
		Target->phase_scene_begin();
		GMBase.r_dsgraph_render_hud();
		GMBase.r_dsgraph_render_lods(true, true);
		if(Details)	Details->Render();
		Target->phase_scene_end();
	}

	if (g_hud && g_hud->RenderActiveItemUIQuery())
	{
		Target->phase_wallmarks();
		GMBase.r_dsgraph_render_hud_ui();
	}

	// Wall marks
	if(Wallmarks)	{
		Target->phase_wallmarks					();
		g_r										= 0;
		Wallmarks->Render						();				// wallmarks has priority as normal geometry
	}

	// Directional light - fucking sun
	if (bSUN)	{
		RImplementation.stats.l_visible		++;
		render_sun_cascades();
		Target->increment_light_marker();
	}

	{
		Target->phase_accumulator					();
		// Render emissive geometry, stencil - write 0x0 at pixel pos
		RCache.set_xform_project					(Device.mProject); 
		RCache.set_xform_view						(Device.mView);
		// Stencil - write 0x1 at pixel pos - 
		RCache.set_Stencil							( TRUE,D3DCMP_ALWAYS,0x01,0xff,0xff,D3DSTENCILOP_KEEP,D3DSTENCILOP_REPLACE,D3DSTENCILOP_KEEP);
		//RCache.set_Stencil						(TRUE,D3DCMP_ALWAYS,0x00,0xff,0xff,D3DSTENCILOP_KEEP,D3DSTENCILOP_REPLACE,D3DSTENCILOP_KEEP);
		RCache.set_CullMode							(CULL_CCW);
		RCache.set_ColorWriteEnable					();
		GMBase.r_dsgraph_render_emissive	();

		// Stencil	- draw only where stencil >= 0x1
		RCache.set_Stencil					(TRUE,D3DCMP_LESSEQUAL,0x01,0xff,0x00);
		RCache.set_CullMode					(CULL_NONE);
		RCache.set_ColorWriteEnable			();
	}

	// Lighting, non dependant on OCCQ
	Target->phase_accumulator				();
	render_lights							(LP_normal);
	
	// Lighting, dependant on OCCQ
	render_lights							(LP_pending);

	// Postprocess
	Target->phase_combine					();
}

void CRender::render_forward				()
{
	RImplementation.o.distortion				= RImplementation.o.distortion_enabled;	// enable distorion

	//******* Main render - second order geometry (the one, that doesn't support deffering)
	//.todo: should be done inside "combine" with estimation of of luminance, tone-mapping, etc.
	{
		HOM.Enable();
		// level
		phase									= PHASE_NORMAL;
		//	Igor: we don't want to render old lods on next frame.
		GMBase.r_dsgraph_render_static(1);					// normal level, secondary priority
		CParticlesAsync::Wait();
		GMBase.r_dsgraph_render_dynamic(1);
		GMBase.fade_render();					// faded-portals
		GMBase.r_dsgraph_render_sorted();					// strict-sorted geoms
		g_pGamePersistent->Environment().RenderLast();					// rain/thunder-bolts
	}

	RImplementation.o.distortion				= FALSE;				// disable distorion
}
