#include "stdafx.h"
#include "../../xrEngine/IGame_Persistent.h"
#include "../../xrEngine/Environment.h"

#include "../xrRender/dxEnvironmentRender.h"

void CRenderTarget::DoAsyncScreenshot()
{
	//	Igor: screenshot will not have postprocess applied.
	//	TODO: fox that later
	if (RImplementation.m_bMakeAsyncSS)
	{
		HRESULT hr;
		ID3DTexture2D* pBuffer = nullptr;
		hr = RSwapchain->GetBuffer(0, IID_PPV_ARGS(&pBuffer));
		RContext->CopyResource( t_ss_async, pBuffer );
		

		RImplementation.m_bMakeAsyncSS = false;
	}
}

float hclip(float v, float dim) {
	return 2.f * v / dim - 1.f;
}

struct v_aa {
	Fvector4 p;
	Fvector2 uv0;
	Fvector2 uv1;
	Fvector2 uv2;
	Fvector2 uv3;
	Fvector2 uv4;
	Fvector4 uv5;
	Fvector4 uv6;
};

void CRenderTarget::phase_combine()
{
	GPU_EVENT(phase_combine);

	//	TODO: DX10: Remove half poxel offset
	bool _menu_pp = g_pGamePersistent ? g_pGamePersistent->OnRenderPPUI_query() : false;

	u32 Offset = 0;
	Fvector2 p0, p1;

	//*** exposure-pipeline
	{
		if (t_LUM_src != rt_LUM_pool[0]->pTexture)
		{
			t_LUM_src->surface_set(rt_LUM_pool[0]->pSurface);
		}

		if (t_LUM_dest != rt_LUM_pool[1]->pTexture)
		{
			t_LUM_dest->surface_set(rt_LUM_pool[1]->pSurface);
		}
	}

	{
		PROF_EVENT("PHASE_AMBIENT_OCCLUSION");

		switch(ps_r_ssao_mode)
		{
			case 0: GRHI->ClearTarget(rt_ssao_temp->pRT, ERTColor::Black); break;
			case 1: phase_ssao(); break;
			case 2: phase_gtao(); break;
		}
	}

	if(RImplementation.o.deffered_reflecitons)
	{
		phase_sslr();
	}

	u_setrt(rt_Generic_0, 0, 0, RDepth);

	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
	RCache.set_Stencil(FALSE);

	// draw skybox
	g_pGamePersistent->Environment().RenderClouds();

	RCache.set_Stencil(TRUE, D3DCMP_LESSEQUAL, 0x01, 0xff, 0x00);	// stencil should be >= 1

	if (RImplementation.o.nvstencil) {
		u_stencil_optimize(CRenderTarget::SO_Combine);
		RCache.set_ColorWriteEnable();
	}

	// Draw full-screen quad textured with our scene image
	if (!_menu_pp)
	{
		GPU_EVENT(combine_1);

		light* fuckingsun = (light*)RImplementation.Lights.sun_adapted._get();
		Fmatrix m_clouds_shadow{};

		{
			static float w_shift = 0.0f;

			Fvector normal;
			normal.setHP(g_pGamePersistent->Environment().CurrentEnv->wind_direction, 0);
			w_shift += 0.003f * Device.fTimeDelta;

			Fvector position;
			position.set(0, 0, 0);

			Fmatrix m_xform;
			m_xform.build_camera_dir(position, fuckingsun->direction, normal);

			Fvector localnormal;
			m_xform.transform_dir(localnormal, normal);
			localnormal.normalize();

			m_clouds_shadow.mul(m_xform, RCache.xforms.m_invv);
			m_xform.scale(0.002f, 0.002f, 1.f);
			m_clouds_shadow.mulA_44(m_xform);
			m_xform.translate(localnormal.mul(w_shift));
			m_clouds_shadow.mulA_44(m_xform);
		}

		Fvector4 sunclr, sundir;

		// sun-params
		{
			Fvector L_dir, L_clr;
			L_clr.set(fuckingsun->color.r, fuckingsun->color.g, fuckingsun->color.b);

			Device.mView.transform_dir(L_dir, fuckingsun->direction);
			L_dir.normalize ();

			sunclr.set(L_clr.x, L_clr.y, L_clr.z, u_diffuse2s(L_clr));
			sundir.set(L_dir.x, L_dir.y, L_dir.z, 0);
		}

		CEnvDescriptorMixer& envdesc = *g_pGamePersistent->Environment().CurrentEnv;
		dxEnvDescriptorMixerRender &envdescren = *(dxEnvDescriptorMixerRender*)(&*envdesc.m_pDescriptorMixer);

		// Setup textures
		IRHISurface* e0 = _menu_pp ? 0 : envdescren.sky_r_textures_env[0].second->surface_get();
		IRHISurface* e1 = _menu_pp ? 0 : envdescren.sky_r_textures_env[1].second->surface_get();

		t_envmap_0->surface_set		(e0);	_RELEASE(e0);
		t_envmap_1->surface_set		(e1);	_RELEASE(e1);
	
		// Draw
		RCache.set_Element (s_combine->E[0]);
		RCache.set_Geometry (FSTriangleGeom);

		RCache.set_c ("Ldynamic_color", sunclr);
		RCache.set_c ("Ldynamic_dir", sundir);

		RCache.set_c ("m_sunmask", m_clouds_shadow);

		RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, 3, 0, 1);
	}

	if(ps_r2_ls_flags_ext.test(R4FLAG_PUDDLES))
	{
		GPU_EVENT(Forward_rendering_puddles);
		phase_puddles();
	}

	// Forward rendering
	{
		GPU_EVENT(Forward_rendering);
		phase_scene_forward();

		GRHI->StateManager->SetCullMode(ERHI_CULLMODE::BACK);
		RCache.set_Stencil (FALSE);
		RCache.set_ColorWriteEnable ();

		RImplementation.render_forward	();
		if (g_pGamePersistent) {
			g_pGamePersistent->OnRenderPPUI_main();	// PP-UI
		}
	}

	//	Igor: for volumetric lights
	//	combine light volume here
	if(m_bHasActiveVolumetric) {
		phase_combine_volumetric();
	}

	// Distortion filter
	BOOL bDistort = RImplementation.o.distortion_enabled; // This can be modified
	{
		u32 count = RImplementation.mapDistort.size() + RImplementation.mapHUDDistort.size();
		if((count < 1 && !_menu_pp)) {
			bDistort= FALSE;
		}
		if(bDistort) {
			GPU_EVENT(render_distort_objects);
			u_setrt(rt_Generic_1, 0, 0, RDepth);		// Now RT is a distortion mask

			RImplementation.rmNormal();
			GRHI->ClearTarget(rt_Generic_1->pRT, ERTColor::Gray);
			GRHI->StateManager->SetCullMode(ERHI_CULLMODE::BACK);
			RCache.set_Stencil(FALSE);
			RCache.set_ColorWriteEnable();
			RImplementation.r_dsgraph_render_distort();

			if(g_pGamePersistent) {
				g_pGamePersistent->OnRenderPPUI_PP();	// PP-UI
			}

			u_setrt(rt_Generic_2, 0, 0, 0);

			// Draw COLOR
			RCache.set_Element(s_combine->E[1]);
			RCache.set_Geometry(FSTriangleGeom);

			RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, 3, 0, 1);
			GRHI->CopySurface(rt_Generic_0->pSurface, rt_Generic_2->pSurface);
		}
	}

	if(ps_r_scale_mode < 2) {
		if(ps_r2_aa_type == 1) {
			GPU_EVENT(phase_fxaa);
			phase_fxaa();
			RCache.set_Stencil(FALSE);
		}
		else if(ps_r2_aa_type == 2) {
			GPU_EVENT(phase_smaa);
			phase_smaa();
			RCache.set_Stencil(FALSE);
		}
		else if(ps_r2_aa_type == 3) {
			GPU_EVENT(phase_taa);
			phase_taa();
		}
	}

	phase_mblur();

	u_setrt(get_width(), get_height(), 0, 0, 0, 0);
	RImplementation.rmNormal();

	switch(ps_r_scale_mode)
	{
		case 4:
		{
			if(!phase_xess())
			{
				ps_proxy_r_scale_mode = ps_r_scale_mode = 1;
			}
			break;
		}
		case 3:
		{
			if(!phase_fsr()) 
			{
				ps_proxy_r_scale_mode = ps_r_scale_mode = 1;
			}
			break;
		}
		case 2:
		{
			if(!phase_dlss())
			{
				ps_proxy_r_scale_mode = ps_r_scale_mode = 3;
			}
			break;
		}
		default:
		{
			phase_scale();
		}
		break;
	}

	dwWidth = get_target_width();
	dwHeight = get_target_height();

	RImplementation.rmNormal();

	// HDR RT invalidated here
	// Perform blooming filter and distortion if needed
	RCache.set_Stencil(FALSE);
	phase_bloom();

	u_setrt(rt_Back_Buffer, 0, 0, 0);			// LDR RT

	RImplementation.rmNormal();

	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
	RCache.set_Stencil(FALSE);
	{
		GPU_EVENT(combine_2);

		//	Set up variable
		Fvector2 vDofKernel;
		vDofKernel.set(0.5f / Device.TargetWidth, 0.5f / Device.TargetHeight);
		vDofKernel.mul(ps_r2_dof_kernel_size);

		// Draw COLOR
		RCache.set_Element(s_combine->E[2]);	// look at blender_combine.cpp

		Fvector3 dof;
		g_pGamePersistent->GetCurrentDof(dof);
		RCache.set_c("dof_params", dof.x, dof.y, dof.z, ps_r2_dof_sky);
		RCache.set_c("dof_kernel", vDofKernel.x, vDofKernel.y, ps_r2_dof_kernel_size, 0);

		RCache.set_Geometry(FSTriangleGeom);
		RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, Offset, 0, 3, 0, 1);
	}

	RCache.set_Stencil		(FALSE);

	//	if FP16-BLEND !not! supported - draw flares here, overwise they are already in the bloom target
	g_pGamePersistent->Environment().RenderFlares();	// lens-flares

	if(ps_r4_cas_sharpening > EPS) {
		GPU_EVENT(phase_cas);
		phase_cas();
	}

	extern bool UseGasmak;
	if (UseGasmak)
	{
		PhaseGasmask();
	}

	extern bool UseRainDrops;
	if (UseRainDrops) {
		PhaseRaindrops();
	}

	if (ps_r2_ls_flags_ext.test(R2FLAG_SPP_SATURATION)) {
		GPU_EVENT(PhaseSaturation);
		PhaseSaturation();
	}

	if(ps_r2_ls_flags_ext.test(R2FLAG_SPP_VIGNETTE)) {
		GPU_EVENT(PhaseVignette);
		PhaseVignette();
	}

	if(ps_r2_ls_flags_ext.test(R2FLAG_SPP_ABERRATION)) {
		GPU_EVENT(PhaseAberration);
		PhaseAberration();
	}

	{
		GPU_EVENT(phase_pp);
		phase_pp();
	}
	
	//	Re-adapt luminance
	RCache.set_Stencil(FALSE);

	//*** exposure-pipeline-clear
	{
		std::swap(rt_LUM_pool[0], rt_LUM_pool[1]);
		t_LUM_src->surface_set		(nullptr);
		t_LUM_dest->surface_set		(nullptr);
	}

#ifdef DEBUG
	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::BACK);
	static	xr_vector<Fplane>		saved_dbg_planes;
	if (bDebug)		saved_dbg_planes= dbg_planes;
	else			dbg_planes		= saved_dbg_planes;
	if (1) for (u32 it=0; it<dbg_planes.size(); it++)
	{
		Fplane&		P	=	dbg_planes[it];
		Fvector		zero	;
		zero.mul	(P.n,P.d);
		
		Fvector             L_dir,L_up=P.n,L_right;
		L_dir.set           (0,0,1);                if (std::abs(L_up.dotproduct(L_dir))>.99f)  L_dir.set(1,0,0);
		L_right.crossproduct(L_up,L_dir);           L_right.normalize       ();
		L_dir.crossproduct  (L_right,L_up);         L_dir.normalize         ();

		Fvector				p0_,p1_,p2,p3;
		float				sz	= 100.f;
		p0_.mad				(zero,L_right,sz).mad	(L_dir,sz);
		p1_.mad				(zero,L_right,sz).mad	(L_dir,-sz);
		p2.mad				(zero,L_right,-sz).mad	(L_dir,-sz);
		p3.mad				(zero,L_right,-sz).mad	(L_dir,+sz);
		RCache.dbg_DrawTRI	(Fidentity,p0_,p1_,p2,0xffffffff);
		RCache.dbg_DrawTRI	(Fidentity,p2,p3,p0_,0xffffffff);
	}

	static	xr_vector<dbg_line_t>	saved_dbg_lines;
	if (bDebug)		saved_dbg_lines	= dbg_lines;
	else			dbg_lines		= saved_dbg_lines;
	if (1) for (u32 it=0; it<dbg_lines.size(); it++)
	{
		RCache.dbg_DrawLINE		(Fidentity,dbg_lines[it].P0,dbg_lines[it].P1,dbg_lines[it].color);
	}

	dbg_spheres.clear	();
	dbg_lines.clear		();
	dbg_planes.clear	();
#endif
}

void CRenderTarget::phase_wallmarks()
{
	// Targets
	u_setrt(rt_Color, nullptr, nullptr, RDepth);
	// Stencil	- draw only where stencil >= 0x1
	RCache.set_Stencil(TRUE, D3DCMP_LESSEQUAL, 0x01, 0xff, 0x00);
	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::BACK);
	RCache.set_ColorWriteEnable(D3DCOLORWRITEENABLE_RED | D3DCOLORWRITEENABLE_GREEN | D3DCOLORWRITEENABLE_BLUE);
}

void CRenderTarget::phase_combine_volumetric()
{
	GPU_EVENT(phase_combine_volumetric);

	u_setrt(rt_Generic_0, 0, 0, RDepth);
	//	Sets limits to both render targets
	RCache.set_ColorWriteEnable(D3DCOLORWRITEENABLE_RED|D3DCOLORWRITEENABLE_GREEN|D3DCOLORWRITEENABLE_BLUE);
	{
		// Draw
		RCache.set_Element			(s_combine_volumetric->E[0]	);
		RCache.set_Geometry			(FSTriangleGeom);
		RCache.Render				(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST,0,0,3,0,1);
	}
	RCache.set_ColorWriteEnable();
}
