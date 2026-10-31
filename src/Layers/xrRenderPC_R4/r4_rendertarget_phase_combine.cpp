#include "stdafx.h"
#include "dxDebugRender.h"
#include "../../xrEngine/IGame_Persistent.h"
#include "../../xrEngine/Environment.h"

#include "dxEnvironmentRender.h"

ENGINE_API extern bool turn_nvg; //Переместить в более подходящее место

void CRenderTarget::DoAsyncScreenshot()
{
	//	Igor: screenshot will not have postprocess applied.
	//	TODO: fox that later
	if (RImplementation.m_bMakeAsyncSS)
	{
		GRHI->CopySwapchain(t_ss_async);
		

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

void CRenderTarget::phase_combine(GfxCmdList& Cmd)
{
	GPU_EVENT(phase_combine);

	//	TODO: DX10: Remove half poxel offset
	bool _menu_pp = g_pGamePersistent ? g_pGamePersistent->OnRenderPPUI_query() : false;

	{
		PROF_EVENT("PHASE_AMBIENT_OCCLUSION");

		switch(ps_r_ssao_mode)
		{
			case 0: Cmd.ClearTarget(rt_ssao_temp->pRT, ERTColor::Black); break;
			case 1: phase_ssao(Cmd); break;
			case 2: phase_gtao(Cmd); break;
		}
	}

	RImplementation.rmNormal();

	if(RImplementation.o.deffered_reflecitons && !RImplementation.o.dx11_use_legacy_light)
	{
		phase_sslr(Cmd);
	}

	u_setrt(Cmd, rt_Generic_0, 0, 0, RDepth);

	Cmd.SetCullMode(ERHI_CULLMODE::NONE);
	Cmd.SetStencil(FALSE);

	// draw skybox
	g_pGamePersistent->Environment().RenderClouds();

	Cmd.SetStencil(TRUE, D3DCMP_LESSEQUAL, 0x01, 0xff, 0x00);	// stencil should be >= 1

	if (RImplementation.o.nvstencil) {
		u_stencil_optimize(Cmd, CRenderTarget::SO_Combine);
		Cmd.SetColorWriteEnable();
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

			m_clouds_shadow.set(m_xform);
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
	
		DrawPassSQ(Cmd, s_combine, 0, [&]
		{
			Cmd.Constants.Write<CBLight>().Ldynamic_color = sunclr;
			Cmd.Constants.Write<CBLight>().Ldynamic_dir = sundir;
			Cmd.Constants.Write<CBLight>().m_sunmask = m_clouds_shadow;
		});
	}

	if (!_menu_pp && RImplementation.o.RainPuddles)
	{
		PhaseRainPuddles(Cmd);
	}

	if (RImplementation.o.dx11_allow_wboit_transparency)
	{
		Cmd.ClearTarget(rt_Forward->pRT);
		Cmd.ClearTarget(rt_Revealage->pRT, ERTColor::Black);
	}

	if (!m_bHasActiveVolumetric)
	{
		Cmd.ClearTarget(rt_Generic_2->pRT);
	}

	// Forward rendering
	{
		GPU_EVENT(Forward_rendering);
		phase_scene_forward(Cmd);

		Cmd.SetCullMode(ERHI_CULLMODE::BACK);

		Cmd.SetStencil(FALSE);
		Cmd.SetColorWriteEnable();

		RImplementation.render_forward(Cmd);
	}

	//	Igor: for volumetric lights
	//	combine light volume here

	//if(g_pGamePersistent)
	//{
	//	g_pGamePersistent->OnRenderPPUI_PP();
	//}

	u_setrt(Cmd, rt_Generic_0, 0, 0, RDepth);

	if (g_pGamePersistent) 
	{
		g_pGamePersistent->OnRenderPPUI_main();
	}

	copy_position();

	if(ps_r_scale_mode < 2) 
	{
		if(ps_r2_aa_type == 1) 
		{
			GPU_EVENT(phase_fxaa);
			PhaseEffectSQ(Cmd, EffectSQ::FXAA);

			Cmd.SetStencil(FALSE);
		}
		else if(ps_r2_aa_type == 2) 
		{
			GPU_EVENT(phase_smaa);
			phase_smaa(Cmd);

			Cmd.SetStencil(FALSE);
		}
		else if(ps_r2_aa_type == 3)
		{
			GPU_EVENT(phase_taa);
			phase_taa(Cmd);
		}
	}

	if (RImplementation.NeedMotionVectors())
	{
		phase_depth_upscale(Cmd);
		phase_mblur(Cmd);
	}

	u_setrt(Cmd, get_width(), get_height(), 0, 0, 0, 0);
	RImplementation.rmNormal();

	switch(ps_r_scale_mode)
	{
		case 4:
		{
			if(!phase_xess())
			{
				ps_proxy_r_scale_mode = ps_r_scale_mode = 1;
                phase_scale(Cmd);
			}
			break;
		}
		case 3:
		{
			if(!phase_fsr()) 
			{
				ps_proxy_r_scale_mode = ps_r_scale_mode = 1;
                phase_scale(Cmd);
			}
			break;
		}
		case 2:
		{
			if(!phase_dlss())
			{
				ps_proxy_r_scale_mode = ps_r_scale_mode = GRHI->APILevel == ERHI_API_LAYER::D3D12 ? 1 : 3;
                phase_scale(Cmd);
			}
			break;
		}
		default:
		{
			phase_scale(Cmd);
		}
		break;
	}

	dwWidth = get_target_width();
	dwHeight = get_target_height();
	RImplementation.rmNormal();

	Cmd.SetStencil(FALSE);

	phase_bloom_downsample(Cmd);
	phase_bloom_upsample(Cmd);
	phase_new_luminance(Cmd);

	u_setrt(Cmd, rt_Back_Buffer, 0, 0, 0);
	RImplementation.rmNormal();

	Cmd.SetCullMode(ERHI_CULLMODE::NONE);
	Cmd.SetStencil(FALSE);

	{
		GPU_EVENT(combine_2);

		//	Set up variable
		Fvector2 vDofKernel;
		vDofKernel.set(0.5f / Device.TargetWidth, 0.5f / Device.TargetHeight);
		vDofKernel.mul(ps_r2_dof_kernel_size);

		Fvector3 dof;
		g_pGamePersistent->GetCurrentDof(dof);

		DrawPassSQ(Cmd, s_combine, 2, [&]
		{
			Cmd.Constants.Write<CBPost>().dof_params.set(dof.x, dof.y, dof.z, ps_r2_dof_sky);
			Cmd.Constants.Write<CBPost>().dof_kernel.set(vDofKernel.x, vDofKernel.y, ps_r2_dof_kernel_size);
			Cmd.Constants.Write<CBPost>().autoexposure_params.set(ps_r2_autoexposure_key, ps_r2_autoexposure_min, ps_r2_autoexposure_max, ps_r2_autoexposure_bias);
			Cmd.Constants.Write<CBPost>().bloom_params.set(ps_r2_bloom_amount, ps_r2_bloom_desaturation, ps_r2_bloom_tint_amount, 0.f);
			Cmd.Constants.Write<CBPost>().tonemap_params.set(ps_r2_tonemap_compression, ps_r2_tonemap_desaturation, ps_r2_tonemap_crossfeed, ps_r2_tonemap_vibrance);
			Cmd.Constants.Write<CBPost>().bloom_tint.set(ps_r2_bloom_tint_color.x, ps_r2_bloom_tint_color.y, ps_r2_bloom_tint_color.z, 1.f);
			Cmd.Constants.Write<CBPost>().spp_params.set(ps_r2_ls_flags_ext.test(R2FLAG_SPP_SATURATION) ? 1.0f : 0.0f, ps_r2_ls_flags_ext.test(R2FLAG_SPP_VIGNETTE) ? 1.0f : 0.0f, 0.0f, 0.0f);
		});
	}

	Cmd.SetStencil		(FALSE);

	//	if FP16-BLEND !not! supported - draw flares here, overwise they are already in the bloom target
	g_pGamePersistent->Environment().RenderFlares();	// lens-flares

	if(ps_r4_cas_sharpening > EPS) {
		GPU_EVENT(phase_cas);
		PhaseEffectSQ(Cmd, EffectSQ::CAS);
	}

	extern bool UseGasmak;
	if (UseGasmak)
	{
		PhaseEffectSQ(Cmd, EffectSQ::Gasmask);
	}

	PhaseEffectSQ(Cmd, EffectSQ::Raindrops);

	if(ps_r2_ls_flags_ext.test(R2FLAG_SPP_ABERRATION)) {
		GPU_EVENT(PhaseAberration);
		PhaseEffectSQ(Cmd, EffectSQ::Aberration);
	}

	
	//Aight, so here we should enable/disable nvg
	if (turn_nvg)
	{
		GPU_EVENT(phase_nvg);
		PhaseEffectSQ(Cmd, EffectSQ::NVG);
	}
	
	{
		GPU_EVENT(phase_pp);
		phase_pp(Cmd);
	}
	
	//	Re-adapt luminance
	Cmd.SetStencil(FALSE);

#ifdef DEBUG
	Cmd.SetCullMode(ERHI_CULLMODE::BACK);
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
		DebugRenderImpl.dbg_DrawTRI	(Fidentity,p0_,p1_,p2,0xffffffff);
		DebugRenderImpl.dbg_DrawTRI	(Fidentity,p2,p3,p0_,0xffffffff);
	}

	static	xr_vector<dbg_line_t>	saved_dbg_lines;
	if (bDebug)		saved_dbg_lines	= dbg_lines;
	else			dbg_lines		= saved_dbg_lines;
	if (1) for (u32 it=0; it<dbg_lines.size(); it++)
	{
		DebugRenderImpl.DrawLine		(Fidentity,dbg_lines[it].P0,dbg_lines[it].P1,dbg_lines[it].color);
	}

	dbg_spheres.clear	();
	dbg_lines.clear		();
	dbg_planes.clear	();
#endif
}

void CRenderTarget::phase_wallmarks(GfxCmdList& Cmd)
{
	if (LightingModeIsStaticRender())
	{
		u_setrt(Cmd, rt_Generic_0, nullptr, RDepth);
		Cmd.SetStencil(false);
		return;
	}
	// Targets
	u_setrt(Cmd, rt_Color, nullptr, nullptr, RDepth);
	// Stencil	- draw only where stencil >= 0x1
	Cmd.SetStencil(TRUE, D3DCMP_LESSEQUAL, 0x01, 0xff, 0x00);
	Cmd.SetCullMode(ERHI_CULLMODE::BACK);
	Cmd.SetColorWriteEnable(D3DCOLORWRITEENABLE_RED | D3DCOLORWRITEENABLE_GREEN | D3DCOLORWRITEENABLE_BLUE);
}

void CRenderTarget::phase_combine_volumetric(GfxCmdList& Cmd)
{
	if (!m_bHasActiveVolumetric)
	{
		return;
	}

	GPU_EVENT(phase_combine_volumetric);

	DrawPassSQ(Cmd, s_combine_volumetric, 0);
}
