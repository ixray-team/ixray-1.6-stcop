#include "stdafx.h"
#include "UpscaleNri.h"
#include "dxDebugRender.h"
#include "ShaderBindings.h"
#include "../../../shaders/common/debug_view.hlsli"
#include "../../xrEngine/IGame_Persistent.h"
#include "../../xrEngine/Environment.h"

#include "dxEnvironmentRender.h"
#include "dxLensFlareRender.h"

ENGINE_API extern bool turn_nvg; //Переместить в более подходящее место

// The picture is read when the game asks for it, from where the screenshot command reads it
void CRenderTarget::DoAsyncScreenshot()
{
	RImplementation.m_bMakeAsyncSS = false;
}

// From the lit colour in rt_Generic_0 to the picture the UI and the gamma pass take: anti-aliasing, scaling, bloom
// and exposure, tonemap, sharpening, the visor's effects, the game's post-process. Every preset ends its frame here
void CRenderTarget::phase_post(GfxCmdList& Cmd)
{
	GPU_EVENT(phase_post);
	const bool _menu_pp = g_pGamePersistent ? g_pGamePersistent->OnRenderPPUI_query() : false;

	copy_position();

	if (!g_RenderFeatures.Upscaling || ps_r_scale_mode < 2)
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
		else if(ps_r2_aa_type == 3 && g_RenderFeatures.MotionVectors)
		{
			GPU_EVENT(phase_taa);
			phase_taa(Cmd);
		}
	}

	if (RImplementation.NeedMotionVectors())
	{
		phase_depth_upscale(Cmd);
		phase_mblur(Cmd);

		// r_debug_view velocity: what the history passes have just read, in place of the picture
		if (ps_r_debug_view == DebugView_Velocity)
		{
			GPU_EVENT(debug_velocity);
			DrawSQ(Cmd, s_taa, rt_Generic_0, 3, [&]
			{
				Cmd.SetCullMode(ERHI_CULLMODE::NONE);
			});
		}
	}

	u_setrt(Cmd, get_width(), get_height(), 0, 0, 0, 0);
	RImplementation.rmNormal();

	switch(g_RenderFeatures.Upscaling ? ps_r_scale_mode : 0)
	{
		case 2:
		case 3:
		case 4:
		{
			// D3D12: DLSS, FSR and XeSS through NRI. D3D11 has FSR 3 alone, whichever of them is asked for
			if (!(UpscaleNri::IsWanted() ? PhaseUpscaleNri(Cmd) : phase_fsr()))
			{
				Msg("! The upscaler failed: the frame is scaled plainly until the video restarts");
				ps_r_scale_mode = 1;
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

	// A debug view shows values, not a scene: nothing is measured and nothing glows, the exposure is one. Exposed by
	// its own average, a count of layers read brighter the fewer layers the frame had
	const bool IsDebugView = ps_r_debug_view != 0;

	if (IsDebugView)
	{
		const float Unexposed[4] = { 0.79f, 0.79f, 0.79f, 0.79f };
		Cmd.ClearTarget(rt_LUM_D->pRT, Unexposed);
		IsExposureFresh = true;
	}
	// Without bloom the first level alone is drawn, which the exposure is measured from
	else if (g_Bloom)
	{
		phase_bloom_downsample(Cmd);
		phase_bloom_upsample(Cmd);
	}
	else
	{
		phase_bloom_downsample(Cmd, 1);
	}

	if (!IsDebugView)
	{
		phase_new_luminance(Cmd);
	}

	if (RImplementation.o.DofCompiled)
	{
		phase_new_dof(Cmd);
	}

	u_setrt(Cmd, rt_Back_Buffer, 0, 0, 0);
	RImplementation.rmNormal();

	Cmd.SetCullMode(ERHI_CULLMODE::NONE);
	Cmd.SetStencil(FALSE);

	{
		GPU_EVENT(combine_2);

		//	Set up variable
		Fvector2 vDofKernel;
		const float DofKernelSize = g_pGamePersistent->GetDofKernel(ps_r2_dof_kernel_size) * (1.0f + (DofLensKernelScale - 1.0f) * LensDofFactor);
		vDofKernel.set(0.5f / Device.TargetWidth, 0.5f / Device.TargetHeight);
		vDofKernel.mul(DofKernelSize);

		Fvector3 dof;
		g_pGamePersistent->GetCurrentDof(dof);

		DrawPassSQ(Cmd, s_combine, 2, [&]
		{
			if (RImplementation.o.DofCompiled)
			{
				const float Width = float(Device.TargetWidth);
				const float Height = float(Device.TargetHeight);
				const float PixelScale = std::max(Width / float(rt_dof_blur1->dwWidth), Height / float(rt_dof_blur1->dwHeight));
				Cmd.Constants.Write<CBPost>().dof_combine_params.set(Width, Height, PixelScale, DofRadiusLimit);
			}
			else
			{
				Cmd.Constants.Write<CBPost>().dof_combine_params.set(0.0f, 0.0f, 0.0f, 0.0f);
			}
			Cmd.Constants.Write<CBPost>().bloom_params.set(g_Bloom && !IsDebugView ? Device.Grade(1, ps_r2_bloom_amount) : 0.0f, 0.0f, 0.0f, Device.Grade(0, 0.f));
			Cmd.Constants.Write<CBPost>().tonemap_params.set(ps_r2_tonemap_compression, ps_r2_tonemap_desaturation, ps_r2_tonemap_white, ps_r2_autoexposure_bias);
			Cmd.Constants.Write<CBPost>().spp_params.set(ps_r2_saturation, ps_r2_ls_flags_ext.test(R2FLAG_SPP_VIGNETTE) ? 1.0f : 0.0f, 0.0f, 0.0f);
		});
	}

	Cmd.SetStencil		(FALSE);

	//	if FP16-BLEND !not! supported - draw flares here, overwise they are already in the bloom target
	dxLensFlareRender::Draw(false, true, true);

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
	else
	{
		NightVisionInterferenceInitialized = false;
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

void CRenderTarget::phase_combine_volumetric(GfxCmdList& Cmd)
{
	if (!m_bHasActiveVolumetric)
	{
		return;
	}

	GPU_EVENT(phase_combine_volumetric);

	DrawPassSQ(Cmd, s_combine_volumetric, 0);
}
