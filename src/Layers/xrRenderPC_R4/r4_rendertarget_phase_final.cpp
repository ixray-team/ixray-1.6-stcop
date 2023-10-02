#include "stdafx.h"
#include "../../xrEngine/igame_persistent.h"
#include "../../xrEngine/environment.h"

#include "../xrRender/dxEnvironmentRender.h"

void CRenderTarget::DoAsyncScreenshot()
{
	//	Igor: screenshot will not have postprocess applied.
	//	TODO: fox that later
	if (RImplementation.m_bMakeAsyncSS)
	{
		HRESULT hr;
		ID3DTexture2D* pBuffer = nullptr;
		hr = HW.m_pSwapChain->GetBuffer(0, IID_PPV_ARGS(&pBuffer));
		HW.pContext->CopyResource(t_ss_async, pBuffer);
		RImplementation.m_bMakeAsyncSS = false;
	}
}

void set_viewport(ID3DDeviceContext* dev, float w, float h);

inline float hclip(float v, float dim)
{
	return 2.f * v / dim - 1.f; 
}

extern int			ps_r4_native_forward;

//	TODO: DX10: Remove half poxel offset
void CRenderTarget::phase_final()
{
	PIX_EVENT(phase_combine);

	//*** exposure-pipeline
	t_LUM_src->surface_set(rt_LUM->pSurface);
	t_LUM_dest->surface_set(rt_LUM->pSurface);

	if (RImplementation.o.ssao_hdao && RImplementation.o.ssao_ultra) {
		if (ps_r_ssao > 0) {
			phase_hdao();
		}
	} else {
		if (RImplementation.o.ssao_opt_data) {
			phase_downsamp();
		} else if (RImplementation.o.ssao_blur_on) {
			phase_ssao();
		}
	}

	FLOAT ColorRGBA[4] = { 0.0f, 0.0f, 0.0f, 0.0f };
	HW.pContext->ClearRenderTargetView(rt_Target->pRT, ColorRGBA);
	RCache.set_CullMode(CULL_NONE);
	RCache.set_Stencil(FALSE);

	u_setrt(rt_Target, nullptr, nullptr, rt_HWDepth->pZRT);
	g_pGamePersistent->Environment().RenderSky();
	u_setrt(rt_Target, nullptr, nullptr, rt_HWDepth->pZRT);
	g_pGamePersistent->Environment().RenderClouds();
	u_setrt(rt_Target, nullptr, nullptr, rt_HWDepth->pZRT);

	RCache.set_Stencil(TRUE, D3DCMP_LESSEQUAL, 0x01, 0xff, 0x00);	// stencil should be >= 1
	if (RImplementation.o.nvstencil) {
		u_stencil_optimize(CRenderTarget::SO_Combine);
		RCache.set_ColorWriteEnable();
	}

	// Draw full-screen quad textured with our scene image
	phase_combine();

	//	Igor: for volumetric lights
	//	combine light volume here
	if (m_bHasActiveVolumetric) {
		phase_combine_volumetric();
	}

	// Perform blooming filter and distortion if needed
	RCache.set_Stencil(FALSE);

	// for msaa we need a resolved color buffer - Holger
	if (ps_r4_native_forward == 0) {
		phase_forward();
	}

	phase_bloom();												// HDR RT invalidated here

	if (ps_r2_aa_type == 1) {
		PIX_EVENT(phase_fxaa);
		phase_fxaa();
		RCache.set_Stencil(FALSE);
	} else if (ps_r2_aa_type == 2) {
		PIX_EVENT(phase_smaa);
		phase_smaa();
		RCache.set_Stencil(FALSE);
	}

	////////////////////////////////////////////////////////////
	// STAGE BEFORE SCALING
	////////////////////////////////////////////////////////////
	switch (ps_r4_upscale_type) {
	case 0:
	case 1:
		phase_output_scale(ps_r4_upscale_type == SCALETYPE_LINEAR);
		break;
	case 2:
		if (!g_Fsr2Wrapper.IsCreated()) {
			phase_output_scale(false);
		} else {
			phase_fsr2_combine();
		}
		break;
	case 3:
		if (!g_DLSSWrapper.IsCreated()) {
			phase_output_scale(false);
		} else {
			phase_dlss_combine();
		}
		break;
	default:
		break;
	}

	////////////////////////////////////////////////////////////
	// STAGE AFTER SCALING
	////////////////////////////////////////////////////////////

	phase_depth_scale();
	if (ps_r4_native_forward == 1) {
		phase_forward();
	}

	phase_distort();
	RCache.set_CullMode(CULL_NONE);
	RCache.set_Stencil(FALSE);

	g_pGamePersistent->Environment().RenderFlares();	// lens-flares
	phase_pp();

	u_setrt(RCache.get_target_width(), RCache.get_target_height(), HW.pBaseRT, NULL, NULL, rt_HWScaledTargetDepth->pZRT);
}