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

		//	HACK: unbind RT. CopyResourcess needs src and targetr to be unbound.
		//u_setrt				( Device.dwWidth,Device.dwHeight,HW.pBaseRT,NULL,NULL,HW.pBaseZB);

		//ID3DTexture2D *pTex = 0;
		//if (RImplementation.o.dx10_msaa)
		//	pTex = rt_Generic->pSurface;
		//else
		//	pTex = rt_Color->pSurface;


		//HW.pDevice->CopyResource( t_ss_async, pTex );
		ID3DTexture2D* pBuffer = nullptr;
		hr = HW.m_pSwapChain->GetBuffer(0, IID_PPV_ARGS(&pBuffer));
		HW.pContext->CopyResource( t_ss_async, pBuffer );
		

		RImplementation.m_bMakeAsyncSS = false;
	}
}

float	hclip(float v, float dim)		{ return 2.f*v/dim - 1.f; }
void	CRenderTarget::phase_combine	()
{
	PIX_EVENT(phase_combine);

	//	TODO: DX10: Remove half poxel offset
	bool	_menu_pp	= g_pGamePersistent?g_pGamePersistent->OnRenderPPUI_query():false;

	u32			Offset					= 0;
	Fvector2	p0,p1;

	//*** exposure-pipeline
	u32			gpu_id	= Device.dwFrame%HW.Caps.iGPUNum;
	{
		t_LUM_src->surface_set		(rt_LUM_pool[gpu_id*2+0]->pSurface);
		t_LUM_dest->surface_set		(rt_LUM_pool[gpu_id*2+1]->pSurface);
	}

    if( RImplementation.o.ssao_hdao && RImplementation.o.ssao_ultra)
    {
        if( ps_r_ssao > 0 )
        {
		    phase_hdao();
        }
    }
    else
    {
        if (RImplementation.o.ssao_opt_data)
        {
            phase_downsamp();
            //phase_ssao();
        } 
        else if (RImplementation.o.ssao_blur_on)
            phase_ssao();
    }

	FLOAT ColorRGBA[4] = {0.0f, 0.0f, 0.0f, 0.0f};
	// low/hi RTs
	HW.pContext->ClearRenderTargetView(rt_Generic_0->pRT, ColorRGBA);
	HW.pContext->ClearRenderTargetView(rt_Generic_1->pRT, ColorRGBA);
	RCache.set_CullMode	( CULL_NONE );
	RCache.set_Stencil	( FALSE		);

	BOOL	split_the_scene_to_minimize_wait			= FALSE;
	if (ps_r2_ls_flags.test(R2FLAG_EXP_SPLIT_SCENE))	split_the_scene_to_minimize_wait=TRUE;

	u_setrt(rt_Generic_0, rt_Generic_1, nullptr, rt_HWDepth->pZRT);
	g_pGamePersistent->Environment().RenderSky();
	u_setrt(rt_Generic_0, nullptr, nullptr, rt_HWDepth->pZRT);
	g_pGamePersistent->Environment().RenderClouds();
	u_setrt(rt_Generic_0, rt_Generic_1, nullptr, rt_HWDepth->pZRT);

	RCache.set_Stencil(TRUE, D3DCMP_LESSEQUAL, 0x01, 0xff, 0x00);	// stencil should be >= 1
	if (RImplementation.o.nvstencil) {
		u_stencil_optimize(CRenderTarget::SO_Combine);
		RCache.set_ColorWriteEnable();
	}


	// Draw full-screen quad textured with our scene image
	if (!_menu_pp)
	{
		PIX_EVENT(combine_1);
		// Compute params
		Fmatrix		m_v2w;			m_v2w.invert				(Device.mView		);
		CEnvDescriptorMixer& envdesc= *g_pGamePersistent->Environment().CurrentEnv		;
		const float minamb			= 0.001f;
		Fvector4	ambclr			= { _max(envdesc.ambient.x*2,minamb),	_max(envdesc.ambient.y*2,minamb),			_max(envdesc.ambient.z*2,minamb),	0	};
					ambclr.mul		(ps_r2_sun_lumscale_amb);

//.		Fvector4	envclr			= { envdesc.sky_color.x*2+EPS,	envdesc.sky_color.y*2+EPS,	envdesc.sky_color.z*2+EPS,	envdesc.weight					};
		Fvector4	envclr			= { envdesc.hemi_color.x*2+EPS,	envdesc.hemi_color.y*2+EPS,	envdesc.hemi_color.z*2+EPS,	envdesc.weight					};

		Fvector4	fogclr			= { envdesc.fog_color.x,	envdesc.fog_color.y,	envdesc.fog_color.z,		0	};
					envclr.x		*= 2*ps_r2_sun_lumscale_hemi; 
					envclr.y		*= 2*ps_r2_sun_lumscale_hemi; 
					envclr.z		*= 2*ps_r2_sun_lumscale_hemi;
		Fvector4	sunclr,sundir;

		float		fSSAONoise = 2.0f;
					fSSAONoise *= tan(deg2rad(67.5f/2.0f));
					fSSAONoise /= tan(deg2rad(Device.fFOV/2.0f));

		float		fSSAOKernelSize = 150.0f;
					fSSAOKernelSize *= tan(deg2rad(67.5f/2.0f));
					fSSAOKernelSize /= tan(deg2rad(Device.fFOV/2.0f));


		// sun-params
		{
			light*		fuckingsun		= (light*)RImplementation.Lights.sun_adapted._get()	;
			Fvector		L_dir,L_clr;	float L_spec;
			L_clr.set					(fuckingsun->color.r,fuckingsun->color.g,fuckingsun->color.b);
			L_spec						= u_diffuse2s	(L_clr);
			Device.mView.transform_dir	(L_dir,fuckingsun->direction);
			L_dir.normalize				();

			sunclr.set				(L_clr.x,L_clr.y,L_clr.z,L_spec);
			sundir.set				(L_dir.x,L_dir.y,L_dir.z,0);
		}

		// Fill VB
		float	scale_X				= RCache.get_width() / float(TEX_jitter);
		float	scale_Y				= RCache.get_height() / float(TEX_jitter);

		// Fill vertex buffer
		FVF::TL* pv					= (FVF::TL*)	RCache.Vertex.Lock	(4,g_combine->vb_stride,Offset);
		pv->set						(-1,	1,	0, 1, 0, 0,			scale_Y	);	pv++;
		pv->set						(-1,	-1,	0, 0, 0, 0,			0		);	pv++;
		pv->set						(1,		1,	1, 1, 0, scale_X,	scale_Y	);	pv++;
		pv->set						(1,		-1,	1, 0, 0, scale_X,	0		);	pv++;
		RCache.Vertex.Unlock		(4,g_combine->vb_stride);

		dxEnvDescriptorMixerRender &envdescren = *(dxEnvDescriptorMixerRender*)(&*envdesc.m_pDescriptorMixer);

		// Setup textures
		ID3DBaseTexture*	e0	= _menu_pp?0:envdescren.sky_r_textures_env[0].second->surface_get();
		ID3DBaseTexture*	e1	= _menu_pp?0:envdescren.sky_r_textures_env[1].second->surface_get();
		t_envmap_0->surface_set		(e0);	_RELEASE(e0);
		t_envmap_1->surface_set		(e1);	_RELEASE(e1);
	
		// Draw
		RCache.set_Element			(s_combine->E[0]	);
		//RCache.set_Geometry			(g_combine_VP		);
		RCache.set_Geometry			(g_combine		);

		RCache.set_c				("m_v2w",			m_v2w	);
		RCache.set_c				("L_ambient",		ambclr	);

		RCache.set_c				("Ldynamic_color",	sunclr	);
		RCache.set_c				("Ldynamic_dir",	sundir	);

		RCache.set_c				("env_color",		envclr	);
		RCache.set_c				("fog_color",		fogclr	);

		RCache.set_c				("ssao_noise_tile_factor",	fSSAONoise	);
		RCache.set_c				("ssao_kernel_size",		fSSAOKernelSize	);

		RCache.Render				(D3DPT_TRIANGLELIST,Offset,0,4,0,2);
   }

	// Forward rendering
	{
		PIX_EVENT(Forward_rendering);
		u_setrt							(rt_Generic_0,0,0, rt_HWDepth->pZRT);
		RCache.set_CullMode				(CULL_CCW);
		RCache.set_Stencil				(FALSE);
		RCache.set_ColorWriteEnable		();
		//	TODO: DX10: CHeck this!
		//g_pGamePersistent->Environment().RenderClouds	();
		RImplementation.render_forward	();
		if (g_pGamePersistent)	g_pGamePersistent->OnRenderPPUI_main()	;	// PP-UI
	}

	//	Igor: for volumetric lights
	//	combine light volume here
	if (m_bHasActiveVolumetric)
		phase_combine_volumetric();

	// Perform blooming filter and distortion if needed
	RCache.set_Stencil	(FALSE);

   // for msaa we need a resolved color buffer - Holger
	phase_bloom			( );												// HDR RT invalidated here

	if (ps_r2_aa_type == 1)
	{
		PIX_EVENT(phase_fxaa);
		phase_fxaa();
		RCache.set_Stencil(FALSE);
	}
	else if (ps_r2_aa_type == 2)
	{
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
	default:
		break;
	}

	////////////////////////////////////////////////////////////
	// STAGE AFTER SCALING
	////////////////////////////////////////////////////////////

	// Distortion filter
	BOOL bDistort = RImplementation.o.distortion_enabled;				// This can be modified
	{
		if ((0 == RImplementation.mapDistort.size()) && !_menu_pp)
			bDistort = FALSE;
		if (bDistort)
		{
			PIX_EVENT(render_distort_objects);
			FLOAT ColorRGBA_[4] = { 127.0f / 255.0f, 127.0f / 255.0f, 0.0f, 127.0f / 255.0f };

			u_setrt(rt_Distort, 0, 0, rt_HWDepth->pZRT);		// Now RT is a distortion mask
			HW.pContext->ClearRenderTargetView(rt_Distort->pRT, ColorRGBA_);

			RCache.set_CullMode(CULL_CCW);
			RCache.set_Stencil(FALSE);
			RCache.set_ColorWriteEnable();
			RImplementation.r_dsgraph_render_distort();
			if (g_pGamePersistent)
			{
				g_pGamePersistent->OnRenderPPUI_PP();	// PP-UI
			}
		}
	}

	RCache.set_CullMode(CULL_NONE);
	RCache.set_Stencil(FALSE);

	g_pGamePersistent->Environment().RenderFlares();	// lens-flares
	phase_pp();
}

void CRenderTarget::phase_wallmarks		()
{
	// Targets
	RCache.set_RT(NULL,2);
	RCache.set_RT(NULL,1);
   	u_setrt								(rt_Color,NULL,NULL, rt_HWDepth->pZRT);
	// Stencil	- draw only where stencil >= 0x1
	RCache.set_Stencil					(TRUE,D3DCMP_LESSEQUAL,0x01,0xff,0x00);
	RCache.set_CullMode					(CULL_CCW);
	RCache.set_ColorWriteEnable			(D3DCOLORWRITEENABLE_RED|D3DCOLORWRITEENABLE_GREEN|D3DCOLORWRITEENABLE_BLUE);
}

void CRenderTarget::phase_combine_volumetric()
{
	PIX_EVENT(phase_combine_volumetric);
	u32			Offset					= 0;

	//	TODO: DX10: Remove half pixel offset here
	u_setrt(rt_Generic_0,rt_Generic_1,0, rt_HWDepth->pZRT);

	//	Sets limits to both render targets
	RCache.set_ColorWriteEnable(D3DCOLORWRITEENABLE_RED|D3DCOLORWRITEENABLE_GREEN|D3DCOLORWRITEENABLE_BLUE);
	{
		// Fill VB
		float	scale_X				= RCache.get_width() / float(TEX_jitter);
		float	scale_Y				= RCache.get_height() / float(TEX_jitter);

		// Fill vertex buffer
		FVF::TL* pv					= (FVF::TL*)	RCache.Vertex.Lock	(4,g_combine->vb_stride,Offset);
		pv->set						(-1,	1,	0, 1, 0, 0,			scale_Y	);	pv++;
		pv->set						(-1,	-1,	0, 0, 0, 0,			0		);	pv++;
		pv->set						(1,		1,	1, 1, 0, scale_X,	scale_Y	);	pv++;
		pv->set						(1,		-1,	1, 0, 0, scale_X,	0		);	pv++;
		RCache.Vertex.Unlock		(4,g_combine->vb_stride);

		// Draw
		RCache.set_Element			(s_combine_volumetric->E[0]	);
		RCache.set_Geometry			(g_combine		);
		RCache.Render				(D3DPT_TRIANGLELIST,Offset,0,4,0,2);
	}
	RCache.set_ColorWriteEnable();
}
