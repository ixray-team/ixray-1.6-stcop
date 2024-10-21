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

void	CRenderTarget::phase_combine	()
{
	PIX_EVENT(phase_combine);

	//	TODO: DX10: Remove half poxel offset
	bool	_menu_pp	= g_pGamePersistent?g_pGamePersistent->OnRenderPPUI_query():false;

	u32			Offset					= 0;
	Fvector2	p0,p1;

	//*** exposure-pipeline
	u32			gpu_id	= Device.dwFrame % 1;
	{
		t_LUM_src->surface_set		(rt_LUM_pool[gpu_id*2+0]->pSurface);
		t_LUM_dest->surface_set		(rt_LUM_pool[gpu_id*2+1]->pSurface);
	}

	if (ps_r_ssao > 0)
	{
		PROF_EVENT("PHASE_AMBIENT_OCCLUSION");
		phase_downsamp();

		if (RImplementation.SSAO.test(ESSAO_DATA::SSAO_GTAO))
		{
			phase_gtao();
		}
		else if (RFeatureLevel >= D3D_FEATURE_LEVEL_11_0 && RImplementation.SSAO.test(ESSAO_DATA::SSAO_HDAO) && RImplementation.SSAO.test(ESSAO_DATA::SSAO_ULTRA_OPT))
		{
			phase_hdao();
		}
		else
		{
			phase_ssao();
		}
	}


	FLOAT ColorRGBA[4] = {0.0f, 0.0f, 0.0f, 0.0f};
	u_setrt(rt_Generic_0, 0, 0, RDepth);

	RCache.set_CullMode(CULL_NONE);
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

		RCache.Render(D3DPT_TRIANGLELIST, Offset, 0, 4, 0, 2);
	}

	if(ps_r2_ls_flags_ext.test(R4FLAG_PUDDLES))
	{
		PIX_EVENT(Forward_rendering_puddles);
		phase_puddles();
	}

	// Forward rendering
	{
		PIX_EVENT(Forward_rendering);
		phase_scene_forward();

		RCache.set_CullMode (CULL_CCW);
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
		if(RImplementation.mapDistort.size() < 1 && !_menu_pp) {
			bDistort= FALSE;
		}
		if(bDistort) {
			PIX_EVENT(render_distort_objects);
			FLOAT ColorRGBA_[4] = {127.0f / 255.0f, 127.0f / 255.0f, 0.0f, 127.0f / 255.0f};
			u_setrt(rt_Generic_1, 0, 0, RDepth);		// Now RT is a distortion mask

			RImplementation.rmNormal();

			RContext->ClearRenderTargetView(rt_Generic_1->pRT, ColorRGBA_);
			RCache.set_CullMode(CULL_CCW);
			RCache.set_Stencil(FALSE);
			RCache.set_ColorWriteEnable();
			RImplementation.r_dsgraph_render_distort();

			if(g_pGamePersistent) {
				g_pGamePersistent->OnRenderPPUI_PP();	// PP-UI
			}

			u_setrt(rt_Generic_2, 0, 0, 0);

			constexpr auto C = color_rgba(255, 255, 255, 255);
			float _w = RCache.get_width();
			float _h = RCache.get_height();
			float d_Z = EPS_S;
			float d_W = 1.f;

			p0.set(.5f / _w, .5f / _h);
			p1.set((_w + .5f) / _w, (_h + .5f) / _h);

			FVF::TL* pv = (FVF::TL*)RCache.Vertex.Lock(4, g_combine->vb_stride, Offset);

			pv->set(EPS, float(_h + EPS), d_Z, d_W, C, p0.x, p1.y);	pv++;
			pv->set(EPS, EPS, d_Z, d_W, C, p0.x, p0.y);	pv++;
			pv->set(float(_w + EPS), float(_h + EPS), d_Z, d_W, C, p1.x, p1.y);	pv++;
			pv->set(float(_w + EPS), EPS, d_Z, d_W, C, p1.x, p0.y);	pv++;

			RCache.Vertex.Unlock(4, g_combine->vb_stride);

			// Draw COLOR
			RCache.set_Element(s_combine->E[1]);
			RCache.set_Geometry(g_combine);

			RCache.Render(D3DPT_TRIANGLELIST, Offset, 0, 4, 0, 2);

			RContext->CopyResource(rt_Generic_0->pSurface, rt_Generic_2->pSurface);
		}
	}

	if(ps_r_scale_mode < 2) {
		if(ps_r2_aa_type == 1) {
			PIX_EVENT(phase_fxaa);
			phase_fxaa();
			RCache.set_Stencil(FALSE);
		}
		else if(ps_r2_aa_type == 2) {
			PIX_EVENT(phase_smaa);
			phase_smaa();
			RCache.set_Stencil(FALSE);
		}
	}

	u_setrt(get_width(), get_height(), 0, 0, 0, 0);
	RImplementation.rmNormal();

	switch(ps_r_scale_mode)
	{
		case 3:
		{
			if(!phase_fsr()) {
				ps_r_scale_mode = 1;
			}
			break;
		}
		case 2:
		{
			if(!phase_dlss()) {
				ps_r_scale_mode = 3;
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

	RCache.set_CullMode(CULL_NONE);
	RCache.set_Stencil(FALSE);
	{
		PIX_EVENT(combine_2);

		float _w = (float)get_width();
		float _h = (float)get_height();

		float ddw = 1.f / _w;
		float ddh = 1.f / _h;
		p0.set(.5f / _w, .5f / _h);
		p1.set((_w + .5f) / _w, (_h + .5f) / _h);

		// Fill vertex buffer
		v_aa* pv = (v_aa*)RCache.Vertex.Lock(4, g_aa_AA->vb_stride, Offset);
		pv->p.set(EPS, float(_h + EPS), EPS, 1.f); pv->uv0.set(p0.x, p1.y); pv->uv1.set(p0.x - ddw, p1.y - ddh); pv->uv2.set(p0.x + ddw, p1.y + ddh); pv->uv3.set(p0.x + ddw, p1.y - ddh); pv->uv4.set(p0.x - ddw, p1.y + ddh); pv->uv5.set(p0.x - ddw, p1.y, p1.y, p0.x + ddw); pv->uv6.set(p0.x, p1.y - ddh, p1.y + ddh, p0.x); pv++;
		pv->p.set(EPS, EPS, EPS, 1.f); pv->uv0.set(p0.x, p0.y); pv->uv1.set(p0.x - ddw, p0.y - ddh); pv->uv2.set(p0.x + ddw, p0.y + ddh); pv->uv3.set(p0.x + ddw, p0.y - ddh); pv->uv4.set(p0.x - ddw, p0.y + ddh); pv->uv5.set(p0.x - ddw, p0.y, p0.y, p0.x + ddw); pv->uv6.set(p0.x, p0.y - ddh, p0.y + ddh, p0.x); pv++;
		pv->p.set(float(_w + EPS), float(_h + EPS), EPS, 1.f); pv->uv0.set(p1.x, p1.y); pv->uv1.set(p1.x - ddw, p1.y - ddh); pv->uv2.set(p1.x + ddw, p1.y + ddh); pv->uv3.set(p1.x + ddw, p1.y - ddh); pv->uv4.set(p1.x - ddw, p1.y + ddh); pv->uv5.set(p1.x - ddw, p1.y, p1.y, p1.x + ddw); pv->uv6.set(p1.x, p1.y - ddh, p1.y + ddh, p1.x); pv++;
		pv->p.set(float(_w + EPS), EPS, EPS, 1.f); pv->uv0.set(p1.x, p0.y); pv->uv1.set(p1.x - ddw, p0.y - ddh); pv->uv2.set(p1.x + ddw, p0.y + ddh); pv->uv3.set(p1.x + ddw, p0.y - ddh); pv->uv4.set(p1.x - ddw, p0.y + ddh); pv->uv5.set(p1.x - ddw, p0.y, p0.y, p1.x + ddw); pv->uv6.set(p1.x, p0.y - ddh, p0.y + ddh, p1.x); pv++;
		RCache.Vertex.Unlock(4, g_aa_AA->vb_stride);

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

		RCache.set_Geometry(g_aa_AA);
		RCache.Render(D3DPT_TRIANGLELIST, Offset, 0, 4, 0, 2);
	}

	RCache.set_Stencil		(FALSE);

	//	if FP16-BLEND !not! supported - draw flares here, overwise they are already in the bloom target
	g_pGamePersistent->Environment().RenderFlares();	// lens-flares

	if(ps_r4_cas_sharpening > EPS) {
		PIX_EVENT(phase_cas);
		phase_cas();
	}

	if (ps_r2_ls_flags_ext.test(R2FLAG_SPP_SATURATION)) {
		PIX_EVENT(PhaseSaturation);
		PhaseSaturation();
	}

	if(ps_r2_ls_flags_ext.test(R2FLAG_SPP_VIGNETTE)) {
		PIX_EVENT(PhaseVignette);
		PhaseVignette();
	}

	if(ps_r2_ls_flags_ext.test(R2FLAG_SPP_ABERRATION)) {
		PIX_EVENT(PhaseAberration);
		PhaseAberration();
	}
	{
		PIX_EVENT(phase_pp);
		phase_pp();
	}

	//	Re-adapt luminance
	RCache.set_Stencil(FALSE);

	//*** exposure-pipeline-clear
	{
		std::swap					(rt_LUM_pool[gpu_id*2+0],rt_LUM_pool[gpu_id*2+1]);
		t_LUM_src->surface_set		(nullptr);
		t_LUM_dest->surface_set		(nullptr);
	}

#ifdef DEBUG
	RCache.set_CullMode	( CULL_CCW );
	static	xr_vector<Fplane>		saved_dbg_planes;
	if (bDebug)		saved_dbg_planes= dbg_planes;
	else			dbg_planes		= saved_dbg_planes;
	if (1) for (u32 it=0; it<dbg_planes.size(); it++)
	{
		Fplane&		P	=	dbg_planes[it];
		Fvector		zero	;
		zero.mul	(P.n,P.d);
		
		Fvector             L_dir,L_up=P.n,L_right;
		L_dir.set           (0,0,1);                if (_abs(L_up.dotproduct(L_dir))>.99f)  L_dir.set(1,0,0);
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
#endif

	// ********************* Debug
	/*
	if (0)		{
		u32		C					= color_rgba	(255,255,255,255);
		float	_w					= float(Device.TargetWidth)/3;
		float	_h					= float(Device.TargetHeight)/3;

		// draw light-spheres
#ifdef DEBUG
		if (0) for (u32 it=0; it<dbg_spheres.size(); it++)
		{
			Fsphere				S	= dbg_spheres[it].first;
			Fmatrix				M;	
			u32				ccc		= dbg_spheres[it].second.get();
			M.scale					(S.R,S.R,S.R);
			M.translate_over		(S.P);
			RCache.dbg_DrawEllipse	(M,ccc);
			RCache.dbg_DrawAABB		(S.P,.05f,.05f,.05f,ccc);
		}
#endif
		// Draw quater-screen quad textured with our direct-shadow-map-image
		if (1) 
		{
			u32							IX=0,IY=1;
			p0.set						(.5f/_w, .5f/_h);
			p1.set						((_w+.5f)/_w, (_h+.5f)/_h );

			// Fill vertex buffer
			FVF::TL* pv					= (FVF::TL*) RCache.Vertex.Lock	(4,g_combine->vb_stride,Offset);
			pv->set						((IX+0)*_w+EPS,	(IY+1)*_h+EPS,	EPS,	1.f, C, p0.x, p1.y);	pv++;
			pv->set						((IX+0)*_w+EPS,	(IY+0)*_h+EPS,	EPS,	1.f, C, p0.x, p0.y);	pv++;
			pv->set						((IX+1)*_w+EPS,	(IY+1)*_h+EPS,	EPS,	1.f, C, p1.x, p1.y);	pv++;
			pv->set						((IX+1)*_w+EPS,	(IY+0)*_h+EPS,	EPS,	1.f, C, p1.x, p0.y);	pv++;
			RCache.Vertex.Unlock		(4,g_combine->vb_stride);

			// Draw COLOR
			RCache.set_Shader			(s_combine_dbg_0);
			RCache.set_Geometry			(g_combine);
			RCache.Render				(D3DPT_TRIANGLELIST,Offset,0,4,0,2);
		}

		// Draw quater-screen quad textured with our accumulator
		if (0)
		{
			u32							IX=1,IY=1;
			p0.set						(.5f/_w, .5f/_h);
			p1.set						((_w+.5f)/_w, (_h+.5f)/_h );

			// Fill vertex buffer
			FVF::TL* pv					= (FVF::TL*) RCache.Vertex.Lock	(4,g_combine->vb_stride,Offset);
			pv->set						((IX+0)*_w+EPS,	(IY+1)*_h+EPS,	EPS,	1.f, C, p0.x, p1.y);	pv++;
			pv->set						((IX+0)*_w+EPS,	(IY+0)*_h+EPS,	EPS,	1.f, C, p0.x, p0.y);	pv++;
			pv->set						((IX+1)*_w+EPS,	(IY+1)*_h+EPS,	EPS,	1.f, C, p1.x, p1.y);	pv++;
			pv->set						((IX+1)*_w+EPS,	(IY+0)*_h+EPS,	EPS,	1.f, C, p1.x, p0.y);	pv++;
			RCache.Vertex.Unlock		(4,g_combine->vb_stride);

			// Draw COLOR
			RCache.set_Shader			(s_combine_dbg_1);
			RCache.set_Geometry			(g_combine);
			RCache.Render				(D3DPT_TRIANGLELIST,Offset,0,4,0,2);
		}
	}
	*/
#ifdef DEBUG
	dbg_spheres.clear	();
	dbg_lines.clear		();
	dbg_planes.clear	();
#endif
}

void CRenderTarget::phase_wallmarks		()
{
	// Targets
	u_setrt(rt_Color, nullptr, nullptr, RDepth);
	// Stencil	- draw only where stencil >= 0x1
	RCache.set_Stencil					(TRUE,D3DCMP_LESSEQUAL,0x01,0xff,0x00);
	RCache.set_CullMode					(CULL_CCW);
	RCache.set_ColorWriteEnable			(D3DCOLORWRITEENABLE_RED|D3DCOLORWRITEENABLE_GREEN|D3DCOLORWRITEENABLE_BLUE);
}

void CRenderTarget::phase_combine_volumetric()
{
	PIX_EVENT(phase_combine_volumetric);
	u32			Offset					= 0;
	//Fvector2	p0,p1;

	//	TODO: DX10: Remove half pixel offset here

	//u_setrt(rt_Generic_0,0,0,RDepth );			// LDR RT
	u_setrt(rt_Generic_0, 0, 0, RDepth);
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
		//RCache.set_Geometry			(g_combine_VP		);
		RCache.set_Geometry			(g_combine		);
		RCache.Render				(D3DPT_TRIANGLELIST,Offset,0,4,0,2);
	}
	RCache.set_ColorWriteEnable();
}
