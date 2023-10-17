#include "stdafx.h"
#include "../../xrEngine/igame_persistent.h"
#include "../../xrEngine/environment.h"

#include "../xrRender/dxEnvironmentRender.h"

void	CRenderTarget::phase_combine	()
{
	PIX_EVENT(combine_1);
	bool _menu_pp = g_pGamePersistent ? g_pGamePersistent->OnRenderPPUI_query() : false;
	if (_menu_pp) {
		return;
	}

	// Compute params
	Fmatrix		m_v2w;			m_v2w.invert(Device.mView);
	CEnvDescriptorMixer& envdesc = *g_pGamePersistent->Environment().CurrentEnv;
	const float minamb = 0.001f;
	Fvector4	ambclr = { _max(envdesc.ambient.x * 2,minamb),	_max(envdesc.ambient.y * 2,minamb),			_max(envdesc.ambient.z * 2,minamb),	0 };
	ambclr.mul(ps_r2_sun_lumscale_amb);

	//.		Fvector4	envclr			= { envdesc.sky_color.x*2+EPS,	envdesc.sky_color.y*2+EPS,	envdesc.sky_color.z*2+EPS,	envdesc.weight					};
	Fvector4	envclr = { envdesc.hemi_color.x * 2 + EPS,	envdesc.hemi_color.y * 2 + EPS,	envdesc.hemi_color.z * 2 + EPS,	envdesc.weight };

	Fvector4	fogclr = { envdesc.fog_color.x,	envdesc.fog_color.y,	envdesc.fog_color.z,		0 };
	envclr.x *= 2 * ps_r2_sun_lumscale_hemi;
	envclr.y *= 2 * ps_r2_sun_lumscale_hemi;
	envclr.z *= 2 * ps_r2_sun_lumscale_hemi;
	Fvector4	sunclr, sundir;

	float		fSSAONoise = 2.0f;
	fSSAONoise *= tan(deg2rad(67.5f / 2.0f));
	fSSAONoise /= tan(deg2rad(Device.fFOV / 2.0f));

	float		fSSAOKernelSize = 150.0f;
	fSSAOKernelSize *= tan(deg2rad(67.5f / 2.0f));
	fSSAOKernelSize /= tan(deg2rad(Device.fFOV / 2.0f));


	// sun-params
	{
		light* fuckingsun = (light*)RImplementation.Lights.sun_adapted._get();
		Fvector		L_dir, L_clr;	float L_spec;
		L_clr.set(fuckingsun->color.r, fuckingsun->color.g, fuckingsun->color.b);
		L_spec = u_diffuse2s(L_clr);
		Device.mView.transform_dir(L_dir, fuckingsun->direction);
		L_dir.normalize();

		sunclr.set(L_clr.x, L_clr.y, L_clr.z, L_spec);
		sundir.set(L_dir.x, L_dir.y, L_dir.z, 0);
	}

	// Fill VB
	float	scale_X = RCache.get_width() / float(TEX_jitter);
	float	scale_Y = RCache.get_height() / float(TEX_jitter);

	// Fill vertex buffer
	u32 Offset = 0;
	FVF::TL* pv = (FVF::TL*)RCache.Vertex.Lock(4, g_combine->vb_stride, Offset);
	pv->set(-1, 1, 0, 1, 0, 0, scale_Y);	pv++;
	pv->set(-1, -1, 0, 0, 0, 0, 0);	pv++;
	pv->set(1, 1, 1, 1, 0, scale_X, scale_Y);	pv++;
	pv->set(1, -1, 1, 0, 0, scale_X, 0);	pv++;
	RCache.Vertex.Unlock(4, g_combine->vb_stride);

	dxEnvDescriptorMixerRender& envdescren = *(dxEnvDescriptorMixerRender*)(&*envdesc.m_pDescriptorMixer);

	// Setup textures
	ID3DBaseTexture* e0 = envdescren.sky_r_textures_env[0].second->surface_get();
	ID3DBaseTexture* e1 = envdescren.sky_r_textures_env[1].second->surface_get();
	t_envmap_0->surface_set(e0);	_RELEASE(e0);
	t_envmap_1->surface_set(e1);	_RELEASE(e1);

	// Draw
	RCache.set_Element(s_combine->E[0]);
	RCache.set_Geometry(g_combine);

	RCache.set_c("m_v2w", m_v2w);
	RCache.set_c("L_ambient", ambclr);

	RCache.set_c("Ldynamic_color", sunclr);
	RCache.set_c("Ldynamic_dir", sundir);

	RCache.set_c("env_color", envclr);
	RCache.set_c("fog_color", fogclr);

	RCache.set_c("ssao_noise_tile_factor", fSSAONoise);
	RCache.set_c("ssao_kernel_size", fSSAOKernelSize);

	RCache.Render(D3DPT_TRIANGLELIST, Offset, 0, 4, 0, 2);
}

void CRenderTarget::phase_combine_bloom()
{
	// 
	struct v_aa {
		Fvector4	p;
		Fvector2	uv0;
		Fvector2	uv1;
		Fvector2	uv2;
		Fvector2	uv3;
		Fvector2	uv4;
		Fvector4	uv5;
		Fvector4	uv6;
	};

	Fvector2	p0, p1;
	float	_w = RCache.get_width();
	float	_h = RCache.get_height();
	float	ddw = 1.f / _w;
	float	ddh = 1.f / _h;
	p0.set(0, 0);
	p1.set((_w ) / _w, (_h) / _h);
	u_setrt(_w, _h, rt_TargetCombined->pRT, nullptr, nullptr, nullptr);

	// Fill vertex buffer
	u32 Offset = 0;
	v_aa* pv = (v_aa*)RCache.Vertex.Lock(4, g_aa_AA->vb_stride, Offset);
	pv->p.set(EPS, float(_h + EPS), EPS, 1.f); pv->uv0.set(p0.x, p1.y); pv->uv1.set(p0.x - ddw, p1.y - ddh); pv->uv2.set(p0.x + ddw, p1.y + ddh); pv->uv3.set(p0.x + ddw, p1.y - ddh); pv->uv4.set(p0.x - ddw, p1.y + ddh); pv->uv5.set(p0.x - ddw, p1.y, p1.y, p0.x + ddw); pv->uv6.set(p0.x, p1.y - ddh, p1.y + ddh, p0.x); pv++;
	pv->p.set(EPS, EPS, EPS, 1.f); pv->uv0.set(p0.x, p0.y); pv->uv1.set(p0.x - ddw, p0.y - ddh); pv->uv2.set(p0.x + ddw, p0.y + ddh); pv->uv3.set(p0.x + ddw, p0.y - ddh); pv->uv4.set(p0.x - ddw, p0.y + ddh); pv->uv5.set(p0.x - ddw, p0.y, p0.y, p0.x + ddw); pv->uv6.set(p0.x, p0.y - ddh, p0.y + ddh, p0.x); pv++;
	pv->p.set(float(_w + EPS), float(_h + EPS), EPS, 1.f); pv->uv0.set(p1.x, p1.y); pv->uv1.set(p1.x - ddw, p1.y - ddh); pv->uv2.set(p1.x + ddw, p1.y + ddh); pv->uv3.set(p1.x + ddw, p1.y - ddh); pv->uv4.set(p1.x - ddw, p1.y + ddh); pv->uv5.set(p1.x - ddw, p1.y, p1.y, p1.x + ddw); pv->uv6.set(p1.x, p1.y - ddh, p1.y + ddh, p1.x); pv++;
	pv->p.set(float(_w + EPS), EPS, EPS, 1.f); pv->uv0.set(p1.x, p0.y); pv->uv1.set(p1.x - ddw, p0.y - ddh); pv->uv2.set(p1.x + ddw, p0.y + ddh); pv->uv3.set(p1.x + ddw, p0.y - ddh); pv->uv4.set(p1.x - ddw, p0.y + ddh); pv->uv5.set(p1.x - ddw, p0.y, p0.y, p1.x + ddw); pv->uv6.set(p1.x, p0.y - ddh, p0.y + ddh, p1.x); pv++;
	RCache.Vertex.Unlock(4, g_aa_AA->vb_stride);

	//	Set up variable
	Fvector2	vDofKernel;
	vDofKernel.set(0, 0);
	vDofKernel.mul(ps_r2_dof_kernel_size);

	// Draw COLOR
	Fmatrix		m_previous, m_current;
	Fvector2	m_blur_scale;
	{
		// (new-camera) -> (world) -> (old_viewproj)
		static Fmatrix		m_saved_viewproj;
		Fmatrix	m_invview;	m_invview.invert(Device.mView);
		m_previous.mul(m_saved_viewproj, m_invview);
		m_current.set(Device.mProject);
		m_saved_viewproj.set(Device.mFullTransform);
		m_blur_scale.set(0,0);
	}

	RCache.set_Element(s_combine->E[2]);	// look at blender_combine.cpp
	RCache.set_c("m_current", m_current);
	RCache.set_c("m_previous", m_previous);
	RCache.set_c("m_blur", m_blur_scale.x, m_blur_scale.y, 0, 0);
	Fvector3					dof;
	g_pGamePersistent->GetCurrentDof(dof);
	RCache.set_c("dof_params", dof.x, dof.y, dof.z, ps_r2_dof_sky);
	RCache.set_c("dof_kernel", vDofKernel.x, vDofKernel.y, ps_r2_dof_kernel_size, 0);

	RCache.set_Geometry(g_aa_AA);
	RCache.Render(D3DPT_TRIANGLELIST, Offset, 0, 4, 0, 2);

	u_setrt(_w, _h, nullptr, nullptr, nullptr, nullptr);
}

void CRenderTarget::phase_wallmarks		()
{
	// Targets
	RCache.set_RT(NULL,2);
	RCache.set_RT(NULL,1);
   	u_setrt								(rt_Color,NULL,NULL, nullptr);

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
	u_setrt(rt_Target, nullptr, nullptr, nullptr);

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
