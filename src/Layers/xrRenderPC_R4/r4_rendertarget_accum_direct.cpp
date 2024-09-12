#include "stdafx.h"
#include "../../xrEngine/igame_persistent.h"
#include "../../xrEngine/environment.h"

//////////////////////////////////////////////////////////////////////////
// tables to calculate view-frustum bounds in world space
// note: D3D uses [0..1] range for Z
static Fvector3		corners [8]			= {
	{ -1, -1,  0.7 },	{ -1, -1, +1},
	{ -1, +1, +1 },		{ -1, +1,  0.7},
	{ +1, +1, +1 },		{ +1, +1,  0.7},
	{ +1, -1, +1 },		{ +1, -1,  0.7}
};
static u16			facetable[16][3]		= {
	{ 3, 2, 1 },  
	{ 3, 1, 0 },		
	{ 7, 6, 5 }, 
	{ 5, 6, 4 },		
	{ 3, 5, 2 },
	{ 4, 2, 5 },		
	{ 1, 6, 7 },
	{ 7, 0, 1 },

	{ 5, 3, 0 },
	{ 7, 5, 0 },

	{ 1, 4, 6 },
	{ 2, 4, 1 },
};


void CRenderTarget::accum_direct_cascade	( u32 sub_phase, Fmatrix& xform, Fmatrix& xform_prev, float fBias )
{
	phase_accumulator();
	RCache.set_Stencil(FALSE);
	//	TODO: DX10: Remove half pixe offset
	// *** assume accumulator setted up ***
	light*			fuckingsun			= (light*)RImplementation.Lights.sun_adapted._get()	;

	// Common constants (light-related)
	Fvector		L_dir,L_clr;	float L_spec;
	L_clr.set					(fuckingsun->color.r,fuckingsun->color.g,fuckingsun->color.b);
	L_spec						= u_diffuse2s	(L_clr);
	Device.mView.transform_dir	(L_dir,fuckingsun->direction);
	L_dir.normalize				();

	// Perform lighting

		Fmatrix xf_invview;
		xf_invview.invert(Device.mView);


		// clouds xform
		Fmatrix m_clouds_shadow;
		{
			static	float	w_shift		= 0;
			Fmatrix			m_xform;
			Fvector			direction	= fuckingsun->direction	;
			float	w_dir				= g_pGamePersistent->Environment().CurrentEnv->wind_direction	;
			//float	w_speed				= g_pGamePersistent->Environment().CurrentEnv->wind_velocity	;
			Fvector			normal	;	normal.setHP(w_dir,0);
			w_shift		+=	0.003f*Device.fTimeDelta;
			Fvector			position;	position.set(0,0,0);
			m_xform.build_camera_dir	(position,direction,normal)	;
			Fvector			localnormal;m_xform.transform_dir(localnormal,normal); localnormal.normalize();
			m_clouds_shadow.mul			(m_xform,xf_invview)		;
			m_xform.scale				(0.002f,0.002f,1.f)			;
			m_clouds_shadow.mulA_44		(m_xform)					;
			m_xform.translate			(localnormal.mul(w_shift))	;
			m_clouds_shadow.mulA_44		(m_xform)					;
		}


		Fmatrix m_TexelAdjust = 
		{
			0.5f, 0.0f, 0.0f, 0.0f,
			0.0f, -0.5f, 0.0f, 0.0f,
			0.0f, 0.0f, 1.0f, 0.0f,
			0.5f, 0.5f, -0.0002, 1.0f
		};


		Fmatrix m_shadow[3];

		for(int casc = 0; casc < 3; casc++)
		{
			Fmatrix xf_project;
			xf_project.mul(m_TexelAdjust, RImplementation.m_sun_cascades[casc].view_proj);
			m_shadow[casc].mul(xf_project, xf_invview);
		}
	


		// Fill vertex buffer

	{
		u32 Offset = 0;
		constexpr u32 vertex_color = color_rgba(0, 0, 0, 255);

		FVF::TL* pv = (FVF::TL*)RCache.Vertex.Lock(3, g_combine->vb_stride, Offset);
		pv->set(-1.0, 1.0, 1.0, 1.0, vertex_color, 0.0, 0.0);
		pv++;
		pv->set(3.0, 1.0, 1.0, 1.0, vertex_color, 2.0, 0.0);
		pv++;
		pv->set(-1.0, -3.0, 1.0, 1.0, vertex_color, 0.0, 2.0);
		pv++;
		RCache.Vertex.Unlock(3, g_combine->vb_stride);

		// setup
		RCache.set_Element(s_accum_direct->E[0]);
		RCache.set_c("Ldynamic_dir", L_dir.x,L_dir.y,L_dir.z,0);
		RCache.set_ca("m_shadow_array", 0, m_shadow[0]);
		RCache.set_ca("m_shadow_array", 1, m_shadow[1]);
		RCache.set_ca("m_shadow_array", 2, m_shadow[2]);
		RCache.set_c("Ldynamic_color", L_clr.x,L_clr.y,L_clr.z,L_spec);
		RCache.set_c("m_sunmask", m_clouds_shadow);

		RCache.set_Geometry(g_combine);
		RCache.Render(D3DPT_TRIANGLELIST, Offset, 0, 3, 0, 1);
	
		if ((ps_r_sun_shafts>0))
		{
			PIX_EVENT(SUN_LIGHTING_VOLUMETRIC);
			phase_vol_accumulator();
			RCache.set_ColorWriteEnable();

			pv = (FVF::TL*)RCache.Vertex.Lock(3, g_combine->vb_stride, Offset);
			pv->set(-1.0, 1.0, 1.0, 1.0, vertex_color, 0.0, 0.0);
			pv++;
			pv->set(3.0, 1.0, 1.0, 1.0, vertex_color, 2.0, 0.0);
			pv++;
			pv->set(-1.0, -3.0, 1.0, 1.0, vertex_color, 0.0, 2.0);
			pv++;
			RCache.Vertex.Unlock(3, g_combine->vb_stride);

			RCache.set_Element(s_accum_direct_volumetric->E[0]);
			RCache.set_c("Ldynamic_dir", L_dir.x, L_dir.y, L_dir.z, 0);
			RCache.set_c("Ldynamic_color", L_clr.x, L_clr.y, L_clr.z, 0);
			RCache.set_Geometry(g_combine);
			RCache.Render(D3DPT_TRIANGLELIST, Offset, 0, 3, 0, 1);
		}
	}
}

void CRenderTarget::accum_direct_volumetric	(u32 sub_phase, const u32, const u32 i_offset, const Fmatrix &mShadow)
{
	PIX_EVENT(accum_direct_volumetric);

	if (!need_to_render_sunshafts())
		return;

	if((sub_phase != SE_SUN_NEAR) && (sub_phase != SE_SUN_FAR)) return;

	phase_vol_accumulator();
	RCache.set_ColorWriteEnable();

	u32 Offset;
	u32 C = color_rgba(255, 255, 255, 255);
	float _w = float(dwWidth);
	float _h = float(dwHeight);
	Fvector2 p0, p1;
	p0.set(.5f / _w, .5f / _h);
	p1.set((_w + .5f) / _w, (_h + .5f) / _h);
	float d_Z = EPS_S, d_W = 1.f;

	// Fill vertex buffer
	FVF::TL2uv* pv = (FVF::TL2uv*)RCache.Vertex.Lock(4, g_combine_2UV->vb_stride, Offset);
	pv->set(-1, -1, 0, d_W, C, 0, 1, 0, 0);	pv++;
	pv->set(-1, 1, d_Z, d_W, C, 0, 0, 0, 0); pv++;
	pv->set(1, -1, d_Z, d_W, C, 1, 1, 0, 0); pv++;
	pv->set(1, 1, d_Z, d_W, C, 1, 0, 0, 0);	pv++;
	RCache.Vertex.Unlock(4, g_combine_2UV->vb_stride);
	RCache.set_Geometry(g_combine_2UV);

	ref_selement Element = s_accum_direct_volumetric->E[0];

	//	Set correct depth surface
	//	It's slow. Make this when shader is created
	{
		const char* pszSMapName;
		BOOL b_HW_smap = RImplementation.o.HW_smap;
		BOOL b_HW_PCF = RImplementation.o.HW_smap_PCF;
		if(b_HW_smap) {
			if(b_HW_PCF) pszSMapName = r2_RT_smap_depth;
			else pszSMapName = r2_RT_smap_depth;
		}
		else pszSMapName = r2_RT_smap_surf;
		//s_smap
		STextureList* _T = &*Element->passes[0]->T;

		STextureList::iterator	_it = _T->begin();
		STextureList::iterator	_end = _T->end();

		for(; _it != _end; _it++) {
			std::pair<u32, ref_texture>& loader = *_it;
			u32			load_id = loader.first;
			//	Shadowmap texture always uses 0 texture unit
			if(load_id == 0) {
				//	Assign correct texture
				loader.second.create(pszSMapName);
			}
		}
	}

	// Perform lighting
	{
		light* fuckingsun = (light*)RImplementation.Lights.sun_adapted._get();

		Fvector L_clr;
		L_clr.set(fuckingsun->color.r, fuckingsun->color.g, fuckingsun->color.b);
		
		Fvector L_dir;
		L_clr.set(fuckingsun->color.r, fuckingsun->color.g, fuckingsun->color.b);
		Device.mView.transform_dir(L_dir, fuckingsun->direction);
		L_dir.normalize();

		RCache.set_Element(Element);
		RCache.set_CullMode(CULL_CCW);
		RCache.set_c("Ldynamic_dir", L_dir.x, L_dir.y, L_dir.z, 0);
		RCache.set_c("Ldynamic_color", L_clr.x, L_clr.y, L_clr.z, 0);
		RCache.set_c("m_shadow", mShadow);

		Fmatrix m_Texgen;
		m_Texgen.identity();

		RCache.xforms.set_W(m_Texgen);
		RCache.xforms.set_V(Device.mView);
		RCache.xforms.set_P(Device.mProject);
		u_compute_texgen_screen(m_Texgen);

		RCache.set_c ("m_texgen", m_Texgen);

		// nv-DBT
		float zMin = 0, zMax = ps_r2_sun_far;

		if(SE_SUN_NEAR == sub_phase) {
			zMax = ps_r2_sun_near;
		}

		RCache.set_c("volume_range", zMin, zMax, 0, 0);

		Fvector	center_pt = {};

		center_pt.mad(Device.vCameraPosition, Device.vCameraDirection, zMin);
		Device.mFullTransform.transform(center_pt);
		zMin = center_pt.z;

		center_pt.mad(Device.vCameraPosition, Device.vCameraDirection, zMax);
		Device.mFullTransform.transform(center_pt);
		zMax = center_pt.z;

		if(SE_SUN_NEAR == sub_phase) {
			RCache.set_ZFunc(D3DCMP_GREATER);
		}
		else {
			RCache.set_ZFunc(D3DCMP_ALWAYS);
		}

		RCache.Render(D3DPT_TRIANGLELIST, Offset, 0, 4, 0, 2);
	}
}
