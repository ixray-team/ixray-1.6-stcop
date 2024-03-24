#include "stdafx.h"

void CRenderTarget::accum_reflected		(light* L)
{
	phase_accumulator				();
	RImplementation.stats.l_visible	++;

	// *** assume accumulator setted up ***
	// *****************************	Mask by stencil		*************************************
	ref_shader		shader				= s_accum_reflected;

	BOOL	bIntersect			= FALSE; //enable_scissor(L);
	L->xform_calc					();
	g_rbackend->set_xform_world		(L->m_xform			);
	g_rbackend->set_xform_view		(Device.mView		);
	g_rbackend->set_xform_project	(Device.mProject	);
	bIntersect					= enable_scissor	(L);
	enable_dbt_bounds			(L);

	// *****************************	Minimize overdraw	*************************************
	// Select shader (front or back-faces), *** back, if intersect near plane
	g_rbackend->set_ColorWriteEnable				();
	if (bIntersect)	g_rbackend->set_CullMode		(CULL_CW);		// back
	else			g_rbackend->set_CullMode		(CULL_CCW);		// front

	// 2D texgen (texture adjustment matrix)
	Fmatrix			m_Texgen;
	{
		float	_w						= g_rbackend->get_width();
		float	_h						= g_rbackend->get_height();
		float	o_w						= (.5f / _w);
		float	o_h						= (.5f / _h);
		Fmatrix			m_TexelAdjust		= 
		{
			0.5f,				0.0f,				0.0f,			0.0f,
			0.0f,				-0.5f,				0.0f,			0.0f,
			0.0f,				0.0f,				1.0f,			0.0f,
			0.5f + o_w,			0.5f + o_h,			0.0f,			1.0f
		};
		m_Texgen.mul	(m_TexelAdjust, g_rbackend->xforms.m_wvp);
	}

	// Common constants
	Fvector		L_dir,L_clr,L_pos;	float L_spec;
	L_clr.set					(L->color.r,L->color.g,L->color.b);
	L_spec						= u_diffuse2s	(L_clr);
	Device.mView.transform_tiny	(L_pos,L->position);
	Device.mView.transform_dir	(L_dir,L->direction);
	L_dir.normalize				();

	{
		// Lighting
		g_rbackend->set_Shader			(shader);

		// Constants
		g_rbackend->set_c				("Ldynamic_pos",	L_pos.x,L_pos.y,L_pos.z,1/(L->range*L->range));
		g_rbackend->set_c				("Ldynamic_color",	L_clr.x,L_clr.y,L_clr.z,L_spec);
		g_rbackend->set_c				("direction",		L_dir.x,L_dir.y,L_dir.z,0);
		g_rbackend->set_c				("m_texgen",		m_Texgen);
		g_rbackend->set_Stencil(TRUE, D3DCMP_LESSEQUAL, 0x01, 0xff, 0x00);
		draw_volume(L);
	}

	// blend-copy
	if (!RImplementation.o.fp16_blend)	{
		u_setrt(rt_Accumulator, nullptr, nullptr, RDepth);
		g_rbackend->set_Element	(s_accum_mask->E[SE_MASK_ACCUM_VOL]	);
		g_rbackend->set_c				("m_texgen",		m_Texgen);
		// per pixel
		g_rbackend->set_Stencil(TRUE, D3DCMP_LESSEQUAL, 0x01, 0xff, 0x00);
		draw_volume(L);
	}

	// 
	u_DBT_disable	();
}
