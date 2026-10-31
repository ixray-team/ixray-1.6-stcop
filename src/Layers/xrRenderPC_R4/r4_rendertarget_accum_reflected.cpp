#include "stdafx.h"

void CRenderTarget::accum_reflected		(GfxCmdList& Cmd, light* L)
{
	phase_accumulator				(Cmd);
	RImplementation.stats.l_visible	++;

	// *** assume accumulator setted up ***
	// *****************************	Mask by stencil		*************************************
	ref_shader		shader				= s_accum_reflected;

	bool	bIntersect			= false; //enable_scissor(L);
	Cmd.SetObjectTransform(L->m_xform);
	Cmd.SetView(Device.mView, Device.mProject);
	bIntersect					= enable_scissor	(L);
	enable_dbt_bounds			(L);

	// *****************************	Minimize overdraw	*************************************
	// Select shader (front or back-faces), *** back, if intersect near plane
	Cmd.SetColorWriteEnable				();
	Cmd.SetCullMode(bIntersect ? ERHI_CULLMODE::FRONT : ERHI_CULLMODE::BACK);

	// 2D texgen (texture adjustment matrix)
	Fmatrix			m_Texgen;
	{
		float	_w						= Cmd.GetWidth();
		float	_h						= Cmd.GetHeight();
		float	o_w						= (.5f / _w);
		float	o_h						= (.5f / _h);
		Fmatrix			m_TexelAdjust		= 
		{
			0.5f,				0.0f,				0.0f,			0.0f,
			0.0f,				-0.5f,				0.0f,			0.0f,
			0.0f,				0.0f,				1.0f,			0.0f,
			0.5f + o_w,			0.5f + o_h,			0.0f,			1.0f
		};
		m_Texgen.mul	(m_TexelAdjust,Cmd.GetWorldViewProject());
	}

	// Common constants
	Fvector		L_clr,L_pos;	float L_spec;
	L_clr.set					(L->color.r,L->color.g,L->color.b);
	L_spec						= u_diffuse2s	(L_clr);
	Device.mView.transform_tiny	(L_pos,L->position);

	{
		// Lighting
		Cmd.SetPass			(shader);

		// Constants
		Cmd.Constants.Write<CBLight>().Ldynamic_pos.set(L_pos.x, L_pos.y, L_pos.z, 1/(L->range*L->range));
		Cmd.Constants.Write<CBLight>().Ldynamic_color.set(L_clr.x, L_clr.y, L_clr.z, L_spec);
		Cmd.Constants.Write<CBPass>().m_texgen = m_Texgen;
		Cmd.SetStencil(true, D3DCMP_LESSEQUAL, 0x01, 0xff, 0x00);
		draw_volume(Cmd, L);
	}

	// 
	u_DBT_disable	();
}
