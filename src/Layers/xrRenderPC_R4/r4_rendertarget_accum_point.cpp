#include "stdafx.h"
#include "CHudInitializer.h"

void CRenderTarget::accum_point		(GfxCmdList& Cmd, light* L)
{
	if (L == nullptr)
	{
		return;
	}
	PROF_EVENT("CRenderTarget::accum_point")
	phase_accumulator				(Cmd);
	RImplementation.stats.l_visible	++;

	ref_shader		shader	    = L->s_point;
	if (!shader)
	{
		shader			= s_accum_point;
	}

	CHudInitializer initalizer(Cmd, false, true);

	if(L->flags.bHudMode) 
	{
		initalizer.SetHudMode();
	}

	// Common
	Fvector		L_pos;
	float		L_spec;
	float		L_R					= L->range;
	//float		L_R					= L->range*.95f;
	Fvector		L_clr;				L_clr.set		(L->color.r,L->color.g,L->color.b);
	L_spec							= u_diffuse2s	(L_clr);
	Device.mView.transform_tiny		(L_pos,L->position);

	// Xforms
	Cmd.SetObjectTransform(L->m_xform);
	Cmd.SetView(Device.mView, Device.mProject);
	enable_scissor					(L);
	enable_dbt_bounds				(L);

	// *****************************	Mask by stencil		*************************************
	// *** similar to "Carmack's reverse", but assumes convex, non intersecting objects,
	// *** thus can cope without stencil clear with 127 lights
	// *** in practice, 'cause we "clear" it back to 0x1 it usually allows us to > 200 lights :)
	Cmd.SetPass				(s_accum_mask->E[SE_MASK_POINT]);			// masker

	// backfaces: if (1<=stencil && zfail)	stencil = light_id
	Cmd.SetCullMode(ERHI_CULLMODE::FRONT);
   	Cmd.SetStencil				(true,D3DCMP_LESSEQUAL,dwLightMarkerID,0x01,0xff,D3DSTENCILOP_KEEP,D3DSTENCILOP_KEEP,D3DSTENCILOP_REPLACE);
	draw_volume						(Cmd, L);

	// frontfaces: if (1<=stencil && zfail)	stencil = 0x1
	Cmd.SetCullMode(ERHI_CULLMODE::BACK);
	Cmd.SetStencil(true, D3DCMP_LESSEQUAL, 0x01, 0xff, 0xff, D3DSTENCILOP_KEEP, D3DSTENCILOP_KEEP, D3DSTENCILOP_REPLACE);
	draw_volume						(Cmd, L);

	// nv-stencil recompression
	if (RImplementation.o.nvstencil)		u_stencil_optimize(Cmd);

	// *****************************	Minimize overdraw	*************************************
	// Select shader (front or back-faces), *** back, if intersect near plane
	Cmd.SetColorWriteEnable				();
	Cmd.SetCullMode(ERHI_CULLMODE::FRONT);

	// 2D texgens 
	Fmatrix			m_Texgen;			u_compute_texgen_screen	(Cmd, m_Texgen	);
	Fmatrix			m_Texgen_J;			u_compute_texgen_jitter	(Cmd, m_Texgen_J	);

	// Draw volume with projective texgen
	{
		// Select shader
		u32		_id					= 0;
		if (L->flags.bShadow)		{
			bool	bFullSize			= (L->X.S.size == u32(RImplementation.o.smapsize));
			if (L->X.S.transluent)	_id	= SE_L_TRANSLUENT;
			else if		(bFullSize)	_id	= SE_L_FULLSIZE;
			else					_id	= SE_L_NORMAL;
		} else {
			_id						= SE_L_UNSHADOWED;
			//m_Shadow				= m_Lmap;
		}
		Cmd.SetPass				(shader->E[ _id ]	);

		// Constants
		Cmd.Constants.Write<CBLight>().Ldynamic_pos.set(L_pos.x, L_pos.y, L_pos.z, 1/(L_R*L_R));
		Cmd.Constants.Write<CBLight>().Ldynamic_color.set(L_clr.x, L_clr.y, L_clr.z, L_spec);
		Cmd.Constants.Write<CBLight>().Ldynamic_hud = (int)L->flags.bHudMode;
		Cmd.Constants.Write<CBPass>().m_texgen = m_Texgen;

		Cmd.SetCullMode(ERHI_CULLMODE::FRONT);
		// Render if (light_id <= stencil && z-pass)
		Cmd.SetStencil(true, D3DCMP_EQUAL, dwLightMarkerID, 0xff, 0x00);
		draw_volume(Cmd, L);
	}

	Cmd.SetScissor(0);

	increment_light_marker(Cmd);

	u_DBT_disable				();

	if(L->flags.bHudMode)
	{
		initalizer.SetDefaultMode();
	}
}
