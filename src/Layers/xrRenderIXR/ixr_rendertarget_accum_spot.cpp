#include "stdafx.h"
#include "du_cone.h"
#include "CHudInitializer.h"

Fvector3 ps_ssfx_volumetric = { .0f, 1.0f, 3.0f }; // Force Volumetric, Vol Intensity, Vol Quality

void CRenderTarget::accum_volumetric_lv(GfxCmdList& Cmd, light* L)
{
	//LV: Point lights will be fixed later... I hope
	if (L == nullptr)
		return;
	
	if (!L->flags.bVolumetric || (ps_ssfx_volumetric.x > 0 && ps_ssfx_volumetric.y > 0))
	{
		return;
	}

	GPU_EVENT(accum_volumetric_lv);

	phase_vol_accumulator(Cmd);

	//Set XFORMs, we gonna transform our geometry in VS
	L->xform_calc();
	Cmd.SetObjectTransform(L->m_xform);
	Cmd.SetView(RFrame.mView, RFrame.mProject);

	//Shadow xform (+texture adjustment matrix)
	Fmatrix m_Shadow, m_Lmap;
	{
		float smapsize = float(RImplementation.o.smapsize);
		float fTexelOffs = (.5f / smapsize);
		float view_dim = float(L->X.S.size - 2) / smapsize;
		float view_sx = float(L->X.S.posX + 1) / smapsize;
		float view_sy = float(L->X.S.posY + 1) / smapsize;
		float fRange = float(1.f) * ps_r2_ls_depth_scale;
		float fBias = ps_r2_ls_depth_bias;

		Fmatrix m_TexelAdjust = {
			view_dim / 2.f, 0.0f, 0.0f, 0.0f,
			0.0f, -view_dim / 2.f, 0.0f, 0.0f,
			0.0f, 0.0f, fRange, 0.0f,
			view_dim / 2.f + view_sx + fTexelOffs, view_dim / 2.f + view_sy + fTexelOffs, fBias, 1.0f
		};

		//Compute xforms
		Fmatrix xf_world;
		xf_world.invert(RFrame.mView);
		Fmatrix xf_view = L->X.S.view;
		Fmatrix xf_project;
		xf_project.mul(m_TexelAdjust, L->X.S.project);

		m_Shadow.set(xf_view);
		m_Shadow.mulA_44(xf_project);
	}

	//Light direction (spot), color, and position. All in world space
	Fvector L_dir, L_clr, L_pos;
	L_clr.set(L->color.r, L->color.g, L->color.b);
	L_clr.mul(std::max(ps_ssfx_volumetric.y, L->m_volumetric_intensity));
	L_clr.mul(L->m_volumetric_distance);
	L_clr.mul(L->get_LOD());

	L_pos.set(L->position);
	L_dir.set(L->direction);
	L_dir.normalize();

	//Attenuation
	float att_R = L->m_volumetric_distance * L->range * .95f;
	float att_factor = 1.f / (att_R * att_R);

	//Set the shader
	Cmd.SetPass(s_combine->E[3]);
	Cmd.SetCullMode(ERHI_CULLMODE::FRONT);
	Cmd.SetStencil(false);

	Cmd.SetColorWriteEnable();
	//Set constants
	Cmd.Constants.Write<CBLight>().Ldynamic_pos.set(L_pos.x, L_pos.y, L_pos.z, att_factor);
	Cmd.Constants.Write<CBLight>().Ldynamic_dir.set(L_dir.x, L_dir.y, L_dir.z, -cos(L->cone * 0.5));
	Cmd.Constants.Write<CBLight>().Ldynamic_color.set(L_clr.x, L_clr.y, L_clr.z, L->get_LOD());
	Cmd.Constants.Write<CBLight>().Ldynamic_smap = float(L->vis.smap_ID % SmapPages);

	Cmd.Constants.Write<CBLight>().m_shadow = m_Shadow;

	//Render, no IB/VB - geometry in VS
	draw_volume(Cmd, L);
}
