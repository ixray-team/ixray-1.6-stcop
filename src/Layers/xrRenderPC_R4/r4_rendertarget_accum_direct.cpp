#include "stdafx.h"
#include "../../xrEngine/IGame_Persistent.h"
#include "../../xrEngine/Environment.h"

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

void CRenderTarget::accum_direct_cascade(GfxCmdList& Cmd)
{
	light* fuckingsun = (light*)RImplementation.Lights.sun_adapted._get();

	Fvector L_dir, L_clr;
	float L_spec;
	L_clr.set(fuckingsun->color.r, fuckingsun->color.g, fuckingsun->color.b);
	L_spec = u_diffuse2s(L_clr);
	Device.mView.transform_dir(L_dir, fuckingsun->direction);
	L_dir.normalize();

	//inv_view
	Fmatrix xf_invview;
	xf_invview.invert(Device.mView);

	// clouds xform
	Fmatrix m_clouds_shadow;
	{
		static float w_shift = 0.0f;

		Fvector normal;
		normal.setHP(g_pGamePersistent->Environment().CurrentEnv->wind_direction, 0);
		w_shift += 0.003f * Device.fTimeDelta;

		Fvector position;
		position.set(0, 0, 0);

		Fmatrix m_xform;
		m_xform.build_camera_dir(position, fuckingsun->direction, normal);

		Fvector localnormal;
		m_xform.transform_dir(localnormal, normal);
		localnormal.normalize();

		m_clouds_shadow.mul(m_xform, xf_invview);
		m_xform.scale(0.002f, 0.002f, 1.f);
		m_clouds_shadow.mulA_44(m_xform);
		m_xform.translate(localnormal.mul(w_shift));
		m_clouds_shadow.mulA_44(m_xform);
	}

	if (g_SunContactSteps > 0)
	{
		GPU_EVENT(sun_contact_shadows);
		DrawSQ(Cmd, SunContactShader, SunContactTarget, 0, [&]
		{
			CBLight& Light = Cmd.Constants.Write<CBLight>();
			Light.Ldynamic_dir.set(L_dir.x, L_dir.y, L_dir.z, 0.0f);
			Light.sun_contact_ray.set(float(g_SunContactSteps), g_SunContactLength, g_SunContactStart, 0.0f);
			Light.sun_contact_depth.set(g_SunContactThickness, g_SunContactThicknessScale, g_SunContactBias, g_SunContactBiasScale);
			Light.sun_contact_fade.set(g_SunContactFar, 1.0f / g_SunContactFade, 0.0f, 0.0f);
		});
	}
	else
	{
		const float Lit[4] = { 1.0f, 1.0f, 1.0f, 1.0f };
		Cmd.ClearTarget(SunContactTarget->pRT, Lit);
	}

	//Render the AO and view-z into new rendertarget
	phase_accumulator(Cmd);

	DrawPassSQ(Cmd, s_accum_direct, 0, [&]
	{
		Cmd.Constants.Write<CBLight>().Ldynamic_dir.set(L_dir.x, L_dir.y, L_dir.z, 0.0);
		Cmd.Constants.Write<CBLight>().Ldynamic_color.set(L_clr.x, L_clr.y, L_clr.z, L_spec);
		Cmd.Constants.Write<CBLight>().m_sunmask = m_clouds_shadow;
	});

	if (ps_r_sun_shafts > 0)
	{
		accum_direct_volumetric(Cmd);
	}	
}

void CRenderTarget::accum_direct_volumetric(GfxCmdList& Cmd)
{
	GPU_EVENT(accum_direct_volumetric);

	if (!need_to_render_sunshafts())
		return;

	light* fuckingsun = (light*)RImplementation.Lights.sun_adapted._get();

	Fvector L_clr;
	L_clr.set(fuckingsun->color.r, fuckingsun->color.g, fuckingsun->color.b);

	Fvector L_dir;
	L_clr.set(fuckingsun->color.r, fuckingsun->color.g, fuckingsun->color.b);

	Device.mView.transform_dir(L_dir, fuckingsun->direction);
	L_dir.normalize();

	phase_vol_accumulator(Cmd);
	Cmd.SetColorWriteEnable();

	DrawPassSQ(Cmd, s_accum_direct_volumetric, 0, [&]
	{
		Cmd.Constants.Write<CBLight>().Ldynamic_dir.set(L_dir.x, L_dir.y, L_dir.z, 0);
		Cmd.Constants.Write<CBLight>().Ldynamic_color.set(L_clr.x, L_clr.y, L_clr.z, 0);
	});
}
