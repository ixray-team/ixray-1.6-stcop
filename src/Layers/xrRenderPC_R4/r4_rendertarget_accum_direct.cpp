#include "stdafx.h"
#include "../../xrEngine/IGame_Persistent.h"
#include "../../xrEngine/Environment.h"

//////////////////////////////////////////////////////////////////////////
// tables to calculate view-frustum bounds in world space
// note: D3D uses [0..1] range for Z
static Fvector3		corners[8] = {
	{ -1, -1,  0.7 },	{ -1, -1, +1},
	{ -1, +1, +1 },		{ -1, +1,  0.7},
	{ +1, +1, +1 },		{ +1, +1,  0.7},
	{ +1, -1, +1 },		{ +1, -1,  0.7}
};
static u16			facetable[16][3] = {
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

void CRenderTarget::accum_direct_cascade()
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


	u32 Offset = 0;
	constexpr u32 vertex_color = color_rgba(0, 0, 0, 255);

	//Render the AO and view-z into new rendertarget
	phase_accumulator();

	FVF::TL* pv = (FVF::TL*)RCache.Vertex.Lock(3, g_combine->vb_stride, Offset);
	pv->set(-1.0, 1.0, 1.0, 1.0, vertex_color, 0.0, 0.0);
	pv++;
	pv->set(3.0, 1.0, 1.0, 1.0, vertex_color, 2.0, 0.0);
	pv++;
	pv->set(-1.0, -3.0, 1.0, 1.0, vertex_color, 0.0, 2.0);
	pv++;
	RCache.Vertex.Unlock(3, g_combine->vb_stride);

	RCache.set_Element(s_accum_direct->E[0]);

	RCache.set_c("Ldynamic_dir", L_dir.x, L_dir.y, L_dir.z, 0.0);
	RCache.set_c("Ldynamic_color", L_clr.x, L_clr.y, L_clr.z, L_spec);
	RCache.set_c("m_sunmask", m_clouds_shadow);

	RCache.set_Geometry(g_combine);
	RCache.Render(D3DPT_TRIANGLELIST, Offset, 0, 3, 0, 1);

	if (ps_r_sun_shafts > 0)
		accum_direct_volumetric();
}

void CRenderTarget::accum_direct_volumetric()
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

	phase_vol_accumulator();
	RCache.set_ColorWriteEnable();

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

	RCache.set_Element(s_accum_direct_volumetric->E[0]);

	RCache.set_c("Ldynamic_dir", L_dir.x, L_dir.y, L_dir.z, 0);
	RCache.set_c("Ldynamic_color", L_clr.x, L_clr.y, L_clr.z, 0);

	RCache.set_Geometry(g_combine);
	RCache.Render(D3DPT_TRIANGLELIST, Offset, 0, 3, 0, 1);
}