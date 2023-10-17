#include "stdafx.h"
#pragma hdrstop

#include "r_backend_xform.h"

extern float ps_r4_jitter_scale_x;
extern float ps_r4_jitter_scale_y;

static Fmatrix xform_apply_jitter(const Fmatrix& m_p, float jitter_x, float jitter_y)
{
	Fmatrix jitter_p = m_p;
	jitter_p._31 += ps_r4_jitter_scale_x * jitter_x;
	jitter_p._32 += ps_r4_jitter_scale_y * jitter_y;
	return jitter_p;
}

void	R_xforms::set_W			(const Fmatrix& m)
{
	m_w.set(m);
	auto jitter_p = xform_apply_jitter(m_p, m_jitter_x, m_jitter_y);

	m_wv.mul_43(m_v, m_w);
	m_vp.mul(jitter_p, m_v);
	m_wvp.mul(jitter_p, m_wv);
	m_vp_unjittered.mul(m_p, m_v);
	m_wvp_unjittered.mul(m_p, m_wv);

	if (c_w)				RCache.set_c(c_w,	m_w);
	if (c_vp)				RCache.set_c(c_vp,  m_vp);
	if (c_wv)				RCache.set_c(c_wv,  m_wv);
	if (c_wvp)				RCache.set_c(c_wvp, m_wvp);
	if (c_vp_unjittered)	RCache.set_c(c_vp_unjittered, m_vp_unjittered);
	if (c_wvp_unjittered)	RCache.set_c(c_wvp_unjittered, m_wvp_unjittered);
	m_bInvWValid	= false;

	if (!m_bPrev)
		RCache.set_xform(D3DTS_WORLD,m);
}
void	R_xforms::set_V			(const Fmatrix& m)
{
	m_v.set(m);
	auto jitter_p = xform_apply_jitter(m_p, m_jitter_x, m_jitter_y);

	m_wv.mul_43(m_v, m_w);
	m_vp.mul(jitter_p, m_v);
	m_wvp.mul(jitter_p, m_wv);
	m_vp_unjittered.mul(m_p, m_v);
	m_wvp_unjittered.mul(m_p, m_wv);

	if (c_v)				RCache.set_c(c_v,	m_v);
	if (c_vp)				RCache.set_c(c_vp,	m_vp);
	if (c_wv)				RCache.set_c(c_wv,	m_wv);
	if (c_wvp)				RCache.set_c(c_wvp,	m_wvp);
	if (c_vp_unjittered)	RCache.set_c(c_vp_unjittered, m_vp_unjittered);
	if (c_wvp_unjittered)	RCache.set_c(c_wvp_unjittered, m_wvp_unjittered);

	if (!m_bPrev)
		RCache.set_xform(D3DTS_VIEW, m);
}
void	R_xforms::set_P			(const Fmatrix& m)
{
	m_p.set(m);
	auto jitter_p = xform_apply_jitter(m_p, m_jitter_x, m_jitter_y);

	m_vp.mul(jitter_p, m_v);
	m_wvp.mul(jitter_p, m_wv);
	m_vp_unjittered.mul(m_p, m_v);
	m_wvp_unjittered.mul(m_p, m_wv);
	
	if (c_p)				RCache.set_c(c_p,	jitter_p);
	if (c_vp)				RCache.set_c(c_vp,	m_vp);
	if (c_wvp)				RCache.set_c(c_wvp,	m_wvp);
	if (c_vp_unjittered)	RCache.set_c(c_vp_unjittered, m_vp_unjittered);
	if (c_wvp_unjittered)	RCache.set_c(c_wvp_unjittered, m_wvp_unjittered);
	apply_invp();

	if (!m_bPrev)
		RCache.set_xform(D3DTS_PROJECTION,m);	
}

void	R_xforms::apply_invp()
{
	auto jitter_p = xform_apply_jitter(m_p, m_jitter_x, m_jitter_y);
	m_invp.invert_b(jitter_p);
	m_invp_unjittered.invert_b(m_p);
	if (c_invp) RCache.set_c(c_invp, m_invp);
	if (c_invp_unjittered) RCache.set_c(c_invp_unjittered, m_invp_unjittered);
}

void	R_xforms::unmap()
{
	c_w					= nullptr;
	c_v					= nullptr;
	c_p					= nullptr;
	c_invp				= nullptr;
	c_wv				= nullptr;
	c_vp				= nullptr;
	c_wvp				= nullptr;
	c_invp_unjittered	= nullptr;
	c_vp_unjittered		= nullptr;
	c_wvp_unjittered	= nullptr;
}

void R_xforms::set_jitter(float JitterX, float JitterY)
{
	m_jitter_x = JitterX / RCache.get_width();
	m_jitter_y = JitterY / RCache.get_height();
	auto jitter_p = xform_apply_jitter(m_p, m_jitter_x, m_jitter_y);

	m_vp.mul(jitter_p, m_v);
	m_wvp.mul(jitter_p, m_wv);

	if (c_invp_unjittered)	RCache.set_c(c_invp_unjittered, jitter_p);
	if (c_invp)				RCache.set_c(c_invp, jitter_p);
	if (c_p)				RCache.set_c(c_p, jitter_p);
	if (c_vp)				RCache.set_c(c_vp, m_vp);
	if (c_wvp)				RCache.set_c(c_wvp, m_wvp);
}

R_xforms::R_xforms				(bool is_prev)
{
	unmap			();
	m_w.identity	();
	m_invw.identity	();
	m_v.identity	();
	m_invv.identity	();
	m_p.identity	();
	m_invp.identity ();
	m_wv.identity	();
	m_vp.identity	();
	m_wvp.identity	();
	m_invp_unjittered.identity();
	m_vp_unjittered.identity();
	m_wvp_unjittered.identity();
	m_bPrev = is_prev;
	m_bInvWValid = true;
}
