#include "stdafx.h"
#pragma hdrstop

#include "r_backend_xform.h"

extern float ps_r4_jitter_scale_x;
extern float ps_r4_jitter_scale_y;

static Fmatrix xform_apply_jitter(const Fmatrix& m_p, float jitter_x, float jitter_y)
{
#if 1
	Fmatrix jitter_p = m_p;
	jitter_p._31 += ps_r4_jitter_scale_x * jitter_x;
	jitter_p._32 += ps_r4_jitter_scale_y * jitter_y;
#else
	Fmatrix jitter = {
		1.0f, 0.0f, 0.0f, 0.0f,
		0.0f, 1.0f, 0.0f, 0.0f,
		0.0f, 0.0f, 1.0f, 0.0f,
		ps_r4_jitter_scale_x * jitter_x, ps_r4_jitter_scale_y* jitter_y, 0.0f, 1.0f
	};

	Fmatrix jitter_p = m_p;
	jitter_p.mulA_44(jitter);
#endif
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

	if (c_w)		RCache.set_c(c_w,	m_w);
	if (c_vp)		RCache.set_c(c_vp,  m_vp);
	if (c_wv)		RCache.set_c(c_wv,  m_wv);
	if (c_wvp)		RCache.set_c(c_wvp, m_wvp);
	if (c_vp_unjittered)	RCache.set_c(c_vp_unjittered, m_vp_unjittered);
	if (c_wvp_unjittered)RCache.set_c(c_wvp_unjittered, m_wvp_unjittered);
	m_bInvWValid	= false;
	if (c_invw)		apply_invw();

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

	if (c_v)		RCache.set_c(c_v,	m_v);
	if (c_vp)		RCache.set_c(c_vp,	m_vp);
	if (c_wv)		RCache.set_c(c_wv,	m_wv);
	if (c_wvp)		RCache.set_c(c_wvp,	m_wvp);
	if (c_vp_unjittered)	RCache.set_c(c_vp_unjittered, m_vp_unjittered);
	if (c_wvp_unjittered)RCache.set_c(c_wvp_unjittered, m_wvp_unjittered);

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

	if (c_p)		RCache.set_c(c_p,	m_p);
	if (c_vp)		RCache.set_c(c_vp,	m_vp);
	if (c_wvp)		RCache.set_c(c_wvp,	m_wvp);
	if (c_vp_unjittered)	RCache.set_c(c_vp_unjittered, m_vp_unjittered);
	if (c_wvp_unjittered)RCache.set_c(c_wvp_unjittered, m_wvp_unjittered);

	if (!m_bPrev)
		RCache.set_xform(D3DTS_PROJECTION,m);	
}

void	R_xforms::apply_invw()
{
	VERIFY(c_invw);

	if (!m_bInvWValid)
	{
		m_invw.invert_b(m_w);
		m_bInvWValid = true;
	}

	RCache.set_c( c_invw, m_invw);
}

void	R_xforms::unmap			()
{
	c_w			= NULL;
	c_invw		= NULL;
	c_v			= NULL;
	c_p			= NULL;
	c_wv		= NULL;
	c_vp		= NULL;
	c_wvp		= NULL;
	c_vp_unjittered	= NULL;
	c_wvp_unjittered	= NULL;
}

void R_xforms::set_Jitter(float JitterX, float JitterY)
{
	m_jitter_x = JitterX / RCache.get_width();
	m_jitter_y = JitterY / RCache.get_height();
	auto jitter_p = xform_apply_jitter(m_p, m_jitter_x, m_jitter_y);

	m_vp.mul(jitter_p, m_v);
	m_wvp.mul(jitter_p, m_wv);
	if (c_p)		RCache.set_c(c_p, m_p);
	if (c_vp)		RCache.set_c(c_vp, m_vp);
	if (c_wvp)		RCache.set_c(c_wvp, m_wvp);
}

R_xforms::R_xforms				(bool is_prev)
{
	unmap			();
	m_w.identity	();
	m_invw.identity	();
	m_v.identity	();
	m_p.identity	();
	m_wv.identity	();
	m_vp.identity	();
	m_wvp.identity	();
	m_bPrev = is_prev;
	m_bInvWValid = true;
}
