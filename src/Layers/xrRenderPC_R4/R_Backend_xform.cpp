#include "stdafx.h"


#include "R_Backend_xform.h"

void	R_xforms::set_W			(const Fmatrix& m)
{
	m_w.set			(m);
	m_wv.mul_43		(m_v,m_w);
	m_wvp.mul		(m_p,m_wv);
	m_w_old.set(Fidentity);
	m_wv_old.set(m_v_old);
	m_wvp_old.set(m_vp_old);
	RCache.Constants.UpdateObject(*this);
	RCache.set_xform(D3DTS_WORLD,m);
}
void	R_xforms::set_V			(const Fmatrix& m)
{
	m_v.set			(m);
	m_wv.mul_43		(m_v,m_w);
	m_vp.mul		(m_p,m_v);
	m_wvp.mul		(m_p,m_wv);
	RCache.set_xform(D3DTS_VIEW,m);
	RCache.Constants.UpdateView(*this);
}
void	R_xforms::set_P			(const Fmatrix& m)
{
	m_p.set			(m);
	m_vp.mul		(m_p,m_v);
	m_wvp.mul		(m_p,m_wv);
	// always setup projection - D3D relies on it to work correctly :(
	RCache.set_xform(D3DTS_PROJECTION,m);
	RCache.Constants.UpdateView(*this);
}

void	R_xforms::set_W_old			(const Fmatrix& m)
{
	m_w_old.set			(m);
	m_wv_old.mul_43		(m_v_old,m_w_old);
	m_wvp_old.mul		(m_p_old,m_wv_old);
	RCache.Constants.Write<CBObject>().m_WVP_old = m_wvp_old;
}
void	R_xforms::set_V_old			(const Fmatrix& m)
{
	m_v_old.set			(m);
	m_wv_old.mul_43		(m_v_old,m_w_old);
	m_vp_old.mul		(m_p_old,m_v_old);
	m_wvp_old.mul		(m_p_old,m_wv_old);
	RCache.Constants.UpdateView(*this);
}
void	R_xforms::set_P_old			(const Fmatrix& m)
{
	m_p_old.set			(m);
	m_vp_old.mul		(m_p_old,m_v_old);
	m_wvp_old.mul		(m_p_old,m_wv_old);
	RCache.Constants.UpdateView(*this);
}

R_xforms::R_xforms ()
{
	m_w.identity	();
	m_v.identity	();
	m_p.identity	();
	m_wv.identity	();
	m_vp.identity	();
	m_wvp.identity	();

	m_w_old.identity	();
	m_v_old.identity	();
	m_p_old.identity	();
	m_wv_old.identity	();
	m_vp_old.identity	();
	m_wvp_old.identity	();
}
