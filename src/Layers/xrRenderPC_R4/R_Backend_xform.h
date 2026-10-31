#ifndef r_backend_xformH
#define r_backend_xformH
#pragma once
#include "RenderAPI.h"

class RENDER_API	R_xforms
{
public:
	Fmatrix			m_w;		// Basic	- world
	Fmatrix			m_v;		// Basic	- view
	Fmatrix			m_p;		// Basic	- projection
	Fmatrix			m_wv;		// Derived	- world2view
	Fmatrix			m_vp;		// Derived	- view2projection
	Fmatrix			m_wvp;		// Derived	- world2view2projection

	Fmatrix			m_w_old;	// Basic	- world old frame
	Fmatrix			m_v_old;	// Basic	- view old frame
	Fmatrix			m_p_old;	// Basic	- projection old frame
	Fmatrix			m_wv_old;	// Derived	- world2view old frame
	Fmatrix			m_vp_old;	// Derived	- view2projection old frame
	Fmatrix			m_wvp_old;	// Derived	- world2view2projection old frame

	R_xforms		();

	void			set_W		(const Fmatrix& m);
	void			set_V		(const Fmatrix& m);
	void			set_P		(const Fmatrix& m);

	void			set_W_old	(const Fmatrix& m);
	void			set_V_old	(const Fmatrix& m);
	void			set_P_old	(const Fmatrix& m);

	IC const Fmatrix&	get_W	() { return m_w; }
	IC const Fmatrix&	get_V	() { return m_v; }
	IC const Fmatrix&	get_P	() { return m_p; }

	IC const Fmatrix&	get_W_old () { return m_w_old; }
	IC const Fmatrix&	get_V_old () { return m_v_old; }
	IC const Fmatrix&	get_P_old () { return m_p_old; }

};
#endif
