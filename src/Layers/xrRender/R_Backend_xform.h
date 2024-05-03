#ifndef r_backend_xformH
#define r_backend_xformH
#pragma once

class ECORE_API	R_xforms
{
public:
	Fmatrix			m_invw;		// derived	- world2local, cached

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
	
	R_constant*		c_invw;

	R_constant*		c_w;
	R_constant*		c_v;
	R_constant*		c_p;
	R_constant*		c_wv;
	R_constant*		c_vp;
	R_constant*		c_wvp;

	R_constant*		c_w_old;
	R_constant*		c_v_old;
	R_constant*		c_p_old;
	R_constant*		c_wv_old;
	R_constant*		c_vp_old;
	R_constant*		c_wvp_old;
private:
	bool			m_bInvWValid;
public:
	R_xforms		();
	void			unmap		();

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

	IC void			set_c_invw	(R_constant* C);

	IC void			set_c_w		(R_constant* C);
	IC void			set_c_v		(R_constant* C);
	IC void			set_c_p		(R_constant* C);
	IC void			set_c_wv	(R_constant* C);
	IC void			set_c_vp	(R_constant* C);
	IC void			set_c_wvp	(R_constant* C);

	IC void			set_c_w_old (R_constant* C);
	IC void			set_c_v_old (R_constant* C);
	IC void			set_c_p_old (R_constant* C);
	IC void			set_c_wv_old(R_constant* C);
	IC void			set_c_vp_old(R_constant* C);
	IC void			set_c_wvp_old(R_constant* C);

private:
	void			apply_invw	();
};
#endif
