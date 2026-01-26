#ifndef r_backend_xformH
#define r_backend_xformH
#pragma once

class ECORE_API	R_xforms
{
public:
	Fmatrix			m_invw;		// derived	- world2local, cached
	Fmatrix			m_invv;

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

	Fmatrix			m_env_view;
	Fmatrix			m_env_view_inv;
	Fmatrix			m_env_view_real;
	
	RHIShaderConstant*		c_invw;
	RHIShaderConstant*		c_invv;

	RHIShaderConstant*		c_w;
	RHIShaderConstant*		c_v;
	RHIShaderConstant*		c_p;
	RHIShaderConstant*		c_wv;
	RHIShaderConstant*		c_vp;
	RHIShaderConstant*		c_wvp;

	RHIShaderConstant*		c_w_old;
	RHIShaderConstant*		c_v_old;
	RHIShaderConstant*		c_p_old;
	RHIShaderConstant*		c_wv_old;
	RHIShaderConstant*		c_vp_old;
	RHIShaderConstant*		c_wvp_old;

	RHIShaderConstant*		c_env_view;
	RHIShaderConstant*		c_env_view_inv;
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

	void			set_env_view	(const Fmatrix& m);

	IC const Fmatrix&	get_W	() { return m_w; }
	IC const Fmatrix&	get_V	() { return m_v; }
	IC const Fmatrix&	get_P	() { return m_p; }

	IC const Fmatrix&	get_W_old () { return m_w_old; }
	IC const Fmatrix&	get_V_old () { return m_v_old; }
	IC const Fmatrix&	get_P_old () { return m_p_old; }

	IC void			set_c_invw	(RHIShaderConstant* C);
	IC void			set_c_invv	(RHIShaderConstant* C);

	IC void			set_c_w		(RHIShaderConstant* C);
	IC void			set_c_v		(RHIShaderConstant* C);
	IC void			set_c_p		(RHIShaderConstant* C);
	IC void			set_c_wv	(RHIShaderConstant* C);
	IC void			set_c_vp	(RHIShaderConstant* C);
	IC void			set_c_wvp	(RHIShaderConstant* C);

	IC void			set_c_w_old (RHIShaderConstant* C);
	IC void			set_c_v_old (RHIShaderConstant* C);
	IC void			set_c_p_old (RHIShaderConstant* C);
	IC void			set_c_wv_old(RHIShaderConstant* C);
	IC void			set_c_vp_old(RHIShaderConstant* C);
	IC void			set_c_wvp_old(RHIShaderConstant* C);

	IC void			set_c_env_view(RHIShaderConstant* C);
	IC void			set_c_env_view_inv(RHIShaderConstant* C);

private:
	void			apply_invw	();
	void			apply_invv	();
};
#endif
