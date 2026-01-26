#ifndef R_BACKEND_RUNTIMEH
#define R_BACKEND_RUNTIMEH
#pragma once

#include "SH_Texture.h"
#include "SH_Matrix.h"
#include "SH_Constant.h"
#include "SH_RT.h"

#ifdef USE_DX11
#include "../xrRenderDX10/dx10R_Backend_Runtime.h"
#include "../xrRenderDX10/StateManager/dx10State.h"
#else //USE_DX11
#include "../xrRenderDX9/dx9R_Backend_Runtime.h"
#include "R_Backend.h"
#endif

IC void		R_xforms::set_c_w			(RHIShaderConstant* C)		{	c_w		= C;	RCache.set_c(C,m_w);	};
IC void		R_xforms::set_c_invw		(RHIShaderConstant* C)		{	c_invw	= C;	apply_invw();			};
IC void		R_xforms::set_c_invv		(RHIShaderConstant* C)		{	c_invv	= C;	apply_invv();			};
IC void		R_xforms::set_c_v			(RHIShaderConstant* C)		{	c_v		= C;	RCache.set_c(C,m_v);	};
IC void		R_xforms::set_c_p			(RHIShaderConstant* C)		{	c_p		= C;	RCache.set_c(C,m_p);	};
IC void		R_xforms::set_c_wv			(RHIShaderConstant* C)		{	c_wv	= C;	RCache.set_c(C,m_wv);	};
IC void		R_xforms::set_c_vp			(RHIShaderConstant* C)		{	c_vp	= C;	RCache.set_c(C,m_vp);	};
IC void		R_xforms::set_c_wvp			(RHIShaderConstant* C)		{	c_wvp	= C;	RCache.set_c(C,m_wvp);	};

IC void		R_xforms::set_c_w_old		(RHIShaderConstant* C)		{	c_w_old = C;	RCache.set_c(C,m_w_old);	};
IC void		R_xforms::set_c_v_old		(RHIShaderConstant* C)		{	c_v_old = C;	RCache.set_c(C,m_v_old);	};
IC void		R_xforms::set_c_p_old		(RHIShaderConstant* C)		{	c_p_old = C;	RCache.set_c(C,m_p_old);	};
IC void		R_xforms::set_c_wv_old		(RHIShaderConstant* C)		{	c_wv_old = C;	RCache.set_c(C,m_wv_old);	};
IC void		R_xforms::set_c_vp_old		(RHIShaderConstant* C)		{	c_vp_old = C;	RCache.set_c(C,m_vp_old);	};
IC void		R_xforms::set_c_wvp_old		(RHIShaderConstant* C)		{	c_wvp_old = C;	RCache.set_c(C,m_wvp_old);	};

IC void	 R_xforms::set_c_env_view(RHIShaderConstant* C) { c_env_view = C; RCache.set_c(C, m_env_view); };
IC void	 R_xforms::set_c_env_view_inv(RHIShaderConstant* C) { c_env_view_inv = C; RCache.set_c(C, m_env_view_inv); };

IC	void	CBackend::set_xform_world	(const Fmatrix& M_)
{ 
	xforms.set_W(M_);	
}
IC	void	CBackend::set_xform_view	(const Fmatrix& M_)					
{ 
	xforms.set_V(M_);	
}
IC	void	CBackend::set_xform_project	(const Fmatrix& M_)
{ 
	xforms.set_P(M_);	
}

IC	void	CBackend::set_xform_world_old	(const Fmatrix& M_)
{
	xforms.set_W_old(M_);
}
IC	void	CBackend::set_xform_view_old	(const Fmatrix& M_)					
{ 
	xforms.set_V_old(M_);
}
IC	void	CBackend::set_xform_project_old	(const Fmatrix& M_)
{ 
	xforms.set_P_old(M_);
}

IC	const Fmatrix&	CBackend::get_xform_world	()	{ return xforms.get_W();	}
IC	const Fmatrix&	CBackend::get_xform_view	()	{ return xforms.get_V();	}
IC	const Fmatrix&	CBackend::get_xform_project	()	{ return xforms.get_P();	}

IC	const Fmatrix&	CBackend::get_xform_world_old	()	{ return xforms.get_W_old();	}
IC	const Fmatrix&	CBackend::get_xform_view_old	()	{ return xforms.get_V_old();	}
IC	const Fmatrix&	CBackend::get_xform_project_old	()	{ return xforms.get_P_old();	}

IC	IRHIRenderTargetView* CBackend::get_RT(u32 ID)
{
	return GRHI->GetRenderTargetView(ID);
}

ICF void	CBackend::set_States		(ID3DState* _state)
{
#ifndef USE_DX11
	if (state!=_state)
#endif //USE_DX11
	{
		state			= _state;
		state->Apply	();
	}
}

#ifdef _EDITOR
IC void CBackend::set_Matrices			(SMatrixList*	_M)
{
	if (M != _M)
	{
		M = _M;
		if (M)	{
			for (u32 it=0; it<M->size(); it++)
			{
				CMatrix*	mat = &*((*M)[it]);
				if (mat && matrices[it]!=mat)
				{
					matrices	[it]	= mat;
					mat->Calculate		();
					set_xform			(D3DTS_TEXTURE0+it,mat->xform);
	//				stat.matrices		++;
				}
			}
		}
	}
}
#endif

IC void CBackend::set_Element			(ShaderElement* S, u32	pass)
{
	//PROF_EVENT("CBackend::set_Element")
	SPass&	P		= *(S->passes[pass]);
	set_States		(P.state);
	set_PS			(P.ps);
	set_VS			(P.vs);
#ifdef USE_DX11
	set_GS			(P.gs);
	set_HS			(P.hs);
	set_DS			(P.ds);
	set_CS			(P.cs);
#endif //USE_DX11
	set_Constants	(P.constants);
	set_Textures	(P.T);
#ifdef _EDITOR
	set_Matrices	(P.M);
#endif
}

ICF void CBackend::set_Shader(Shader* S, u32 pass)
{
	if (S == nullptr)
	{
		Msg("! Not found shader in library!");
		return;
	}

	set_Element(S->E[0], pass);
}

#endif
