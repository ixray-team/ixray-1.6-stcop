#ifndef R_BACKEND_RUNTIMEH
#define R_BACKEND_RUNTIMEH
#pragma once

#include "sh_texture.h"
#include "sh_matrix.h"
#include "sh_constant.h"
#include "sh_rt.h"

#ifdef USE_DX11
#include "../xrRenderDX10/dx10R_Backend_Runtime.h"
#include "../xrRenderDX10/StateManager/dx10State.h"
#else //USE_DX11
#include "../xrRenderDX9/dx9R_Backend_Runtime.h"
#endif

IC void		R_xforms::set_c_w			(R_constant* C)		{	c_w		= C;	RCache.set_c(C,m_w);	};
IC void		R_xforms::set_c_invw		(R_constant* C)		{	c_invw	= C;	apply_invw();			};
IC void		R_xforms::set_c_v			(R_constant* C)		{	c_v		= C;	RCache.set_c(C,m_v);	};
IC void		R_xforms::set_c_p			(R_constant* C)		{	c_p		= C;	RCache.set_c(C,m_p);	};
IC void		R_xforms::set_c_wv			(R_constant* C)		{	c_wv	= C;	RCache.set_c(C,m_wv);	};
IC void		R_xforms::set_c_vp			(R_constant* C)		{	c_vp	= C;	RCache.set_c(C,m_vp);	};
IC void		R_xforms::set_c_wvp			(R_constant* C)		{	c_wvp	= C;	RCache.set_c(C,m_wvp);	};

IC void		R_xforms::set_c_w_old		(R_constant* C)		{	c_w_old = C;	RCache.set_c(C,m_w_old);	};
IC void		R_xforms::set_c_v_old		(R_constant* C)		{	c_v_old = C;	RCache.set_c(C,m_v_old);	};
IC void		R_xforms::set_c_p_old		(R_constant* C)		{	c_p_old = C;	RCache.set_c(C,m_p_old);	};
IC void		R_xforms::set_c_wv_old		(R_constant* C)		{	c_wv_old = C;	RCache.set_c(C,m_wv_old);	};
IC void		R_xforms::set_c_vp_old		(R_constant* C)		{	c_vp_old = C;	RCache.set_c(C,m_vp_old);	};
IC void		R_xforms::set_c_wvp_old		(R_constant* C)		{	c_wvp_old = C;	RCache.set_c(C,m_wvp_old);	};

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

IC	ID3DRenderTargetView* CBackend::get_RT(u32 ID)
{
	VERIFY((ID>=0)&&(ID<4));

	return pRT[ID];
}

IC	ID3DDepthStencilView* CBackend::get_ZB				()
{
	return pZB;
}

ICF void	CBackend::set_States		(ID3DState* _state)
{
//	DX10 Manages states using it's own algorithm. Don't mess with it.
#ifndef USE_DX11
	if (state!=_state)
#endif //USE_DX11
	{
		PGO				(Msg("PGO:state_block"));
#ifdef DEBUG
		stat.states		++;
#endif
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

ICF void CBackend::set_Shader			(Shader* S, u32 pass)
{
	set_Element			(S->E[0],pass);
}

#endif
