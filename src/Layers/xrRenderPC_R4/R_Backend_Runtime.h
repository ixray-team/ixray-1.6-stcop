#ifndef R_BACKEND_RUNTIMEH
#define R_BACKEND_RUNTIMEH
#pragma once

#include "SH_Texture.h"
#include "SH_Matrix.h"
#include "SH_Constant.h"
#include "SH_RT.h"

#include "dx10R_Backend_Runtime.h"
#include "StateManager/dx10State.h"

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

ICF void	CBackend::set_States		(dx10State* _state)
{
	{
		state			= _state;
		state->Apply	();
	}
}

// FFP texture transform emulation (D3DTS_TEXTURE0)
IC void CBackend::set_Matrices			(SMatrixList*	_M)
{
	M = _M;
	CMatrix* mat = (M && !M->empty()) ? (*M)[0]._get() : nullptr;
	if (mat && mat->dwMode == CMatrix::modeTCM)
	{
		mat->Calculate();
		Constants.Write<CBVisor>().m_tc_xform = mat->xform;
	}
	else
	{
		Constants.Write<CBVisor>().m_tc_xform = Fidentity;
	}
}

IC void CBackend::set_Element			(ShaderElement* S, u32	pass)
{
	//PROF_EVENT("CBackend::set_Element")
	SPass&	P		= *(S->passes[pass]);
	set_States		(P.state);
	set_PS			(P.ps);
	set_VS			(P.vs);
	set_GS			(P.gs);
	set_HS			(P.hs);
	set_DS			(P.ds);
	set_CS			(P.cs);
	set_Constants	(P.constants);
	set_Textures	(P.T);
	if (EditorMode)
	{
		set_Matrices	(P.M);
	}
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
