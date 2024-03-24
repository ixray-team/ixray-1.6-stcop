#include "R_IBackend.h"
#ifndef R_IBACKEND_INLINE_H
#define R_IBACKEND_INLINE_H


IC ITexture1D* CBackendBase::CreateTexture1D(const TextureDesc* pDesc, const SUBRESOURCE_DATA* pSubresource)
{
	return (ITexture1D*)CreateTexture(pDesc, pSubresource);
}

IC ITexture2D* CBackendBase::CreateTexture2D(const TextureDesc* pDesc, const SUBRESOURCE_DATA* pSubresource)
{
	return (ITexture2D*)CreateTexture(pDesc, pSubresource);
}

IC ITexture3D* CBackendBase::CreateTexture3D(const TextureDesc* pDesc, const SUBRESOURCE_DATA* pSubresource)
{
	return (ITexture3D*)CreateTexture(pDesc, pSubresource);
}

IC float CBackendBase::get_width()
{
	return (float)RDEVICE.TargetWidth;
}

IC float CBackendBase::get_height()
{
	return (float)RDEVICE.TargetHeight;
}

IC float CBackendBase::get_target_width()
{
	return (float)RDEVICE.TargetWidth;
}

IC float CBackendBase::get_target_height()
{
	return (float)RDEVICE.TargetHeight;
}

IC void	CBackendBase::set_xform(u32 ID, const Fmatrix& M_)
{
	stats.xforms++;
	//	TODO: DX10: Implement CBackend::set_xform
}
IC void CBackendBase::set_xform_world(const Fmatrix& M_)
{
	xforms.set_W(M_);
}
IC void CBackendBase::set_xform_view(const Fmatrix& M_)
{
	xforms.set_V(M_);
}
IC void CBackendBase::set_xform_project(const Fmatrix& M_)
{
	xforms.set_P(M_);
}
IC const Fmatrix& CBackendBase::get_xform_world() { return xforms.get_W(); }
IC const Fmatrix& CBackendBase::get_xform_view() { return xforms.get_V(); }
IC const Fmatrix& CBackendBase::get_xform_project() { return xforms.get_P(); }

#ifdef USE_DX11
IC void CBackendBase::set_RT(ID3DRenderTargetView* RT, u32 ID)
{
	if (RT != pRT[ID])
	{
		PGO(Msg("PGO:setRT"));
		stats.target_rt++;
		pRT[ID] = RT;

		//	Mark RT array dirty
		//	Reset all RT's here to allow RT to be bounded as input
		if (!m_bChangedRTorZB)
			RContext->OMSetRenderTargets(0, 0, 0);

		m_bChangedRTorZB = true;
	}
}

IC void CBackendBase::set_ZB(ID3DDepthStencilView* ZB)
{
	if (ZB != pZB)
	{
		PGO(Msg("PGO:setZB"));
		stats.target_zb++;
		pZB = ZB;

		//	Reset all RT's here to allow RT to be bounded as input
		if (!m_bChangedRTorZB)
			RContext->OMSetRenderTargets(0, 0, 0);

		m_bChangedRTorZB = true;
	}
}

IC ID3DRenderTargetView* CBackendBase::get_RT(u32 ID)
{
	VERIFY((ID >= 0) && (ID < 4));

	return pRT[ID];
}

IC ID3DDepthStencilView* CBackendBase::get_ZB()
{
	return pZB;
}

IC void CBackendBase::get_ConstantDirect(shared_str& n, u32 DataSize, void** pVData, void** pGData, void** pPData)
{
	ref_constant C_ = get_c(n);

	if (C_)
		constants->access_direct(&*C_, DataSize, pVData, pGData, pPData);
	else
	{
		if (pVData)	*pVData = 0;
		if (pGData)	*pGData = 0;
		if (pPData)	*pPData = 0;
	}
}

ICF void CBackendBase::set_Format(SDeclaration* _decl)
{
	if (decl != _decl)
	{
		PGO(Msg("PGO:v_format:%x", _decl));
#ifdef DEBUG
		stats.decl++;
#endif
		decl = _decl;
	}
}

#else
ICF void CBackendBase::set_Format(IDirect3DVertexDeclaration9* _decl)
{
	if (decl != _decl)
	{
		PGO(Msg("PGO:v_format:%x", _decl));
#ifdef DEBUG
		stats.decl++;
#endif
		decl = _decl;
		CHK_DX(RDevice->SetVertexDeclaration(decl));
	}
}
#endif // USE_DX11

IC void CBackendBase::set_Geometry(SGeometry* _geom)
{
#ifdef USE_DX11
	set_Format(&*_geom->dcl);
#else
	set_Format(&*_geom->dcl->dcl);
#endif // USE_DX11
	set_Vertices(_geom->vb, _geom->vb_stride);
	set_Indices(_geom->ib);
}

IC void CBackendBase::get_Stats(backend_stats* pStats)
{
	R_ASSERT(pStats);
	*pStats = stats;
}

IC void CBackendBase::set_Shader(Shader* S, u32 pass)
{
	set_Element(S->E[0], pass);
}

ICF void CBackendBase::set_States(ID3DState* _state)
{
	//	DX10 Manages states using it's own algorithm. Don't mess with it.
#ifndef USE_DX11
	if (state != _state)
#endif //USE_DX11
	{
		PGO(Msg("PGO:state_block"));
#ifdef DEBUG
		stats.states++;
#endif
		state = _state;
		state->Apply();
	}
}

ICF bool CBackendBase::is_TessEnabled()
{
#ifdef USE_DX11
	return true;
#else
	return false;
#endif // USE_DX11
}

ICF void CBackendBase::Compute(UINT ThreadGroupCountX, UINT ThreadGroupCountY, UINT ThreadGroupCountZ)
{
#ifdef USE_DX11
	stats.calls++;

	SRVSManager_Apply();
	StateManager.Apply();
	//	State manager may alter constants
	constants->flush();
	RContext->Dispatch(ThreadGroupCountX, ThreadGroupCountY, ThreadGroupCountZ);
#else
	Msg("! CBackendBase::Compute: Called in DX9 !!!");
#endif
}

// Shader Installation
// #TODO: make it pure virtual or convert to using IShaderProgram/SShaderCollection

#ifdef USE_DX11

ICF void CBackendBase::set_VS(ref_vs& _vs)
{
	m_pInputSignature = _vs->signature->signature;
	set_VS(_vs->vs, _vs->cName.c_str());
}

ICF void CBackendBase::set_VS(SVS* _vs)
{
	m_pInputSignature = _vs->signature->signature;
	set_VS(_vs->vs, _vs->cName.c_str());
}

ICF void CBackendBase::set_VS(ID3DVertexShader* _vs, LPCSTR _n)
{
	if (vs != _vs)
	{
		PGO(Msg("PGO:Vshader:%x", _vs));
		stats.vs++;
		vs = _vs;

		RContext->VSSetShader(vs, 0, 0);

#ifdef DEBUG
		vs_name = _n;
#endif
	}
}

ICF void CBackendBase::set_PS(ID3DPixelShader* _ps, LPCSTR _n)
{
	if (ps != _ps)
	{
		PGO(Msg("PGO:Pshader:%x", _ps));
		stats.ps++;
		ps = _ps;

		RContext->PSSetShader(ps, 0, 0);


#ifdef DEBUG
		ps_name = _n;
#endif
	}
}

ICF void CBackendBase::set_GS(ID3DGeometryShader* _gs, LPCSTR _n)
{
	if (gs != _gs)
	{
		PGO(Msg("PGO:Gshader:%x", _ps));
		//	TODO: DX10: Get statistics for G Shader change

		gs = _gs;

		RContext->GSSetShader(gs, 0, 0);

#ifdef DEBUG
		gs_name = _n;
#endif
	}
}

ICF void CBackendBase::set_HS(ID3D11HullShader* _hs, LPCSTR _n)
{
	if (hs != _hs)
	{
		PGO(Msg("PGO:Hshader:%x", _ps));
		//	TODO: DX10: Get statistics for H Shader change

		hs = _hs;
		RContext->HSSetShader(hs, 0, 0);

#ifdef DEBUG
		hs_name = _n;
#endif
	}
}

ICF void CBackendBase::set_DS(ID3D11DomainShader* _ds, LPCSTR _n)
{
	if (ds != _ds)
	{
		PGO(Msg("PGO:Dshader:%x", _ps));
		//	TODO: DX10: Get statistics for D Shader change

		ds = _ds;
		RContext->DSSetShader(ds, 0, 0);

#ifdef DEBUG
		ds_name = _n;
#endif
	}
}

ICF void CBackendBase::set_CS(ID3D11ComputeShader* _cs, LPCSTR _n)
{
	if (cs != _cs)
	{
		PGO(Msg("PGO:Cshader:%x", _ps));
		//	TODO: DX10: Get statistics for D Shader change
		//stat.cs			++;
		cs = _cs;
		RContext->CSSetShader(cs, 0, 0);

#ifdef DEBUG
		cs_name = _n;
#endif
	}
}
#else
ICF void CBackendBase::set_VS(ref_vs& _vs)
{
	set_VS(_vs->vs, _vs->cName.c_str());
}

ICF void CBackendBase::set_PS(ID3DPixelShader* _ps, LPCSTR _n)
{
	if (ps != _ps)
	{
		PGO(Msg("PGO:Pshader:%x", _ps));
		stats.ps++;
		ps = _ps;
		CHK_DX(RDevice->SetPixelShader(ps));
#ifdef DEBUG
		ps_name = _n;
#endif
	}
}

ICF void CBackendBase::set_VS(ID3DVertexShader* _vs, LPCSTR _n)
{
	if (vs != _vs)
	{
		PGO(Msg("PGO:Vshader:%x", _vs));
		stats.vs++;
		vs = _vs;
		CHK_DX(RDevice->SetVertexShader(vs));
#ifdef DEBUG
		vs_name = _n;
#endif
	}
}
#endif // USE_DX11

// #TODO: Render Target abstraction
#ifndef USE_DX11

IC void CBackendBase::set_RT(ID3DRenderTargetView* RT, u32 ID)
{
	if (RT != pRT[ID])
	{
		PGO(Msg("PGO:setRT"));
		stats.target_rt++;
		pRT[ID] = RT;
		CHK_DX(RDevice->SetRenderTarget(ID, RT));
	}
}

IC void	CBackendBase::set_ZB(ID3DDepthStencilView* ZB)
{
	if (ZB != pZB)
	{
		PGO(Msg("PGO:setZB"));
		stats.target_zb++;
		pZB = ZB;
		CHK_DX(RDevice->SetDepthStencilSurface(ZB));
	}
}
#endif

// #TODO : REFACTOR DX9
IC R_constant_array& CBackendBase::get_ConstantCache_Vertex()
{
	R_constant_array* pConstantArray = constants->get_ConstantCache_Vertex();
	return *pConstantArray;
}

inline IC R_constant_array& CBackendBase::get_ConstantCache_Pixel()
{
	R_constant_array* pConstantArray = constants->get_ConstantCache_Pixel();
	return *pConstantArray;
}

#endif // !R_IBACKEND_INLINE_H