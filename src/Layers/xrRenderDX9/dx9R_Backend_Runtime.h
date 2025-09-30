#pragma once

IC void		CBackend::set_xform			(u32 ID, const Fmatrix& M_)
{
	stat.xforms			++;
	CHK_DX				(RDevice->SetTransform((D3DTRANSFORMSTATETYPE)ID,(D3DMATRIX*)&M_));
}

IC void CBackend::set_RT(ID3DRenderTargetView* RT, u32 ID)
{
	if (RT != pRT[ID])
	{
		pRT[ID] = RT;
		CHK_DX(RDevice->SetRenderTarget(ID, RT));
	}
}

IC void	CBackend::set_ZB(IRHIDepthStencilView* ZB)
{
	if (ZB != pZB)
	{
		stat.target_zb++;
		pZB = ZB;
		CHK_DX(RDevice->SetDepthStencilSurface(ZB ? (IDirect3DSurface9*)ZB->GetRawDSV() : nullptr));
	}
}

ICF void CBackend::set_Format(IDirect3DVertexDeclaration9* _decl)
{
	if (decl!=_decl)
	{
		decl			= _decl;
		CHK_DX			(RDevice->SetVertexDeclaration(decl));
	}
}

ICF void CBackend::Render(D3DPRIMITIVETYPE T_, u32 baseV, u32 startV, u32 countV, u32 startI, u32 PC)
{
	//Fix D3D ERROR
	if (PC==0)
		return;

	stat.calls			++;
	stat.verts			+= countV;
	stat.polys			+= PC;
	constants.flush		();
	CHK_DX				(RDevice->DrawIndexedPrimitive(T_,baseV, startV, countV,startI,PC));
}

ICF void CBackend::Render(D3DPRIMITIVETYPE T_, u32 startV, u32 PC)
{
	//Fix D3D ERROR
	if (PC==0)
		return;

	stat.calls			++;
	stat.verts			+= 3*PC;
	stat.polys			+= PC;
	constants.flush		();
	CHK_DX				(RDevice->DrawPrimitive(T_, startV, PC));
}

IC void CBackend::set_Geometry(SGeometry* _geom)
{
	set_Format			(_geom->dcl._get()->dcl);
	set_Vertices		(_geom->vb, _geom->vb_stride);
	set_Indices			(_geom->ib);
}

IC void	CBackend::set_Scissor(Irect*	R)
{
	if (R)			
	{
		CHK_DX		(RDevice->SetRenderState(D3DRS_SCISSORTESTENABLE,TRUE));
		RECT	*	clip	= (RECT	*)R;
		CHK_DX		(RDevice->SetScissorRect(clip));
	} 
	else
	{
		CHK_DX		(RDevice->SetRenderState(D3DRS_SCISSORTESTENABLE,FALSE));
	}
}

IC void CBackend::set_Stencil(u32 _enable, u32 _func, u32 _ref, u32 _mask, u32 _writemask, u32 _fail, u32 _pass, u32 _zfail)
{
	// Simple filter
	if (stencil_enable		!= _enable)		{ stencil_enable=_enable;		CHK_DX(RDevice->SetRenderState	( D3DRS_STENCILENABLE,		_enable				)); }
	if (!stencil_enable)					return;
	if (stencil_func		!= _func)		{ stencil_func=_func;			CHK_DX(RDevice->SetRenderState	( D3DRS_STENCILFUNC,		_func				)); }
	if (stencil_ref			!= _ref)		{ stencil_ref=_ref;				CHK_DX(RDevice->SetRenderState	( D3DRS_STENCILREF,			_ref				)); }
	if (stencil_mask		!= _mask)		{ stencil_mask=_mask;			CHK_DX(RDevice->SetRenderState	( D3DRS_STENCILMASK,		_mask				)); }
	if (stencil_writemask	!= _writemask)	{ stencil_writemask=_writemask;	CHK_DX(RDevice->SetRenderState	( D3DRS_STENCILWRITEMASK,	_writemask			)); }
	if (stencil_fail		!= _fail)		{ stencil_fail=_fail;			CHK_DX(RDevice->SetRenderState	( D3DRS_STENCILFAIL,		_fail				)); }
	if (stencil_pass		!= _pass)		{ stencil_pass=_pass;			CHK_DX(RDevice->SetRenderState	( D3DRS_STENCILPASS,		_pass				)); }
	if (stencil_zfail		!= _zfail)		{ stencil_zfail=_zfail;			CHK_DX(RDevice->SetRenderState	( D3DRS_STENCILZFAIL,		_zfail				)); }
}

IC  void CBackend::set_Z(u32 _enable)
{
	if (z_enable != _enable)
	{ 
		z_enable=_enable;
		CHK_DX(RDevice->SetRenderState	( D3DRS_ZENABLE, _enable )); 
	}
}

IC  void CBackend::set_ZFunc(u32 _func)
{
	if (z_func!=_func)
	{
		z_func = _func;
		CHK_DX(RDevice->SetRenderState( D3DRS_ZFUNC, _func));
	}
}

IC  void CBackend::set_AlphaRef (u32 _value)
{
	if (alpha_ref != _value)
	{ 
		alpha_ref = _value;
		CHK_DX(RDevice->SetRenderState(D3DRS_ALPHAREF,_value));
	}
}

IC void	CBackend::set_ColorWriteEnable	(u32 _mask )
{
	if (colorwrite_mask		!= _mask)		{ 
		colorwrite_mask=_mask;		
		CHK_DX(RDevice->SetRenderState	( D3DRS_COLORWRITEENABLE,	_mask	));	
		CHK_DX(RDevice->SetRenderState	( D3DRS_COLORWRITEENABLE1,	_mask	));	
		CHK_DX(RDevice->SetRenderState	( D3DRS_COLORWRITEENABLE2,	_mask	));	
		CHK_DX(RDevice->SetRenderState	( D3DRS_COLORWRITEENABLE3,	_mask	));	
	}
}

ICF void	CBackend::set_CullMode		(u32 _mode)
{
	if (cull_mode		!= _mode)		{ cull_mode = _mode;			CHK_DX(RDevice->SetRenderState	( D3DRS_CULLMODE,			_mode				)); }
}

ICF void CBackend::set_VS(ref_vs& _vs)
{
	GRHI->SetShader(_vs->vs, ERHI_SHADER_TYPE::VS);
}

IC void CBackend::set_Constants			(R_constant_table* C_)
{
	// caching
	if (ctable==C_)
		return;

	ctable			= C_;
	xforms.unmap	();
	hemi.unmap		();
	tree.unmap		();

	if (0==C_)
		return;

	// process constant-loaders
	R_constant_table::c_table::iterator	it	= C_->table.begin();
	R_constant_table::c_table::iterator	end	= C_->table.end	();
	for (; it!=end; it++)	{
		R_constant*		Cs	= &**it;
		VERIFY(Cs);
		if (Cs && Cs->handler) {
			Cs->handler->setup(Cs);
		}
	}
}

IC float CBackend::get_width()
{
	return RDEVICE.TargetWidth;
}

IC float CBackend::get_height()
{
	return RDEVICE.TargetHeight;
}

IC float CBackend::get_target_width()
{
	return RDEVICE.TargetWidth;
}

IC float CBackend::get_target_height()
{
	return RDEVICE.TargetHeight;
}