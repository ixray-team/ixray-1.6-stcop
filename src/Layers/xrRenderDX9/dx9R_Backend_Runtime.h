#pragma once

IC void		CBackend::set_xform			(u32 ID, const Fmatrix& M_)
{
	stat.xforms			++;
	CHK_DX				(RDevice->SetTransform((D3DTRANSFORMSTATETYPE)ID,(D3DMATRIX*)&M_));
}

IC void CBackend::set_RT(IRHIRenderTargetView* RT, u32 ID)
{
	GRHI->SetRenderTargetView(RT, ID);
}

ICF void CBackend::set_Format(IDirect3DVertexDeclaration9* _decl)
{
	if (decl!=_decl)
	{
		decl			= _decl;
		CHK_DX			(RDevice->SetVertexDeclaration(decl));
	}
}

ICF void CBackend::Render(ERHI_PRIMITIVE_TOPOLOGY topology, u32 baseV, u32 startV, u32 countV, u32 startI, u32 PC)
{
    //Fix D3D ERROR
    if (PC==0)
        return;

    stat.calls++;
    stat.verts += countV;
    stat.polys += PC;
    constants.flush();

	GRHI->ApplyRenderTargetChange();
    GRHI->SetPrimitiveTopology(topology);
    GRHI->DrawIndexed(baseV, startV, countV, startI, PC);
}

ICF void CBackend::Render(ERHI_PRIMITIVE_TOPOLOGY topology, u32 startV, u32 PC)
{
    //Fix D3D ERROR
    if (PC==0)
        return;

    stat.calls++;
    stat.verts += 3*PC;
    stat.polys += PC;
    constants.flush();
    
	GRHI->ApplyRenderTargetChange();
    GRHI->SetPrimitiveTopology(topology);
    GRHI->Draw(startV, PC);
}

IC void CBackend::set_Geometry(SGeometry* _geom)
{
	set_Format			(_geom->dcl._get()->dcl);
	set_Vertices		(_geom->vb, _geom->vb_stride);
	set_Indices			(_geom->ib);
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
		RHIShaderConstant*		Cs	= &**it;
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

IC void CBackend::DrawTriangleFan(ERHI_PRIMITIVE_TOPOLOGY topology, ref_geom geom, u32 vBase, u32 pc)
{
	RCache.set_Geometry(geom);
	RCache.Render(topology, vBase, pc);
}