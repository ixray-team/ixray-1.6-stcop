#pragma once

IC void CBackend::set_xform( u32 ID, const Fmatrix& M_ )
{
	stat.xforms			++;
	//	TODO: DX10: Implement CBackend::set_xform
}

IC void CBackend::set_RT(IRHIRenderTargetView* RT, u32 ID)
{
	GRHI->SetRenderTargetView(RT, ID);
}

ICF void CBackend::set_Format(SDeclaration* _decl)
{
	if (decl != _decl)
	{
		decl = _decl;
	}
}

IC void CBackend::Compute(UINT ThreadGroupCountX, UINT ThreadGroupCountY, UINT ThreadGroupCountZ)
{
	stat.calls++;

	GRHI->ShaderResourceCache->Apply();
	GRHI->StateManager->Apply();
	Constants.Flush();
	GRHI->Dispatch(ThreadGroupCountX,ThreadGroupCountY,ThreadGroupCountZ);
}

IC void CBackend::RenderInstancedIndexed(ERHI_PRIMITIVE_TOPOLOGY topology, u32 baseV, u32 startV, u32 countV, u32 startI, u32 PC, u32 instanceCount, u32 startInstanceLocation)
{
	u32	iIndexCount = RHITopologyUtils::GetIndexCount(PC, topology);

	//!!! HACK !!!
	if (GRHI->IsTessPass())
	{
		R_ASSERT(topology == ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST);
		topology = ERHI_PRIMITIVE_TOPOLOGY::CONTROL_POINT_3_PATCH;
	}

	stat.calls++;
	stat.verts += countV;
	stat.polys += PC;

	GRHI->SetPrimitiveTopology(topology);

	GRHI->ShaderResourceCache->Apply();
	GRHI->ApplyRenderTargetChange();
	ApplyVertexLayout();
	GRHI->StateManager->Apply();
	
	Constants.Flush();

	GRHI->DrawIndexedInstanced(baseV, startV, countV, startI, PC, instanceCount, startInstanceLocation);
}

IC void CBackend::RenderInstanced(ERHI_PRIMITIVE_TOPOLOGY topology, u32 startV, u32 PC, u32 instanceCount)
{
	if (PC == 0 || instanceCount == 0)
	{
		return;
	}

	const u32 VertexCount = RHITopologyUtils::GetVertexCount(PC, topology);

	stat.calls++;
	stat.verts += VertexCount * instanceCount;
	stat.polys += PC * instanceCount;

	GRHI->SetPrimitiveTopology(topology);
	GRHI->ShaderResourceCache->Apply();
	GRHI->ApplyRenderTargetChange();
	ApplyVertexLayout();
	GRHI->StateManager->Apply();

	Constants.Flush();
	GRHI->DrawInstanced(startV, PC, instanceCount, 0);
}

IC void CBackend::Render(ERHI_PRIMITIVE_TOPOLOGY topology, u32 baseV, u32 startV, u32 countV, u32 startI, u32 PC)
{
    // Don't render if primitive count is 0
    if (PC == 0)
        return;
    
    // Don't render triangle fans in DX11
    if (topology == ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_FAN)
        return;
    
    if (GRHI->IsTessPass())
    {
        R_ASSERT(topology == ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST);
        topology = ERHI_PRIMITIVE_TOPOLOGY::CONTROL_POINT_3_PATCH;
    }

    stat.calls++;
    stat.verts += countV;
    stat.polys += PC;

    GRHI->SetPrimitiveTopology(topology);
    GRHI->ShaderResourceCache->Apply();
    GRHI->ApplyRenderTargetChange();
    ApplyVertexLayout();
    GRHI->StateManager->Apply();

    Constants.Flush();

    GRHI->DrawIndexed(baseV, startV, countV, startI, PC);
}

IC void CBackend::Render(ERHI_PRIMITIVE_TOPOLOGY topology, u32 startV, u32 PC)
{
    // Don't render if primitive count is 0
    if (PC == 0)
        return;
    
    // Don't render triangle fans in DX11
    if (topology == ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_FAN)
        return;

    stat.calls++;
    stat.verts += 3*PC;
    stat.polys += PC;

    GRHI->SetPrimitiveTopology(topology);
    GRHI->ShaderResourceCache->Apply();
	GRHI->ApplyRenderTargetChange();
    ApplyVertexLayout();
    GRHI->StateManager->Apply();
    
    Constants.Flush();
    GRHI->Draw(startV, PC);
}

IC void CBackend::Render_noIA(u32 iVertexCount)
{
    stat.calls++;
    stat.verts += iVertexCount;

    GRHI->ShaderResourceCache->Apply();
	GRHI->ApplyRenderTargetChange();

    // Use RHI primitive topology
    GRHI->SetPrimitiveTopology(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST);

    GRHI->StateManager->Apply();

    Constants.Flush();
    
    GRHI->DrawNoInputAssembly(iVertexCount);
}

IC void CBackend::set_Geometry(SGeometry* _geom)
{
	set_Format(&*_geom->dcl);

	set_Vertices(_geom->vb, _geom->vb_stride);
	set_Indices(_geom->ib);
}

IC void CBackend::ApplyVertexLayout()
{
	VERIFY(decl);
	if (!decl || !m_pInputSignature)
		return;

	xr_map<RHIBlob*, RHIObject*>::iterator	it;

	it = decl->vs_to_layout.find(m_pInputSignature);

	if (it==decl->vs_to_layout.end())
	{
		RHIObject* pLayout;

		CHK_DX(GRHI->CreateInputLayout
		(
			decl->dx10_dcl_code.data(),
			decl->dx10_dcl_code.size(),
			m_pInputSignature->GetBufferPointer(),
			m_pInputSignature->GetBufferSize(),
			&pLayout
			)
		);

		it = decl->vs_to_layout.insert(
			std::pair<RHIBlob*, RHIObject*>(m_pInputSignature, pLayout)).first;
	}

	if ( m_pInputLayout != it->second)
	{
		m_pInputLayout = it->second;
		GRHI->SetInputLayout(m_pInputLayout);
	}	
}

ICF void CBackend::set_VS(ref_vs& _vs)
{
	CurrentVS = _vs._get();
	m_pInputSignature = (_vs && _vs->signature) ? _vs->signature->signature : nullptr;
	GRHI->SetShader(_vs ? _vs->vs : nullptr, ERHI_SHADER_TYPE::VS);
}

ICF void CBackend::set_VS(SVS* _vs)
{
	CurrentVS = _vs;
	m_pInputSignature = (_vs && _vs->signature) ? _vs->signature->signature : nullptr;
	GRHI->SetShader(_vs ? _vs->vs : nullptr, ERHI_SHADER_TYPE::VS);
}

IC void CBackend::set_Constants			(R_constant_table* C_)
{
	ctable = C_;
	if (!C_)
		return;
	Constants.OnShaderBind(xforms);
	for (RHIShaderConstant* Cs : C_->get_handlers()) Cs->handler->setup();
}

IC float CBackend::get_width()
{
	return HalfTarget.x;
}

IC float CBackend::get_height()
{
	return HalfTarget.y;
}

IC float CBackend::get_target_width()
{
	return float(RDEVICE.TargetWidth);
}

IC float CBackend::get_target_height()
{
	return float(RDEVICE.TargetHeight);
}

IC void CBackend::DrawTriangleFan(ERHI_PRIMITIVE_TOPOLOGY topology, ref_geom geom, u32 vBase, u32 pc)
{
	const u32 cnt_v = pc + 2;
	const u32 cnt_indices = pc * 3; // triangle
	u32 of_indices = 0;
	u16* ptr_indices = Index.Lock(cnt_indices, of_indices);

	for (int i = 0; i < cnt_indices; i += 3)
	{
		ptr_indices[i * 3] = 0;
		ptr_indices[i * 3 +1] = i+1;
		ptr_indices[i * 3 +2] = i+2;
	}

	Index.Unlock(cnt_indices);

	set_Geometry(geom);
	set_Indices(Index.Buffer()); // overwrite geom IB with dyn IB
	
	Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, vBase, 0, cnt_indices, of_indices, pc);
}
