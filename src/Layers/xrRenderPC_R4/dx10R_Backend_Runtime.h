#pragma once

ICF void GfxCmdList::SetFormat(SDeclaration* _decl)
{
	if (decl != _decl)
	{
		decl = _decl;
	}
}

IC void GfxCmdList::Dispatch(u32 GroupsX, u32 GroupsY, u32 GroupsZ)
{
	Stats.Calls++;
	GRHI->ShaderResourceCache->Apply();
	FlushStates();
	Constants.Flush();
	GRHI->Dispatch(GroupsX, GroupsY, GroupsZ);
}

IC bool GfxCmdList::PrepareDraw(ERHI_PRIMITIVE_TOPOLOGY Topology, u32 Count, u32 Instances, bool Indexed)
{
	if (Count == 0 || Instances == 0 || Topology == ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_FAN)
	{
		return false;
	}
	if (Indexed && GRHI->IsTessPass())
	{
		R_ASSERT(Topology == ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST);
		Topology = ERHI_PRIMITIVE_TOPOLOGY::CONTROL_POINT_3_PATCH;
	}
	Stats.Calls++;
	Stats.Polys += RHITopologyUtils::GetPrimitiveCount(Count, Topology) * Instances;
	GRHI->SetPrimitiveTopology(Topology);
	GRHI->ShaderResourceCache->Apply();
	GRHI->ApplyRenderTargetChange();
	ApplyVertexLayout();
	FlushStates();
	Constants.Flush();
	return true;
}

IC void GfxCmdList::Draw(ERHI_PRIMITIVE_TOPOLOGY Topology, u32 VertexCount, u32 StartVertex)
{
	if (PrepareDraw(Topology, VertexCount, 1, false))
	{
		GRHI->Draw(VertexCount, StartVertex);
	}
}

IC void GfxCmdList::DrawIndexed(ERHI_PRIMITIVE_TOPOLOGY Topology, u32 IndexCount, u32 StartIndex, u32 BaseVertex)
{
	if (PrepareDraw(Topology, IndexCount, 1, true))
	{
		GRHI->DrawIndexed(IndexCount, StartIndex, BaseVertex);
	}
}

IC void GfxCmdList::DrawInstanced(ERHI_PRIMITIVE_TOPOLOGY Topology, u32 VertexCount, u32 InstanceCount, u32 StartVertex)
{
	if (PrepareDraw(Topology, VertexCount, InstanceCount, false))
	{
		GRHI->DrawInstanced(VertexCount, InstanceCount, StartVertex, 0);
	}
}

IC void GfxCmdList::DrawIndexedInstanced(ERHI_PRIMITIVE_TOPOLOGY Topology, u32 IndexCount, u32 InstanceCount, u32 StartIndex, u32 BaseVertex, u32 StartInstance)
{
	if (PrepareDraw(Topology, IndexCount, InstanceCount, true))
	{
		GRHI->DrawIndexedInstanced(IndexCount, InstanceCount, StartIndex, BaseVertex, StartInstance);
	}
}

IC void GfxCmdList::DrawNoInput(u32 VertexCount)
{
	Stats.Calls++;
	GRHI->ShaderResourceCache->Apply();
	GRHI->ApplyRenderTargetChange();
	GRHI->SetPrimitiveTopology(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST);
	FlushStates();
	Constants.Flush();
	GRHI->DrawNoInputAssembly(VertexCount);
}

IC void GfxCmdList::SetGeometry(SGeometry* _geom)
{
	SetFormat(&*_geom->dcl);

	SetVertices(_geom->vb, _geom->vb_stride);
	SetIndices(_geom->ib);
}

IC void GfxCmdList::ApplyVertexLayout()
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

ICF void GfxCmdList::SetVS(ref_vs& _vs)
{
	CurrentVS = _vs._get();
	m_pInputSignature = (_vs && _vs->signature) ? _vs->signature->signature : nullptr;
	GRHI->SetShader(_vs ? _vs->vs : nullptr, ERHI_SHADER_TYPE::VS);
}

ICF void GfxCmdList::SetVS(SVS* _vs)
{

	CurrentVS = _vs;
	m_pInputSignature = (_vs && _vs->signature) ? _vs->signature->signature : nullptr;
	GRHI->SetShader(_vs ? _vs->vs : nullptr, ERHI_SHADER_TYPE::VS);
}

IC float GfxCmdList::GetWidth()
{
	return HalfTarget.x;
}

IC float GfxCmdList::GetHeight()
{
	return HalfTarget.y;
}

IC float GfxCmdList::GetTargetWidth()
{
	return float(RDEVICE.TargetWidth);
}

IC float GfxCmdList::GetTargetHeight()
{
	return float(RDEVICE.TargetHeight);
}

