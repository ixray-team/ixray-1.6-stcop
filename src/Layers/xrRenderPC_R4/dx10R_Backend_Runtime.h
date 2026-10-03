#pragma once
#include "dx10FixedConstants.h"

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
	//	State manager may alter constants
	constants.flush();
	GRHI->Dispatch(ThreadGroupCountX,ThreadGroupCountY,ThreadGroupCountZ);
}

IC void CBackend::RenderInstancedIndexed(ERHI_PRIMITIVE_TOPOLOGY topology, u32 baseV, u32 startV, u32 countV, u32 startI, u32 PC, u32 instanceCount, u32 startInstanceLocation, bool flush_constants)
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
	
	//	State manager may alter constants
	if(flush_constants)
		constants.flush();
	else
		FixedConstants::Flush();

	GRHI->DrawIndexedInstanced(baseV, startV, countV, startI, PC, instanceCount, startInstanceLocation);
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

    // State manager may alter constants
    constants.flush();

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
    
    // State manager may alter constants
    constants.flush();
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

    // State manager may alter constants
    constants.flush();
    
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
	m_pInputSignature = (_vs && _vs->signature) ? _vs->signature->signature : nullptr;
	GRHI->SetShader(_vs ? _vs->vs : nullptr, ERHI_SHADER_TYPE::VS);
}

ICF void CBackend::set_VS(SVS* _vs)
{
	m_pInputSignature = (_vs && _vs->signature) ? _vs->signature->signature : nullptr;
	GRHI->SetShader(_vs ? _vs->vs : nullptr, ERHI_SHADER_TYPE::VS);
}

IC void CBackend::set_Constants			(R_constant_table* C_)
{
	if (ctable==C_)
	{
		if (C_)
		{
			FixedConstants::OnShaderBind();
			for (RHIShaderConstant* Cs : C_->get_handlers()) Cs->handler->setup(Cs);
		}
		return;
	}
	ctable = C_;
	xforms.unmap();
	hemi.unmap();
	tree.unmap();
	LOD.unmap();
	GRHI->StateManager->UnmapConstants();
	if (!C_)
		return;
	FixedConstants::OnShaderBind();
	FixedConstants::BindAll();
	{
		ref_cbuffer* const dst[] = { m_aPixelConstants, m_aVertexConstants, m_aGeometryConstants, m_aHullConstants, m_aDomainConstants, m_aComputeConstants };
		static const ERHI_SHADER_TYPE stage[] = { ERHI_SHADER_TYPE::PS, ERHI_SHADER_TYPE::VS, ERHI_SHADER_TYPE::GS, ERHI_SHADER_TYPE::HS, ERHI_SHADER_TYPE::DS, ERHI_SHADER_TYPE::CS };
		dx10ConstantBuffer* next[std::size(dst)][MaxCBuffers] = {};
		for (const auto& rec : C_->m_CBTable)
		{
			const u32 slot = rec.first & CB_BufferIndexMask;
			VERIFY(slot < MaxCBuffers);

			switch (rec.first & CB_BufferTypeMask)
			{
			case CB_BufferPixelShader: next[0][slot]=rec.second._get(); break;
			case CB_BufferVertexShader: next[1][slot]=rec.second._get(); break;
			case CB_BufferGeometryShader: next[2][slot]=rec.second._get(); break;
			case CB_BufferHullShader: next[3][slot]=rec.second._get(); break;
			case CB_BufferDomainShader: next[4][slot]=rec.second._get(); break;
			case CB_BufferComputeShader: next[5][slot]=rec.second._get(); break;
			default: VERIFY("Invalid enumeration");
			}
		}
		bool written = false;
		for (u32 s=0; s<std::size(dst); ++s)
		{
			for (u32 i=0; i<MaxCBuffers; ++i)
			{
				if (dst[s][i]._get() != next[s][i])
				{
					dst[s][i] = next[s][i];
					IRHIBuffer* bind = next[s][i] ? next[s][i]->GetBuffer() : nullptr;
					if (next[s][i])
						constants.MarkDirty(*next[s][i]);

					if (i < FixedConstants::kSlots)
					{
						if (!next[s][i])
							continue;
						written = true;
					}
					GRHI->SetConstantBuffers(i, 1, &bind, stage[s]);
				}
			}
		}
		if (written)
			FixedConstants::InvalidateBindings();
	}
	for (RHIShaderConstant* Cs : C_->get_handlers()) Cs->handler->setup(Cs);
}

IC	void CBackend::get_ConstantDirect(shared_str& n, u32 DataSize, void** pVData, void** pGData, void** pPData)
{
	void* bone = nullptr;
	if (FixedConstants::MapBone(n.c_str(), DataSize, &bone))
	{
		if (pVData) *pVData = bone;
		if (pGData) *pGData = bone;
		if (pPData) *pPData = bone;
		return;
	}
	ref_constant C_ = get_c(n);

	if (C_)
		constants.access_direct(&*C_, DataSize, pVData, pGData, pPData);
	else
	{
		if (pVData)	*pVData = 0;
		if (pGData)	*pGData = 0;
		if (pPData)	*pPData = 0;
	}
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
