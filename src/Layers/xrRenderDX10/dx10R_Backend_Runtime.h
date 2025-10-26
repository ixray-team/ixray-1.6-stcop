#pragma once
#include "StateManager/dx10StateManager.h"

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

ICF	bool CBackend::is_TessEnabled()
{
	return true;
}

IC void CBackend::Compute(UINT ThreadGroupCountX, UINT ThreadGroupCountY, UINT ThreadGroupCountZ)
{
	stat.calls++;

	GRHI->ShaderResourceCache->Apply();
	StateManager.Apply();
	//	State manager may alter constants
	constants.flush();
	RContext->Dispatch(ThreadGroupCountX,ThreadGroupCountY,ThreadGroupCountZ);
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
	ApplyRTandZB();
	ApplyVertexLayout();
	StateManager.Apply();
	
	//	State manager may alter constants
	constants.flush();

	RContext->DrawIndexedInstanced(iIndexCount, instanceCount, startI, baseV, startInstanceLocation);
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
	ApplyRTandZB();
    ApplyVertexLayout();
    StateManager.Apply();

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
    ApplyRTandZB();
    ApplyVertexLayout();
    StateManager.Apply();
    
    // State manager may alter constants
    constants.flush();
    GRHI->Draw(startV, PC);
}

IC void CBackend::Render_noIA(u32 iVertexCount)
{
    stat.calls++;
    stat.verts += iVertexCount;

    GRHI->ShaderResourceCache->Apply();
    ApplyRTandZB();

    // Use RHI primitive topology
    GRHI->SetPrimitiveTopology(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST);
    RContext->IASetInputLayout(nullptr);

    StateManager.Apply();

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

IC void	CBackend::set_Scissor(Irect* R)
{
	if (R)
	{
		StateManager.EnableScissoring();
		RECT* clip = (RECT*)R;
		RContext->RSSetScissorRects(1, clip);
	}
	else
	{
		StateManager.EnableScissoring(FALSE);
		RContext->RSSetScissorRects(0, 0);
	}
}

IC void CBackend::set_Stencil(u32 _enable, u32 _func, u32 _ref, u32 _mask, u32 _writemask, u32 _fail, u32 _pass, u32 _zfail)
{
	StateManager.SetStencil(_enable, _func, _ref, _mask, _writemask, _fail, _pass, _zfail);
}

IC  void CBackend::set_Z(u32 _enable)
{
	StateManager.SetDepthEnable(_enable);
}

IC  void CBackend::set_ZFunc(u32 _func)
{
	StateManager.SetDepthFunc(_func);
}

IC  void CBackend::set_AlphaRef(u32 _value)
{
	VERIFY(!"Not implemented.");
}

IC void	CBackend::set_ColorWriteEnable(u32 _mask )
{
	StateManager.SetColorWriteEnable(_mask);
}
ICF void CBackend::set_CullMode(u32 _mode)
{
	StateManager.SetCullMode(_mode);
}

IC void CBackend::ApplyVertexLayout()
{
	VERIFY(decl);
	VERIFY(m_pInputSignature);

	xr_map<ID3DBlob*, ID3DInputLayout*>::iterator	it;

	it = decl->vs_to_layout.find(m_pInputSignature);

	if (it==decl->vs_to_layout.end())
	{
		ID3DInputLayout* pLayout;

		CHK_DX(RDevice->CreateInputLayout
		(
			(D3D11_INPUT_ELEMENT_DESC*)decl->dx10_dcl_code.data(),
			decl->dx10_dcl_code.size(),
			m_pInputSignature->GetBufferPointer(),
			m_pInputSignature->GetBufferSize(),
			&pLayout
			)
		);

		it = decl->vs_to_layout.insert(
			std::pair<ID3DBlob*, ID3DInputLayout*>(m_pInputSignature, pLayout)).first;
	}

	if ( m_pInputLayout != it->second)
	{
		m_pInputLayout = it->second;
		RContext->IASetInputLayout(m_pInputLayout);
	}	
}

ICF void CBackend::set_VS(ref_vs& _vs)
{
	m_pInputSignature = _vs->signature->signature;
	GRHI->SetShader(_vs->vs, ERHI_SHADER_TYPE::VS);
}

ICF void CBackend::set_VS(SVS* _vs)
{
	m_pInputSignature = _vs->signature->signature;
	GRHI->SetShader(_vs->vs, ERHI_SHADER_TYPE::VS);
}

IC bool CBackend::CBuffersNeedUpdate( ref_cbuffer buf1[MaxCBuffers], ref_cbuffer buf2[MaxCBuffers], u32 &uiMin, u32 &uiMax)
{
	bool	bRes = false;
	int i=0;
	while ( (i<MaxCBuffers) && (buf1[i]==buf2[i]))
		++i;

	uiMin = i;

	for ( ; i<MaxCBuffers; ++i)
	{
		if (buf1[i]!=buf2[i])
		{
			bRes = true;
			uiMax = i;
		}
	}

	return bRes;
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
	LOD.unmap		();
	StateManager.UnmapConstants();

	if (0==C_)
		return;

	//	Setup constant tables
	{
		ref_cbuffer	aPixelConstants[MaxCBuffers];
		ref_cbuffer	aVertexConstants[MaxCBuffers];
		ref_cbuffer	aGeometryConstants[MaxCBuffers];
		ref_cbuffer	aHullConstants[MaxCBuffers];
		ref_cbuffer	aDomainConstants[MaxCBuffers];
		ref_cbuffer	aComputeConstants[MaxCBuffers];

		for (int i=0; i<MaxCBuffers; ++i)
		{
			aPixelConstants[i] = m_aPixelConstants[i];
			aVertexConstants[i] = m_aVertexConstants[i];
			aGeometryConstants[i] = m_aGeometryConstants[i];

			aHullConstants[i] = m_aHullConstants[i];
			aDomainConstants[i] = m_aDomainConstants[i];
			aComputeConstants[i] = m_aComputeConstants[i];

			m_aPixelConstants[i] = 0;
			m_aVertexConstants[i] = 0;
			m_aGeometryConstants[i] = 0;

			m_aHullConstants[i] = 0;
			m_aDomainConstants[i] = 0;
			m_aComputeConstants[i] = 0;
		}
		R_constant_table::cb_table::iterator	it	= C_->m_CBTable.begin();
		R_constant_table::cb_table::iterator	end	= C_->m_CBTable.end	();
		for (; it!=end; ++it)
		{
			//ID3DxxBuffer*	pBuffer = (it->second)->GetBuffer();
			u32				uiBufferIndex = it->first; 

			if ( (uiBufferIndex&CB_BufferTypeMask) == CB_BufferPixelShader)
			{
				VERIFY((uiBufferIndex&CB_BufferIndexMask)<MaxCBuffers);
				m_aPixelConstants[uiBufferIndex&CB_BufferIndexMask] = it->second;
			}
			else if ( (uiBufferIndex&CB_BufferTypeMask) == CB_BufferVertexShader)
			{
				VERIFY((uiBufferIndex&CB_BufferIndexMask)<MaxCBuffers);
				m_aVertexConstants[uiBufferIndex&CB_BufferIndexMask] = it->second;
			}
			else if ( (uiBufferIndex&CB_BufferTypeMask) == CB_BufferGeometryShader)
			{
				VERIFY((uiBufferIndex&CB_BufferIndexMask)<MaxCBuffers);
				m_aGeometryConstants[uiBufferIndex&CB_BufferIndexMask] = it->second;
			}
			else if ( (uiBufferIndex&CB_BufferTypeMask) == CB_BufferHullShader)
			{
				VERIFY((uiBufferIndex&CB_BufferIndexMask)<MaxCBuffers);
				m_aHullConstants[uiBufferIndex&CB_BufferIndexMask] = it->second;
			}
			else if ( (uiBufferIndex&CB_BufferTypeMask) == CB_BufferDomainShader)
			{
				VERIFY((uiBufferIndex&CB_BufferIndexMask)<MaxCBuffers);
				m_aDomainConstants[uiBufferIndex&CB_BufferIndexMask] = it->second;
			}
			else if ( (uiBufferIndex&CB_BufferTypeMask) == CB_BufferComputeShader)
			{
				VERIFY((uiBufferIndex&CB_BufferIndexMask)<MaxCBuffers);
				m_aComputeConstants[uiBufferIndex&CB_BufferIndexMask] = it->second;
			}
			else
				VERIFY("Invalid enumeration");
		}

		xr_vector<IRHIBuffer*> tempBuffer;
		tempBuffer.resize(MaxCBuffers);

		u32 uiMin;
		u32 uiMax;

		if (CBuffersNeedUpdate(m_aPixelConstants, aPixelConstants, uiMin, uiMax))
		{
			++uiMax;

			for (u32 i=uiMin; i<uiMax; ++i)
			{
				if (m_aPixelConstants[i])
					tempBuffer[i] = m_aPixelConstants[i]->GetBuffer();
				else
					tempBuffer[i] = 0;
			}

			GRHI->SetConstantBuffers(uiMin, uiMax - uiMin, tempBuffer, ERHI_SHADER_TYPE::PS);
		}
		

		if (CBuffersNeedUpdate(m_aVertexConstants, aVertexConstants, uiMin, uiMax))
		{
			++uiMax;

			for (u32 i=uiMin; i<uiMax; ++i)
			{
				if (m_aVertexConstants[i])
					tempBuffer[i] = m_aVertexConstants[i]->GetBuffer();
				else
					tempBuffer[i] = 0;
			}
			GRHI->SetConstantBuffers(uiMin, uiMax - uiMin, tempBuffer, ERHI_SHADER_TYPE::VS);
		}

			
		if (CBuffersNeedUpdate(m_aGeometryConstants, aGeometryConstants, uiMin, uiMax))
		{
			++uiMax;

			for (u32 i=uiMin; i<uiMax; ++i)
			{
				if (m_aGeometryConstants[i])
					tempBuffer[i] = m_aGeometryConstants[i]->GetBuffer();
				else
					tempBuffer[i] = 0;
			}
			GRHI->SetConstantBuffers(uiMin, uiMax-uiMin, tempBuffer, ERHI_SHADER_TYPE::CS);
		}

		if (CBuffersNeedUpdate(m_aHullConstants, aHullConstants, uiMin, uiMax))
		{
			++uiMax;

			for (u32 i=uiMin; i<uiMax; ++i)
			{
				if (m_aHullConstants[i])
					tempBuffer[i] = m_aHullConstants[i]->GetBuffer();
				else
					tempBuffer[i] = 0;
			}
			GRHI->SetConstantBuffers(uiMin, uiMax-uiMin, tempBuffer, ERHI_SHADER_TYPE::HS);
		}

		if (CBuffersNeedUpdate(m_aDomainConstants, aDomainConstants, uiMin, uiMax))
		{
			++uiMax;

			for (u32 i=uiMin; i<uiMax; ++i)
			{
				if (m_aDomainConstants[i])
					tempBuffer[i] = m_aDomainConstants[i]->GetBuffer();
				else
					tempBuffer[i] = 0;
			}
			GRHI->SetConstantBuffers(uiMin, uiMax-uiMin, tempBuffer, ERHI_SHADER_TYPE::DS);
		}

		if (CBuffersNeedUpdate(m_aComputeConstants, aComputeConstants, uiMin, uiMax))
		{
			++uiMax;

			for (u32 i=uiMin; i<uiMax; ++i)
			{
				if (m_aComputeConstants[i])
					tempBuffer[i] = m_aComputeConstants[i]->GetBuffer();
				else
					tempBuffer[i] = 0;
			}
			GRHI->SetConstantBuffers(uiMin, uiMax-uiMin, tempBuffer, ERHI_SHADER_TYPE::CS);
		}
	}

	// process constant-loaders
	R_constant_table::c_table::iterator	it	= C_->table.begin();
	R_constant_table::c_table::iterator	end	= C_->table.end	();
	for (; it!=end; it++)	
	{
		R_constant*		Cs	= &**it;
		VERIFY(Cs);
		if (Cs && Cs->handler)
			Cs->handler->setup(Cs);
	}
}

ICF void CBackend::ApplyRTandZB()
{
	GRHI->ApplyRenderTargetChange();
}

IC	void CBackend::get_ConstantDirect(shared_str& n, u32 DataSize, void** pVData, void** pGData, void** pPData)
{
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