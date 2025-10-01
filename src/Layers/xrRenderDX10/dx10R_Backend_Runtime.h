#pragma once

#include "StateManager/dx10StateManager.h"
#include "StateManager/dx10ShaderResourceStateCache.h"

IC void CBackend::set_xform( u32 ID, const Fmatrix& M_ )
{
	stat.xforms			++;
	//	TODO: DX10: Implement CBackend::set_xform
}

IC void CBackend::set_RT(ID3DRenderTargetView* RT, u32 ID)
{
	if (RT!=pRT[ID])
	{
		pRT[ID]			= RT;

		//	Mark RT array dirty
		//	Reset all RT's here to allow RT to be bounded as input
		if (!m_bChangedRTorZB)
			RContext->OMSetRenderTargets(0, 0, 0);

		m_bChangedRTorZB = true;
	}
}

IC void	CBackend::set_ZB(IRHIDepthStencilView* ZB)
{
	if (ZB != pZB)
	{
		stat.target_zb++;
		pZB = ZB;

		//	Reset all RT's here to allow RT to be bounded as input
		if (!m_bChangedRTorZB)
			RContext->OMSetRenderTargets(0, 0, 0);

		m_bChangedRTorZB = true;
	}
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

IC D3D_PRIMITIVE_TOPOLOGY TranslateTopology(D3DPRIMITIVETYPE T)
{
	static	D3D_PRIMITIVE_TOPOLOGY translateTable[] =
	{
		D3D_PRIMITIVE_TOPOLOGY_UNDEFINED,		//	None
		D3D_PRIMITIVE_TOPOLOGY_POINTLIST,		//	D3DPT_POINTLIST = 1,
		D3D_PRIMITIVE_TOPOLOGY_LINELIST,		//	D3DPT_LINELIST = 2,
		D3D_PRIMITIVE_TOPOLOGY_LINESTRIP,		//	D3DPT_LINESTRIP = 3,
		D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST,	//	D3DPT_TRIANGLELIST = 4,
		D3D_PRIMITIVE_TOPOLOGY_TRIANGLESTRIP,	//	D3DPT_TRIANGLESTRIP = 5,
		D3D_PRIMITIVE_TOPOLOGY_UNDEFINED,		//	D3DPT_TRIANGLEFAN = 6,
	};

	VERIFY(T<sizeof(translateTable)/sizeof(translateTable[0]));
	VERIFY(T>=0);

	D3D_PRIMITIVE_TOPOLOGY	result = translateTable[T];

	VERIFY( result != D3D_PRIMITIVE_TOPOLOGY_UNDEFINED );

	return result;
}

IC u32 GetIndexCount(D3DPRIMITIVETYPE T, u32 iPrimitiveCount)
{
	switch (T)
	{
	case D3DPT_POINTLIST:
		return iPrimitiveCount;
	case D3DPT_LINELIST:
		return iPrimitiveCount*2;
	case D3DPT_LINESTRIP:
		return iPrimitiveCount+1;
	case D3DPT_TRIANGLELIST:
		return iPrimitiveCount*3;
	case D3DPT_TRIANGLESTRIP:
		return iPrimitiveCount+2;
	default: NODEFAULT;
	}
	return 0;
}

IC void CBackend::ApplyPrimitieTopology( D3D_PRIMITIVE_TOPOLOGY Topology )
{
	if ( m_PrimitiveTopology != Topology )
	{
		m_PrimitiveTopology = Topology;
		RContext->IASetPrimitiveTopology(m_PrimitiveTopology);
	}
}

IC void CBackend::Compute(UINT ThreadGroupCountX, UINT ThreadGroupCountY, UINT ThreadGroupCountZ)
{
	stat.calls++;

	SRVSManager.Apply();
	StateManager.Apply();
	//	State manager may alter constants
	constants.flush();
	RContext->Dispatch(ThreadGroupCountX,ThreadGroupCountY,ThreadGroupCountZ);
}

IC void CBackend::RenderInstancedIndexed(D3DPRIMITIVETYPE T_, u32 baseV, u32 startV, u32 countV, u32 startI, u32 PC, u32 instanceCount, u32 startInstanceLocation)
{
	D3D_PRIMITIVE_TOPOLOGY Topology = TranslateTopology(T_);
	u32	iIndexCount = GetIndexCount(T_, PC);

	//!!! HACK !!!
	if (GRHI->IsTessPass())
	{
		R_ASSERT(Topology == D3D11_PRIMITIVE_TOPOLOGY_TRIANGLELIST);
		Topology = D3D11_PRIMITIVE_TOPOLOGY_3_CONTROL_POINT_PATCHLIST;
	}

	stat.calls++;
	stat.verts += countV;
	stat.polys += PC;

	ApplyPrimitieTopology(Topology);

	SRVSManager.Apply();
	ApplyRTandZB();
	ApplyVertexLayout();
	StateManager.Apply();
	
	//	State manager may alter constants
	constants.flush();

	RContext->DrawIndexedInstanced(iIndexCount, instanceCount, startI, baseV, startInstanceLocation);
}

IC void CBackend::Render(D3DPRIMITIVETYPE T_, u32 baseV, u32 startV, u32 countV, u32 startI, u32 PC)
{
	D3D_PRIMITIVE_TOPOLOGY Topology = TranslateTopology(T_);
	u32	iIndexCount = GetIndexCount(T_, PC);

	//!!! HACK !!!
	if (GRHI->IsTessPass())
	{
		R_ASSERT(Topology == D3D11_PRIMITIVE_TOPOLOGY_TRIANGLELIST);
		Topology = D3D11_PRIMITIVE_TOPOLOGY_3_CONTROL_POINT_PATCHLIST;
	}

	stat.calls++;
	stat.verts += countV;
	stat.polys += PC;

	ApplyPrimitieTopology(Topology);
	SRVSManager.Apply();
	ApplyRTandZB();
	ApplyVertexLayout();
	StateManager.Apply();

	//	State manager may alter constants
	constants.flush();
	RContext->DrawIndexed(iIndexCount, startI, baseV);
}

IC void CBackend::Render(D3DPRIMITIVETYPE T_, u32 startV, u32 PC)
{
	//	TODO: DX10: Remove triangle fan usage from the engine
	if (T_ == D3DPT_TRIANGLEFAN)
		return;

	D3D_PRIMITIVE_TOPOLOGY Topology = TranslateTopology(T_);
	u32	iVertexCount = GetIndexCount(T_, PC);

	stat.calls++;
	stat.verts += 3*PC;
	stat.polys += PC;

	ApplyPrimitieTopology(Topology);
	SRVSManager.Apply();
	ApplyRTandZB();
	ApplyVertexLayout();
	StateManager.Apply();
	//	State manager may alter constants
	constants.flush();
	RContext->Draw(iVertexCount, startV);
}

IC void CBackend::Render_noIA(u32 iVertexCount)
{
	stat.calls++;
	stat.verts += iVertexCount;

	SRVSManager.Apply();
	ApplyRTandZB();

	//Unbind IA (VB, IB)
	RContext->IASetPrimitiveTopology(D3D11_PRIMITIVE_TOPOLOGY_TRIANGLELIST);
	RContext->IASetInputLayout(nullptr);

	StateManager.Apply();

	//State manager may alter constants
	constants.flush();

	RContext->Draw(iVertexCount, 0);
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

		CHK_DX(RDevice->CreateInputLayout(
			&decl->dx10_dcl_code[0],
			(u32)decl->dx10_dcl_code.size()-1,
			m_pInputSignature->GetBufferPointer(),
			m_pInputSignature->GetBufferSize(),
			&pLayout
			));

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
	if (m_bChangedRTorZB)
	{
		m_bChangedRTorZB = false;
		RContext->OMSetRenderTargets(sizeof(pRT)/sizeof(pRT[0]), pRT, pZB ? (ID3D11DepthStencilView*)pZB->GetRawDSV() : nullptr);
	}
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