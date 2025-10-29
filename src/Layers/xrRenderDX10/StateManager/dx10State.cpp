#include "stdafx.h"
#include "dx10State.h"
#include "../dx10StateUtils.h"

dx10State::dx10State() : 
	m_pRasterizerState(0),
	m_pDepthStencilState(0),
	m_pBlendState(0),
	m_uiStencilRef(UINT(-1)),
	m_uiAlphaRef(0)
{
}

dx10State::~dx10State()
{
	//	m_pRasterizerState is a weak link
	//	m_pDepthStencilState is a weak link
	//	m_pBlendState is a weak link
}

dx10State* dx10State::Create(SimulatorStates& state_code)
{
	dx10State *pState = new dx10State();
	state_code.UpdateState(*pState);

	D3D_RASTERIZER_DESC DescRS = {};
	D3D_DEPTH_STENCIL_DESC DescDS = {};
	D3D_BLEND_DESC DescBS = {};

	dx10StateUtils::ResetDescription(DescRS);
	dx10StateUtils::ResetDescription(DescDS);
	dx10StateUtils::ResetDescription(DescBS);

	state_code.UpdateDesc(DescRS);
	state_code.UpdateDesc(DescDS);
	state_code.UpdateDesc(DescBS);
	dx10StateUtils::ValidateState(DescDS);
	dx10StateUtils::ValidateState(DescBS);

	pState->m_pRasterizerState = (ID3DRasterizerState*)GRHI->StateManager->GetCache(ERHI_STATE_CACHE_TYPE::RS, &DescRS);
	pState->m_pDepthStencilState = (ID3DDepthStencilState*)GRHI->StateManager->GetCache(ERHI_STATE_CACHE_TYPE::DS, &DescDS);
	pState->m_pBlendState = (ID3DBlendState*)GRHI->StateManager->GetCache(ERHI_STATE_CACHE_TYPE::BS, &DescBS);;
	//ID3DxxDevice::CreateSamplerState

	//	Create samplers here
	{
		InitSamplers( pState->m_VSSamplers, state_code, CTexture::rstVertex);
		InitSamplers( pState->m_PSSamplers, state_code, CTexture::rstPixel);
		InitSamplers( pState->m_GSSamplers, state_code, CTexture::rstGeometry);
		InitSamplers( pState->m_HSSamplers, state_code, CTexture::rstHull);
		InitSamplers( pState->m_DSSamplers, state_code, CTexture::rstDomain);
		InitSamplers( pState->m_CSSamplers, state_code, CTexture::rstCompute);
	}


	return pState;
}

HRESULT dx10State::Apply()
{
	VERIFY(m_pRasterizerState);
	GRHI->StateManager->SetRasterizerState(m_pRasterizerState);
	VERIFY(m_pDepthStencilState);
	GRHI->StateManager->SetDepthStencilState(m_pDepthStencilState);
	if( m_uiStencilRef != -1 )
		GRHI->StateManager->SetStencilRef(m_uiStencilRef);
	VERIFY(m_pBlendState);
	GRHI->StateManager->SetBlendState(m_pBlendState);
	GRHI->StateManager->SetAlphaRef(m_uiAlphaRef);

	SSManager.GSApplySamplers(m_GSSamplers);
	SSManager.VSApplySamplers(m_VSSamplers);
	SSManager.PSApplySamplers(m_PSSamplers);
	SSManager.HSApplySamplers(m_HSSamplers);
	SSManager.DSApplySamplers(m_DSSamplers);
	SSManager.CSApplySamplers(m_CSSamplers);

	return S_OK;
}

void dx10State::Release()
{
	dx10State	*pState = this;
	xr_delete<dx10State> (pState);
}

void dx10State::InitSamplers(tSamplerHArray& SamplerArray, SimulatorStates& state_code, int iBaseSamplerIndex)
{
	RHISampleDesc descArray[RHI_COMMONSHADER_SAMPLER_SLOT_COUNT];
	bool SamplerUsed[RHI_COMMONSHADER_SAMPLER_SLOT_COUNT];

	for (int i = 0; i < RHI_COMMONSHADER_SAMPLER_SLOT_COUNT; ++i)
	{
		SamplerUsed[i] = false;
		dx10StateUtils::ResetDescription(descArray[i]);
	}

	state_code.UpdateDesc(descArray, SamplerUsed, iBaseSamplerIndex);

	int iMaxSampler = RHI_COMMONSHADER_SAMPLER_SLOT_COUNT - 1;
	for (; iMaxSampler > -1; --iMaxSampler)
	{
		if (SamplerUsed[iMaxSampler])
			break;
	}

	if (iMaxSampler > -1)
	{
		SamplerArray.reserve(iMaxSampler + 1);
		for (int i = 0; i <= iMaxSampler; ++i)
		{
			if (SamplerUsed[i])
				SamplerArray.push_back(SSManager.GetState(descArray[i]));
			else
				SamplerArray.push_back(u32(dx10SamplerStateCache::hInvalidHandle));
		}
	}
}