#include "stdafx.h"
#include "dx10SamplerStateCache.h"

#include "../dx10StateUtils.h"

using dx10StateUtils::operator==;

dx10SamplerStateCache	SSManager;

dx10SamplerStateCache::dx10SamplerStateCache():
	m_uiMaxAnisotropy(1), m_mipLodBias(0.0f)
{
	static const int iMaxRSStates = 10;
	m_StateArray.reserve(iMaxRSStates);
	ResetDeviceState();
}

dx10SamplerStateCache::~dx10SamplerStateCache()
{
	ClearStateArray();
}

dx10SamplerStateCache::SHandle dx10SamplerStateCache::GetState( D3D_SAMPLER_DESC& desc )
{
	SHandle	hResult;

	//	MaxAnisitropy is reset by ValidateState if not aplicable
	//	to the filter mode used.
	desc.MaxAnisotropy = m_uiMaxAnisotropy;
	desc.MipLODBias = m_mipLodBias;

	dx10StateUtils::ValidateState(desc);

	u32 crc = dx10StateUtils::GetHash(desc);

	hResult = FindState( desc, crc);

	if ( hResult == hInvalidHandle )
	{
		StateRecord rec{};
		rec.m_crc = crc;
		CreateState(desc, &rec.m_pState);
		hResult = (u32)m_StateArray.size();
		m_StateArray.push_back(rec);
	}

	return hResult;
}

void dx10SamplerStateCache::CreateState( StateDecs desc, IDeviceState** ppIState )
{
	CHK_DX(RDevice->CreateSamplerState( &desc, ppIState));
}

dx10SamplerStateCache::SHandle dx10SamplerStateCache::FindState( const StateDecs& desc, u32 StateCRC )
{
    u32 res = 0xffffffff;
	u32 i=0;

	for (; i<m_StateArray.size(); ++i)
	{
		if (m_StateArray[i].m_crc==StateCRC)
		{
			StateDecs	descCandidate;
			m_StateArray[i].m_pState->GetDesc(&descCandidate);
			if (descCandidate==desc)
				//return i;
				//	TEST
			{
				//return i;
                res = i;
                break;
			}
			//else
			//{
			//	VERIFY(0);
			//}
		}
	}

    return res != 0xffffffff ? i : (u32)hInvalidHandle;
}

void dx10SamplerStateCache::ClearStateArray()
{
	for (u32 i=0; i<m_StateArray.size(); ++i)
	{
		_RELEASE(m_StateArray[i].m_pState);
	}

	m_StateArray.resize(0);
}

void dx10SamplerStateCache::PrepareSamplerStates(
	HArray &samplers, 
	ID3DSamplerState	*pSS[D3D_COMMONSHADER_SAMPLER_SLOT_COUNT],
	SHandle				pCurrentState[D3D_COMMONSHADER_SAMPLER_SLOT_COUNT],
	u32	&uiMin,
	u32	&uiMax
) const
{
	//	It seems that sizeof pSS is 4 wor win32!
	ZeroMemory(pSS, sizeof(pSS[0])*D3D_COMMONSHADER_SAMPLER_SLOT_COUNT);

	for ( u32 i=0; i<samplers.size(); ++i )
	{
		if (samplers[i]!=hInvalidHandle)
		{
			VERIFY(samplers[i]<m_StateArray.size());
			pSS[i] = m_StateArray[samplers[i]].m_pState;
		}
	}

	uiMin = 0;
	uiMax = D3D_COMMONSHADER_SAMPLER_SLOT_COUNT-1;
}

void dx10SamplerStateCache::VSApplySamplers(HArray &samplers)
{
	ID3DSamplerState	*pSS[D3D_COMMONSHADER_SAMPLER_SLOT_COUNT];
	u32 uiMin;
	u32 uiMax;
	PrepareSamplerStates( samplers, pSS, m_aVSSamplers, uiMin, uiMax);
	RContext->VSSetSamplers(uiMin, uiMax-uiMin+1, &pSS[uiMin]);
}

void dx10SamplerStateCache::PSApplySamplers(HArray &samplers)
{
	ID3DSamplerState	*pSS[D3D_COMMONSHADER_SAMPLER_SLOT_COUNT];
	u32 uiMin;
	u32 uiMax;
	PrepareSamplerStates( samplers, pSS, m_aPSSamplers, uiMin, uiMax);
	RContext->PSSetSamplers(uiMin, uiMax-uiMin+1, &pSS[uiMin]);
}

void dx10SamplerStateCache::GSApplySamplers(HArray &samplers)
{
	ID3DSamplerState	*pSS[D3D_COMMONSHADER_SAMPLER_SLOT_COUNT];
	u32 uiMin;
	u32 uiMax;
	PrepareSamplerStates( samplers, pSS, m_aGSSamplers, uiMin, uiMax);
	RContext->GSSetSamplers(uiMin, uiMax-uiMin+1, &pSS[uiMin]);
}

void dx10SamplerStateCache::HSApplySamplers(HArray &samplers)
{
	ID3DSamplerState	*pSS[D3D_COMMONSHADER_SAMPLER_SLOT_COUNT];
	u32 uiMin;
	u32 uiMax;
	PrepareSamplerStates( samplers, pSS, m_aHSSamplers, uiMin, uiMax);
	RContext->HSSetSamplers(uiMin, uiMax-uiMin+1, &pSS[uiMin]);
}

void dx10SamplerStateCache::DSApplySamplers(HArray &samplers)
{
	ID3DSamplerState	*pSS[D3D_COMMONSHADER_SAMPLER_SLOT_COUNT];
	u32 uiMin;
	u32 uiMax;
	PrepareSamplerStates( samplers, pSS, m_aDSSamplers, uiMin, uiMax);
	RContext->DSSetSamplers(uiMin, uiMax-uiMin+1, &pSS[uiMin]);
}

void dx10SamplerStateCache::CSApplySamplers(HArray &samplers)
{
	ID3DSamplerState	*pSS[D3D_COMMONSHADER_SAMPLER_SLOT_COUNT];
	u32 uiMin;
	u32 uiMax;
	PrepareSamplerStates( samplers, pSS, m_aCSSamplers, uiMin, uiMax);
	RContext->CSSetSamplers(uiMin, uiMax-uiMin+1, &pSS[uiMin]);
}


void dx10SamplerStateCache::SetMaxAnisotropy( UINT uiMaxAniso)
{
	clamp( uiMaxAniso, (u32)1, (u32)16);

	if (m_uiMaxAnisotropy == uiMaxAniso) {
		return;
	}

	m_uiMaxAnisotropy = uiMaxAniso;
	for (u32 i = 0; i < m_StateArray.size(); ++i)
	{
		StateRecord& rec = m_StateArray[i];
		StateDecs desc;

		if (rec.m_pState != nullptr)
		{
			rec.m_pState->GetDesc(&desc);
			desc.MaxAnisotropy = m_uiMaxAnisotropy;
			dx10StateUtils::ValidateState(desc);
			rec.m_pState->Release();
			CreateState(desc, &rec.m_pState);
		}
	}
}

void dx10SamplerStateCache::SetMipLodBias(float mipMapLodBias) 
{
	if (ps_r_scale_mode > 1) {
		mipMapLodBias += std::min(0.0f, log2(RCache.get_width() / RCache.get_target_width()) - 1.0f + EPS);
	}

	clamp(mipMapLodBias, -3.0f, 3.0f);

	if (m_mipLodBias == mipMapLodBias) {
		return;
	}

	m_mipLodBias = mipMapLodBias;

	for (u32 i = 0; i < m_StateArray.size(); ++i)
	{
		StateRecord& rec = m_StateArray[i];
		StateDecs desc{};

		if (rec.m_pState != nullptr)
		{
			rec.m_pState->GetDesc(&desc);

			if(desc.Filter == D3D_FILTER_ANISOTROPIC) {
				desc.MipLODBias = m_mipLodBias;
			}
			else {
				desc.MipLODBias = 0.0f;
			}

			dx10StateUtils::ValidateState(desc);
			rec.m_pState->Release();

			CreateState(desc, &rec.m_pState);
		}
	}
}

void dx10SamplerStateCache::ResetDeviceState()
{
	for (int i=0; i<sizeof(m_aPSSamplers)/sizeof(m_aPSSamplers[0]); ++i)
	{
		m_aPSSamplers[i] = (SHandle)hInvalidHandle;
		m_aVSSamplers[i] = (SHandle)hInvalidHandle;
		m_aGSSamplers[i] = (SHandle)hInvalidHandle;
		m_aHSSamplers[i] = (SHandle)hInvalidHandle;
		m_aDSSamplers[i] = (SHandle)hInvalidHandle;
		m_aCSSamplers[i] = (SHandle)hInvalidHandle;
	}
}