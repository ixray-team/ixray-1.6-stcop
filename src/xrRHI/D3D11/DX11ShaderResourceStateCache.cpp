#include "../RHI.h"
#include "DX11ShaderResourceStateCache.h"

DX11ShaderResourceStateCache::DX11ShaderResourceStateCache(ID3D11DeviceContext* ContextPtr) :
	RContext(ContextPtr)
{
	ResetDeviceState();
}

void DX11ShaderResourceStateCache::ResetDeviceState()
{
	ZeroMemory(m_PSViews, sizeof(m_PSViews));
	ZeroMemory(m_GSViews, sizeof(m_GSViews));
	ZeroMemory(m_VSViews, sizeof(m_VSViews));
	ZeroMemory(m_HSViews, sizeof(m_HSViews));
	ZeroMemory(m_DSViews, sizeof(m_DSViews));

	m_uiMinPSView = 0xFFFFFFFF;
	m_uiMaxPSView = 0xFFFFFFFF;

	m_uiMinGSView = 0xFFFFFFFF;
	m_uiMaxGSView = 0xFFFFFFFF;

	m_uiMinVSView = 0xFFFFFFFF;
	m_uiMaxVSView = 0xFFFFFFFF;

	m_uiMinHSView = 0xFFFFFFFF;
	m_uiMaxHSView = 0xFFFFFFFF;

	m_uiMinDSView = 0xFFFFFFFF;
	m_uiMaxDSView = 0xFFFFFFFF;

	m_bUpdatePSViews = false;
	m_bUpdateGSViews = false;
	m_bUpdateVSViews = false;
	m_bUpdateDSViews = false;
	m_bUpdateHSViews = false;
}

void DX11ShaderResourceStateCache::Apply()
{
	if (m_bUpdatePSViews)
	{
		RContext->PSSetShaderResources(m_uiMinPSView, m_uiMaxPSView - m_uiMinPSView + 1, &m_PSViews[m_uiMinPSView]);
		m_uiMinPSView = 0xFFFFFFFF;
		m_uiMaxPSView = 0xFFFFFFFF;
		m_bUpdatePSViews = false;
	}

	if (m_bUpdateGSViews)
	{
		RContext->GSSetShaderResources(m_uiMinGSView, m_uiMaxGSView - m_uiMinGSView + 1, &m_GSViews[m_uiMinGSView]);
		m_uiMinGSView = 0xFFFFFFFF;
		m_uiMaxGSView = 0xFFFFFFFF;
		m_bUpdateGSViews = false;
	}

	if (m_bUpdateVSViews)
	{
		RContext->VSSetShaderResources(m_uiMinVSView, m_uiMaxVSView - m_uiMinVSView + 1, &m_VSViews[m_uiMinVSView]);
		m_uiMinVSView = 0xFFFFFFFF;
		m_uiMaxVSView = 0xFFFFFFFF;
		m_bUpdateVSViews = false;
	}

	if (m_bUpdateHSViews)
	{
		RContext->HSSetShaderResources(m_uiMinHSView, m_uiMaxHSView - m_uiMinHSView + 1, &m_HSViews[m_uiMinHSView]);
		m_uiMinHSView = 0xFFFFFFFF;
		m_uiMaxHSView = 0xFFFFFFFF;
		m_bUpdateHSViews = false;
	}

	if (m_bUpdateDSViews)
	{
		RContext->DSSetShaderResources(m_uiMinDSView, m_uiMaxDSView - m_uiMinDSView + 1, &m_DSViews[m_uiMinDSView]);
		m_uiMinDSView = 0xFFFFFFFF;
		m_uiMaxDSView = 0xFFFFFFFF;
		m_bUpdateDSViews = false;
	}

	if (m_bUpdateCSViews)
	{
		RContext->CSSetShaderResources(m_uiMinCSView, m_uiMaxCSView - m_uiMinCSView + 1, &m_CSViews[m_uiMinCSView]);
		m_uiMinCSView = 0xFFFFFFFF;
		m_uiMaxCSView = 0xFFFFFFFF;
		m_bUpdateCSViews = false;
	}
}

void DX11ShaderResourceStateCache::SetPSResource(u32 uiSlot, IRHIShaderResourceView* pRes)
{
	ID3D11ShaderResourceView* SRV = pRes ? (ID3D11ShaderResourceView*)pRes->GetRawSRV() : nullptr;
	if (m_PSViews[uiSlot] != SRV)
	{
		m_PSViews[uiSlot] = SRV;
		if (m_bUpdatePSViews)
		{
			m_uiMinPSView = _min(m_uiMinPSView, uiSlot);
			m_uiMaxPSView = _max(m_uiMaxPSView, uiSlot);
		}
		else
		{
			m_bUpdatePSViews = true;
			m_uiMinPSView = uiSlot;
			m_uiMaxPSView = uiSlot;
		}
	}
}

void DX11ShaderResourceStateCache::SetGSResource(u32 uiSlot, IRHIShaderResourceView* pRes)
{
	ID3D11ShaderResourceView* SRV = pRes ? (ID3D11ShaderResourceView*)pRes->GetRawSRV() : nullptr;
	if (m_GSViews[uiSlot] != SRV)
	{
		m_GSViews[uiSlot] = SRV;
		if (m_bUpdateGSViews)
		{
			m_uiMinGSView = _min(m_uiMinGSView, uiSlot);
			m_uiMaxGSView = _max(m_uiMaxGSView, uiSlot);
		}
		else
		{
			m_bUpdateGSViews = true;
			m_uiMinGSView = uiSlot;
			m_uiMaxGSView = uiSlot;
		}
	}
}

void DX11ShaderResourceStateCache::SetVSResource(u32 uiSlot, IRHIShaderResourceView* pRes)
{
	ID3D11ShaderResourceView* SRV = pRes ? (ID3D11ShaderResourceView*)pRes->GetRawSRV() : nullptr;
	if (m_VSViews[uiSlot] != SRV)
	{
		m_VSViews[uiSlot] = SRV;
		if (m_bUpdateVSViews)
		{
			m_uiMinVSView = _min(m_uiMinVSView, uiSlot);
			m_uiMaxVSView = _max(m_uiMaxVSView, uiSlot);
		}
		else
		{
			m_bUpdateVSViews = true;
			m_uiMinVSView = uiSlot;
			m_uiMaxVSView = uiSlot;
		}
	}
}

void DX11ShaderResourceStateCache::SetHSResource(u32 uiSlot, IRHIShaderResourceView* pRes)
{
	ID3D11ShaderResourceView* SRV = pRes ? (ID3D11ShaderResourceView*)pRes->GetRawSRV() : nullptr;
	if (m_HSViews[uiSlot] != SRV)
	{
		m_HSViews[uiSlot] = SRV;
		if (m_bUpdateHSViews)
		{
			m_uiMinHSView = _min(m_uiMinHSView, uiSlot);
			m_uiMaxHSView = _max(m_uiMaxHSView, uiSlot);
		}
		else
		{
			m_bUpdateHSViews = true;
			m_uiMinHSView = uiSlot;
			m_uiMaxHSView = uiSlot;
		}
	}
}

void DX11ShaderResourceStateCache::SetDSResource(u32 uiSlot, IRHIShaderResourceView* pRes)
{
	ID3D11ShaderResourceView* SRV = pRes ? (ID3D11ShaderResourceView*)pRes->GetRawSRV() : nullptr;
	if (m_DSViews[uiSlot] != SRV)
	{
		m_DSViews[uiSlot] = SRV;
		if (m_bUpdateDSViews)
		{
			m_uiMinDSView = _min(m_uiMinDSView, uiSlot);
			m_uiMaxDSView = _max(m_uiMaxDSView, uiSlot);
		}
		else
		{
			m_bUpdateDSViews = true;
			m_uiMinDSView = uiSlot;
			m_uiMaxDSView = uiSlot;
		}
	}
}

void DX11ShaderResourceStateCache::SetCSResource(u32 uiSlot, IRHIShaderResourceView* pRes)
{
	ID3D11ShaderResourceView* SRV = pRes ? (ID3D11ShaderResourceView*)pRes->GetRawSRV() : nullptr;
	if (m_CSViews[uiSlot] != SRV)
	{
		m_CSViews[uiSlot] = SRV;
		if (m_bUpdateCSViews)
		{
			m_uiMinCSView = _min(m_uiMinCSView, uiSlot);
			m_uiMaxCSView = _max(m_uiMaxCSView, uiSlot);
		}
		else
		{
			m_bUpdateCSViews = true;
			m_uiMinCSView = uiSlot;
			m_uiMaxCSView = uiSlot;
		}
	}
}
