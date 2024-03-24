#ifndef	dx10ShaderResourceStateCache_included
#define	dx10ShaderResourceStateCache_included
#pragma once

#include "../xrRenderDX10/dx11Backend.h"

class dx10ShaderResourceStateCache
{
public:
	dx10ShaderResourceStateCache();

	void	ResetDeviceState();

	void	Apply();

	void	SetPSResource( u32 uiSlot, ID3DShaderResourceView	*pRes );
	void	SetGSResource( u32 uiSlot, ID3DShaderResourceView	*pRes );
	void	SetVSResource( u32 uiSlot, ID3DShaderResourceView	*pRes );
	void	SetDSResource( u32 uiSlot, ID3DShaderResourceView	*pRes );
	void	SetHSResource( u32 uiSlot, ID3DShaderResourceView	*pRes );
	void	SetCSResource( u32 uiSlot, ID3DShaderResourceView	*pRes );

private:
	ID3DShaderResourceView	*m_PSViews[CBackend_DX11::mtMaxPixelShaderTextures];
	ID3DShaderResourceView	*m_GSViews[CBackend_DX11::mtMaxGeometryShaderTextures];
	ID3DShaderResourceView	*m_VSViews[CBackend_DX11::mtMaxVertexShaderTextures];
	ID3DShaderResourceView	*m_HSViews[CBackend_DX11::mtMaxHullShaderTextures];
	ID3DShaderResourceView	*m_DSViews[CBackend_DX11::mtMaxDomainShaderTextures];
	ID3DShaderResourceView	*m_CSViews[CBackend_DX11::mtMaxComputeShaderTextures];

	u32		m_uiMinPSView;
	u32		m_uiMaxPSView;

	u32		m_uiMinGSView;
	u32		m_uiMaxGSView;

	u32		m_uiMinVSView;
	u32		m_uiMaxVSView;

	u32		m_uiMinHSView;
	u32		m_uiMaxHSView;

	u32		m_uiMinDSView;
	u32		m_uiMaxDSView;

	u32		m_uiMinCSView;
	u32		m_uiMaxCSView;

	bool	m_bUpdatePSViews;
	bool	m_bUpdateGSViews;
	bool	m_bUpdateVSViews;
	bool	m_bUpdateHSViews;
	bool	m_bUpdateDSViews;
	bool	m_bUpdateCSViews;
};

extern	dx10ShaderResourceStateCache	SRVSManager;

#endif	//	dx10ShaderResourceStateCache_included