#pragma once
#include <d3d11.h>

class DX11ShaderResourceStateCache:
	public IRHIShaderResourceStateCache
{
public:
	DX11ShaderResourceStateCache(ID3D11DeviceContext* ContextPtr);

	virtual void ResetDeviceState() override;
	virtual void Apply() override;
		 
	virtual void SetPSResource(u32 uiSlot, IRHIShaderResourceView* pRes) override;
	virtual void SetGSResource(u32 uiSlot, IRHIShaderResourceView* pRes) override;
	virtual void SetVSResource(u32 uiSlot, IRHIShaderResourceView* pRes) override;
	virtual void SetDSResource(u32 uiSlot, IRHIShaderResourceView* pRes) override;
	virtual void SetHSResource(u32 uiSlot, IRHIShaderResourceView* pRes) override;
	virtual void SetCSResource(u32 uiSlot, IRHIShaderResourceView* pRes) override;

private:
	ID3D11DeviceContext* RContext = nullptr;

	ID3D11ShaderResourceView *m_PSViews[(u32)ERHI_MAX_TEXTURE::PixelShaderTextures];
	ID3D11ShaderResourceView *m_GSViews[(u32)ERHI_MAX_TEXTURE::GeometryShaderTextures];
	ID3D11ShaderResourceView *m_VSViews[(u32)ERHI_MAX_TEXTURE::VertexShaderTextures];
	ID3D11ShaderResourceView *m_HSViews[(u32)ERHI_MAX_TEXTURE::HullShaderTextures];
	ID3D11ShaderResourceView *m_DSViews[(u32)ERHI_MAX_TEXTURE::DomainShaderTextures];
	ID3D11ShaderResourceView *m_CSViews[(u32)ERHI_MAX_TEXTURE::ComputeShaderTextures];

	u32 m_uiMinPSView;
	u32 m_uiMaxPSView;
	    
	u32 m_uiMinGSView;
	u32 m_uiMaxGSView;
	    
	u32 m_uiMinVSView;
	u32 m_uiMaxVSView;
	    
	u32 m_uiMinHSView;
	u32 m_uiMaxHSView;
	    
	u32 m_uiMinDSView;
	u32 m_uiMaxDSView;
	    
	u32 m_uiMinCSView;
	u32 m_uiMaxCSView;

	bool m_bUpdatePSViews;
	bool m_bUpdateGSViews;
	bool m_bUpdateVSViews;
	bool m_bUpdateHSViews;
	bool m_bUpdateDSViews;
	bool m_bUpdateCSViews;
};