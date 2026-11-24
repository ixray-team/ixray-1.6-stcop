/**************************************************************************************
* Copyright (C) 2026 IXRay-Team
* IX-Ray Renderer
***************************************************************************************
* Source code is licensed under the following terms:
*
* "Team" means the maintainers of the official IX-Ray repository.
*
* 1. IX-Ray Team License
*    The Team may copy, modify, merge, publish and distribute this Software
*    and its documentation, in source and binary form, without restriction.
*
* 2. Public Access License
*    Non-exclusive rights granted to everyone else.
*
*    Permitted rights:
*      a. Read and clone the source code.
*      b. Fork the repository and modify the source code for the sole purpose
*         of proposing the change to the official repository. By proposing it,
*         the author grants the Team a worldwide, perpetual, irrevocable,
*         royalty-free right to use, modify, relicense and distribute it.
*      c. Redistribute binaries built and published by the Team, unmodified
*         and together with this notice.
*
* Usage of this Software beyond the rights granted above is strictly prohibited,
* including distribution of the source code or of any modified version of it and
* distribution of binaries not built by the Team.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND. THE COPYRIGHT
* HOLDERS ARE NOT LIABLE FOR ANY CLAIM OR DAMAGE ARISING FROM IT.
*
* The above copyright notice and this license text must be included in all
* copies or substantial portions of the Software.
**************************************************************************************/
#pragma once
#include <d3d11.h>

class DX11ShaderResourceStateCache:
	public IRHIShaderResourceStateCache
{
public:
	DX11ShaderResourceStateCache(ID3D11DeviceContext* ContextPtr);

	virtual void ResetDeviceState() override;
	virtual void Apply() override;
	// Binds everything again on the next Apply: something else has bound views on the context
	void Restore();
		 
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