#pragma once

#include "DeviceRHI.h"

class CD3D11RenderTargetView :
	public IRenderTargetView
{
public:
	CD3D11RenderTargetView();
	~CD3D11RenderTargetView();

	HRESULT Create(IRHIResource* pResource, const SRenderTargetViewDesc* pDesc);

	// IRHIResource
	void GetType(eResourceDimension* pResourceDimension) override;
	void SetDebugName(const char* name) override;

	// IRenderTargetView
	void GetDesc(SRenderTargetViewDesc* desc) override;
	void GetResource(IRHIResource** ppResource) override;

	ID3D11RenderTargetView* GetRenderTargetView() { return m_pRenderTargetView; }

private:
	ID3D11RenderTargetView* m_pRenderTargetView;

	IRHIResource* m_pResource;
	SRenderTargetViewDesc m_ResourceDesc;

};

class CD3D11DepthStencilView :
	public IDepthStencilView
{
public:
	CD3D11DepthStencilView();
	~CD3D11DepthStencilView();

	HRESULT Create(IRHIResource* pResource, const SDepthStencilViewDesc* pDesc);

	// IRHIResource
	void GetType(eResourceDimension* pResourceDimension) override;
	void SetDebugName(const char* name) override;

	// IDepthStencilView
	void GetDesc(SDepthStencilViewDesc* desc) override;
	void GetResource(IRHIResource** ppResource) override;

	ID3D11DepthStencilView* GetDepthStencilView() { return m_pDepthStencilView; }

private:
	ID3D11DepthStencilView* m_pDepthStencilView;

	IRHIResource* m_pResource;
	SDepthStencilViewDesc m_ResourceDesc;

};