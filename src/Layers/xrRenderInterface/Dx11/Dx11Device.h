#pragma once

#include <d3d11_1.h>

class CRenderRHI_DX11 :
	public IRender_RHI
{

public:
	CRenderRHI_DX11();
	~CRenderRHI_DX11();

	void Create(void* renderDevice, void* renderContext);

	ITexture1D* CreateTexture1D(const STexture1DDesc& textureDesc, const SubresourceData* pSubresourceDesc) override;
	ITexture2D* CreateTexture2D(const STexture2DDesc& textureDesc, const SubresourceData* pSubresourceDesc) override;
	ITexture3D* CreateTexture3D(const STexture3DDesc& textureDesc, const SubresourceData* pSubresourceDesc) override;

	IRenderTargetView* CreateRenderTargetView(IRHIResource* pResource, const SRenderTargetViewDesc* pDesc) override;
	IDepthStencilView* CreateDepthStencilView(IRHIResource* pResource, const SDepthStencilViewDesc* pDesc) override;

	IBuffer* CreateBuffer(eBufferType bufferType, const void* pData, u32 DataSize, bool bImmutable) override;

	void SetVertexBuffer(u32 StartSlot, IBuffer* pVertexBuffer, const u32 Stride, const u32 Offset) override;
	void SetIndexBuffer(IBuffer* pIndexBuffer, bool Is32BitBuffer, u32 Offset) override;

	void VSSetConstantBuffers(u32 StartSlot, u32 NumBuffers, IBuffer* const* ppConstantBuffers) override;
	void PSSetConstantBuffers(u32 StartSlot, u32 NumBuffers, IBuffer* const* ppConstantBuffers) override;
	void HSSetConstantBuffers(u32 StartSlot, u32 NumBuffers, IBuffer* const* ppConstantBuffers) override;
	void CSSetConstantBuffers(u32 StartSlot, u32 NumBuffers, IBuffer* const* ppConstantBuffers) override;
	void DSSetConstantBuffers(u32 StartSlot, u32 NumBuffers, IBuffer* const* ppConstantBuffers) override;
	void GSSetConstantBuffers(u32 StartSlot, u32 NumBuffers, IBuffer* const* ppConstantBuffers) override;

	void ClearRenderTargetView(IRenderTargetView* pRenderTargetView, const float ColorRGBA[4]) override;
	void ClearDepthStencilView(IDepthStencilView* pDepthStencilView, u32 ClearFlags, float Depth, u8 Stencil) override;

	// Note: maximum is 8 render targets.
	void SetRenderTargets(u32 NumViews, IRenderTargetView* const* ppRenderTargetViews, IDepthStencilView* pDepthStencilView) override;

	void CopyResource(IRHIResource* pDstResource, IRHIResource* pSrcResource) override;

	// DX11 Stuff
	ID3D11Device* GetDevice();
	ID3D11DeviceContext* GetDeviceContext();

private:
	D3D_FEATURE_LEVEL FeatureLevel = D3D_FEATURE_LEVEL::D3D_FEATURE_LEVEL_11_0;
	IDXGISwapChain* HWSwapchain = nullptr;


	ID3D11Device* HWRenderDevice = nullptr;
	ID3D11DeviceContext* HWRenderContext = nullptr;

	ID3DUserDefinedAnnotation* RenderAnnotation = nullptr;

	ID3D11Texture2D* RenderTexture = nullptr;
	ID3D11RenderTargetView* RenderRTV = nullptr;

	ID3D11DepthStencilView* RenderDSV = nullptr;
	ID3D11RenderTargetView* SwapChainRTV = nullptr;

protected:
	virtual bool Create() override;
	virtual void Destroy() override;

	virtual bool UpdateBuffers() override; 
	virtual void ResizeBuffers(u16 Width, u16 Height) override;

	virtual void CreateRDoc() override;
};

// D3D11 Stuff
D3D11_MAP GetD3D11Map(eBufferMapping Mapping);
u32 GetD3D11BindFlags(eBufferType bufferType);

extern CRenderRHI_DX11 g_RenderRHI_DX11Implementation;