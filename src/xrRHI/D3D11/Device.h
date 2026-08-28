#pragma once
#include <d3d11_4.h>

#include "../RHI.h"
#include "DX11Texture.h"
#include "DX11Buffer.h"

class InternalDevice11:
	public IRHIDevice
{
public:
	InternalDevice11();
	~InternalDevice11();

	// Inherited via IRHIDevice

	virtual void ResizeBuffers(u32 Width, u32 Height) override;
	virtual void ClearTarget(void* Target, ERTColor Transparent) override;
	virtual void ClearTarget(void* Target, const float* Color) override;
	virtual void ClearDepthStencil(IRHIDepthStencilView* View, ERHI_CLEAR_TARGET TargetFlags, float Depth, u8 Stencil) override;
	virtual void GenerateMips(IRHIShaderResourceView* SRV) override;
	virtual void Present() override;

	virtual IRHITextureFactory* GetTextureFactory() override;
	virtual void SetTextureFactory(IRHITextureFactory* factory) override;
	virtual void CopySurface(IRHISurface* Dest, IRHISurface* Source) override;
	virtual void CopySurface(IRHIRenderTargetView* Dest, IRHIRenderTargetView* Source) override;
	virtual void SetViewport(RHIViewport& VP) override;

	IRHIBuffer* CreateBuffer(const RHIBufferDesc& desc, const RHIBufferSubresource* pSubresource) override;

	// Scissor rect
	void SetScissorRect(Irect* R) override;

	void SetRenderTargets(u32 NumViews, IRHIRenderTargetView* const* ppRenderTargetViews, IRHIUnorderedAccessView* const* ppRenderUAViews) override;
	void SetDSV(IRHIDepthStencilView* pDepthStencilView) override;

	// Readback helper
	virtual bool ReadRenderTargetPixels(IRHIRenderTargetView* Rtv, void* Dst, u32 DstSize, u32& OutWidth, u32& OutHeight, u32& OutRowPitch) override;

    // Drawing methods
    virtual void SetPrimitiveTopology(ERHI_PRIMITIVE_TOPOLOGY topology) override;
    virtual void DrawIndexed(u32 baseVertex, u32 startVertex, u32 vertexCount, u32 startIndex, u32 primitiveCount) override;
    virtual void Draw(u32 startVertex, u32 primitiveCount) override;
    virtual void DrawIndexedInstanced(u32 baseVertex, u32 startVertex, u32 vertexCount, u32 startIndex, u32 primitiveCount, u32 instanceCount, u32 startInstanceLocation) override;
    virtual void DrawNoInputAssembly(u32 vertexCount) override;

    // Context helpers
    ID3D11DeviceContext* GetImmediateContext() const;
    ID3D11DeviceContext* CreateDeferredContext();
    void ReleaseDeferredContext(ID3D11DeviceContext* context);

private:
    D3D_PRIMITIVE_TOPOLOGY d3dTopology = D3D_PRIMITIVE_TOPOLOGY_UNDEFINED;
    ERHI_PRIMITIVE_TOPOLOGY currentTopology = (ERHI_PRIMITIVE_TOPOLOGY)-1;
	IRHIDepthStencilView* DepthStencilView = nullptr;

public:
	IDXGISwapChain* HWSwapchain = nullptr;
	ID3D11DeviceContext* HWRenderContext = nullptr;

private:
	bool CreateD3D11();
	void DestroyD3D11();
	bool UpdateBuffersD3D11();
	
	DX11TextureFactory* TextureFactory = nullptr;
};