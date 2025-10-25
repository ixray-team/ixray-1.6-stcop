#pragma once
#include "RHITypes.h"
#include "RHIEnums.h"

class IRHIDevice
{
public:
    void* RawDevice = nullptr;
    void* RenderRTV = nullptr;
    void* RenderSRV = nullptr;
    IRHIDepthStencilView* RenderDSV = nullptr;
    void* RenderTexture = nullptr;
    void* SwapChainRTV = nullptr;

    float RenderScale = 1.f;
    u32 FeatureLevel;
    
    // Texture factory
    IRHITextureFactory* TextureFactory = nullptr;

    // Drawing methods
    virtual void SetPrimitiveTopology(ERHI_PRIMITIVE_TOPOLOGY topology) = 0;
    virtual void DrawIndexed(u32 baseVertex, u32 startVertex, u32 vertexCount, u32 startIndex, u32 primitiveCount) = 0;
    virtual void Draw(u32 startVertex, u32 primitiveCount) = 0;

    virtual void DrawIndexedInstanced(
        u32 baseVertex, u32 startVertex, u32 vertexCount,
        u32 startIndex, u32 primitiveCount,
        u32 instanceCount, u32 startInstanceLocation) = 0;
    
    virtual void DrawNoInputAssembly(u32 vertexCount) = 0;

public:
	virtual ~IRHIDevice() = default;
	virtual void ResizeBuffers(u32 Width, u32 Height) = 0;
	virtual void ClearTarget(void* Target, ERTColor Transparent) = 0;
	virtual void ClearTarget(void* Target, const float* Color) = 0;
	virtual void ClearDepthStencil(IRHIDepthStencilView* View, ERHI_CLEAR_TARGET TargetFlags, float Depth, u8 Stencil) = 0;
	virtual void GenerateMips(IRHIShaderResourceView* SRV) = 0;
	virtual void Present() = 0;
	virtual void CopySurface(IRHISurface* Dest, IRHISurface* Source) = 0;
	virtual void CopySurface(IRHIRenderTargetView* Dest, IRHIRenderTargetView* Source) = 0;

	// Texture management
	virtual IRHITextureFactory* GetTextureFactory() = 0;
	virtual void SetTextureFactory(IRHITextureFactory* factory) = 0;
	virtual void SetViewport(RHIViewport& VP) = 0;

	// Buffer stuff
	virtual IRHIBuffer* CreateBuffer(const RHIBufferDesc& desc = {}, const RHIBufferSubresource* pSubresource = nullptr) = 0;

    // Render Taget setup
    virtual void SetRenderTargets(u32 NumViews, IRHIRenderTargetView* const* ppRenderTargetViews, IRHIDepthStencilView* pDepthStencilView) = 0;
};