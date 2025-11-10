#pragma once
#include "RHITypes.h"
#include "RHIEnums.h"

class IRHIDevice
{
public:
	void* RawDevice = nullptr;
	IRHIRenderTargetView* RenderRTV = nullptr;
	void* RenderSRV = nullptr;
	IRHIDepthStencilView* RenderDSV = nullptr;
	IRHIRenderTargetView* RenderTexture = nullptr;
	IRHIRenderTargetView* SwapChainRTV = nullptr;

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

	// Scissor rect
	virtual void SetScissorRect(Irect* R) = 0;

	// Read pixels from a render target into a contiguous buffer (RGBA8 or 32-bit per pixel layout)
	// Dst: pointer to destination buffer; DstSize: size of the destination buffer in bytes
	// OutWidth/OutHeight: returned width/height of the captured region
	// OutRowPitch: number of bytes per row written into Dst (may be Width*4)
	virtual bool ReadRenderTargetPixels(IRHIRenderTargetView* Rtv, void* Dst, u32 DstSize, u32& OutWidth, u32& OutHeight, u32& OutRowPitch) = 0;

	// Render Taget setup
	virtual void SetRenderTargets(u32 NumViews, IRHIRenderTargetView* const* ppRenderTargetViews) = 0;
	virtual void SetDSV(IRHIDepthStencilView* pDepthStencilView) = 0;
};