#pragma once
#include "IRHITypes.h"

class IRHIDevice
{
public:
	void* RawDevice = nullptr;
	void* RenderRTV = nullptr;
	void* RenderSRV = nullptr;
	void* RenderDSV = nullptr;
	void* RenderTexture = nullptr;
	void* SwapChainRTV = nullptr;

	float RenderScale = 1.f;
	u32 FeatureLevel;
	
	// Texture factory
	IRHITextureFactory* TextureFactory = nullptr;

public:
	virtual void ResizeBuffers(u32 Width, u32 Height) = 0;
	virtual void ClearTarget(void* Target, ERTColor Transparent) = 0;
	virtual void Present() = 0;
	virtual void CopySurface(IRHISurface* Dest, IRHISurface* Source) = 0;
	virtual void CopySurface(IRHIRenderTargetView* Dest, IRHIRenderTargetView* Source) = 0;

	// Texture management
	virtual IRHITextureFactory* GetTextureFactory() = 0;
	virtual void SetTextureFactory(IRHITextureFactory* factory) = 0;
	virtual void SetViewport(RHIViewport& VP) = 0;

	// Buffer stuff
	virtual IRHIBuffer* CreateBuffer(const RHIBufferDesc& desc = {}, const RHIBufferSubresource* pSubresource = nullptr) = 0;
};