#pragma once
#include "../../xrEngine/IRender_RHI.h"
#include "linker.h"
#include "PixEvent.h"

extern void* HWRenderDevice;
extern void* HWRenderContext;
extern void* HWSwapchain;

extern void* RenderTexture;
extern void* RenderSRV;
extern void* RenderDSV;

extern void* RenderRTV;
extern void* SwapChainRTV;

class RHI_API CRender_RHI:
	public IRender_RHI
{
public:
	CRender_RHI();
	~CRender_RHI();

public:
	virtual bool Create(APILevel);
	virtual bool UpdateBuffers();
	virtual void ResizeBuffers(u16 Width, u16 Height);
	virtual void Destroy();

	virtual void FillModes();

	virtual void* GetRenderSRV();
	virtual void* GetRenderDevice();
	virtual void* GetRenderContext();
	virtual void* GetRenderTexture();
	virtual void* GetDepthTexture();
	virtual void* GetSwapchainTexture();
	virtual void* GetSwapchain();

	IRender_Texture* CreateAPITexture( const TextureDesc* pTextureDesc, const void* pData, const int size ) override;
	IRender_BufferBase* CreateAPIBuffer(eBufferType bufferType, const void* pData, u32 DataSize, bool bImmutable) override;

public:
	int GetFeatureLevel();
};