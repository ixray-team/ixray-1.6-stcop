#pragma once
#include "../../xrEngine/IRender_RHI.h"
#include "linker.h"
#include "PixEvent.h"
#include "TextureFormat.h"

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

	IRHITexture* CreateAPITexture( const TextureDesc* pTextureDesc, LPSUBRESOURCE_DATA pSubresourceData ) override;
	IRHIBuffer* CreateAPIBuffer(eBufferType bufferType, const void* pData, u32 DataSize, bool bImmutable) override;

	// Унаследовано через IRender_RHI
	void SetVertexBuffer(u32 StartSlot, IRHIBuffer* pVertexBuffer, const u32 Strides, const u32 Offsets) override;
	void SetIndexBuffer(IRHIBuffer* pIndexBuffer, bool Is32BitBuffer, u32 Offset) override;

	ERHITextureFormat GetRHIFormatFromAPI(int dxgiFormat) override;

public:
	int GetFeatureLevel();


	// Inherited via IRender_RHI
	void SetRenderTarget(u32 RenderTargetIndex, IRHISurface* pRenderTarget) override;

};

extern CRender_RHI g_RenderRHI_Implementation;
