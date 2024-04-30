#include "DeviceRHI.h"
#include "Dx9/Dx9API.h"
#include "Dx11/Dx11API.h"
#include "DxGI/DxgiAPI.h"
#include <dxgi.h>

void* HWRenderDevice = nullptr;
void* HWRenderContext = nullptr;
void* HWSwapchain = nullptr;

void* RenderTexture = nullptr;
void* RenderSRV = nullptr;
void* RenderRTV = nullptr;

IRHIDepthStencilView* RenderDSV = nullptr;
void* SwapChainRTV = nullptr;

CRender_RHI g_RenderRHI_Implementation;

CRender_RHI::CRender_RHI()
{
}

CRender_RHI::~CRender_RHI()
{
}

bool CRender_RHI::Create(APILevel NewAPI)
{
    API = NewAPI;

    switch (API)
    {
    case APILevel::DX9:  return CreateD3D9();
    case APILevel::DX11: return CreateD3D11();
    }

    return false;
}

bool CRender_RHI::UpdateBuffers()
{
    switch (API)
    {
    case APILevel::DX9:  return UpdateBuffersD3D9();
    case APILevel::DX11: return UpdateBuffersD3D11();
    }

    return false;
}

void CRender_RHI::ResizeBuffers(u16 Width, u16 Height)
{
    switch (API)
    {
    case APILevel::DX9:  ResizeBuffersD3D9(Width, Height); break;
    case APILevel::DX11: ResizeBuffersD3D11(Width, Height); break;
    }
}

void CRender_RHI::Destroy()
{
    switch (API)
    {
    case APILevel::DX9:  DestroyD3D9(); break;
    case APILevel::DX11: DestroyD3D11(); break;
    }
}

void* CRender_RHI::GetRenderSRV()
{
    return RenderSRV;
}

void CRender_RHI::FillModes()
{
    fill_vid_mode_list();
}

void* CRender_RHI::GetRenderDevice()
{
    return HWRenderDevice;
}

void* CRender_RHI::GetRenderContext()
{
    return HWRenderContext;
}

void* CRender_RHI::GetRenderTexture()
{
    // FX: Use ImGui render for Debug Draw mode
#ifdef DEBUG_DRAW
    return RenderRTV;
#else
    return SwapChainRTV;
#endif
}

void* CRender_RHI::GetDepthTexture()
{
    return RenderDSV;
}

void* CRender_RHI::GetSwapchainTexture()
{
    return SwapChainRTV;
}

void* CRender_RHI::GetSwapchain()
{
    return HWSwapchain;
}

IRHIDepthStencilView* CRender_RHI::CreateAPIDepthStencilSurface(u32 Width, u32 Height, ERHITextureFormat Format, u32 MultiSample, u32 MultisampleQuality, bool Discard)
{
    IRHIDepthStencilView* Ptr = nullptr;
    switch (API)
    {
    case APILevel::DX9:  Ptr = CreateD3D9DepthStencilSurface(Width, Height, Format, MultiSample, MultisampleQuality, Discard);  break;
  //  case APILevel::DX11: Ptr = CreateD3D11Texture(pTextureDesc, pSubresourceData); break;
    }

    return Ptr;
}

IRHITexture* CRender_RHI::CreateAPITexture( const TextureDesc* pTextureDesc, LPSUBRESOURCE_DATA pSubresourceData )
{
    IRHITexture* Ptr = nullptr;
    switch (API)
    {
    case APILevel::DX9:  Ptr = CreateD3D9Texture(pTextureDesc, pSubresourceData);  break;
    case APILevel::DX11: Ptr = CreateD3D11Texture(pTextureDesc, pSubresourceData); break;
    }

#ifdef DEBUG
    TextureList.emplace_back(Ptr);
#endif

    return Ptr;
}

IRHISurface* CRender_RHI::CreateAPIOffscreenPlainSurface(u32 Width, u32 Height, ERHITextureFormat Format, bool DefaultPool)
{
    switch (API)
    {
    case APILevel::DX9:
        return CreateOffscreenPlainSurfaceD3D9(Width, Height, Format, DefaultPool);
    }

    return nullptr;
}

IRHIBuffer* CRender_RHI::CreateAPIBuffer(eBufferType bufferType, const void* pData, u32 DataSize, bool bImmutable)
{
    switch (API)
    {
    case APILevel::DX9:
        return CreateD3D9Buffer(bufferType, pData, DataSize, bImmutable);
    case APILevel::DX11:
        return CreateD3D11Buffer(bufferType, pData, DataSize, bImmutable);
    }

    return nullptr;
}

int CRender_RHI::GetFeatureLevel()
{
    return 0xb100;
}

void CRender_RHI::SetVertexBuffer(u32 StartSlot, IRHIBuffer* pVertexBuffer, const u32 Strides, const u32 Offsets)
{
    switch (API)
    {
    case APILevel::DX9:
        return SetVertexBufferD3D9(StartSlot, pVertexBuffer, Strides, Offsets);
    case APILevel::DX11:
        return SetVertexBufferD3D11(StartSlot, pVertexBuffer, Strides, Offsets);
    }
}

void CRender_RHI::SetIndexBuffer(IRHIBuffer* pIndexBuffer, bool Is32BitBuffer, u32 Offset)
{
    switch (API)
    {
    case APILevel::DX9:
        return SetIndexBufferD3D9(pIndexBuffer, Is32BitBuffer, Offset);
    case APILevel::DX11:
        return SetIndexBufferD3D11(pIndexBuffer, Is32BitBuffer, Offset);
    }
}

void CRender_RHI::GetRenderTargetData(IRHISurface* pRenderTarget, IRHISurface* pDestSurface)
{
    switch (API)
    {
    case APILevel::DX9:
        return GetRenderTargetDataD3D9(pRenderTarget, pDestSurface);
    }
}

void CRender_RHI::SetRenderTarget(u32 RenderTargetIndex, IRHISurface* pRenderTarget)
{
    switch (API)
    {
    case APILevel::DX9:
        return SetRenderTargetD3D9(RenderTargetIndex, pRenderTarget);
   /* case APILevel::DX11:
        return SetRenderTargetD3D11(RenderTargetIndex, pRenderTarget);*/
    }
}

void CRender_RHI::SetDepthStencilView(IRHIDepthStencilView* pDepthStencilView)
{
    switch (API)
    {
    case APILevel::DX9:
        return SetDepthStencilViewD3D9(pDepthStencilView);
    case APILevel::DX11:
        break;

    }
}

void CRender_RHI::CopyResource(IRHITexture* pDstResource, IRHITexture* pSrcResource)
{
    FATAL(" CRender_RHI::CopyResource: NOT IMPLEMENTED ");
}

ERHITextureFormat CRender_RHI::GetRHIFormatFromAPI(int dxgiFormat)
{
    extern ERHITextureFormat ConvertTextureFormatAPI(DXGI_FORMAT dx9FMT);
    return ConvertTextureFormatAPI((DXGI_FORMAT)dxgiFormat);
}
