#pragma once

bool CreateD3D9();
bool UpdateBuffersD3D9();
void ResizeBuffersD3D9(u16 Width, u16 Height);
void DestroyD3D9();

class IRHITexture;
IRHITexture* CreateD3D9Texture( const TextureDesc* pTextureDesc, LPSUBRESOURCE_DATA pSubresourceData );

class IRHIDepthStencilView;
IRHIDepthStencilView* CreateD3D9DepthStencilSurface(u32 Width, u32 Height, ERHITextureFormat Format, u32 MultiSample, u32 MultisampleQuality, bool Discard);

class IRHIBuffer;
IRHIBuffer* CreateD3D9Buffer(eBufferType bufferType, const void* pData, u32 DataSize, bool bImmutable);

void SetVertexBufferD3D9(u32 StartSlot, IRHIBuffer* pVertexBuffer, const u32 Strides, const u32 Offsets);
void SetIndexBufferD3D9(IRHIBuffer* pIndexBuffer, bool Is32BitBuffer, u32 Offset);

void SetRenderTargetD3D9(u32 RenderTargetIndex, IRHISurface* pRenderTarget);
void SetDepthStencilViewD3D9(IRHIDepthStencilView* pDepthStencilView);

class IRHISurface;
IRHISurface* CreateOffscreenPlainSurfaceD3D9(u32 Width, u32 Height, ERHITextureFormat Format, bool DefaultPool);
void GetRenderTargetDataD3D9(IRHISurface* pRenderTarget, IRHISurface* pDestSurface);