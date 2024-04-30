#pragma once

bool CreateD3D9();
bool UpdateBuffersD3D9();
void ResizeBuffersD3D9(u16 Width, u16 Height);
void DestroyD3D9();

class IRHITexture;
IRHITexture* CreateD3D9Texture( const TextureDesc* pTextureDesc, LPSUBRESOURCE_DATA pSubresourceData );

class IRHIBuffer;
IRHIBuffer* CreateD3D9Buffer(eBufferType bufferType, const void* pData, u32 DataSize, bool bImmutable);

void SetVertexBufferD3D9(u32 StartSlot, IRHIBuffer* pVertexBuffer, const u32 Strides, const u32 Offsets);
void SetIndexBufferD3D9(IRHIBuffer* pIndexBuffer, bool Is32BitBuffer, u32 Offset);

void SetRenderTargetD3D9(u32 RenderTargetIndex, IRHISurface* pRenderTarget);