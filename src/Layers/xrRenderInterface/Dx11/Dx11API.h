#pragma once

bool CreateD3D11();
bool UpdateBuffersD3D11();
void ResizeBuffersD3D11(u16 Width, u16 Height);
void DestroyD3D11();

class IRHITexture;
IRHITexture* CreateD3D11Texture(const TextureDesc* pTextureDesc, const void* pData, const int Size, const int Pitch);

class IRHIBuffer;
IRHIBuffer* CreateD3D11Buffer(eBufferType bufferType, const void* pData, u32 DataSize, bool bImmutable);

void SetVertexBufferD3D11(u32 StartSlot, IRHIBuffer* pVertexBuffer, const u32 Strides, const u32 Offsets);
void SetIndexBufferD3D11(IRHIBuffer* pIndexBuffer, bool Is32BitBuffer, u32 Offset);