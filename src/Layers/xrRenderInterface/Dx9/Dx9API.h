#pragma once

bool CreateD3D9();
bool UpdateBuffersD3D9();
void ResizeBuffersD3D9(u16 Width, u16 Height);
void DestroyD3D9();

class IRHITexture;
IRHITexture* CreateD3D9Texture( const TextureDesc* pTextureDesc, const void* pData, const int size );

class IRHIBuffer;
IRHIBuffer* CreateD3D9Buffer(eBufferType bufferType, const void* pData, u32 DataSize, bool bImmutable);
