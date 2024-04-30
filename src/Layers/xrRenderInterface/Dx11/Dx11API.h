#pragma once

bool CreateD3D11();
bool UpdateBuffersD3D11();
void ResizeBuffersD3D11(u16 Width, u16 Height);
void DestroyD3D11();

class IRender_BufferBase;
IRender_BufferBase* CreateD3D11Buffer(eBufferType bufferType, const void* pData, u32 DataSize, bool bImmutable);
