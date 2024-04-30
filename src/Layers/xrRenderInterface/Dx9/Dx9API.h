#pragma once

bool CreateD3D9();
bool UpdateBuffersD3D9();
void ResizeBuffersD3D9(u16 Width, u16 Height);
void DestroyD3D9();

class IRender_Texture;
IRender_Texture* CreateD3D9Texture( const TextureDesc* pTextureDesc, const void* pData, const int size );
