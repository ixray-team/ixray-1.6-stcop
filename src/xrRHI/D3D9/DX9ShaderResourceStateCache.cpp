#include "../RHI.h"
#include "DX9ShaderResourceStateCache.h"

void DX9ShaderResourceStateCache::SetPSResource(u32 slot, IRHIShaderResourceView* pTex)
{
	IDirect3DDevice9* DxDevice = (IDirect3DDevice9*)GRHI->DevicePtr->RawDevice;
	DxDevice->SetTexture(slot, pTex ? (IDirect3DBaseTexture9*)pTex->GetRawSRV() : nullptr);
}

void DX9ShaderResourceStateCache::SetVSResource(u32 slot, IRHIShaderResourceView* pTex)
{
	IDirect3DDevice9* DxDevice = (IDirect3DDevice9*)GRHI->DevicePtr->RawDevice;
	DxDevice->SetTexture(RHI_VERTEX_TEXTURESAMPLER + slot, pTex ? (IDirect3DBaseTexture9*)pTex->GetRawSRV() : nullptr);
}