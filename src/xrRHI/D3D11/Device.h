#pragma once
#include "../RHI.h"
#include "DX11TextureImplementations.h"
#include <d3d11.h>

class InternalDevice11:
	public IRHIDevice
{
public:
	InternalDevice11();
	~InternalDevice11();

	virtual void ResizeBuffers(u32 Width, u32 Height) override;
	virtual void ClearTarget(void* Target, ERTColor Transparent) override;
	virtual void Present() override;
	
	virtual IRHITextureFactory* GetTextureFactory() override;
	virtual void SetTextureFactory(IRHITextureFactory* factory) override;

public:
	IDXGISwapChain* HWSwapchain = nullptr;
	ID3D11DeviceContext* HWRenderContext = nullptr;
	ID3D11Device* HWRenderDevice = nullptr;

private:
	bool CreateD3D11();
	void DestroyD3D11();
	bool UpdateBuffersD3D11();
	
	DX11TextureFactory* m_pTextureFactory = nullptr;
};