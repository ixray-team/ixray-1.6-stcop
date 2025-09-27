#pragma once
#include <d3d11_4.h>

#include "../RHI.h"
#include "DX11Texture.h"
#include "DX11Buffer.h"

class InternalDevice11:
	public IRHIDevice
{
public:
	InternalDevice11();
	~InternalDevice11();

	// Inherited via IRHIDevice

	virtual void ResizeBuffers(u32 Width, u32 Height) override;
	virtual void ClearTarget(void* Target, ERTColor Transparent) override;
	virtual void Present() override;

	virtual IRHITextureFactory* GetTextureFactory() override;
	virtual void SetTextureFactory(IRHITextureFactory* factory) override;
	virtual void CopySurface(IRHISurface* Dest, IRHISurface* Source) override;
	virtual void CopySurface(IRHIRenderTargetView* Dest, IRHIRenderTargetView* Source) override;

	IRHIBuffer* CreateBuffer(const RHIBufferDesc& desc, const RHIBufferSubresource* pSubresource) override;

public:
	IDXGISwapChain* HWSwapchain = nullptr;
	ID3D11DeviceContext* HWRenderContext = nullptr;
	ID3D11Device* HWRenderDevice = nullptr;

private:
	bool CreateD3D11();
	void DestroyD3D11();
	bool UpdateBuffersD3D11();
	
	DX11TextureFactory* TextureFactory = nullptr;
};