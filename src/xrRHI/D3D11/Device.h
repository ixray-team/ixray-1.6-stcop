#pragma once
#include "../RHI.h"
#include <d3d11.h>

class InternalDevice11:
	public IRHIDevice
{
public:
	InternalDevice11();
	~InternalDevice11();

	bool CreateD3D11();
	void DestroyD3D11();

	virtual void ResizeBuffers(u32 Width, u32 Height) override;
	bool UpdateBuffersD3D11();

public:
	IDXGISwapChain* HWSwapchain = nullptr;
	ID3D11DeviceContext* HWRenderContext = nullptr;
};