#pragma once

class IRHIDevice
{
public:
	void* RawDevice = nullptr;
	void* RenderRTV = nullptr;
	void* RenderSRV = nullptr;
	void* RenderDSV = nullptr;
	void* RenderTexture = nullptr;
	void* SwapChainRTV = nullptr;

	float RenderScale = 1.f;
	u32 FeatureLevel;

public:
	virtual void ResizeBuffers(u32 Width, u32 Height) = 0;
};