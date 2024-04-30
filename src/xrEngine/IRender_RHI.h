#pragma once

class IRender_RHI
{
public:
	enum class APILevel
	{
		DX9,
		DX11
	};

	APILevel API = APILevel::DX9;

public:
	virtual bool Create(APILevel) = 0;
	virtual bool UpdateBuffers() = 0;
	virtual void ResizeBuffers(u16 Width, u16 Height) = 0;
	virtual void Destroy() = 0;

	virtual void* GetRenderSRV() = 0;
	virtual void* GetRenderDevice() = 0;
	virtual void* GetRenderContext() = 0;
	virtual void* GetRenderTexture() = 0;
	virtual void* GetDepthTexture() = 0;
	virtual void* GetSwapchainTexture() = 0;
	virtual void* GetSwapchain() = 0;

	virtual void FillModes() = 0;
	virtual int GetFeatureLevel() = 0;
};

extern ENGINE_API IRender_RHI* g_RenderRHI;