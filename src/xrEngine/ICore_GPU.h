#pragma once
struct ID3D11Device;
struct ID3D11DeviceContext;
struct IDXGISwapChain;

class ICore_GPU
{
public:
	bool IsAMD = false;

public:
	virtual void	Initialize() = 0;
	virtual void	Destroy() {}

	virtual u32		GetPercentActive() = 0;
	virtual u32		GetGPUCount() = 0;

	virtual void	GetDX11Device
	(
		ID3D11Device** pDevice,
		ID3D11DeviceContext** pImmediateContext,
		IDXGISwapChain** pSwapChain,
		D3D_FEATURE_LEVEL& FeatureLevel
	)
	{}
};

extern ENGINE_API ICore_GPU* g_pGPU;