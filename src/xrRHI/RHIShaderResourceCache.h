#pragma once

enum class ERHI_MAX_TEXTURE
{
	PixelShaderTextures = 16,
	VertexShaderTextures = 4,
	GeometryShaderTextures = 16,
	HullShaderTextures = 16,
	DomainShaderTextures = 16,
	ComputeShaderTextures = 16,
};

class IRHIShaderResourceStateCache
{
public:
	virtual ~IRHIShaderResourceStateCache() = default;
	virtual void ResetDeviceState() = 0;
	virtual void Apply() = 0;

	virtual void SetPSResource(u32 uiSlot, IRHIShaderResourceView* pRes) = 0;
	virtual void SetVSResource(u32 uiSlot, IRHIShaderResourceView* pRes) = 0;
	virtual void SetGSResource(u32 uiSlot, IRHIShaderResourceView* pRes) {};
	virtual void SetDSResource(u32 uiSlot, IRHIShaderResourceView* pRes) {};
	virtual void SetHSResource(u32 uiSlot, IRHIShaderResourceView* pRes) {};
	virtual void SetCSResource(u32 uiSlot, IRHIShaderResourceView* pRes) {};
};

class IRHISamplerStateCache
{
public:
	enum
	{
		hInvalidHandle = 0xFFFFFFFF
	};

	//	State handle
	using HArray = xr_vector<u32>;
public:
	virtual ~IRHISamplerStateCache() = default;

	virtual void ClearStateArray() = 0;

	//u32	GetState(D3D_SAMPLER_DESC& desc);

	virtual void VSApplySamplers(HArray& samplers) = 0;
	virtual void PSApplySamplers(HArray& samplers) = 0;
	virtual void GSApplySamplers(HArray& samplers) {};
	virtual void HSApplySamplers(HArray& samplers) {};
	virtual void DSApplySamplers(HArray& samplers) {};
	virtual void CSApplySamplers(HArray& samplers) {};

	virtual void SetMaxAnisotropy(UINT uiMaxAniso) = 0;
	virtual void SetMipLodBias(float mipMapLODBias) = 0;
	virtual void ResetDeviceState() = 0;
};