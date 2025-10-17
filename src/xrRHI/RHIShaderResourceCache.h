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