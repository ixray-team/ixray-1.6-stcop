#pragma once
#include <d3d9.h>

class DX9ShaderResourceStateCache:
	public IRHIShaderResourceStateCache
{
public:

	virtual void ResetDeviceState() override {}
	virtual void Apply() override {}

	virtual void SetPSResource(u32 slot, IRHIShaderResourceView* pTex) override;
	virtual void SetVSResource(u32 slot, IRHIShaderResourceView* pTex) override;
};