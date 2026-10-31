#pragma once

class RHI_API IRHIStateManager
{
public:
	virtual ~IRHIStateManager() = default;

	virtual void Apply() = 0;
	virtual void Reset() = 0;
	virtual void SetRasterizerState(void* NewState) = 0;
	virtual void SetDepthStencilState(void* NewState) = 0;
	virtual void SetBlendState(void* NewState) = 0;
	virtual void SetStencilRef(u32 NewStencilRef) = 0;
};
