#pragma once

enum class ERHI_STATE_CACHE_TYPE
{
	RS,
	DS,
	BS
};

class RHI_API IRHIStateManager
{
public:
	using BindAlphaCallbackDecl = std::function<void(u32)>;

public:
	virtual ~IRHIStateManager() = default;

	virtual void Apply() = 0;
	virtual void Reset() = 0;
	virtual void EnableScissoring(bool enable = true) = 0;
	virtual void SetStencil(u32 enable, u32 func, u32 ref, u32 mask, u32 writemask, u32 fail, u32 pass, u32 zfail) = 0;
	virtual void SetDepthEnable(u32 enable) = 0;
	virtual void SetDepthFunc(u32 func) = 0;
	virtual void SetColorWriteEnable(u32 mask) = 0;
	virtual void SetCullMode(ERHI_CULLMODE mode) = 0;
	virtual void UnmapConstants() = 0;
	virtual void BindAlphaRefCallback(const BindAlphaCallbackDecl& Callback) {};

	virtual void SetRasterizerState(void* NewState) {};
	virtual void SetDepthStencilState(void* NewState) {};
	virtual void SetBlendState(void* NewState) {};
	virtual void SetStencilRef(u32 NewStencilRef) {};
	virtual void SetAlphaRef(u32 NewAlphaRef) = 0;
	virtual void* GetCache(ERHI_STATE_CACHE_TYPE Type, void* Desc) { return nullptr; };
	virtual void OverrideScissoring(bool Override = true, bool Value = true) {};

	virtual void SetRenderState(u32, u32) {};

	ERHI_CULLMODE GetCullMode() const { return CacheCullMode; }

protected:
	BindAlphaCallbackDecl BindAlphaCallback;
	ERHI_CULLMODE CacheCullMode = ERHI_CULLMODE::NONE;
};