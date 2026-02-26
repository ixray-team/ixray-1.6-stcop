#pragma once
#include "../RHI.h"
#include "../Private/RHIStateCache.h"

class RHIStateManagerDX11 :
	public IRHIStateManager
{
public:
	explicit RHIStateManagerDX11(ID3D11DeviceContext* InContext = nullptr);
	~RHIStateManagerDX11();

	virtual void Reset() override;
	void ResetBDesc();
	void ResetDDesc();
	void ResetRDesc();
	virtual void Apply() override;
	virtual void UnmapConstants() override;

	virtual void SetRasterizerState(void* NewState) override;
	virtual void SetDepthStencilState(void* NewState) override;
	virtual void SetBlendState(void* NewState) override;
	virtual void SetStencilRef(u32 NewStencilRef) override;
	virtual void SetAlphaRef(u32 NewAlphaRef) override;

	virtual void EnableScissoring(bool Enable = true) override;
	virtual void SetStencil(u32 Enable, u32 Func, u32 Ref, u32 Mask, u32 WriteMask, u32 Fail, u32 Pass, u32 ZFail) override;
	virtual void SetDepthFunc(u32 Func) override;
	virtual void SetDepthEnable(u32 Enable) override;
	virtual void SetColorWriteEnable(u32 WriteMask) override;
	virtual void SetCullMode(ERHI_CULLMODE Mode) override;
	virtual void BindAlphaRefCallback(const BindAlphaCallbackDecl& Callback) override;
	virtual void* GetCache(ERHI_STATE_CACHE_TYPE Type, void* Desc) override;

	void SetMultisample(u32 Enable);
	void SetSampleMask(u32 Mask);

	virtual void OverrideScissoring(bool Override = true, bool Value = true) override;

	// Bind to a specific D3D11 device context (immediate or deferred)
	void SetContext(ID3D11DeviceContext* InContext);

private:
	void ValidateRDesc();
	void ValidateDSDesc();
	void ValidateBDesc();

private:
	ID3D11DeviceContext* D3DContext = nullptr;
	RHIStateCache StateCache;
	ID3D11RasterizerState* RasterizerState;
	ID3D11DepthStencilState* DepthStencilState;
	ID3D11BlendState* BlendState;

	u32 StencilRef;
	u32 AlphaRef;
	u32 SampleMask;

	bool bRSNeedApply;
	bool bDSSNeedApply;
	bool bBSNeedApply;

	bool bRSChanged;
	bool bDSSChanged;
	bool bBSChanged;

	bool bOverrideScissoring;
	bool bOverrideScissoringValue;

	bool bRSForced;
	bool bDSSForced;
	bool bBSForced;

	D3D11_RASTERIZER_DESC RDesc;
	D3D11_DEPTH_STENCIL_DESC DSDesc;
	D3D11_BLEND_DESC BDesc;
};