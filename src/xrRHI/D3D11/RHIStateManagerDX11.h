#pragma once
#include "../RHI.h"
#include "../Private/RHIStateCache.h"

enum class ERHI_STATE_CACHE_TYPE
{
	RS,
	DS,
	BS
};

class RHIStateManagerDX11 final : public IRHIStateManager
{
public:
	explicit RHIStateManagerDX11(ID3D11DeviceContext* InContext = nullptr);

	void Apply() override;
	void Reset() override;
	void SetRasterizerState(void* NewState) override;
	void SetDepthStencilState(void* NewState) override;
	void SetBlendState(void* NewState) override;
	void SetStencilRef(u32 NewStencilRef) override;

	void SetContext(ID3D11DeviceContext* InContext);
	void* GetCache(ERHI_STATE_CACHE_TYPE Type, void* Desc);

private:
	ID3D11DeviceContext* D3DContext = nullptr;
	RHIStateCache StateCache;
	ID3D11RasterizerState* RasterizerState = nullptr;
	ID3D11DepthStencilState* DepthStencilState = nullptr;
	ID3D11BlendState* BlendState = nullptr;
	u32 StencilRef = 0;
	bool RasterizerDirty = true;
	bool DepthStencilDirty = true;
	bool BlendDirty = true;
};
