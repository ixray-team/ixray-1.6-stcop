#pragma once

#include "../RHI.h"

class CRHIRenderViewManager
{
public:
	void SetRenderTargetView(IRHIRenderTargetView* pRenderTargetView, u32 ID, bool bForce = false);
	void SetUnorderedAccessViews(IRHIUnorderedAccessView* pRenderTargetView, u32 ID, bool bForce = false);
	void SetDepthStencilView(IRHIDepthStencilView* pDepthStencilView, bool bForce = false);
	void ApplyRenderTargetChange();
	void Clear();

public:
	IRHIRenderTargetView* RenderTargetViews[RHI_MAX_RENDER_TARGETS] = {nullptr};
	IRHIUnorderedAccessView* RenderUnorderedAccessView[RHI_MAX_RENDER_TARGETS] = {nullptr};
	IRHIDepthStencilView* DepthStencilView = nullptr;
	bool ChangedRTorZB = false;
};

extern CRHIRenderViewManager GRHIRenderViewManager;