#include "RHIRenderViewManager.h"

CRHIRenderViewManager GRHIRenderViewManager;

void CRHIRenderViewManager::Clear()
{
	static IRHIRenderTargetView* NullRTV[RHI_MAX_RENDER_TARGETS] = { nullptr };
	static IRHIUnorderedAccessView* NullUAV[RHI_MAX_RENDER_TARGETS] = { nullptr };

	GRHI->DevicePtr->SetDSV(nullptr);
	GRHI->DevicePtr->SetRenderTargets(RHI_MAX_RENDER_TARGETS, NullRTV, NullUAV);
}

void CRHIRenderViewManager::SetUnorderedAccessViews(IRHIUnorderedAccessView* pRenderTargetView, u32 ID, bool bForce)
{
	if (RenderUnorderedAccessView[ID] != pRenderTargetView)
	{
		if (!ChangedRTorZB && GRHI->APILevel == ERHI_API_LAYER::D3D11)
		{
			Clear();
		}

		RenderUnorderedAccessView[ID] = pRenderTargetView;
		ChangedRTorZB = true;
	}

	if (GRHI->APILevel == ERHI_API_LAYER::D3D9)
	{
		VERIFY(!"Unsupported");
	}
	else if (bForce)
	{
		ApplyRenderTargetChange();
	}
}

void CRHIRenderViewManager::SetRenderTargetView(IRHIRenderTargetView* pRenderTargetView, u32 ID, bool bForce)
{
	if (RenderTargetViews[ID] != pRenderTargetView)
	{
		if (!ChangedRTorZB && GRHI->APILevel == ERHI_API_LAYER::D3D11)
		{
			Clear();
		}

		RenderTargetViews[ID] = pRenderTargetView;
		ChangedRTorZB = true;
	}

	if (GRHI->APILevel == ERHI_API_LAYER::D3D9)
	{
		GRHI->DevicePtr->SetRenderTargets(RHI_MAX_RENDER_TARGETS, RenderTargetViews, RenderUnorderedAccessView);
	}
	else if (bForce)
	{
		ApplyRenderTargetChange();
	}
}

void CRHIRenderViewManager::SetDepthStencilView(IRHIDepthStencilView* pDepthStencilView, bool bForce)
{
	if (DepthStencilView != pDepthStencilView)
	{
		if (!ChangedRTorZB && GRHI->APILevel == ERHI_API_LAYER::D3D11)
		{
			Clear();
		}

		DepthStencilView = pDepthStencilView;
		ChangedRTorZB = true;
	}

	if (GRHI->APILevel == ERHI_API_LAYER::D3D9)
	{
		GRHI->DevicePtr->SetDSV(DepthStencilView);
	}
	else if (bForce)
	{
		ApplyRenderTargetChange();
	}
}

void CRHIRenderViewManager::ApplyRenderTargetChange()
{
	if (!ChangedRTorZB)
		return;

	if (GRHI->APILevel != ERHI_API_LAYER::D3D9)
	{
		GRHI->DevicePtr->SetDSV(DepthStencilView);
		GRHI->DevicePtr->SetRenderTargets(RHI_MAX_RENDER_TARGETS, RenderTargetViews, RenderUnorderedAccessView);
	}

	ChangedRTorZB = false;
}