#include "RHIRenderViewManager.h"

CRHIRenderViewManager GRHIRenderViewManager;

void CRHIRenderViewManager::Clear()
{
	IRHIRenderTargetView* NullSRV[RHI_MAX_RENDER_TARGETS] = { nullptr };
	GRHI->DevicePtr->SetDSV(nullptr);
	GRHI->DevicePtr->SetRenderTargets(RHI_MAX_RENDER_TARGETS, NullSRV);
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
		GRHI->DevicePtr->SetRenderTargets(RHI_MAX_RENDER_TARGETS, RenderTargetViews);
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
	if (GRHI->APILevel != ERHI_API_LAYER::D3D9)
	{
		GRHI->DevicePtr->SetDSV(DepthStencilView);
		GRHI->DevicePtr->SetRenderTargets(RHI_MAX_RENDER_TARGETS, RenderTargetViews);
	}

	ChangedRTorZB = false;
}