#include "stdafx.h"

#include "OverlayAPI/XESSWrapper.h"

void CRenderTarget::init_xess()
{
	g_XESSWrapper.Destroy();

	if (ps_proxy_r_scale_mode != 4)
	{
		return;
	}

	XeSSWrapper::ContextParameters InitParams;
	InitParams.D3DDevice = RDevice;

	InitParams.DisplaySize = {
		(int)RCache.get_target_width(),
		(int)RCache.get_target_height()
	};

	InitParams.RenderSize = {
		(int)RCache.get_width(),
		(int)RCache.get_height()
	};

	g_XESSWrapper.Create(InitParams);
}

bool CRenderTarget::phase_xess()
{
	if (!g_XESSWrapper.IsCreated())
	{
		return false;
	}

	GPU_EVENT(XESS);

	XeSSWrapper::DrawParameters XessParams;
	XessParams.DeviceContext = RContext;

	XessParams.ColorResource = (ID3D11Resource*)rt_Generic_0->pSurface->GetRawTexture();
	XessParams.VelocityResource = (ID3D11Resource*)rt_Velocity->pSurface->GetRawTexture();
	XessParams.DepthResource = (ID3D11Resource*)rt_Position->pSurface->GetRawTexture();

	XessParams.OutputResource = (ID3D11Resource*)rt_Generic->pSurface->GetRawTexture();

	XessParams.RenderWidth = (u32)RCache.get_width();
	XessParams.RenderHeight = (u32)RCache.get_height();

	XessParams.JitterX = ps_r_taa_jitter_full.x;
	XessParams.JitterY = ps_r_taa_jitter_full.y;

	XessParams.CameraReset = false;

	return g_XESSWrapper.Draw(XessParams);
}
