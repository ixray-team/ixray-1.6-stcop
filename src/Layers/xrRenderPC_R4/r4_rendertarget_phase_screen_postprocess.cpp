#include "stdafx.h"

void CRenderTarget::phase_aberration()
{
	u32 Offset = 0;
	float d_Z = EPS_S;
	float d_W = 1.0f;
	u32	C = color_rgba(0, 0, 0, 255);

	u_setrt(rt_Back_Buffer_AA, nullptr, nullptr, nullptr);

	RCache.set_CullMode(CULL_NONE);
	RCache.set_Stencil(false);

	FVF::TL* pv = (FVF::TL*)RCache.Vertex.Lock(4, g_combine->vb_stride, Offset);
	pv->set(0, Device.TargetHeight, d_Z, d_W, C, 0, 1); pv++;
	pv->set(0, 0, d_Z, d_W, C, 0, 0); pv++;
	pv->set(Device.TargetWidth, Device.TargetHeight, d_Z, d_W, C, 1, 1); pv++;
	pv->set(Device.TargetWidth, 0, d_Z, d_W, C, 1, 0); pv++;
	RCache.Vertex.Unlock(4, g_combine->vb_stride);

	RCache.set_Element(s_spp->E[1]);
	RCache.set_Geometry(g_combine);
	RCache.Render(D3DPT_TRIANGLELIST, Offset, 0, 4, 0, 2);

	RContext->CopyResource(rt_Back_Buffer->pTexture->surface_get(), rt_Back_Buffer_AA->pTexture->surface_get());
}

void CRenderTarget::phase_vignette()
{
	u32 Offset = 0;
	float d_Z = EPS_S;
	float d_W = 1.0f;
	constexpr u32 C = color_rgba(0, 0, 0, 255);

	u_setrt(rt_Back_Buffer_AA, nullptr, nullptr, nullptr);

	RCache.set_CullMode(CULL_NONE);
	RCache.set_Stencil(false);

	FVF::TL* pv = (FVF::TL*)RCache.Vertex.Lock(4, g_combine->vb_stride, Offset);
	pv->set(0, Device.TargetHeight, d_Z, d_W, C, 0, 1); pv++;
	pv->set(0, 0, d_Z, d_W, C, 0, 0); pv++;
	pv->set(Device.TargetWidth, Device.TargetHeight, d_Z, d_W, C, 1, 1); pv++;
	pv->set(Device.TargetWidth, 0, d_Z, d_W, C, 1, 0); pv++;
	RCache.Vertex.Unlock(4, g_combine->vb_stride);

	RCache.set_Element(s_spp->E[0]);
	RCache.set_Geometry(g_combine);
	RCache.Render(D3DPT_TRIANGLELIST, Offset, 0, 4, 0, 2);

	RContext->CopyResource(rt_Back_Buffer->pTexture->surface_get(), rt_Back_Buffer_AA->pTexture->surface_get());
}