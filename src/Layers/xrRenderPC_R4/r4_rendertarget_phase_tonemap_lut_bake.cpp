#include "stdafx.h"
#include "r4_rendertarget.h"
#include "blender_tonemap_lut_bake.h"
#include "../xrRender/dxRenderDeviceRender.h"

namespace
{
	constexpr u32 TonemapLUTSize = 32;
}

void CRenderTarget::create_tonemap_lut()
{
	RHITextureDesc desc = {};
	desc.Width = desc.Height = desc.Depth = TonemapLUTSize;
	desc.MipLevels = 1;
	desc.Format = ERHI_FORMAT::R16G16B16A16_FLOAT;
	desc.Usage = ERHI_USAGE::USAGE_DEFAULT;
	desc.BindFlags = ERHI_BIND_FLAG::SHADER_RESOURCE | ERHI_BIND_FLAG::UNORDERED_ACCESS;
	s_tonemap_lut_surface = GRHI->CreateTexture3D(desc, nullptr);
	R_ASSERT(s_tonemap_lut_surface);
	t_tonemap_lut = dxRenderDeviceRender::Instance().Resources->_CreateTexture(r4_RT_tonemap_lut);
	t_tonemap_lut->surface_set(s_tonemap_lut_surface);

	RHIUAVDesc uav_desc = {};
	uav_desc.Format = desc.Format;
	uav_desc.ViewDimension = ERHI_VIEW_DIMENSION::Texture3D;
	uav_desc.WSize = desc.Depth;
	u_tonemap_lut = GRHI->CreateUAV(s_tonemap_lut_surface, uav_desc);
	R_ASSERT(u_tonemap_lut);

	CBlender_tonemap_lut_bake blender;
	s_tonemap_lut_bake.create(&blender);
}

void CRenderTarget::phase_bake_tonemap_lut()
{
	GPU_EVENT(phase_bake_tonemap_lut);
	u_setrt(get_target_width(), get_target_height(), nullptr, nullptr, nullptr, nullptr);
	SPass& P = *s_tonemap_lut_bake->E[0]->passes[0];
	RCache.set_States(P.state);
	RCache.set_Constants(P.constants);
	RCache.set_Textures(P.T);
	RCache.set_CS(P.cs);
	// Flush cached readers before binding the LUT for writing.
	GRHI->ShaderResourceCache->Apply();
	ID3D11UnorderedAccessView* uav = reinterpret_cast<ID3D11UnorderedAccessView*>(u_tonemap_lut->GetRaw());
	RContext->CSSetUnorderedAccessViews(0, 1, &uav, nullptr);
	const u32 groups = (TonemapLUTSize + 7) / 8;
	RCache.Compute(groups, groups, groups);
	ID3D11UnorderedAccessView* null_uav = nullptr;
	RContext->CSSetUnorderedAccessViews(0, 1, &null_uav, nullptr);
}
