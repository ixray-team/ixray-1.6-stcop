#include "stdafx.h"

#include "ResourceManager.h"
#include <magic_enum/magic_enum.hpp>
#include "blender_combine.h"
#include "blender_screen_postprocess.h"
#include "blender_scale.h"
#include "blender_cas.h"
#include "blender_gtao.h"
#include "blender_taa.h"
#include "BlenderGasMask.h"
#include "blender_nvg.h"
#include "blender_sslr.h"

#include "blender_bloom_downsample.h"
#include "blender_bloom_upsample.h"
#include "blender_new_adaptation.h"
#include "blender_new_dof.h"

#include "blender_fxaa.h"
#include "blender_smaa.h"
#include "BlenderGamma.h"
#include "dxRenderDeviceRender.h"

#include "OverlayAPI/FSR3Wrapper.h"

namespace
{
	// The scene's depth is drawn into as far as the view reaches; any other depth is drawn whole
	void DepthView(IRHIDepthStencilView* Depth, u32& Width, u32& Height)
	{
		Width = Depth->GetSurface()->GetWidth();
		Height = Depth->GetSurface()->GetHeight();

		if (Width == u32(AllocHalfTarget.x) && Height == u32(AllocHalfTarget.y))
		{
			Width = u32(HalfTarget.x);
			Height = u32(HalfTarget.y);
		}
	}
}

void CRenderTarget::u_setrt(GfxCmdList& Cmd, const ref_rt& _1, const ref_rt& _2, const ref_rt& _3, const ref_rt& _4, IRHIDepthStencilView* zb)
{
	VERIFY(_1 || zb);

	if (_1)
	{
		dwWidth = _1->ViewWidth();
		dwHeight = _1->ViewHeight();
	}
	else
	{
		VERIFY(zb->GetDimension() == ERHI_DSV_DIMENSION::TEXTURE2D);
		DepthView(zb, dwWidth, dwHeight);
	}

	Cmd.SetRenderTargets({ _1 ? _1->pRT : nullptr, _2 ? _2->pRT : nullptr, _3 ? _3->pRT : nullptr, _4 ? _4->pRT : nullptr }, zb);
}

void CRenderTarget::ResolveSurface(const ref_rt& dst, const ref_rt& src)
{
	RImplementation.Imm().CopySurface(dst->pSurface, src->pSurface);
}

void CRenderTarget::u_setrt(GfxCmdList& Cmd, const ref_rt& _1, const ref_rt& _2, const ref_rt& _3, IRHIDepthStencilView* zb)
{
	VERIFY(_1 || zb);
	if (_1)
	{
		dwWidth = _1->ViewWidth();
		dwHeight = _1->ViewHeight();
	}
	else
	{
		VERIFY(zb->GetSurface() != nullptr);
		DepthView(zb, dwWidth, dwHeight);
	}

	Cmd.SetRenderTargets({ _1 ? _1->pRT : nullptr, _2 ? _2->pRT : nullptr, _3 ? _3->pRT : nullptr }, zb);
}

void CRenderTarget::u_setrt(GfxCmdList& Cmd, const ref_rt& _1, const ref_rt& _2, IRHIDepthStencilView* zb)
{
	VERIFY(_1 || zb);
	if (_1)
	{
		dwWidth = _1->ViewWidth();
		dwHeight = _1->ViewHeight();
	}
	else
	{
		VERIFY(zb->GetDimension() == ERHI_DSV_DIMENSION::TEXTURE2D);
		DepthView(zb, dwWidth, dwHeight);
	}

	Cmd.SetRenderTargets({ _1 ? _1->pRT : nullptr, _2 ? _2->pRT : nullptr }, zb);
}

void CRenderTarget::u_setrt(GfxCmdList& Cmd, u32 W, u32 H, IRHIRenderTargetView* _1, IRHIRenderTargetView* _2, IRHIRenderTargetView* _3, IRHIDepthStencilView* zb)
{
	dwWidth = W;
	dwHeight = H;

	Cmd.SetRenderTargets({ _1, _2, _3 }, zb);
}

void CRenderTarget::u_setrt(GfxCmdList& Cmd, u32 W, u32 H, IRHIRenderTargetView* _1, IRHIRenderTargetView* _2, IRHIRenderTargetView* _3, IRHIRenderTargetView* _4, IRHIDepthStencilView* zb)
{
	dwWidth = W;
	dwHeight = H;

	Cmd.SetRenderTargets({ _1, _2, _3, _4 }, zb);
}

// 2D texgen (texture adjustment matrix)
void CRenderTarget::u_compute_texgen_screen(GfxCmdList& Cmd, Fmatrix& m_Texgen)
{
	Fmatrix			m_TexelAdjust		= 
	{
		0.5f,				0.0f,				0.0f,			0.0f,
		0.0f,				-0.5f,				0.0f,			0.0f,
		0.0f,				0.0f,				1.0f,			0.0f,
		0.5f,				0.5f ,				0.0f,			1.0f
	};
	m_Texgen.mul	(m_TexelAdjust,Cmd.GetWorldViewProject());
}

// 2D texgen for jitter (texture adjustment matrix)
void CRenderTarget::u_compute_texgen_jitter(GfxCmdList& Cmd, Fmatrix& m_Texgen_J)
{
	// place into	0..1 space
	Fmatrix TexelAdjust = 
	{
		0.5f,				0.0f,				0.0f,			0.0f,
		0.0f,				-0.5f,				0.0f,			0.0f,
		0.0f,				0.0f,				1.0f,			0.0f,
		0.5f,				0.5f,				0.0f,			1.0f
	};
	m_Texgen_J.mul	(TexelAdjust,Cmd.GetWorldViewProject());

	// rescale - tile it
	float	scale_X			= Cmd.GetWidth() / float(TEX_jitter);
	float	scale_Y			= Cmd.GetHeight() / float(TEX_jitter);
	//float	offset			= (.5f / float(TEX_jitter));
	TexelAdjust.scale			(scale_X,	scale_Y,1.f	);
	//TexelAdjust.translate_over(offset,	offset,	0	);
	m_Texgen_J.mulA_44			(TexelAdjust);
}

u8		fpack			(float v)				{
	s32	_v	= iFloor	(((v+1)*.5f)*255.f + .5f);
	clamp	(_v,0,255);
	return	u8(_v);
}
u8		fpackZ			(float v)				{
	s32	_v	= iFloor	(std::abs(v)*255.f + .5f);
	clamp	(_v,0,255);
	return	u8(_v);
}
Fvector	vunpack			(s32 x, s32 y, s32 z)	{
	Fvector	pck;
	pck.x	= (float(x)/255.f - .5f)*2.f;
	pck.y	= (float(y)/255.f - .5f)*2.f;
	pck.z	= -float(z)/255.f;
	return	pck;
}
Fvector	vunpack			(Ivector src)			{
	return	vunpack	(src.x,src.y,src.z);
}
Ivector	vpack			(Fvector src)
{
	Fvector			_v;
	int	bx			= fpack	(src.x);
	int by			= fpack	(src.y);
	int bz			= fpackZ(src.z);
	// dumb test
	float	e_best	= flt_max;
	int		r=bx,g=by,b=bz;
#ifdef DEBUG
	int		d=0;
#else
	int		d=3;
#endif
	for (int x=std::max(bx-d,0); x<=std::min(bx+d,255); x++)
	for (int y=std::max(by-d,0); y<=std::min(by+d,255); y++)
	for (int z=std::max(bz-d,0); z<=std::min(bz+d,255); z++)
	{
		_v				= vunpack(x,y,z);
		float	m		= _v.magnitude();
		float	me		= std::abs(m-1.f);
		if	(me>0.03f)	continue;
		_v.div	(m);
		float	e		= std::abs(src.dotproduct(_v)-1.f);
		if (e<e_best)	{
			e_best		= e;
			r=x,g=y,b=z;
		}
	}
	Ivector		ipck;
	ipck.set	(r,g,b);
	return		ipck;
}

void generate_jitter(u32* dest, u32 elem_count)
{
	const	int		cmax		= 8;
	FixedVector<Ivector2,cmax>		samples;
	while (samples.size()<elem_count*2)
	{
		Ivector2	test;
		test.set	(::Random.randI(0,256),::Random.randI(0,256));
		bool		valid = true;
		for (u32 t=0; t<samples.size(); t++)
		{
			int		dist	= std::abs(test.x-samples[t].x)+ std::abs(test.y-samples[t].y);
			if (dist<32)	{
				valid		= false;
				break;
			}
		}
		if (valid)	samples.push_back	(test);
	}
	for	(u32 it=0; it<elem_count; it++, dest++)
		*dest	= color_rgba(samples[2*it].x,samples[2*it].y,samples[2*it+1].y,samples[2*it+1].x);
}

u32 CRenderTarget::get_width()
{
	return dwWidth;
}

u32 CRenderTarget::get_height()
{
	return dwHeight;
}

u32 CRenderTarget::get_core_width()
{
	return (u32)GfxCmdList::GetWidth();
}

u32 CRenderTarget::get_core_height()
{
	return (u32)GfxCmdList::GetHeight();
}

u32 CRenderTarget::get_target_width()
{
	return (u32)GfxCmdList::GetTargetWidth();
}

u32 CRenderTarget::get_target_height()
{
	return (u32)GfxCmdList::GetTargetHeight();
}

CRenderTarget::CRenderTarget()
{
	GfxCmdList& Cmd = RImplementation.Imm();

	// The clears below find a slice of an array among this object's targets
	RImplementation.Target = this;
	if (g_debug_blend_state == nullptr) {
		RHIBlendDesc desc;
		ZeroMemory(&desc, sizeof(desc));
		desc.AlphaToCoverageEnable = false;
		desc.RenderTarget[0].BlendEnable = false;
		desc.RenderTarget[0].SrcBlend = RHI_BLEND_SRC_ALPHA;
		desc.RenderTarget[0].DestBlend = RHI_BLEND_DEST_COLOR;
		desc.RenderTarget[0].BlendOp = RHI_BLEND_OP_ADD;
		desc.RenderTarget[0].SrcBlendAlpha = RHI_BLEND_ONE;
		desc.RenderTarget[0].DestBlendAlpha = RHI_BLEND_ZERO;
		desc.RenderTarget[0].BlendOpAlpha = RHI_BLEND_OP_ADD;
		desc.RenderTarget[0].RenderTargetWriteMask = RHI_COLOR_WRITE_ENABLE_ALL;
		R_CHK(GRHI->CreateBlendState(desc, &g_debug_blend_state));
	}

	CImGuiManager::Instance().Subscribe("GraphicDebug", CImGuiManager::ERenderPriority::eMedium, [this]() { DrawDebugWindow(); });

	param_blur = 0.f;
	param_gray = 0.f;
	param_noise = 0.f;
	param_duality_h = 0.f;
	param_duality_v = 0.f;
	param_noise_fps = 25.f;
	param_noise_scale = 1.f;

	im_noise_time = 1.0f / 100.0f;
	im_noise_shift_w = 0;
	im_noise_shift_h = 0;

	param_color_base = color_rgba(127, 127, 127, 0);
	param_color_gray = color_rgba(85, 85, 85, 0);
	//param_color_add		= color_rgba(0,0,0,			0);
	param_color_add.set(0.0f, 0.0f, 0.0f);

	dxRenderDeviceRender::Instance().Resources->Evict();

	s_r1_distort.create("r1_distort");

	// Blenders
	b_combine = new CBlender_combine();


	u32 s_dwWidth = (u32)Cmd.GetWidth(), s_dwHeight = (u32)Cmd.GetHeight();

	// What is made from here on is made for the screen, bar the few of a fixed size named at the end
	CRT::MarkViewSized = true;

	// The editor viewport is not the window: depth has to match the viewport targets
	if (Device.IsEditorMode())
	{
		rt_EditorDepth.create("$user$editor_depth", s_dwWidth, s_dwHeight, ERHI_FORMAT::D24_UNORM_S8_UINT);
		EditorWindowDepth = GRHI->DevicePtr->RenderDSV;
		GRHI->DevicePtr->RenderDSV = rt_EditorDepth->pZRT;

		if (s_dwWidth != get_target_width() || s_dwHeight != get_target_height())
		{
			rt_EditorOverlayDepth.create("$user$editor_overlay_depth", get_target_width(), get_target_height(), ERHI_FORMAT::D24_UNORM_S8_UINT);
		}
	}

	// NORMAL
	{
		// Half the bytes of four halves: the passes that only move the frame's colour are bound by memory alone
		auto HDR_Format = ERHI_FORMAT::R11G11B10_FLOAT;

		// The normals the depth pass writes for the occlusion pass; rt_Forward and rt_Revealage are made when the
		// frame first weighs a surface (RenderForwardOit)
		rt_Thin.create(r2_RT_thin, s_dwWidth, s_dwHeight, ERHI_FORMAT::R32_UINT, 1, CRT::USE_UAV_FLAG);
		rt_Ids.create(r2_RT_ids, s_dwWidth, s_dwHeight, ERHI_FORMAT::R32_UINT);
		rt_Surface.create(r2_RT_surface, s_dwWidth, s_dwHeight, ERHI_FORMAT::R32G32_UINT, 1, CRT::USE_UAV_FLAG);
		rt_Accumulator.create(r2_RT_accum, s_dwWidth, s_dwHeight, HDR_Format);
		// Written by the scene's resolve too, which is a compute pass
		rt_Generic_0.create(r2_RT_generic0, s_dwWidth, s_dwHeight, HDR_Format, 1, CRT::USE_UAV_FLAG);
		rt_SkyBehind.create(r2_RT_sky_behind, s_dwWidth, s_dwHeight, HDR_Format);

		rt_Generic_2.create(r2_RT_generic2, s_dwWidth, s_dwHeight, HDR_Format);

		rt_Velocity.create(r2_RT_velocity, s_dwWidth, s_dwHeight, ERHI_FORMAT::R16G16_SNORM);
		ReactiveMask.create(r2_RT_reactive, s_dwWidth, s_dwHeight, ERHI_FORMAT::R8_UNORM);
		CreateRainStreakTargets(HDR_Format);
		rt_Generic_1.create(r2_RT_generic1, s_dwWidth, s_dwHeight, ERHI_FORMAT::R8G8B8A8_UNORM);

		rt_Position.create(r2_RT_P, s_dwWidth, s_dwHeight, RDepth->GetSurface()->GetFormat());
		rt_DofScopeMask.create(r2_RT_dof_scope_mask, s_dwWidth, s_dwHeight, ERHI_FORMAT::R8_UNORM);

		rt_Back_Buffer_AA.create(r2_RT_backbuffer_AA, get_target_width(), get_target_height(), HDR_Format);
		rt_Back_Buffer.create(r2_RT_backbuffer_final, get_target_width(), get_target_height(), HDR_Format);

		rt_Generic.create(r2_RT_generic, get_target_width(), get_target_height(), ERHI_FORMAT::R16G16B16A16_FLOAT, 1, CRT::USE_UAV_FLAG);

		rt_BackbufferLUT.create(r2_RT_backbuffer_lut, get_target_width(), get_target_height(), ERHI_FORMAT::R10G10B10A2_UNORM);
		rt_ui_pda.create(r_ui_pda, get_target_width(), get_target_height(), ERHI_FORMAT::R16G16B16A16_FLOAT);
	}

	rt_upscaled_depth.create(r2_RT_upscaled_depth, s_dwWidth, s_dwHeight, ERHI_FORMAT::R16_FLOAT);
	rt_upscaled_depth_old.create(r2_RT_upscaled_depth_old, s_dwWidth, s_dwHeight, ERHI_FORMAT::R16_FLOAT);

	rt_ui_depth.create(r2_RT_ui_depth, get_target_width() * 2.0f, get_target_height() * 2.0f, ERHI_FORMAT::D16_UNORM);
	rt_ui_color.create(r2_RT_ui_color, get_target_width() * 2.0f, get_target_height() * 2.0f, ERHI_FORMAT::R16G16B16A16_FLOAT);
	RImplementation.Scene.ForgetIcons();

	init_fsr();

	// Scale
	{
		CreateEffectSQ<CBlender_scale>(s_scale);
	}

	// FXAA
	{
		CreateEffectSQ<CBlender_FXAA>(s_fxaa);
	}

	// Screen Post Process
	{
		CreateEffectSQ<CBlender_SPP>(s_spp);
		CreateEffectSQ<CBlenderGasMask>(s_gasmask);
		FlatGasMaskBlender = new CBlenderGasMask(VisorGlassType::Flat);
		FlatGasMaskShader.create(FlatGasMaskBlender);
	}

	// SMAA
	{
		CreateEffectSQ<CBlender_SMAA>(s_smaa);

		rt_smaa_edgetex.create(r2_RT_smaa_edgetex, s_dwWidth, s_dwHeight, ERHI_FORMAT::R8G8_UNORM);
		rt_smaa_blendtex.create(r2_RT_smaa_blendtex, s_dwWidth, s_dwHeight, ERHI_FORMAT::R8G8B8A8_UNORM);
	}

	//Sharpening
	{
		CreateEffectSQ<CBlender_Sharpening>(s_cas);
	}

	//Ground-truth based ambient occlusion
	{
		CreateEffectSQ<CBlender_gtao>(s_gtao);

		const u32 GtaoShift = RImplementation.o.GtaoHq ? 0 : 1;
		rt_gtao_0.create("$user$gtao_0", (s_dwWidth + GtaoShift) >> GtaoShift, (s_dwHeight + GtaoShift) >> GtaoShift, ERHI_FORMAT::R32_UINT); //AO.view-z
		rt_ao_depth.create(r2_RT_ao_depth, s_dwWidth, s_dwHeight, ERHI_FORMAT::R32_FLOAT, 1, CRT::CRTCreationFlags(CRT::USE_UAV_FLAG | CRT::MIPPED_RT_FLAG));
	}

	//Puddles
	{
		s_puddles.create("effects_water_puddles");
	}

	//TAA
	{
		CreateEffectSQ<CBlender_taa>(s_taa);

		// The frame's colour is copied into it: the two have one format
		rt_Generic_0_prev.create(r2_RT_generic0_prev, s_dwWidth, s_dwHeight, rt_Generic_0->pSurface->GetFormat());	
	}

	// Gamma
	CreateEffectSQ<CBlender_gamma>(s_gamma);

	//NVG
	{
		CreateEffectSQ<CBlender_nvg>(s_nvg);
	}

	//SSLR
	{
		if(RImplementation.o.offscreen_reflecitons)
		{
			u32 RefSize = 256;

			rt_Reflection.create(r2_RT_env, RefSize, ERHI_FORMAT::R11G11B10_FLOAT); // Color
			rt_Reflection_temp.create(r2_RT_env_temp, RefSize, ERHI_FORMAT::R16_FLOAT); // Distance2Point

			// Maybe remove rt_Depth? (like LV)
			rt_Depth.create(r2_RT_env_depth, RefSize, RefSize, ERHI_FORMAT::D16_UNORM);
			rt_Reflection_forward.create(r2_RT_env_fwd, RefSize * 2u, RefSize * 2u, ERHI_FORMAT::R8G8B8A8_UNORM_SRGB, 1, CRT::CRTCreationFlags::AUTOGEN_MIP_MAPS);

			rt_water_probe_prev.create(r2_RT_water_probe_prev, s_dwWidth, s_dwHeight, ERHI_FORMAT::R16G16B16A16_FLOAT);
			Cmd.ClearTarget(rt_water_probe_prev->pRT);
		}
		else
		{
			rt_Reflection.create(r2_RT_env, 4u, ERHI_FORMAT::R11G11B10_FLOAT); // Color
			rt_Reflection_temp.create(r2_RT_env_temp, 4u, ERHI_FORMAT::R16_FLOAT); // Distance2Point
			rt_Reflection_forward.create(r2_RT_env_fwd, 4u, 4u, ERHI_FORMAT::R8G8B8A8_UNORM_SRGB);

			rt_water_probe_prev.create(r2_RT_water_probe_prev, 4u, 4u, ERHI_FORMAT::R16G16B16A16_FLOAT);
			Cmd.ClearTarget(rt_water_probe_prev->pRT);

			for(auto& pRT : rt_Reflection_temp->pRT)
			{
				Fvector4 FogPlane = { 2000, 0, 0, 0 };
				Cmd.ClearTarget(pRT, &FogPlane.x);
			}

			Fvector4 Reflections = { 0, 0, 0, 0.75f }; // Hack
			Cmd.ClearTarget(rt_Reflection_forward->pRT, &Reflections.x);
		}

		CreateEffectSQ<CBlender_sslr>(s_sslr);
	}

	// DIRECT (spot)
	ERHI_FORMAT depth_format = (ERHI_FORMAT)RImplementation.o.HW_smap_FORMAT;

	u32	size = RImplementation.o.smapsize;
	{
		RHITextureDesc TextureDesc = {};
		TextureDesc.Width = size;
		TextureDesc.Height = size;
		TextureDesc.BindFlags = ERHI_BIND_FLAG::DEPTH_STENCIL | ERHI_BIND_FLAG::SHADER_RESOURCE;
		TextureDesc.Format = depth_format;
		TextureDesc.ArraySize = SmapPages;
		TextureDesc.MipLevels = 1;
		TextureDesc.CPUAccessFlags = 0;
		TextureDesc.SampleDescCount = 1;
		TextureDesc.Usage = ERHI_USAGE::USAGE_DEFAULT;

		RHISubResource Subres = {};
		rt_smap_depth_tex = GRHI->CreateTexture2D(TextureDesc, Subres);

		RHIDepthStencilViewDesc ViewDesc = {};
		ViewDesc.ViewDimension = ERHI_DSV_DIMENSION::TEXTURE2DARRAY;
		ViewDesc.Format = ERHI_FORMAT::D24_UNORM_S8_UINT;
		ViewDesc.ArraySize = 1;
		ViewDesc.MipSlice = 0;

		for (u32 Page = 0; Page < SmapPages; ++Page)
		{
			ViewDesc.FirstArraySlice = Page;
			rt_smap_depth_dsv[Page] = GRHI->CreateDepthStencilView(rt_smap_depth_tex, ViewDesc);
		}

		rt_smap_depth.create(r2_RT_smap_depth);
		rt_smap_depth->surface_set(rt_smap_depth_tex);
	}

	s_accum_direct_volumetric.create("accum_volumetric_sun");

	{
		RHITextureDesc TextureDesc = {};
		TextureDesc.Width = size;
		TextureDesc.Height = size;
		TextureDesc.BindFlags = ERHI_BIND_FLAG::DEPTH_STENCIL | ERHI_BIND_FLAG::SHADER_RESOURCE;
		TextureDesc.Format = ERHI_FORMAT::R32_TYPELESS;
		TextureDesc.ArraySize = SunSlices;
		TextureDesc.MipLevels = 1;
		TextureDesc.CPUAccessFlags = 0;
		TextureDesc.SampleDescCount = 1;
		TextureDesc.Usage = ERHI_USAGE::USAGE_DEFAULT;

		//creating a texture
		RHISubResource Subres = {};
		rt_smap_depth_sun_tex = GRHI->CreateTexture2D(TextureDesc, Subres);

		//Create DSV for each slice
		RHIDepthStencilViewDesc depthStencilViewDesc = {};
		depthStencilViewDesc.ViewDimension = ERHI_DSV_DIMENSION::TEXTURE2DARRAY;
		depthStencilViewDesc.Format = ERHI_FORMAT::D32_FLOAT;
		depthStencilViewDesc.ArraySize = 1;
		depthStencilViewDesc.MipSlice = 0;

		for(int dsv_idx = 0; dsv_idx < int(SunSlices); dsv_idx++)
		{
			depthStencilViewDesc.FirstArraySlice = dsv_idx;
			rt_smap_depth_sun_dsv[dsv_idx] = GRHI->CreateDepthStencilView(rt_smap_depth_sun_tex, depthStencilViewDesc);
		}

		//Now bind create SRV for it
		rt_smap_depth_sun.create(r2_RT_smap_depth_sun);
		rt_smap_depth_sun->surface_set(rt_smap_depth_sun_tex);
	}

	t_rain_splash.create("water\\water_SBumpVolume");
	t_rain_streaks.create("water\\water_flowing_nmap");
	t_puddle_normal.create("shaders\\rain_puddles_normal");

	if (EngineExternal()[EEngineExternalRender::UseDynamicSnowMask])
	{
		const char* Snow = pSettings->section_exist("custom_textures") && pSettings->line_exist("custom_textures", "snow_texture") ? pSettings->r_string("custom_textures", "snow_texture") : "shaders\\snowmask\\snow";
		t_snow_base.create(Snow);

		if (t_snow_base.bump_exist())
		{
			string_path Extra;
			t_snow_bump.create(t_snow_base.bump_get().c_str());
			t_snow_bump_x.create(xr_strconcat(Extra, t_snow_base.bump_get().c_str(), "#"));
			SnowIsPbr = DEV->m_textures_description.UsePBRTexures(Snow);
			SnowInMaterial = true;
		}
	}
	t_puddle_ripples.create("shaders\\rain_puddles_ripples");
	t_puddle_water.create("$user$rain_puddles_water_height");

	string_path name{};

	// New BLOOM and LUM
	{
		ERHI_FORMAT	fmt = ERHI_FORMAT::R11G11B10_FLOAT;

		u32 BW_G = std::ceil(Device.TargetWidth / 128.0f), 
			BH_G = std::ceil(Device.TargetHeight / 128);

		u32 BW_F = BW_G * 2, BH_F = BH_G * 2;
		u32 BW_E = BW_F * 2, BH_E = BH_F * 2;
		u32 BW_D = BW_E * 2, BH_D = BH_E * 2;
		u32 BW_C = BW_D * 2, BH_C = BH_D * 2;
		u32 BW_B = BW_C * 2, BH_B = BH_C * 2;
		u32 BW_A = BW_B * 2, BH_A = BH_B * 2;

		CreateEffectSQ<CBlender_bloom_downsample>(s_bloom_downsample);
		CreateEffectSQ<CBlender_bloom_upsample>(s_bloom_upsample);

		rt_Bloom_A.create(r2_RT_bloomA, BW_A, BH_A, fmt);
		rt_Bloom_B.create(r2_RT_bloomB, BW_B, BH_B, fmt);
		rt_Bloom_C.create(r2_RT_bloomC, BW_C, BH_C, fmt);
		rt_Bloom_D.create(r2_RT_bloomD, BW_D, BH_D, fmt);
		rt_Bloom_E.create(r2_RT_bloomE, BW_E, BH_E, fmt);
		rt_Bloom_F.create(r2_RT_bloomF, BW_F, BH_F, fmt);
		rt_Bloom_G.create(r2_RT_bloomG, BW_G, BH_G, fmt);
		rt_Bloom_A2.create(r2_RT_bloomA2, BW_A, BH_A, fmt);
		rt_Bloom_B2.create(r2_RT_bloomB2, BW_B, BH_B, fmt);
		rt_Bloom_C2.create(r2_RT_bloomC2, BW_C, BH_C, fmt);
		rt_Bloom_D2.create(r2_RT_bloomD2, BW_D, BH_D, fmt);
		rt_Bloom_E2.create(r2_RT_bloomE2, BW_E, BH_E, fmt);
		rt_Bloom_F2.create(r2_RT_bloomF2, BW_F, BH_F, fmt);

		fmt = ERHI_FORMAT::R32_FLOAT;
		u32 LW_A = 1024, LH_A = 1024;
		u32 LW_B = LW_A / 8, LH_B = LH_A / 8;
		u32 LW_C = LW_B / 8, LH_C = LH_B / 8;
		u32 LW_D = 1, LH_D = 1;

		CreateEffectSQ<CBlender_new_adaptation>(s_lum_copy);

		rt_LUM_A.create(r2_RT_lumA, LW_A, LH_A, fmt);
		rt_LUM_B.create(r2_RT_lumB, LW_B, LH_B, fmt);
		rt_LUM_C.create(r2_RT_lumC, LW_C, LH_C, fmt);

		rt_LUM_D.create(r2_RT_luminance_cur, LW_D, LH_D, fmt);

		Cmd.ClearTarget(rt_LUM_D->pRT, ERTColor::Gray);

		f_bloom_factor = 0.5f;
	}
	// new dof
	
	{
		ERHI_FORMAT fmt = ERHI_FORMAT::R32_FLOAT;
		u32 FW = 1, FH = 1;
		rt_dof_focus.create(r2_RT_dof_focus, FW, FH, fmt);
		rt_dof_focus_prev.create(r2_RT_dof_focus_prev, FW, FH, fmt);

		// Zero marks uninitialized focus history; the first valid sample seeds it.
		Cmd.ClearTarget(rt_dof_focus->pRT, ERTColor::Transparent);
		Cmd.ClearTarget(rt_dof_focus_prev->pRT, ERTColor::Transparent);
		//fmt = ERHI_FORMAT::R16G16B16A16_FLOAT;
		u32 CoCW = Device.TargetWidth, CoCH = Device.TargetHeight;
		rt_dof_coc.create(r2_RT_dof_coc, CoCW, CoCH, fmt);
		rt_dof_coc_prev.create(r2_RT_dof_coc_prev, CoCW, CoCH, fmt);

		CreateDofBlurTargets(Cmd);

		CreateEffectSQ<CBlender_new_dof>(s_dof_coc);
		CBlender_new_dof BlurBlender(true);
		s_dof_blur1.create(&BlurBlender);
	}

	rt_ssao_temp.create(r2_RT_ssao_temp, s_dwWidth, s_dwHeight, ERHI_FORMAT::R8_UNORM, 1, CRT::USE_UAV_FLAG);

	// COMBINE
	{

		s_combine.create(b_combine, "r2\\combine");
		s_combine_volumetric.create("combine_volumetric");

		t_envmap_0.create(r2_T_envs0);
		t_envmap_1.create(r2_T_envs1);
	}

	// Build textures
	{
		// Build material(s)
		{
			u16	tempData[TEX_material_LdotN * TEX_material_LdotH * TEX_material_Count];

			RHITextureDesc desc;
			desc.Width = TEX_material_LdotN;
			desc.Height = TEX_material_LdotH;
			desc.Depth	= TEX_material_Count;
			desc.MipLevels = 1;
			desc.Format = ERHI_FORMAT::R8G8_UNORM;
			desc.Usage = ERHI_USAGE::USAGE_IMMUTABLE;
			desc.BindFlags = ERHI_BIND_FLAG::SHADER_RESOURCE;
			desc.CPUAccessFlags = 0;
			desc.MiscFlags = 0;

			RHISubResource subData;

			subData.Data = tempData;
			subData.RowPitch = desc.Width*2;
			subData.DepthPitch = desc.Height*subData.RowPitch;

			for (u32 slice=0; slice<TEX_material_Count; slice++)
			{
				for (u32 y=0; y<TEX_material_LdotH; y++)
				{
					for (u32 x=0; x<TEX_material_LdotN; x++)
					{
						u16*	p	=	(u16*)		
							((u8*)(subData.Data)
							+ slice*subData.DepthPitch
							+ y*subData.RowPitch + x*2);
						float	ld	=	float(x)	/ float	(TEX_material_LdotN-1);
						float	ls	=	float(y)	/ float	(TEX_material_LdotH-1) + EPS_S;
						ls			*=	powf(ld,1/32.f);
						float	fd,fs;

						switch	(slice)
						{
						case 0:	{ // looks like OrenNayar
							fd	= powf(ld,0.75f);		// 0.75
							fs	= powf(ls,16.f)*.5f;
								}	break;
						case 1:	{// looks like Blinn
							fd	= powf(ld,0.90f);		// 0.90
							fs	= powf(ls,24.f);
								}	break;
						case 2:	{ // looks like Phong
							fd	= ld;					// 1.0
							fs	= powf(ls*1.01f,128.f	);
								}	break;
						case 3:	{ // looks like Metal
							float	s0	= std::abs(1-std::abs	(0.05f*std::sin(33.f*ld)+ld-ls));
							float	s1	= std::abs(1-std::abs	(0.05f*std::cos(33.f*ld*ls)+ld-ls));
							float	s2	= std::abs(1-std::abs	(ld-ls));
							fd		=	ld;				// 1.0
							fs		=	powf	(std::max(std::max(s0,s1),s2), 24.f);
							fs		*=	powf	(ld,1/7.f);
								}	break;
						default:
							fd	= fs = 0;
						}
						s32		_d	=	clampr	(iFloor	(fd*255.5f),	0,255);
						s32		_s	=	clampr	(iFloor	(fs*255.5f),	0,255);
						if ((y==(TEX_material_LdotH-1)) && (x==(TEX_material_LdotN-1)))	{ _d = 255; _s=255;	}
						*p			=	u16		(_s*256 + _d);
					}
				}
			}

			t_material = dxRenderDeviceRender::Instance().Resources->_CreateTexture(r2_material);
			t_material->Make(desc, &subData);
			R_ASSERT(t_material->pSurface);
		}

		// Build noise table
		{
			static const int sampleSize = 4;
			u32	tempData[TEX_jitter_count][TEX_jitter * TEX_jitter];

			RHITextureDesc desc;
			desc.Width = TEX_jitter;
			desc.Height = TEX_jitter;
			desc.MipLevels = 1;
			desc.ArraySize = 1;
			desc.Format = ERHI_FORMAT::R8G8B8A8_SNORM;

			desc.Usage = ERHI_USAGE::USAGE_DEFAULT;
			desc.BindFlags = ERHI_BIND_FLAG::SHADER_RESOURCE;
			desc.CPUAccessFlags = 0;
			desc.MiscFlags = 0;

			RHISubResource subData[TEX_jitter_count];

			for(int it = 0; it < TEX_jitter_count; it++)
			{
				subData[it].Data = tempData[it];
				subData[it].DataSize = desc.Width * sampleSize;
			}

			for(u32 y = 0; y < TEX_jitter; y++)
			{
				for(u32 x = 0; x < TEX_jitter; x++)
				{
					u32 data[TEX_jitter_count];
					generate_jitter(data, TEX_jitter_count);
					for(u32 it = 0; it < TEX_jitter_count; it++)
					{
						u32* p = (u32*)((u8*)(subData[it].Data) + y * subData[it].DataSize + x * 4);
						*p = data[it];
					}
				}
			}

			for(int it = 0; it < TEX_jitter_count; it++)
			{
				xr_sprintf(name, "%s%d", r2_jitter, it);
				t_noise[it] = dxRenderDeviceRender::Instance().Resources->_CreateTexture(name);
				t_noise[it]->Make(desc, &subData[it]);
			}
		}
	}

	// PP
	s_postprocess.create("postprocess");
	ScopePostprocessShader.create("scope_postprocess");

	// Menu
	s_menu.create("distort");
	g_menu.create(EGeomSource::Ring);

	CRT::MarkViewSized = false;

	for (const ref_rt* Fixed : { &rt_LUM_A, &rt_LUM_B, &rt_LUM_C, &rt_LUM_D, &rt_dof_focus, &rt_dof_focus_prev, &rt_Depth, &rt_Reflection_forward })
	{
		if (*Fixed)
		{
			(*Fixed)->IsViewSized = false;
		}
	}

	u_setrt(Cmd, Device.TargetWidth, Device.TargetHeight, rt_BackbufferLUT->pRT, nullptr, nullptr, nullptr);

	dwWidth = Device.TargetWidth;
	dwHeight = Device.TargetHeight;
}

CRenderTarget::~CRenderTarget	()
{
	ClearSpotShadowCache();
	if (EditorWindowDepth != nullptr)
	{
		RImplementation.Imm().SetDepthStencil(nullptr);
		GRHI->DevicePtr->RenderDSV = EditorWindowDepth;
		EditorWindowDepth = nullptr;
	}

	rt_smap_depth_sun->surface_set(nullptr);
	_RELEASE(rt_smap_depth_sun_tex);

	rt_smap_depth->surface_set(nullptr);
	_RELEASE(rt_smap_depth_tex);

	for (IRHIDepthStencilView*& Page : rt_smap_depth_dsv)
	{
		_RELEASE(Page);
	}

	for(auto i = 0; i < int(SunSlices); ++i)
	{
		_RELEASE(rt_smap_depth_sun_dsv[i]);
	}
	
	// Textures
	t_material->surface_set		(nullptr);

#ifdef DEBUG
	IRHISurface*	pSurf = 0;

	pSurf = t_envmap_0->surface_get();
	if (pSurf) pSurf->Release();
	_SHOW_REF("t_envmap_0 - #small", pSurf);

	pSurf = t_envmap_1->surface_get();
	if (pSurf) pSurf->Release();
	_SHOW_REF("t_envmap_1 - #small", pSurf);
	//_SHOW_REF("t_envmap_0 - #small",t_envmap_0->pSurface);
	//_SHOW_REF("t_envmap_1 - #small",t_envmap_1->pSurface);
#endif // DEBUG
	t_envmap_0->surface_set		(nullptr);
	t_envmap_1->surface_set		(nullptr);
	t_envmap_0.destroy			();
	t_envmap_1.destroy			();

	//	TODO: DX10: Check if we need old style SMAPs
//	_RELEASE					(rt_smap_ZB);

	// Jitter
	for (int it = 0; it < TEX_jitter_count; it++)
	{
		t_noise[it]->surface_set(nullptr);
	}

	// Blenders
	xr_delete(b_combine);
	xr_delete(b_luminance);
	xr_delete(b_bloom);
	xr_delete(FlatGasMaskBlender);

	g_Fsr3Wrapper.Destroy();

	CImGuiManager::Instance().Unsubscribe("GraphicDebug");

	if (g_debug_blend_state != nullptr) 
	{
		g_debug_blend_state->Release();
		g_debug_blend_state = nullptr;
	}
}

bool CRenderTarget::need_to_render_sunshafts()
{
	if(ps_r_sun_shafts) {
		const CEnvDescriptor& E = RFrame.Env();
		Fcolor sun_color = ((light*)RImplementation.Lights.sun_adapted._get())->color;
		float fValue = E.m_fSunShaftsIntensity * u_diffuse2s(sun_color.r, sun_color.g, sun_color.b);

		if(fValue > EPS) {
			return true;
		}
	}

	return false;
}

void CRenderTarget::copy_position()
{
	GPU_EVENT(ZBUFFER_COPY);

	RImplementation.Imm().SetDepthStencil(nullptr);
	RImplementation.Imm().CopySurface(rt_Position->pSurface, RDepth->GetSurface());
}
