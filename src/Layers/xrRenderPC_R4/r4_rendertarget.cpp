#include "stdafx.h"

#include "../xrRender/resourcemanager.h"
#include "blender_light_occq.h"
#include "blender_light_mask.h"
#include "blender_light_direct.h"
#include "blender_light_point.h"
#include "blender_light_spot.h"
#include "blender_light_reflected.h"
#include "blender_combine.h"
#include "../xrRender/blender_screen_postprocess.h"
#include "blender_bloom_build.h"
#include "blender_luminance.h"
#include "blender_ssao.h"
#include "blender_scale.h"
#include "blender_cas.h"
#include "blender_gtao.h"
#include "dx11HDAOCSBlender.h"
#include "../xrRenderDX10/DX10 Rain/dx10RainBlender.h"
#include "../xrRender/blender_fxaa.h"
#include "../xrRender/blender_smaa.h"
#include "../xrRender/dxRenderDeviceRender.h"
#include "magic_enum/magic_enum.hpp"
#include "FSR2Wrapper.h"
#include "DLSSWrapper.h"

void	CRenderTarget::u_setrt(const ref_rt& _1, const ref_rt& _2, const ref_rt& _3, const ref_rt& _4, ID3DDepthStencilView* zb)
{
	VERIFY(_1 || zb);
	if (_1)	{
		dwWidth = _1->dwWidth;
		dwHeight = _1->dwHeight;
	}
	else {
		D3D_DEPTH_STENCIL_VIEW_DESC	desc;
		zb->GetDesc(&desc);

		VERIFY(desc.ViewDimension == D3D_DSV_DIMENSION_TEXTURE2D);

		ID3DResource* pRes;

		zb->GetResource(&pRes);

		ID3DTexture2D* pTex = (ID3DTexture2D*)pRes;

		D3D_TEXTURE2D_DESC	TexDesc;

		pTex->GetDesc(&TexDesc);

		dwWidth = TexDesc.Width;
		dwHeight = TexDesc.Height;
		_RELEASE(pRes);
	}

	RCache.set_RT(_1 ? _1->pRT : NULL, 0);
	RCache.set_RT(_2 ? _2->pRT : NULL, 1);
	RCache.set_RT(_3 ? _3->pRT : NULL, 2);
	RCache.set_RT(_4 ? _4->pRT : NULL, 3);
	RCache.set_ZB(zb);
}

void	CRenderTarget::u_setrt			(const ref_rt& _1, const ref_rt& _2, const ref_rt& _3, ID3DDepthStencilView* zb)
{
	VERIFY									(_1||zb);
	if (_1)
	{
		dwWidth									= _1->dwWidth;
		dwHeight								= _1->dwHeight;
	}
	else
	{
		D3D_DEPTH_STENCIL_VIEW_DESC	desc;
		zb->GetDesc(&desc);

		VERIFY(desc.ViewDimension == D3D_DSV_DIMENSION_TEXTURE2D);

		ID3DResource* pRes;

		zb->GetResource(&pRes);

		ID3DTexture2D* pTex = (ID3DTexture2D*)pRes;

		D3D_TEXTURE2D_DESC	TexDesc;

		pTex->GetDesc(&TexDesc);

		dwWidth = TexDesc.Width;
		dwHeight = TexDesc.Height;
		_RELEASE(pRes);
	}

	if (_1) RCache.set_RT(_1->pRT,	0); else RCache.set_RT(nullptr,0);
	if (_2) RCache.set_RT(_2->pRT,	1); else RCache.set_RT(nullptr,1);
	if (_3) RCache.set_RT(_3->pRT,	2); else RCache.set_RT(nullptr,2);
	RCache.set_RT(nullptr, 3);

	RCache.set_ZB							(zb);
}

void	CRenderTarget::u_setrt			(const ref_rt& _1, const ref_rt& _2, ID3DDepthStencilView* zb)
{
	VERIFY									(_1||zb);
	if (_1)
	{
		dwWidth									= _1->dwWidth;
		dwHeight								= _1->dwHeight;
	}
	else
	{
		D3D_DEPTH_STENCIL_VIEW_DESC	desc;
		zb->GetDesc(&desc);
		VERIFY(desc.ViewDimension==D3D_DSV_DIMENSION_TEXTURE2D);

		ID3DResource *pRes;

		zb->GetResource( &pRes);

		ID3DTexture2D *pTex = (ID3DTexture2D *)pRes;

		D3D_TEXTURE2D_DESC	TexDesc;

		pTex->GetDesc(&TexDesc);

		dwWidth = TexDesc.Width;
		dwHeight = TexDesc.Height;
		_RELEASE( pRes );
	}

	if (_1) RCache.set_RT(_1->pRT,	0); else RCache.set_RT(nullptr,0);
	if (_2) RCache.set_RT(_2->pRT,	1); else RCache.set_RT(nullptr,1);
	RCache.set_RT(nullptr, 2);
	RCache.set_RT(nullptr, 3);

	RCache.set_ZB (zb);
}

void	CRenderTarget::u_setrt			(u32 W, u32 H, ID3DRenderTargetView* _1, ID3DRenderTargetView* _2, ID3DRenderTargetView* _3, ID3DDepthStencilView* zb)
{
	//VERIFY									(_1);
	dwWidth									= W;
	dwHeight								= H;
	//VERIFY									(_1);
	RCache.set_RT							(_1,	0);
	RCache.set_RT							(_2,	1);
	RCache.set_RT							(_3,	2);
	RCache.set_RT(nullptr, 3);
	RCache.set_ZB							(zb);
//	RImplementation.rmNormal				();
}

void	CRenderTarget::u_stencil_optimize	(eStencilOptimizeMode eSOM)
{
	//	TODO: DX10: remove half pixel offset?
	VERIFY	(RImplementation.o.nvstencil);
	//RCache.set_ColorWriteEnable	(FALSE);
	u32		Offset;
	float	_w					= RCache.get_width();
	float	_h					= RCache.get_height();
	u32		C					= color_rgba	(255,255,255,255);
	float	eps					= 0;
	float	_dw					= 0.5f;
	float	_dh					= 0.5f;
	FVF::TL* pv					= (FVF::TL*) RCache.Vertex.Lock	(4,g_combine->vb_stride,Offset);
	pv->set						(-_dw,		_h-_dh,		eps,	1.f, C, 0, 0);	pv++;
	pv->set						(-_dw,		-_dh,		eps,	1.f, C, 0, 0);	pv++;
	pv->set						(_w-_dw,	_h-_dh,		eps,	1.f, C, 0, 0);	pv++;
	pv->set						(_w-_dw,	-_dh,		eps,	1.f, C, 0, 0);	pv++;
	RCache.Vertex.Unlock		(4,g_combine->vb_stride);
	RCache.set_Element			(s_occq->E[1]	);

	switch(eSOM)
	{
	case SO_Light:
		StateManager.SetStencilRef(dwLightMarkerID);
		break;
	case SO_Combine:
		StateManager.SetStencilRef(0x01);
		break;
	default:
		VERIFY(!"CRenderTarget::u_stencil_optimize. switch no default!");
	}	

	RCache.set_Geometry			(g_combine		);
	RCache.Render				(D3DPT_TRIANGLELIST,Offset,0,4,0,2);
	
}

// 2D texgen (texture adjustment matrix)
void	CRenderTarget::u_compute_texgen_screen	(Fmatrix& m_Texgen)
{
	//float	_w						= float(Device.TargetWidth);
	//float	_h						= float(Device.TargetHeight);
	//float	o_w						= (.5f / _w);
	//float	o_h						= (.5f / _h);
	Fmatrix			m_TexelAdjust		= 
	{
		0.5f,				0.0f,				0.0f,			0.0f,
		0.0f,				-0.5f,				0.0f,			0.0f,
		0.0f,				0.0f,				1.0f,			0.0f,
		//	Removing half pixel offset
		//0.5f + o_w,			0.5f + o_h,			0.0f,			1.0f
		0.5f,				0.5f ,				0.0f,			1.0f
	};
	m_Texgen.mul	(m_TexelAdjust,RCache.xforms.m_wvp);
}

// 2D texgen for jitter (texture adjustment matrix)
void	CRenderTarget::u_compute_texgen_jitter	(Fmatrix&		m_Texgen_J)
{
	// place into	0..1 space
	Fmatrix			m_TexelAdjust		= 
	{
		0.5f,				0.0f,				0.0f,			0.0f,
		0.0f,				-0.5f,				0.0f,			0.0f,
		0.0f,				0.0f,				1.0f,			0.0f,
		0.5f,				0.5f,				0.0f,			1.0f
	};
	m_Texgen_J.mul	(m_TexelAdjust,RCache.xforms.m_wvp);

	// rescale - tile it
	float	scale_X			= RCache.get_width() / float(TEX_jitter);
	float	scale_Y			= RCache.get_height() / float(TEX_jitter);
	//float	offset			= (.5f / float(TEX_jitter));
	m_TexelAdjust.scale			(scale_X,	scale_Y,1.f	);
	//m_TexelAdjust.translate_over(offset,	offset,	0	);
	m_Texgen_J.mulA_44			(m_TexelAdjust);
}

u8		fpack			(float v)				{
	s32	_v	= iFloor	(((v+1)*.5f)*255.f + .5f);
	clamp	(_v,0,255);
	return	u8(_v);
}
u8		fpackZ			(float v)				{
	s32	_v	= iFloor	(_abs(v)*255.f + .5f);
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
	for (int x=_max(bx-d,0); x<=_min(bx+d,255); x++)
	for (int y=_max(by-d,0); y<=_min(by+d,255); y++)
	for (int z=_max(bz-d,0); z<=_min(bz+d,255); z++)
	{
		_v				= vunpack(x,y,z);
		float	m		= _v.magnitude();
		float	me		= _abs(m-1.f);
		if	(me>0.03f)	continue;
		_v.div	(m);
		float	e		= _abs(src.dotproduct(_v)-1.f);
		if (e<e_best)	{
			e_best		= e;
			r=x,g=y,b=z;
		}
	}
	Ivector		ipck;
	ipck.set	(r,g,b);
	return		ipck;
}

void	generate_jitter	(DWORD*	dest, u32 elem_count)
{
	const	int		cmax		= 8;
	svector<Ivector2,cmax>		samples;
	while (samples.size()<elem_count*2)
	{
		Ivector2	test;
		test.set	(::Random.randI(0,256),::Random.randI(0,256));
		BOOL		valid = TRUE;
		for (u32 t=0; t<samples.size(); t++)
		{
			int		dist	= _abs(test.x-samples[t].x)+_abs(test.y-samples[t].y);
			if (dist<32)	{
				valid		= FALSE;
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
	return (u32)RCache.get_width();
}

u32 CRenderTarget::get_core_height()
{
	return (u32)RCache.get_height();
}

u32 CRenderTarget::get_target_width()
{
	return (u32)RCache.get_target_width();
}

u32 CRenderTarget::get_target_height()
{
	return (u32)RCache.get_target_height();
}

#include "imgui_impl_dx11.h"
ImGui_ImplDX11_Data* ImGui_ImplDX11_GetBackendData();

xr_map<xr_string, float> PowerMap;

CRenderTarget::CRenderTarget()
{
	if (g_debug_blend_state == nullptr) {
		D3D11_BLEND_DESC desc;
		ZeroMemory(&desc, sizeof(desc));
		desc.AlphaToCoverageEnable = false;
		desc.RenderTarget[0].BlendEnable = false;
		desc.RenderTarget[0].SrcBlend = D3D11_BLEND_SRC_ALPHA;
		desc.RenderTarget[0].DestBlend = D3D11_BLEND_DEST_COLOR;
		desc.RenderTarget[0].BlendOp = D3D11_BLEND_OP_ADD;
		desc.RenderTarget[0].SrcBlendAlpha = D3D11_BLEND_ONE;
		desc.RenderTarget[0].DestBlendAlpha = D3D11_BLEND_ZERO;
		desc.RenderTarget[0].BlendOpAlpha = D3D11_BLEND_OP_ADD;
		desc.RenderTarget[0].RenderTargetWriteMask = D3D11_COLOR_WRITE_ENABLE_ALL;
		R_CHK(RDevice->CreateBlendState(&desc, &g_debug_blend_state));
	}

	CImGuiManager::Instance().Subscribe("GraphicDebug", CImGuiManager::ERenderPriority::eMedium, [this]() {
		if (!Engine.External.EditorStates[static_cast<std::uint8_t>(EditorUI::Shaders)]) {
			return;
		}

		auto State = g_debug_blend_state;
		auto DisplayTarget = [State](const ref_rt& rt, bool& ShowData) {
			if (rt._get() == nullptr || rt->pTexture == nullptr || rt->pTexture->get_SRView() == nullptr) {
				return;
			}

			string64 ShowText = {};
			sprintf(ShowText, "Show ##%p", (void*)&ShowData);

			ImGui::Checkbox(ShowText, &ShowData);
			ImGui::SameLine();

			const auto& Texture = *rt->pTexture;
			
			ImGui::Text(
				"Target %s (fmt: %s, width: %i, height: %i)",
				Texture.cName.c_str(), magic_enum::enum_name(rt->fmt).data(), rt->dwWidth, rt->dwHeight
			);

			if (!ShowData)
				return;

			auto ContentRegion = ImGui::GetContentRegionAvail();
			float scale = ImGui::GetContentRegionAvail().x / rt->dwWidth;
			auto& DrawList = *ImGui::GetWindowDrawList();

			if (!PowerMap.contains(Texture.cName.c_str())) {
				PowerMap[Texture.cName.c_str()] = 1;
			}

			float& ImagePower = PowerMap[Texture.cName.c_str()];
			ImGui::SliderFloat(Texture.cName.c_str(), &ImagePower, 0.0f, 1.0f);

			DrawList.AddCallback([](const ImDrawList* parent_list, const ImDrawCmd* cmd) {
				const float blend_factor[4] = { 0.f, 0.f, 0.f, 0.f };
				RContext->OMSetBlendState((ID3D11BlendState*)cmd->UserCallbackData, blend_factor, 0xffffffff);
				}, State);
			ImGui::Image(
				rt->pTexture->get_SRView(),
				ImVec2(ImGui::GetContentRegionAvail().x, rt->dwHeight * scale),
				ImVec2(0, 0), ImVec2(1, 1), ImVec4(ImagePower, ImagePower, ImagePower, 1.0f)
			);
			DrawList.AddCallback([](const ImDrawList* parent_list, const ImDrawCmd* cmd) {
				auto bd = ImGui_ImplDX11_GetBackendData();
				if (bd != nullptr) {
					const float blend_factor[4] = { 0.f, 0.f, 0.f, 0.f };
					RContext->OMSetBlendState((ID3D11BlendState*)bd->pBlendState, blend_factor, 0xffffffff);
				}
				}, State);
			};

		ID3D11BlendState* BlendState = nullptr;
		if (!ImGui::Begin("GraphicDebug", &Engine.External.EditorStates[static_cast<std::uint8_t>(EditorUI::Shaders)], ImGuiWindowFlags_NoTitleBar | ImGuiWindowFlags_NoCollapse)) {
			ImGui::End();
			return;
		}

#define DisplayRT(rt) \
		static bool Show##rt = false; \
		DisplayTarget(rt, Show##rt);

		DisplayRT(rt_Accumulator);
		DisplayRT(rt_Color);
		DisplayRT(rt_Back_Buffer_AA);
		DisplayRT(rt_Back_Buffer);
		DisplayRT(rt_Bloom_1);
		DisplayRT(rt_Generic);
		DisplayRT(rt_Generic_0);
		DisplayRT(rt_Generic_1);
		DisplayRT(rt_Generic_2);
		DisplayRT(rt_Normal);
		DisplayRT(rt_Position);
		DisplayRT(rt_ssao_temp);
		DisplayRT(rt_Velocity);
		DisplayRT(rt_ui_pda);

#undef DisplayRT

		ImGui::End();
	});

	if(ps_r_ssao_mode != 2) {
		ps_r_ssao = _min(ps_r_ssao, 3);
	}

	RImplementation.SSAO.set(ESSAO_DATA::SSAO_ULTRA_OPT, ps_r_ssao > 3);

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

	dwAccumulatorClearMark = 0;
	dxRenderDeviceRender::Instance().Resources->Evict();

	// Blenders
	b_occq = new CBlender_light_occq();
	b_accum_mask = new CBlender_accum_direct_mask();
	b_accum_direct = new CBlender_accum_direct();
	b_accum_point = new CBlender_accum_point();
	b_accum_spot = new CBlender_accum_spot();
	b_accum_reflected = new CBlender_accum_reflected();
	b_bloom = new CBlender_bloom_build();
	b_luminance = new CBlender_luminance();
	b_combine = new CBlender_combine();
	b_hdao_cs = new CBlender_CS_HDAO();
	b_ssao = new CBlender_SSAO();


	u32 s_dwWidth = (u32)RCache.get_width(), s_dwHeight = (u32)RCache.get_height();

	// NORMAL
	{
		rt_Position.create(r2_RT_P, s_dwWidth, s_dwHeight, DxgiFormat::DXGI_FORMAT_R24G8_TYPELESS);
		rt_Surface.create(r2_RT_S, s_dwWidth, s_dwHeight, DxgiFormat::DXGI_FORMAT_R8G8B8A8_UNORM);
		rt_Normal.create(r2_RT_N, s_dwWidth, s_dwHeight, DxgiFormat::DXGI_FORMAT_R16G16B16A16_UNORM);
		rt_Color.create(r2_RT_albedo, s_dwWidth, s_dwHeight, DxgiFormat::DXGI_FORMAT_R8G8B8A8_UNORM);

		rt_Accumulator.create(r2_RT_accum, s_dwWidth, s_dwHeight, DxgiFormat::DXGI_FORMAT_R16G16B16A16_FLOAT);

		// generic(LDR) RTs
		rt_Generic_0.create(r2_RT_generic0, s_dwWidth, s_dwHeight, DxgiFormat::DXGI_FORMAT_R16G16B16A16_FLOAT);
		rt_Generic_1.create(r2_RT_generic1, s_dwWidth, s_dwHeight, DxgiFormat::DXGI_FORMAT_R8G8B8A8_UNORM);

		rt_Generic_2.create(r2_RT_generic2, s_dwWidth, s_dwHeight, DxgiFormat::DXGI_FORMAT_R16G16B16A16_FLOAT);

		rt_Velocity.create(r2_RT_velocity, s_dwWidth, s_dwHeight, DxgiFormat::DXGI_FORMAT_R16G16_FLOAT);

		rt_Back_Buffer_AA.create(r2_RT_backbuffer_AA, get_target_width(), get_target_height(), DxgiFormat::DXGI_FORMAT_R16G16B16A16_FLOAT);
		rt_Back_Buffer.create(r2_RT_backbuffer_final, get_target_width(), get_target_height(), DxgiFormat::DXGI_FORMAT_R16G16B16A16_FLOAT);

		rt_Generic.create(r2_RT_generic, get_target_width(), get_target_height(), DxgiFormat::DXGI_FORMAT_R16G16B16A16_FLOAT, 1, RFeatureLevel >= D3D_FEATURE_LEVEL_11_0);
	
		rt_ui_pda.create(r_ui_pda, get_target_width(), get_target_height(), DxgiFormat::DXGI_FORMAT_R8G8B8A8_UNORM);
	}

	init_fsr();
	init_dlss();

	// Scale
	{
		b_scale = new CBlender_scale();
		s_scale.create(b_scale);
	}

	// FXAA
	{
		b_fxaa = new CBlender_FXAA();
		s_fxaa.create(b_fxaa);
	}

	// Screen Post Process
	{
		b_spp = new CBlender_SPP();
		s_spp.create(b_spp);
	}

	// SMAA
	{
		b_smaa = new CBlender_SMAA();
		s_smaa.create(b_smaa);

		rt_smaa_edgetex.create(r2_RT_smaa_edgetex, s_dwWidth, s_dwHeight, DxgiFormat::DXGI_FORMAT_R8G8_UNORM);
		rt_smaa_blendtex.create(r2_RT_smaa_blendtex, s_dwWidth, s_dwHeight, DxgiFormat::DXGI_FORMAT_R8G8B8A8_UNORM);
	}

	//Contrast Adaptive Sharpening
	{
		b_cas = new CBlender_cas();
		s_cas.create(b_cas);
	}

	//Ground-truth based ambient occlusion
	{
		b_gtao = new CBlender_gtao();
		s_gtao.create(b_gtao);

		rt_gtao_0.create("$user$gtao_0", s_dwWidth, s_dwHeight, DxgiFormat::DXGI_FORMAT_R32_UINT); //AO.view-z
	}

	//Puddles
	{
		s_puddles.create("effects_water_puddles");
	}

	// OCCLUSION
	s_occq.create(b_occq, "r2\\occq");

	// DIRECT (spot)
	DxgiFormat depth_format = (DxgiFormat)RImplementation.o.HW_smap_FORMAT;

	if (RImplementation.o.HW_smap)
	{
		D3DFORMAT	nullrt = D3DFMT_R5G6B5;
		if (RImplementation.o.nullrt)	nullrt = (D3DFORMAT)MAKEFOURCC('N', 'U', 'L', 'L');

		u32	size = RImplementation.o.smapsize;
		rt_smap_depth.create(r2_RT_smap_depth, size, size, depth_format);

		s_accum_mask.create(b_accum_mask, "r3\\accum_mask");
		s_accum_direct.create(b_accum_direct, "r3\\accum_direct");

		s_accum_direct_volumetric.create("accum_volumetric_sun");
	}

	//	RAIN
	//	TODO: DX10: Create resources only when DX10 rain is enabled.
	//	Or make DX10 rain switch dynamic?
	{
		CBlender_rain	TempBlender;
		s_rain.create(&TempBlender, "null");
	}

	// POINT
	{
		s_accum_point.create(b_accum_point, "r2\\accum_point_s");
		accum_point_geom_create();
		g_accum_point.create(D3DFVF_XYZ, g_accum_point_vb, g_accum_point_ib);
		accum_omnip_geom_create();
		g_accum_omnipart.create(D3DFVF_XYZ, g_accum_omnip_vb, g_accum_omnip_ib);
	}

	// SPOT
	{
		s_accum_spot.create(b_accum_spot, "r2\\accum_spot_s", "lights\\lights_spot01");
		accum_spot_geom_create();
		g_accum_spot.create(D3DFVF_XYZ, g_accum_spot_vb, g_accum_spot_ib);
	}

	{
		s_accum_volume.create("accum_volumetric", "lights\\lights_spot01");
		accum_volumetric_geom_create();
		g_accum_volumetric.create(D3DFVF_XYZ, g_accum_volumetric_vb, g_accum_volumetric_ib);
	}

	string_path name{};

	// REFLECTED
	{
		s_accum_reflected.create(b_accum_reflected, "r2\\accum_refl");
	}

	// BLOOM
	{
		DxgiFormat	fmt = DxgiFormat::DXGI_FORMAT_R8G8B8A8_UNORM;			//;		// D3DFMT_X8R8G8B8
		u32	w = BLOOM_size_X, h = BLOOM_size_Y;
		u32 fvf_build = D3DFVF_XYZRHW | D3DFVF_TEX4 | D3DFVF_TEXCOORDSIZE2(0) | D3DFVF_TEXCOORDSIZE2(1) | D3DFVF_TEXCOORDSIZE2(2) | D3DFVF_TEXCOORDSIZE2(3);
		u32 fvf_filter = (u32)D3DFVF_XYZRHW | D3DFVF_TEX8 | D3DFVF_TEXCOORDSIZE4(0) | D3DFVF_TEXCOORDSIZE4(1) | D3DFVF_TEXCOORDSIZE4(2) | D3DFVF_TEXCOORDSIZE4(3) | D3DFVF_TEXCOORDSIZE4(4) | D3DFVF_TEXCOORDSIZE4(5) | D3DFVF_TEXCOORDSIZE4(6) | D3DFVF_TEXCOORDSIZE4(7);
		rt_Bloom_1.create(r2_RT_bloom1, w, h, fmt);
		rt_Bloom_2.create(r2_RT_bloom2, w, h, fmt);
		g_bloom_build.create(fvf_build, RCache.Vertex.Buffer(), RCache.QuadIB);
		g_bloom_filter.create(fvf_filter, RCache.Vertex.Buffer(), RCache.QuadIB);
		s_bloom_dbg_1.create("effects\\screen_set", r2_RT_bloom1);
		s_bloom_dbg_2.create("effects\\screen_set", r2_RT_bloom2);
		s_bloom.create(b_bloom, "r2\\bloom");
		f_bloom_factor = 0.5f;
	}

	// TONEMAP
	{
		rt_LUM_64.create(r2_RT_luminance_t64, 64, 64, DxgiFormat::DXGI_FORMAT_R16G16B16A16_FLOAT);
		rt_LUM_8.create(r2_RT_luminance_t8, 8, 8, DxgiFormat::DXGI_FORMAT_R16G16B16A16_FLOAT);
		s_luminance.create(b_luminance, "r2\\luminance");
		f_luminance_adapt = 0.5f;

		t_LUM_src.create(r2_RT_luminance_src);
		t_LUM_dest.create(r2_RT_luminance_cur);

		// create pool
		for(u32 it = 0; it < 2; it++) {
			xr_sprintf(name, "%s_%d", r2_RT_luminance_pool, it);
			rt_LUM_pool[it].create(name, 1, 1, DxgiFormat::DXGI_FORMAT_R32_FLOAT);

			FLOAT ColorRGBA[4] = {127.0f / 255.0f, 127.0f / 255.0f, 127.0f / 255.0f, 127.0f / 255.0f};
			RContext->ClearRenderTargetView(rt_LUM_pool[it]->pRT, ColorRGBA);
		}

		u_setrt(Device.TargetWidth, Device.TargetHeight, RTarget, nullptr, nullptr, nullptr);
	}

	// HBAO
	{
		u32 w = 0;
		u32 h = 0;

		if (RImplementation.SSAO.test(ESSAO_DATA::SSAO_HALF_DATA))
		{
			w = s_dwWidth / 2;
			h = s_dwHeight / 2;
		}
		else
		{
			w = s_dwWidth;
			h = s_dwHeight;
		}

		DxgiFormat fmt = DxgiFormat::DXGI_FORMAT_R16_FLOAT;
		rt_half_depth.create(r2_RT_half_depth, w, h, fmt);
	}

	s_ssao.create(b_ssao, "r2\\ssao");
	rt_ssao_temp.create(r2_RT_ssao_temp, s_dwWidth, s_dwHeight, DxgiFormat::DXGI_FORMAT_R16_FLOAT, 1, RFeatureLevel >= D3D_FEATURE_LEVEL_11_0);

	if(RFeatureLevel >= D3D_FEATURE_LEVEL_11_0) {
		s_hdao_cs.create(b_hdao_cs, "r2\\ssao");
	}

	// COMBINE
	{
		static D3DVERTEXELEMENT9 dwDecl[] =
		{
			{ 0, 0,  D3DDECLTYPE_FLOAT4,	D3DDECLMETHOD_DEFAULT, 	D3DDECLUSAGE_POSITION,	0 },	// pos+uv
			D3DDECL_END()
		};
		s_combine.create					(b_combine,					"r2\\combine");
		s_combine_volumetric.create			("combine_volumetric");
		s_combine_dbg_0.create				("effects\\screen_set",		r2_RT_smap_surf		);	
		s_combine_dbg_1.create				("effects\\screen_set",		r2_RT_luminance_t8	);
		s_combine_dbg_Accumulator.create	("effects\\screen_set",		r2_RT_accum			);
		g_combine_VP.create					(dwDecl,		RCache.Vertex.Buffer(), RCache.QuadIB);
		g_combine.create					(FVF::F_TL,		RCache.Vertex.Buffer(), RCache.QuadIB);
		g_combine_2UV.create				(FVF::F_TL2uv,	RCache.Vertex.Buffer(), RCache.QuadIB);
		g_combine_cuboid.create				(dwDecl,	RCache.Vertex.Buffer(), RCache.Index.Buffer());

		u32 fvf_aa_blur				= D3DFVF_XYZRHW|D3DFVF_TEX4|D3DFVF_TEXCOORDSIZE2(0)|D3DFVF_TEXCOORDSIZE2(1)|D3DFVF_TEXCOORDSIZE2(2)|D3DFVF_TEXCOORDSIZE2(3);
		g_aa_blur.create			(fvf_aa_blur,	RCache.Vertex.Buffer(), RCache.QuadIB);

		u32 fvf_aa_AA				= D3DFVF_XYZRHW|D3DFVF_TEX7|D3DFVF_TEXCOORDSIZE2(0)|D3DFVF_TEXCOORDSIZE2(1)|D3DFVF_TEXCOORDSIZE2(2)|D3DFVF_TEXCOORDSIZE2(3)|D3DFVF_TEXCOORDSIZE2(4)|D3DFVF_TEXCOORDSIZE4(5)|D3DFVF_TEXCOORDSIZE4(6);
		g_aa_AA.create				(fvf_aa_AA,		RCache.Vertex.Buffer(), RCache.QuadIB);

		t_envmap_0.create			(r2_T_envs0);
		t_envmap_1.create			(r2_T_envs1);
	}

	// Build textures
	{
		// Testure for async sreenshots
		{
			D3D_TEXTURE2D_DESC	desc;
			desc.Width = s_dwWidth;
			desc.Height = s_dwHeight;
			desc.MipLevels = 1;
			desc.ArraySize = 1;
			desc.SampleDesc.Count = 1;
			desc.SampleDesc.Quality = 0;
			desc.Format = DXGI_FORMAT_R8G8B8A8_SNORM;
			desc.Usage = D3D_USAGE_STAGING;
			desc.BindFlags = 0;
			desc.CPUAccessFlags = D3D_CPU_ACCESS_READ;
			desc.MiscFlags = 0;

			R_CHK( RDevice->CreateTexture2D(&desc, 0, &t_ss_async) );
		}
		// Build material(s)
		{
			u16	tempData[TEX_material_LdotN * TEX_material_LdotH * TEX_material_Count];

			D3D_TEXTURE3D_DESC	desc;
			desc.Width = TEX_material_LdotN;
			desc.Height = TEX_material_LdotH;
			desc.Depth	= TEX_material_Count;
			desc.MipLevels = 1;
			desc.Format = DXGI_FORMAT_R8G8_UNORM;
			desc.Usage = D3D_USAGE_IMMUTABLE;
			desc.BindFlags = D3D_BIND_SHADER_RESOURCE;
			desc.CPUAccessFlags = 0;
			desc.MiscFlags = 0;

			D3D_SUBRESOURCE_DATA	subData;

			subData.pSysMem = tempData;
			subData.SysMemPitch = desc.Width*2;
			subData.SysMemSlicePitch = desc.Height*subData.SysMemPitch;

			for (u32 slice=0; slice<TEX_material_Count; slice++)
			{
				for (u32 y=0; y<TEX_material_LdotH; y++)
				{
					for (u32 x=0; x<TEX_material_LdotN; x++)
					{
						u16*	p	=	(u16*)		
							(LPBYTE (subData.pSysMem) 
							+ slice*subData.SysMemSlicePitch 
							+ y*subData.SysMemPitch + x*2);
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
							float	s0	=	_abs	(1-_abs	(0.05f*_sin(33.f*ld)+ld-ls));
							float	s1	=	_abs	(1-_abs	(0.05f*_cos(33.f*ld*ls)+ld-ls));
							float	s2	=	_abs	(1-_abs	(ld-ls));
							fd		=	ld;				// 1.0
							fs		=	powf	(_max(_max(s0,s1),s2), 24.f);
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

			R_CHK(RDevice->CreateTexture3D(&desc, &subData, &t_material_surf));
			t_material = dxRenderDeviceRender::Instance().Resources->_CreateTexture(r2_material);
			t_material->surface_set(t_material_surf);
		}

		// Build noise table
		{
			static const int sampleSize = 4;
			u32	tempData[TEX_jitter_count][TEX_jitter*TEX_jitter];

			D3D_TEXTURE2D_DESC	desc;
			desc.Width = TEX_jitter;
			desc.Height = TEX_jitter;
			desc.MipLevels = 1;
			desc.ArraySize = 1;
			desc.SampleDesc.Count = 1;
			desc.SampleDesc.Quality = 0;
			desc.Format = DXGI_FORMAT_R8G8B8A8_SNORM;
			//desc.Usage = D3D_USAGE_IMMUTABLE;
			desc.Usage = D3D_USAGE_DEFAULT;
			desc.BindFlags = D3D_BIND_SHADER_RESOURCE;
			desc.CPUAccessFlags = 0;
			desc.MiscFlags = 0;

			D3D_SUBRESOURCE_DATA	subData[TEX_jitter_count];

			for (int it=0; it<TEX_jitter_count-1; it++)
			{
				subData[it].pSysMem = tempData[it];
				subData[it].SysMemPitch = desc.Width*sampleSize;
			}

			// Fill it,
			for (u32 y=0; y<TEX_jitter; y++)
			{
				for (u32 x=0; x<TEX_jitter; x++)
				{
					DWORD	data	[TEX_jitter_count-1];
					generate_jitter	(data,TEX_jitter_count-1);
					for (u32 it=0; it<TEX_jitter_count-1; it++)
					{
						u32*	p	=	(u32*)	
							(LPBYTE (subData[it].pSysMem) 
							+ y*subData[it].SysMemPitch 
							+ x*4);

						*p	=	data	[it];
					}
				}
			}

			int it=0;

			for(; it < TEX_jitter_count - 1; it++) {
				xr_sprintf(name, "%s%d", r2_jitter, it);
				R_CHK(RDevice->CreateTexture2D(&desc, &subData[it], &t_noise_surf[it]));
				t_noise[it] = dxRenderDeviceRender::Instance().Resources->_CreateTexture(name);
				t_noise[it]->surface_set(t_noise_surf[it]);
			}

			float tempDataHBAO[TEX_jitter*TEX_jitter*4];

			// generate HBAO jitter texture (last)
			D3D_TEXTURE2D_DESC	descHBAO;
			descHBAO.Width = TEX_jitter;
			descHBAO.Height = TEX_jitter;
			descHBAO.MipLevels = 1;
			descHBAO.ArraySize = 1;
			descHBAO.SampleDesc.Count = 1;
			descHBAO.SampleDesc.Quality = 0;
			descHBAO.Format = DXGI_FORMAT_R32G32B32A32_FLOAT;

			descHBAO.Usage = D3D_USAGE_DEFAULT;
			descHBAO.BindFlags = D3D_BIND_SHADER_RESOURCE;
			descHBAO.CPUAccessFlags = 0;
			descHBAO.MiscFlags = 0;

			it = TEX_jitter_count - 1;
			subData[it].pSysMem = tempDataHBAO;
			subData[it].SysMemPitch = descHBAO.Width * sampleSize * sizeof(float);

			// Fill it,
			for (u32 y=0; y<TEX_jitter; y++)
			{
				for (u32 x=0; x<TEX_jitter; x++)
				{
					float numDir = 1.0f;
					switch (ps_r_ssao)
					{
					case 1: numDir = 4.0f; break;
					case 2: numDir = 6.0f; break;
					case 3: numDir = 8.0f; break;
					case 4: numDir = 8.0f; break;
					}
					float angle = 2 * PI * ::Random.randF(0.0f, 1.0f) / numDir;
					float dist = ::Random.randF(0.0f, 1.0f);

					float *p	=	(float*)	
						(LPBYTE (subData[it].pSysMem) 
						+ y*subData[it].SysMemPitch 
						+ x*4*sizeof(float));
					*p = (float)(_cos(angle));
					*(p+1) = (float)(_sin(angle));
					*(p+2) = (float)(dist);
					*(p+3) = 0;
				}
			}			

			xr_sprintf(name, "%s%d", r2_jitter, it);
			R_CHK(RDevice->CreateTexture2D(&descHBAO, &subData[it], &t_noise_surf[it]));
			t_noise[it] = dxRenderDeviceRender::Instance().Resources->_CreateTexture(name);
			t_noise[it]->surface_set(t_noise_surf[it]);
		}
	}

	// PP
	s_postprocess.create("postprocess");
	g_postprocess.create(D3DFVF_XYZRHW | D3DFVF_DIFFUSE | D3DFVF_SPECULAR | D3DFVF_TEX3, RCache.Vertex.Buffer(), RCache.QuadIB);

	// Menu
	s_menu.create("distort");
	g_menu.create(FVF::F_TL, RCache.Vertex.Buffer(), RCache.QuadIB);


	dwWidth = Device.TargetWidth;
	dwHeight = Device.TargetHeight;
}

CRenderTarget::~CRenderTarget	()
{
	_RELEASE					(t_ss_async);

	// Textures
	t_material->surface_set		(nullptr);

#ifdef DEBUG
	_SHOW_REF					("t_material_surf",t_material_surf);
#endif // DEBUG
	_RELEASE					(t_material_surf);

	t_LUM_src->surface_set		(nullptr);
	t_LUM_dest->surface_set		(nullptr);

#ifdef DEBUG
	ID3DBaseTexture*	pSurf = 0;

	pSurf = t_envmap_0->surface_get();
	if (pSurf) pSurf->Release();
	_SHOW_REF("t_envmap_0 - #small",pSurf);

	pSurf = t_envmap_1->surface_get();
	if (pSurf) pSurf->Release();
	_SHOW_REF("t_envmap_1 - #small",pSurf);
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
	for (int it=0; it<TEX_jitter_count; it++)	{
		t_noise	[it]->surface_set	(nullptr);
#ifdef DEBUG
		_SHOW_REF("t_noise_surf[it]",t_noise_surf[it]);
#endif // DEBUG
		_RELEASE					(t_noise_surf[it]);
	}

	// 
	accum_spot_geom_destroy		();
	accum_omnip_geom_destroy	();
	accum_point_geom_destroy	();
	accum_volumetric_geom_destroy();

	// Blenders
	xr_delete					(b_combine				);
	xr_delete					(b_luminance			);
	xr_delete					(b_bloom				);
	xr_delete					(b_accum_reflected		);
	xr_delete					(b_accum_spot			);
	xr_delete					(b_accum_point			);
	xr_delete					(b_accum_direct			);
	xr_delete					(b_ssao					);
	xr_delete					(b_fxaa					);
	xr_delete					(b_smaa					);
	xr_delete					(b_spp					);
	xr_delete					(b_accum_mask			);
	xr_delete					(b_occq					);
	xr_delete					(b_hdao_cs				);
	xr_delete					(b_cas					);
	xr_delete					(b_gtao					);	
	g_Fsr2Wrapper.Destroy();
	g_DLSSWrapper.Destroy();

	CImGuiManager::Instance().Unsubscribe("GraphicDebug");

	if (g_debug_blend_state != nullptr) 
	{
		g_debug_blend_state->Release();
		g_debug_blend_state = nullptr;
	}
}

void CRenderTarget::reset_light_marker( bool bResetStencil)
{
	dwLightMarkerID = 5;
	if (bResetStencil)
	{
		u32		Offset;
		float	_w					= RCache.get_width();
		float	_h					= RCache.get_height();
		u32		C					= color_rgba	(255,255,255,255);
		float	eps					= 0;
		float	_dw					= 0.5f;
		float	_dh					= 0.5f;
		FVF::TL* pv					= (FVF::TL*) RCache.Vertex.Lock	(4,g_combine->vb_stride,Offset);
		pv->set						(-_dw,		_h-_dh,		eps,	1.f, C, 0, 0);	pv++;
		pv->set						(-_dw,		-_dh,		eps,	1.f, C, 0, 0);	pv++;
		pv->set						(_w-_dw,	_h-_dh,		eps,	1.f, C, 0, 0);	pv++;
		pv->set						(_w-_dw,	-_dh,		eps,	1.f, C, 0, 0);	pv++;
		RCache.Vertex.Unlock		(4,g_combine->vb_stride);
		RCache.set_Element			(s_occq->E[2]	);
		RCache.set_Geometry			(g_combine		);
		RCache.Render				(D3DPT_TRIANGLELIST,Offset,0,4,0,2);
	}
}

void CRenderTarget::increment_light_marker()
{
	dwLightMarkerID += 2;

	const u32 iMaxMarkerValue = 255;
	
	if ( dwLightMarkerID > iMaxMarkerValue )
		reset_light_marker(true);
}

bool CRenderTarget::need_to_render_sunshafts()
{
	if (!ps_r_sun_shafts)
		return false;

	{
		CEnvDescriptor&	E = *g_pGamePersistent->Environment().CurrentEnv;
		Fcolor sun_color = ((light*)RImplementation.Lights.sun_adapted._get())->color;
		float fValue = E.m_fSunShaftsIntensity * u_diffuse2s(sun_color.r, sun_color.g, sun_color.b);
		if (fValue < EPS)
			return false;
	}

	return true;
}