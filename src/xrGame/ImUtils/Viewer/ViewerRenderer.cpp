#include "StdAfx.h"
#include "ViewerRenderer.h"

#include "../../../xrEngine/device.h"

#include <d3dcompiler.h>
#include <d3d11.h>

namespace ImUtils::Viewer
{
// Implementation details. Named, not anonymous.
namespace ViewerRendererDetail
{
	// Resolves the selected scene, with optional stock UI, into the panel-sized target.
	// Preserve the scene aspect ratio and cover the panel without black borders.
	const char ResolveShaderSource[] =
		"Texture2D SceneTex : register(t0);\n"
		"SamplerState SceneSamp : register(s0);\n"
		"struct VSOut { float4 pos : SV_POSITION; float2 uv : TEXCOORD0; };\n"
		"VSOut vs_main(uint id : SV_VertexID)\n"
		"{\n"
		"	VSOut o;\n"
		"	o.uv = float2((id << 1) & 2, id & 2);\n"
		"	o.pos = float4(o.uv.x * 2.0 - 1.0, 1.0 - o.uv.y * 2.0, 0.0, 1.0);\n"
		"	return o;\n"
		"}\n"
		"float4 ps_main(VSOut i) : SV_TARGET\n"
		"{\n"
		"	return float4(saturate(SceneTex.Sample(SceneSamp, i.uv).rgb), 1.0);\n"
		"}\n";

	// Compiles one embedded resolve shader.
	void* CompileShader(const char* source, const char* name, const char* entryPoint, const char* target)
	{
		if (GRHI == nullptr || GRHI->APILevel != ERHI_API_LAYER::D3D11)
			return nullptr;

		xr_vector<u8> code, errors;
		// BuildShader returns the raw HRESULT (S_OK == 0 on success).
		const HRESULT hrBuild = GRHI->BuildShader(nullptr, source, strlen(source) + 1, name, nullptr, nullptr,
			entryPoint, target, 0, 0, code, errors);
		if (FAILED(hrBuild) || code.empty())
		{
			Msg("! ViewerRenderer: shader '%s' failed: %s", entryPoint, errors.empty() ? "<no details>" : (const char*)errors.data());
			return nullptr;
		}

		ID3D11Device* device = (ID3D11Device*)GRHI->DevicePtr->RawDevice;
		void* shader = nullptr;
		HRESULT hr;
		if (target[0] == 'v')
			hr = device->CreateVertexShader(code.data(), code.size(), nullptr, (ID3D11VertexShader**)&shader);
		else
			hr = device->CreatePixelShader(code.data(), code.size(), nullptr, (ID3D11PixelShader**)&shader);
		if (FAILED(hr))
		{
			Msg("! ViewerRenderer: CreateShader('%s') failed: 0x%08lx", entryPoint, (unsigned long)hr);
			return nullptr;
		}
		return shader;
	}

	template <typename T>
	void ReleaseIf(T*& ptr);

	class CResolveOutputState
	{
	public:
		explicit CResolveOutputState(ID3D11DeviceContext* context) : Context(context)
		{
			Context->RSGetState(&Rasterizer);
			Context->OMGetDepthStencilState(&Depth, &StencilRef);
			Context->OMGetBlendState(&Blend, BlendFactor, &SampleMask);
			ColorWriteMask = GRHI->StateManager->GetColorWriteEnable();
			Context->OMSetDepthStencilState(nullptr, 0);
			Context->OMSetBlendState(nullptr, BlendFactor, SampleMask);
			Context->RSSetState(nullptr);
			GRHI->StateManager->SetColorWriteEnable(0xF);
		}

		~CResolveOutputState()
		{
			Context->OMSetDepthStencilState(Depth, StencilRef);
			Context->OMSetBlendState(Blend, BlendFactor, SampleMask);
			Context->RSSetState(Rasterizer);
			GRHI->StateManager->SetColorWriteEnable(ColorWriteMask);
			ReleaseIf(Depth);
			ReleaseIf(Blend);
			ReleaseIf(Rasterizer);
		}

	private:
		ID3D11DeviceContext* Context;
		ID3D11DepthStencilState* Depth = nullptr;
		ID3D11BlendState* Blend = nullptr;
		ID3D11RasterizerState* Rasterizer = nullptr;
		u32 ColorWriteMask = 0xF;
		u32 StencilRef = 0;
		u32 SampleMask = 0;
		float BlendFactor[4] = {};
	};

	template <typename T>
	void ReleaseIf(T*& ptr)
	{
		if (ptr != nullptr)
		{
			ptr->Release();
			ptr = nullptr;
		}
	}
}

bool CViewerRenderer::Init(u32 width, u32 height)
{
	Shutdown();

	if (GRHI == nullptr || GRHI->APILevel != ERHI_API_LAYER::D3D11 || GRHI->DevicePtr == nullptr)
	{
		Msg("! ViewerRenderer: no D3D11 device - viewport render target unavailable");
		return false;
	}

	Width = std::clamp(width, 16u, 16384u);
	Height = std::clamp(height, 16u, 16384u);
	RHITextureDesc colorDesc(Width, Height, ERHI_FORMAT::R8G8B8A8_UNORM);
	colorDesc.BindFlags = ERHI_BIND_FLAG(RENDER_TARGET | SHADER_RESOURCE);
	ColorRT = GRHI->CreateRenderTarget(colorDesc);
	if (ColorRT != nullptr)
	{
		RHIRenderTargetViewDesc rtvDesc;
		rtvDesc.Format = ERHI_FORMAT::R8G8B8A8_UNORM;
		rtvDesc.ViewDimension = ERHI_RTV_DIMENSION::TEXTURE2D;
		ColorRTV = GRHI->CreateRenderTargetView(ColorRT, rtvDesc);
		ColorSRV = GRHI->CreateShaderResourceView(ColorRT, nullptr);
	}

	ResolveVS = ViewerRendererDetail::CompileShader(ViewerRendererDetail::ResolveShaderSource, "viewer_resolve", "vs_main", "vs_5_0");
	ResolvePS = ViewerRendererDetail::CompileShader(ViewerRendererDetail::ResolveShaderSource, "viewer_resolve", "ps_main", "ps_5_0");
	ID3D11Device* device = (ID3D11Device*)GRHI->DevicePtr->RawDevice;
	D3D11_SAMPLER_DESC sampDesc = {};
	sampDesc.Filter = D3D11_FILTER_MIN_MAG_MIP_LINEAR;
	sampDesc.AddressU = sampDesc.AddressV = sampDesc.AddressW = D3D11_TEXTURE_ADDRESS_CLAMP;
	sampDesc.MaxLOD = D3D11_FLOAT32_MAX;
	device->CreateSamplerState(&sampDesc, (ID3D11SamplerState**)&ResolveSampler);

	if (ColorSRV == nullptr || ColorRTV == nullptr || ResolveVS == nullptr || ResolvePS == nullptr || ResolveSampler == nullptr)
	{
		Msg("! ViewerRenderer: viewport resources could not be created");
		Shutdown();
		return false;
	}

	Msg("* ViewerRenderer: viewport render target %ux%u ready", Width, Height);
	return true;
}

void CViewerRenderer::Shutdown()
{
	if (ResolveVS != nullptr)
	{
		((ID3D11VertexShader*)ResolveVS)->Release();
		ResolveVS = nullptr;
	}
	if (ResolvePS != nullptr)
	{
		((ID3D11PixelShader*)ResolvePS)->Release();
		ResolvePS = nullptr;
	}
	if (ResolveSampler != nullptr)
	{
		((ID3D11SamplerState*)ResolveSampler)->Release();
		ResolveSampler = nullptr;
	}
	ViewerRendererDetail::ReleaseIf(ColorSRV);
	ViewerRendererDetail::ReleaseIf(ColorRTV);
	ViewerRendererDetail::ReleaseIf(ColorRT);
	Width = 0;
	Height = 0;
}

void CViewerRenderer::ClearScene()
{
	if (ColorRTV == nullptr)
		return;
	constexpr float clearColor[4] = {};
	GRHI->ClearTarget(ColorRTV, clearColor);
}

void* CViewerRenderer::ViewSRV() const
{
	return ColorSRV != nullptr ? ColorSRV->GetRawSRV() : nullptr;
}

void CViewerRenderer::ResolveFromFinalScene()
{
	if (ColorRTV == nullptr || Render == nullptr)
		return;
	ResolveFromSceneTexture(Render->get_scene_color());
}

void CViewerRenderer::ResolveFromSceneTexture(IRHIShaderResourceView* sceneSRV)
{
	if (ColorRTV == nullptr || sceneSRV == nullptr || sceneSRV->GetRawSRV() == nullptr || ResolveVS == nullptr || ResolvePS == nullptr)
	{
		ClearScene();
		return;
	}

	ID3D11DeviceContext* ctx = (ID3D11DeviceContext*)GRHI->GetContext();
	const ViewerRendererDetail::CResolveOutputState outputState(ctx);

	// Save the pipeline state we touch: shaders, PS texture slot 0, the target.
	ID3D11VertexShader* oldVS = nullptr;
	ID3D11PixelShader* oldPS = nullptr;
	ctx->VSGetShader(&oldVS, nullptr, nullptr);
	ctx->PSGetShader(&oldPS, nullptr, nullptr);
	ID3D11ShaderResourceView* oldSRV = nullptr;
	ID3D11SamplerState* oldSampler = nullptr;
	ID3D11InputLayout* oldLayout = nullptr;
	D3D11_PRIMITIVE_TOPOLOGY oldTopology = D3D11_PRIMITIVE_TOPOLOGY_UNDEFINED;
	ctx->PSGetShaderResources(0, 1, &oldSRV);
	ctx->PSGetSamplers(0, 1, &oldSampler);
	ctx->IAGetInputLayout(&oldLayout);
	ctx->IAGetPrimitiveTopology(&oldTopology);
	IRHIRenderTargetView* oldRTV = GRHI->GetRenderTargetView(0);
	IRHIDepthStencilView* oldDSV = GRHI->GetDepthStencilView();
	D3D11_VIEWPORT oldViewport = {};
	UINT viewportCount = 1;
	ctx->RSGetViewports(&viewportCount, &oldViewport);

	const float sourceWidth = float(sceneSRV->GetSurface()->GetWidth());
	const float sourceHeight = float(sceneSRV->GetSurface()->GetHeight());
	const float scale = std::max(float(Width) / sourceWidth, float(Height) / sourceHeight);
	const float contentWidth = sourceWidth * scale;
	const float contentHeight = sourceHeight * scale;
	RHIViewport viewport((float(Width) - contentWidth) * 0.5f, (float(Height) - contentHeight) * 0.5f, contentWidth, contentHeight);
	if (LoggedSourceWidth != u32(sourceWidth) || LoggedSourceHeight != u32(sourceHeight) || LoggedViewportWidth != Width || LoggedViewportHeight != Height)
	{
		LoggedSourceWidth = u32(sourceWidth);
		LoggedSourceHeight = u32(sourceHeight);
		LoggedViewportWidth = Width;
		LoggedViewportHeight = Height;
		Msg("* ViewerRenderer: resolve source=%ux%u target=%ux%u viewport=[%.1f %.1f %.1f %.1f]", LoggedSourceWidth, LoggedSourceHeight, Width, Height, viewport.TopLeftX, viewport.TopLeftY, viewport.Width, viewport.Height);
	}
	constexpr float clearColor[4] = { 0.f, 0.f, 0.f, 1.f };
	GRHI->ClearTarget(ColorRTV, clearColor);
	GRHI->SetRenderTargetView(ColorRTV, 0, true);
	GRHI->SetDepthStencilView(nullptr, true);
	GRHI->ApplyRenderTargetChange();
	GRHI->SetViewport(viewport);

	GRHI->SetShader(ResolveVS, ERHI_SHADER_TYPE::VS);
	GRHI->SetShader(ResolvePS, ERHI_SHADER_TYPE::PS);
	ID3D11ShaderResourceView* rawSrv = (ID3D11ShaderResourceView*)sceneSRV->GetRawSRV();
	ctx->PSSetShaderResources(0, 1, &rawSrv);
	if (ResolveSampler != nullptr)
		ctx->PSSetSamplers(0, 1, (ID3D11SamplerState**)&ResolveSampler);
	GRHI->DrawNoInputAssembly(3);
	GRHI->SetShader(oldVS, ERHI_SHADER_TYPE::VS);
	GRHI->SetShader(oldPS, ERHI_SHADER_TYPE::PS);
	ctx->PSSetShaderResources(0, 1, &oldSRV);
	ctx->PSSetSamplers(0, 1, &oldSampler);
	ctx->IASetInputLayout(oldLayout);
	ctx->IASetPrimitiveTopology(oldTopology);
	ctx->RSSetViewports(viewportCount, &oldViewport);
	if (oldVS) oldVS->Release();
	if (oldPS) oldPS->Release();
	if (oldSRV) oldSRV->Release();
	if (oldSampler) oldSampler->Release();
	if (oldLayout) oldLayout->Release();
	GRHI->SetRenderTargetView(oldRTV, 0, true);
	GRHI->SetDepthStencilView(oldDSV, true);
	GRHI->ApplyRenderTargetChange();
}

float CViewerRenderer::VerifyReadback(const char* bmpPath)
{
	if (!IsReady())
		return -1.f;

	xr_vector<u8> pixels;
	pixels.resize(Width * Height * 4);
	u32 outW = 0, outH = 0, outPitch = 0;
	if (!GRHI->DevicePtr->ReadRenderTargetPixels(ColorRTV, pixels.data(), u32(pixels.size()), outW, outH, outPitch) || outW == 0 || outH == 0)
	{
		Msg("! ViewerRenderer: render target readback failed");
		return -1.f;
	}

	u32 covered = 0;
	u32 lowerCovered = 0;
	u32 lowerSamples = 0;
	u64 lowerLuminance = 0;
	u64 alpha = 0;
	for (u32 y = 0; y < outH; ++y)
	{
		const u8* row = pixels.data() + y * outPitch;
		const bool lower = y >= outH * 2 / 3;
		for (u32 x = 0; x < outW; ++x)
		{
			const u8* px = row + x * 4;
			const u32 luminance = u32(px[0]) + u32(px[1]) + u32(px[2]);
			alpha += px[3];
			if (luminance > 24)
				++covered;
			if (lower)
			{
				++lowerSamples;
				lowerLuminance += luminance;
				lowerCovered += luminance > 24;
			}
		}
	}
	const float fraction = float(covered) / float(outW * outH);
	const float lowerFraction = lowerSamples != 0 ? float(lowerCovered) / float(lowerSamples) : 0.f;
	const float lowerMean = lowerSamples != 0 ? float(lowerLuminance) / float(lowerSamples * 3) : 0.f;

	// BMP dump (bottom-up BGRA->BGR rows) for visual inspection.
	if (bmpPath != nullptr && bmpPath[0] != 0)
	{
		IWriter* writer = FS.w_open(bmpPath);
		if (writer != nullptr)
		{
			// BITMAPFILEHEADER + BITMAPINFOHEADER, 24-bit
			const u32 rowSize = outW * 3;
			const u32 imageSize = rowSize * outH;
			const u32 fileSize = 54 + imageSize;
			writer->w_u8('B'); writer->w_u8('M');
			writer->w_u32(fileSize);
			writer->w_u32(0);
			writer->w_u32(54);
			writer->w_u32(40);
			writer->w_u32(outW);
			writer->w_u32(outH);
			writer->w_u16(1);
			writer->w_u16(24);
			writer->w_u32(0);
			writer->w_u32(imageSize);
			writer->w_u32(2835); writer->w_u32(2835);
			writer->w_u32(0); writer->w_u32(0);
			for (s32 y = s32(outH) - 1; y >= 0; --y)
			{
				const u8* row = pixels.data() + u32(y) * outPitch;
				for (u32 x = 0; x < outW; ++x)
				{
					const u8* px = row + x * 4;
					writer->w_u8(px[0]); // B
					writer->w_u8(px[1]); // G
					writer->w_u8(px[2]); // R
				}
			}
			FS.w_close(writer);
			Msg("* ViewerRenderer: readback dumped to '%s'", bmpPath);
		}
	}

	Msg("* ViewerRenderer: readback %ux%u, non-black scene %.1f%% (%u px), lower frame %.1f%% mean %.2f, mean alpha %.2f", outW, outH, fraction * 100.f, covered, lowerFraction * 100.f, lowerMean, float(alpha) / float(outW * outH));
	return alpha == u64(outW) * u64(outH) * 255 ? fraction : -fraction;
}

bool CViewerRenderer::DumpRTVToBmp(IRHIRenderTargetView* rtv, const char* bmpPath)
{
	if (rtv == nullptr || bmpPath == nullptr || bmpPath[0] == 0)
		return false;

	// Read back the full target; then write a 24-bit bottom-up BGR BMP.
	xr_vector<u8> pixels;
	pixels.resize(size_t(Device.Width) * size_t(Device.Height) * 4);
	u32 outW = 0, outH = 0, outPitch = 0;
	if (!GRHI->DevicePtr->ReadRenderTargetPixels(rtv, pixels.data(), u32(pixels.size()), outW, outH, outPitch) || outW == 0 || outH == 0)
	{
		Msg("! ViewerRenderer: RTV readback failed");
		return false;
	}

	string_path fullPath;
	FS.update_path(fullPath, "$logs$", bmpPath);
	IWriter* writer = FS.w_open(fullPath);
	if (writer == nullptr)
	{
		Msg("! ViewerRenderer: cannot open '%s' for writing", fullPath);
		return false;
	}

	const u32 rowSize = outW * 3;
	const u32 imageSize = rowSize * outH;
	writer->w_u8('B'); writer->w_u8('M');
	writer->w_u32(54 + imageSize); writer->w_u32(0); writer->w_u32(54);
	writer->w_u32(40); writer->w_u32(outW); writer->w_u32(outH);
	writer->w_u16(1); writer->w_u16(24);
	writer->w_u32(0); writer->w_u32(imageSize);
	writer->w_u32(2835); writer->w_u32(2835); writer->w_u32(0); writer->w_u32(0);
	for (s32 y = s32(outH) - 1; y >= 0; --y)
	{
		const u8* row = pixels.data() + size_t(y) * outPitch;
		for (u32 x = 0; x < outW; ++x)
		{
			const u8* px = row + x * 4;
			writer->w_u8(px[0]); writer->w_u8(px[1]); writer->w_u8(px[2]);
		}
	}
	FS.w_close(writer);
	Msg("* ViewerRenderer: swapchain dumped to '%s'", fullPath);
	return true;
}

bool CViewerRenderer::DumpSurfaceToBmp(IRHISurface* surface, const char* bmpPath)
{
	if (surface == nullptr || bmpPath == nullptr || GRHI == nullptr || GRHI->APILevel != ERHI_API_LAYER::D3D11)
		return false;
	if (ResolveVS == nullptr || ResolvePS == nullptr || ResolveSampler == nullptr)
	{
		CViewerRenderer helper;
		if (!helper.Init(Device.TargetWidth, Device.TargetHeight))
			return false;
		const bool result = helper.DumpSurfaceToBmp(surface, bmpPath);
		helper.Shutdown();
		return result;
	}
	RHITextureDesc desc(surface->GetWidth(), surface->GetHeight(), ERHI_FORMAT::R8G8B8A8_UNORM);
	desc.BindFlags = ERHI_BIND_FLAG(RENDER_TARGET);
	desc.MipLevels = 1;
	IRHISurface* target = GRHI->CreateRenderTarget(desc);
	if (target == nullptr)
		return false;
	RHIRenderTargetViewDesc viewDesc;
	viewDesc.Format = desc.Format;
	viewDesc.ViewDimension = ERHI_RTV_DIMENSION::TEXTURE2D;
	IRHIRenderTargetView* targetView = GRHI->CreateRenderTargetView(target, viewDesc);
	RHIShaderResourceViewDesc inputDesc = {};
	inputDesc.Format = surface->GetFormat() == ERHI_FORMAT::D32_FLOAT ? ERHI_FORMAT::R32_FLOAT : surface->GetFormat();
	inputDesc.ViewDimension = ERHI_SRV_DIMENSION::TEXTURE2D;
	inputDesc.MipLevels = surface->GetMipLevels();
	if ((surface->GetMiscFlags() & u32(ERHI_RESOURCE_MISC_FLAG::TEXTURECUBE)) != 0)
	{
		inputDesc.ViewDimension = ERHI_SRV_DIMENSION::TEXTURE2DARRAY;
		inputDesc.FirstArraySlice = 0;
		inputDesc.ArraySize = 1;
	}
	IRHIShaderResourceView* input = GRHI->CreateShaderResourceView(surface, &inputDesc);
	if (targetView == nullptr || input == nullptr || input->GetRawSRV() == nullptr)
	{
		Msg("! ViewerRenderer: dump view creation failed (target=%p input=%p raw=%p misc=0x%x)", targetView, input, input != nullptr ? input->GetRawSRV() : nullptr, surface->GetMiscFlags());
		ViewerRendererDetail::ReleaseIf(input);
		ViewerRendererDetail::ReleaseIf(targetView);
		ViewerRendererDetail::ReleaseIf(target);
		return false;
	}

	ID3D11DeviceContext* context = (ID3D11DeviceContext*)GRHI->GetContext();
	const ViewerRendererDetail::CResolveOutputState outputState(context);
	ID3D11VertexShader* oldVS = nullptr;
	ID3D11PixelShader* oldPS = nullptr;
	ID3D11ShaderResourceView* oldSRV = nullptr;
	ID3D11SamplerState* oldSampler = nullptr;
	ID3D11InputLayout* oldLayout = nullptr;
	D3D11_PRIMITIVE_TOPOLOGY oldTopology = D3D11_PRIMITIVE_TOPOLOGY_UNDEFINED;
	context->VSGetShader(&oldVS, nullptr, nullptr);
	context->PSGetShader(&oldPS, nullptr, nullptr);
	context->PSGetShaderResources(0, 1, &oldSRV);
	context->PSGetSamplers(0, 1, &oldSampler);
	context->IAGetInputLayout(&oldLayout);
	context->IAGetPrimitiveTopology(&oldTopology);
	IRHIRenderTargetView* oldRTV = GRHI->GetRenderTargetView(0);
	IRHIDepthStencilView* oldDSV = GRHI->GetDepthStencilView();
	D3D11_VIEWPORT oldViewport;
	UINT viewportCount = 1;
	context->RSGetViewports(&viewportCount, &oldViewport);

	GRHI->SetRenderTargetView(targetView, 0, true);
	GRHI->SetDepthStencilView(nullptr, true);
	GRHI->ApplyRenderTargetChange();
	RHIViewport viewport(0.f, 0.f, float(desc.Width), float(desc.Height));
	GRHI->SetViewport(viewport);
	GRHI->SetShader(ResolveVS, ERHI_SHADER_TYPE::VS);
	GRHI->SetShader(ResolvePS, ERHI_SHADER_TYPE::PS);
	ID3D11ShaderResourceView* rawInput = (ID3D11ShaderResourceView*)input->GetRawSRV();
	context->PSSetShaderResources(0, 1, &rawInput);
	if (ResolveSampler != nullptr)
		context->PSSetSamplers(0, 1, (ID3D11SamplerState**)&ResolveSampler);
	GRHI->DrawNoInputAssembly(3);

	GRHI->SetShader(oldVS, ERHI_SHADER_TYPE::VS);
	GRHI->SetShader(oldPS, ERHI_SHADER_TYPE::PS);
	context->PSSetShaderResources(0, 1, &oldSRV);
	context->PSSetSamplers(0, 1, &oldSampler);
	context->IASetInputLayout(oldLayout);
	context->IASetPrimitiveTopology(oldTopology);
	context->RSSetViewports(viewportCount, &oldViewport);
	if (oldVS) oldVS->Release();
	if (oldPS) oldPS->Release();
	if (oldSRV) oldSRV->Release();
	if (oldSampler) oldSampler->Release();
	if (oldLayout) oldLayout->Release();
	GRHI->SetRenderTargetView(oldRTV, 0, true);
	GRHI->SetDepthStencilView(oldDSV, true);
	GRHI->ApplyRenderTargetChange();

	const bool result = DumpRTVToBmp(targetView, bmpPath);
	ViewerRendererDetail::ReleaseIf(input);
	ViewerRendererDetail::ReleaseIf(targetView);
	ViewerRendererDetail::ReleaseIf(target);
	return result;
}
}
