#ifdef _WIN32
#include <d3d11.h>
#include <d3dcompiler.h>
#include <wrl/client.h>
#include <algorithm>
#include <array>
#include <cmath>
#include <cstdint>
#include <cstring>
#include <filesystem>
#include <iostream>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

using u32 = std::uint32_t;
using Microsoft::WRL::ComPtr;
using Vector = std::array<float, 4>;
using Values = std::vector<std::pair<const char*, Vector>>;

namespace
{
void Check(bool Condition, const char* Message)
{
	if (!Condition)
	{
		throw std::runtime_error(Message);
	}
}

void Verify(HRESULT Result, const char* Message)
{
	Check(SUCCEEDED(Result), Message);
}

bool Near(const Vector& Left, const Vector& Right, float Tolerance = 0.003f)
{
	for (u32 Index = 0; Index < 3; ++Index)
	{
		if (std::abs(Left[Index] - Right[Index]) > Tolerance)
		{
			return false;
		}
	}
	return true;
}

float GroundHeight(float X, float Z)
{
	return 1.0f + 0.4f * X + 0.2f * Z;
}

constexpr u32 Resolution = 16;
constexpr Vector BaseColor = {0.15f, 0.5f, 0.1f, 1.0f};
constexpr Vector GroundColor = {0.8f, 0.2f, 0.1f, 1.0f};

struct TintSettings
{
	Vector Base = BaseColor;
	Vector Adjustment = {0, 1, 1, 0};
	float MaskUV = 5.0f / 6.0f;
	float Tips = 1;
	float LocalHeight = 0;
	int BlendMode = 1;
	bool GreenOnly = false;
};

float Luminance(const Vector& Color)
{
	return 0.2126f * std::pow(Color[0], 2.2f) + 0.7152f * std::pow(Color[1], 2.2f) + 0.0722f * std::pow(Color[2], 2.2f);
}

class WarpFixture
{
public:
	explicit WarpFixture(const std::filesystem::path& Shaders) : Shaders(Shaders)
	{
		D3D_FEATURE_LEVEL Level;
		Verify(D3D11CreateDevice(nullptr, D3D_DRIVER_TYPE_WARP, nullptr, 0, nullptr, 0,
			D3D11_SDK_VERSION, &Device, &Level, &Context), "Create WARP device");
		CaptureVSCode = Compile("grass_terrain_tint_capture.vs.hlsl", nullptr, "main", "vs_5_0");
		CapturePSCode = Compile("grass_terrain_tint_capture.ps.hlsl", nullptr, "main", "ps_5_0");
		Verify(Device->CreateVertexShader(CaptureVSCode->GetBufferPointer(), CaptureVSCode->GetBufferSize(), nullptr, &CaptureVS), "Create capture VS");
		Verify(Device->CreatePixelShader(CapturePSCode->GetBufferPointer(), CapturePSCode->GetBufferSize(), nullptr, &CapturePS), "Create capture PS");

		const char* ProbeSource = R"(
#include "common.hlsli"
#define USE_DETAIL_TINT_MASK 1
#include "grass_terrain_tint.hlsli"
uniform float4 probe_root;
uniform float4 probe_base;
uniform float4 probe_mask;
struct ProbeOutput { float4 position : SV_POSITION; float4 tint : TEXCOORD0; };
ProbeOutput ProbeVS(uint Id : SV_VertexID)
{
    ProbeOutput O;
    float2 UV = float2((Id << 1) & 2, Id & 2);
    O.position = float4(UV * float2(2, -2) + float2(-1, 1), 0, 1);
    O.tint = GrassTerrainTintSample(probe_root.xyz, probe_root.w);
    return O;
}
float4 ProbePS(ProbeOutput I) : SV_Target
{
    return float4(GrassTerrainTintApply(probe_base.rgb, I.tint, GrassTerrainTintMask(probe_mask.xy)), 1);
}
)";
		ProbeVSCode = Compile("grass_terrain_tint_capture.vs.hlsl", ProbeSource, "ProbeVS", "vs_5_0");
		ProbePSCode = Compile("grass_terrain_tint_capture.ps.hlsl", ProbeSource, "ProbePS", "ps_5_0");
		Verify(Device->CreateVertexShader(ProbeVSCode->GetBufferPointer(), ProbeVSCode->GetBufferSize(), nullptr, &ProbeVS), "Create probe VS");
		Verify(Device->CreatePixelShader(ProbePSCode->GetBufferPointer(), ProbePSCode->GetBufferSize(), nullptr, &ProbePS), "Create probe PS");

		Color = Texture(Resolution, Resolution, DXGI_FORMAT_R8G8B8A8_UNORM_SRGB, D3D11_BIND_RENDER_TARGET | D3D11_BIND_SHADER_RESOURCE, nullptr, true);
		Height = Texture(Resolution, Resolution, DXGI_FORMAT_R32_FLOAT, D3D11_BIND_RENDER_TARGET | D3D11_BIND_SHADER_RESOURCE);
		Depth = Texture(Resolution, Resolution, DXGI_FORMAT_D32_FLOAT, D3D11_BIND_DEPTH_STENCIL);
		const std::array<unsigned char, 12> MaskPixels = {255, 255, 255, 0, 0, 255, 0, 128, 0, 0, 0, 255};
		D3D11_TEXTURE2D_DESC MaskDescription = {};
		MaskDescription.Width = 3;
		MaskDescription.Height = MaskDescription.MipLevels = MaskDescription.ArraySize = 1;
		MaskDescription.Format = DXGI_FORMAT_R8G8B8A8_UNORM;
		MaskDescription.SampleDesc.Count = 1;
		MaskDescription.BindFlags = D3D11_BIND_SHADER_RESOURCE;
		D3D11_SUBRESOURCE_DATA MaskData = {MaskPixels.data(), 12, 0};
		Verify(Device->CreateTexture2D(&MaskDescription, &MaskData, &MaskTexture), "Create atlas mask");
		Verify(Device->CreateShaderResourceView(MaskTexture.Get(), nullptr, &MaskView), "Create atlas mask SRV");
		Output = Texture(1, 1, DXGI_FORMAT_R32G32B32A32_FLOAT, D3D11_BIND_RENDER_TARGET);
		Verify(Device->CreateRenderTargetView(Color.Get(), nullptr, &ColorTarget), "Create color RTV");
		Verify(Device->CreateRenderTargetView(Height.Get(), nullptr, &HeightTarget), "Create height RTV");
		Verify(Device->CreateDepthStencilView(Depth.Get(), nullptr, &DepthTarget), "Create depth DSV");
		Verify(Device->CreateRenderTargetView(Output.Get(), nullptr, &OutputTarget), "Create probe RTV");
		Verify(Device->CreateShaderResourceView(Color.Get(), nullptr, &ColorView), "Create color SRV");
		Verify(Device->CreateShaderResourceView(Height.Get(), nullptr, &HeightView), "Create height SRV");

		D3D11_RASTERIZER_DESC Raster = {};
		Raster.FillMode = D3D11_FILL_SOLID;
		Raster.CullMode = D3D11_CULL_NONE;
		Raster.DepthClipEnable = true;
		Verify(Device->CreateRasterizerState(&Raster, &RasterState), "Create rasterizer");
		Context->RSSetState(RasterState.Get());
		D3D11_DEPTH_STENCIL_DESC DepthDescription = {};
		DepthDescription.DepthEnable = true;
		DepthDescription.DepthWriteMask = D3D11_DEPTH_WRITE_MASK_ALL;
		DepthDescription.DepthFunc = D3D11_COMPARISON_LESS_EQUAL;
		Verify(Device->CreateDepthStencilState(&DepthDescription, &CaptureDepth), "Create capture depth state");
		DepthDescription.DepthEnable = false;
		Verify(Device->CreateDepthStencilState(&DepthDescription, &ProbeDepth), "Create probe depth state");
		D3D11_SAMPLER_DESC Sampler = {};
		Sampler.Filter = D3D11_FILTER_MIN_MAG_MIP_POINT;
		Sampler.AddressU = Sampler.AddressV = Sampler.AddressW = D3D11_TEXTURE_ADDRESS_CLAMP;
		Sampler.MaxLOD = D3D11_FLOAT32_MAX;
		Sampler.MaxAnisotropy = 1;
		Sampler.ComparisonFunc = D3D11_COMPARISON_NEVER;
		Verify(Device->CreateSamplerState(&Sampler, &PointSampler), "Create point sampler");
		Sampler.Filter = D3D11_FILTER_MIN_MAG_LINEAR_MIP_POINT;
		Verify(Device->CreateSamplerState(&Sampler, &LinearSampler), "Create linear sampler");

		struct Vertex
		{
			float Position[4];
			int UV[2];
			float Tangent[4];
			float Binormal[4];
			float Normal[4];
			int Lightmap[2];
		};
		std::array<Vertex, 4> Vertices = {};
		for (u32 Index = 0; Index < 4; ++Index)
		{
			const float X = (Index & 1) ? 1.0f : -1.0f;
			const float Z = (Index & 2) ? 1.0f : -1.0f;
			Vertices[Index].Position[0] = X;
			Vertices[Index].Position[1] = GroundHeight(X, Z);
			Vertices[Index].Position[2] = Z;
			Vertices[Index].Position[3] = 1.0f;
		}
		const u32 Indices[] = {0, 1, 2, 2, 1, 3};
		VertexBuffer = Buffer(Vertices.data(), sizeof(Vertices), D3D11_BIND_VERTEX_BUFFER);
		IndexBuffer = Buffer(Indices, sizeof(Indices), D3D11_BIND_INDEX_BUFFER);
		const D3D11_INPUT_ELEMENT_DESC Layout[] = {
			{"POSITION", 0, DXGI_FORMAT_R32G32B32A32_FLOAT, 0, 0, D3D11_INPUT_PER_VERTEX_DATA, 0},
			{"TEXCOORD", 0, DXGI_FORMAT_R32G32_SINT, 0, 16, D3D11_INPUT_PER_VERTEX_DATA, 0},
			{"TANGENT", 0, DXGI_FORMAT_R32G32B32A32_FLOAT, 0, 24, D3D11_INPUT_PER_VERTEX_DATA, 0},
			{"BINORMAL", 0, DXGI_FORMAT_R32G32B32A32_FLOAT, 0, 40, D3D11_INPUT_PER_VERTEX_DATA, 0},
			{"NORMAL", 0, DXGI_FORMAT_R32G32B32A32_FLOAT, 0, 56, D3D11_INPUT_PER_VERTEX_DATA, 0},
			{"TEXCOORD", 1, DXGI_FORMAT_R32G32_SINT, 0, 72, D3D11_INPUT_PER_VERTEX_DATA, 0}};
		Verify(Device->CreateInputLayout(Layout, 6, CaptureVSCode->GetBufferPointer(), CaptureVSCode->GetBufferSize(), &InputLayout), "Create capture input layout");
		Stride = sizeof(Vertex);

		for (const Vector& Pixel : {GroundColor, Vector{1, 0, 0, 0}, Vector{0.5f, 0.5f, 0.5f, 1}})
		{
			auto Source = Texture(1, 1, DXGI_FORMAT_R32G32B32A32_FLOAT, D3D11_BIND_SHADER_RESOURCE, Pixel.data());
			ComPtr<ID3D11ShaderResourceView> View;
			Verify(Device->CreateShaderResourceView(Source.Get(), nullptr, &View), "Create material SRV");
			MaterialViews.push_back(View);
		}
	}

	void Capture(bool CorrectViewport, const Vector& Ground = GroundColor)
	{
		ID3D11ShaderResourceView* Empty[4] = {};
		Context->VSSetShaderResources(0, 4, Empty);
		Context->PSSetShaderResources(0, 4, Empty);
		ComPtr<ID3D11Resource> Material;
		MaterialViews[0]->GetResource(&Material);
		Context->UpdateSubresource(Material.Get(), 0, nullptr, Ground.data(), sizeof(Vector), 0);
		ID3D11RenderTargetView* Targets[] = {ColorTarget.Get(), HeightTarget.Get()};
		Context->OMSetRenderTargets(2, Targets, DepthTarget.Get());
		Context->OMSetDepthStencilState(CaptureDepth.Get(), 0);
		D3D11_VIEWPORT Viewport = {0, 0, 32, 24, 0, 1};
		Context->RSSetViewports(1, &Viewport);
		if (CorrectViewport)
		{
			Viewport.Width = Viewport.Height = float(Resolution);
			Context->RSSetViewports(1, &Viewport);
		}
		const float Clear[] = {0, 0, 0, 0};
		Context->ClearRenderTargetView(ColorTarget.Get(), Clear);
		Context->ClearRenderTargetView(HeightTarget.Get(), Clear);
		Context->ClearDepthStencilView(DepthTarget.Get(), D3D11_CLEAR_DEPTH, 1.0f, 0);
		Context->VSSetShader(CaptureVS.Get(), nullptr, 0);
		Context->PSSetShader(CapturePS.Get(), nullptr, 0);
		const Values Constants = {{"grass_tint_bounds", {-1, -1, 0.5f, 0.5f}},
			{"grass_tint_height_range", {-4, 8, 4, 0.125f}}, {"dt_params", {1, 1, 0, 0}}};
		SetConstants(CaptureVSCode.Get(), true, Constants);
		SetConstants(CapturePSCode.Get(), false, Constants);
		Resource(CapturePSCode.Get(), false, "s_base", MaterialViews[0].Get());
		Resource(CapturePSCode.Get(), false, "s_mask", MaterialViews[1].Get());
		for (const char* Name : {"s_dt_r", "s_dt_g", "s_dt_b", "s_dt_a"})
		{
			Resource(CapturePSCode.Get(), false, Name, MaterialViews[2].Get());
		}
		Sampler(CapturePSCode.Get(), false, "smp_base", PointSampler.Get());
		ID3D11Buffer* Vertices = VertexBuffer.Get();
		const u32 Offset = 0;
		Context->IASetVertexBuffers(0, 1, &Vertices, &Stride, &Offset);
		Context->IASetIndexBuffer(IndexBuffer.Get(), DXGI_FORMAT_R32_UINT, 0);
		Context->IASetInputLayout(InputLayout.Get());
		Context->IASetPrimitiveTopology(D3D11_PRIMITIVE_TOPOLOGY_TRIANGLELIST);
		Context->DrawIndexed(6, 0, 0);
		Context->OMSetRenderTargets(0, nullptr, nullptr);
		Context->GenerateMips(ColorView.Get());
	}

	float HeightError()
	{
		const auto Bytes = Read(Height.Get(), sizeof(float));
		float Error = 0;
		for (u32 Z = 0; Z < Resolution; ++Z)
		{
			for (u32 X = 0; X < Resolution; ++X)
			{
				float HeightValue;
				std::memcpy(&HeightValue, Bytes.data() + (Z * Resolution + X) * sizeof(float), sizeof(float));
				const float WorldX = (float(X) + 0.5f) * 2.0f / Resolution - 1.0f;
				const float WorldZ = (float(Z) + 0.5f) * 2.0f / Resolution - 1.0f;
				Error = std::max(Error, std::abs(HeightValue - GroundHeight(WorldX, WorldZ)));
			}
		}
		return Error;
	}

	Vector Probe(int Mode, Vector Root, float Strength = 1, bool Enabled = true, const TintSettings& Settings = {})
	{
		ID3D11RenderTargetView* Target = OutputTarget.Get();
		Context->OMSetRenderTargets(1, &Target, nullptr);
		Context->OMSetDepthStencilState(ProbeDepth.Get(), 0);
		D3D11_VIEWPORT Viewport = {0, 0, 1, 1, 0, 1};
		Context->RSSetViewports(1, &Viewport);
		Context->VSSetShader(ProbeVS.Get(), nullptr, 0);
		Context->PSSetShader(ProbePS.Get(), nullptr, 0);
		Root[3] = Settings.LocalHeight;
		const Values Constants = {{"grass_tint_bounds", {-1, -1, 0.5f, 0.5f}},
			{"grass_tint_params", {Strength, 0, 0.01f, float(Mode)}},
			{"grass_tint_options", {Settings.GreenOnly ? 1.0f : 0.0f, Enabled ? 1.0f : 0.0f, Settings.Tips, float(Settings.BlendMode)}},
			{"grass_tint_adjustment", Settings.Adjustment}, {"probe_root", Root}, {"probe_base", Settings.Base}, {"probe_mask", {Settings.MaskUV, 0.5f, 0, 0}}};
		SetConstants(ProbeVSCode.Get(), true, Constants);
		SetConstants(ProbePSCode.Get(), false, Constants);
		Resource(ProbeVSCode.Get(), true, "s_grass_tint_color", ColorView.Get());
		Resource(ProbeVSCode.Get(), true, "s_grass_tint_height", HeightView.Get());
		Resource(ProbePSCode.Get(), false, "s_grass_tint_mask", MaskView.Get());
		Sampler(ProbePSCode.Get(), false, "smp_base", PointSampler.Get());
		Sampler(ProbeVSCode.Get(), true, "smp_nofilter", PointSampler.Get());
		Sampler(ProbeVSCode.Get(), true, "smp_rtlinear", LinearSampler.Get());
		Context->IASetInputLayout(nullptr);
		Context->IASetPrimitiveTopology(D3D11_PRIMITIVE_TOPOLOGY_TRIANGLELIST);
		Context->Draw(3, 0);
		Context->OMSetRenderTargets(0, nullptr, nullptr);
		const auto Bytes = Read(Output.Get(), sizeof(Vector));
		Vector Result;
		std::memcpy(Result.data(), Bytes.data(), sizeof(Result));
		return Result;
	}

	void ClearCoverage()
	{
		ID3D11ShaderResourceView* Empty[2] = {};
		Context->VSSetShaderResources(1, 2, Empty);
		const float Clear[] = {0, 0, 0, 0};
		Context->ClearRenderTargetView(ColorTarget.Get(), Clear);
	}

private:
	ComPtr<ID3DBlob> Compile(const char* File, const char* Source, const char* Entry, const char* Profile)
	{
		const D3D_SHADER_MACRO Defines[] = {{"USE_LM_HEMI", "1"}, {"USE_LEGACY_LIGHT", "1"}, {nullptr, nullptr}};
		ComPtr<ID3DBlob> Code, Errors;
		const auto Path = Shaders / File;
		const u32 Flags = D3DCOMPILE_PACK_MATRIX_ROW_MAJOR | D3DCOMPILE_ENABLE_STRICTNESS;
		HRESULT Result;
		if (Source)
		{
			Result = D3DCompile(Source, std::strlen(Source), Path.string().c_str(), Defines,
				D3D_COMPILE_STANDARD_FILE_INCLUDE, Entry, Profile, Flags, 0, &Code, &Errors);
		}
		else
		{
			Result = D3DCompileFromFile(Path.c_str(), Defines, D3D_COMPILE_STANDARD_FILE_INCLUDE, Entry, Profile, Flags, 0, &Code, &Errors);
		}
		if (FAILED(Result) && Errors)
		{
			std::cerr.write(static_cast<const char*>(Errors->GetBufferPointer()), Errors->GetBufferSize());
		}
		Verify(Result, "Compile real terrain tint HLSL");
		return Code;
	}

	ComPtr<ID3D11Texture2D> Texture(u32 Width, u32 Height, DXGI_FORMAT Format, u32 Bind, const void* Pixel = nullptr, bool Mips = false)
	{
		D3D11_TEXTURE2D_DESC Description = {};
		Description.Width = Width;
		Description.Height = Height;
		Description.MipLevels = Mips ? 0 : 1;
		Description.ArraySize = 1;
		Description.Format = Format;
		Description.SampleDesc.Count = 1;
		Description.BindFlags = Bind;
		Description.MiscFlags = Mips ? D3D11_RESOURCE_MISC_GENERATE_MIPS : 0;
		D3D11_SUBRESOURCE_DATA Initial = {Pixel, sizeof(Vector), 0};
		ComPtr<ID3D11Texture2D> Result;
		Verify(Device->CreateTexture2D(&Description, Pixel ? &Initial : nullptr, &Result), "Create test texture");
		return Result;
	}

	ComPtr<ID3D11Buffer> Buffer(const void* Data, u32 Size, u32 Bind)
	{
		D3D11_BUFFER_DESC Description = {};
		Description.ByteWidth = Size;
		Description.BindFlags = Bind;
		D3D11_SUBRESOURCE_DATA Initial = {Data, 0, 0};
		ComPtr<ID3D11Buffer> Result;
		Verify(Device->CreateBuffer(&Description, &Initial, &Result), "Create test buffer");
		return Result;
	}

	ComPtr<ID3D11ShaderReflection> Reflection(ID3DBlob* Code)
	{
		ComPtr<ID3D11ShaderReflection> Result;
		Verify(D3DReflect(Code->GetBufferPointer(), Code->GetBufferSize(), IID_PPV_ARGS(&Result)), "Reflect test shader");
		return Result;
	}

	void SetConstants(ID3DBlob* Code, bool Vertex, const Values& Constants)
	{
		auto Shader = Reflection(Code);
		D3D11_SHADER_DESC Description;
		Verify(Shader->GetDesc(&Description), "Read shader description");
		for (u32 Index = 0; Index < Description.ConstantBuffers; ++Index)
		{
			auto* Table = Shader->GetConstantBufferByIndex(Index);
			D3D11_SHADER_BUFFER_DESC TableDescription;
			Verify(Table->GetDesc(&TableDescription), "Read cbuffer description");
			if (TableDescription.Type != D3D_CT_CBUFFER)
			{
				continue;
			}
			D3D11_SHADER_INPUT_BIND_DESC Binding;
			Verify(Shader->GetResourceBindingDescByName(TableDescription.Name, &Binding), "Read cbuffer slot");
			std::vector<char> Bytes(TableDescription.Size, 0);
			for (const auto& Constant : Constants)
			{
				D3D11_SHADER_VARIABLE_DESC Variable;
				if (SUCCEEDED(Table->GetVariableByName(Constant.first)->GetDesc(&Variable)))
				{
					Check(Variable.Size == sizeof(Vector), "Unexpected constant size");
					std::memcpy(Bytes.data() + Variable.StartOffset, Constant.second.data(), sizeof(Vector));
				}
			}
			auto Data = Buffer(Bytes.data(), u32(Bytes.size()), D3D11_BIND_CONSTANT_BUFFER);
			ID3D11Buffer* Pointer = Data.Get();
			if (Vertex)
			{
				Context->VSSetConstantBuffers(Binding.BindPoint, 1, &Pointer);
			}
			else
			{
				Context->PSSetConstantBuffers(Binding.BindPoint, 1, &Pointer);
			}
		}
	}

	void Resource(ID3DBlob* Code, bool Vertex, const char* Name, ID3D11ShaderResourceView* View)
	{
		D3D11_SHADER_INPUT_BIND_DESC Binding;
		Verify(Reflection(Code)->GetResourceBindingDescByName(Name, &Binding), "Find texture binding");
		if (Vertex)
		{
			Context->VSSetShaderResources(Binding.BindPoint, 1, &View);
		}
		else
		{
			Context->PSSetShaderResources(Binding.BindPoint, 1, &View);
		}
	}

	void Sampler(ID3DBlob* Code, bool Vertex, const char* Name, ID3D11SamplerState* State)
	{
		D3D11_SHADER_INPUT_BIND_DESC Binding;
		Verify(Reflection(Code)->GetResourceBindingDescByName(Name, &Binding), "Find sampler binding");
		if (Vertex)
		{
			Context->VSSetSamplers(Binding.BindPoint, 1, &State);
		}
		else
		{
			Context->PSSetSamplers(Binding.BindPoint, 1, &State);
		}
	}

	std::vector<char> Read(ID3D11Texture2D* Source, u32 PixelSize)
	{
		D3D11_TEXTURE2D_DESC Description;
		Source->GetDesc(&Description);
		Description.MipLevels = 1;
		Description.BindFlags = Description.MiscFlags = 0;
		Description.Usage = D3D11_USAGE_STAGING;
		Description.CPUAccessFlags = D3D11_CPU_ACCESS_READ;
		ComPtr<ID3D11Texture2D> Staging;
		Verify(Device->CreateTexture2D(&Description, nullptr, &Staging), "Create staging texture");
		Context->CopySubresourceRegion(Staging.Get(), 0, 0, 0, 0, Source, 0, nullptr);
		D3D11_MAPPED_SUBRESOURCE Mapped;
		Verify(Context->Map(Staging.Get(), 0, D3D11_MAP_READ, 0, &Mapped), "Read back shader output");
		std::vector<char> Bytes(Description.Width * Description.Height * PixelSize);
		for (u32 Row = 0; Row < Description.Height; ++Row)
		{
			std::memcpy(Bytes.data() + Row * Description.Width * PixelSize,
				static_cast<const char*>(Mapped.pData) + Row * Mapped.RowPitch, Description.Width * PixelSize);
		}
		Context->Unmap(Staging.Get(), 0);
		return Bytes;
	}

	std::filesystem::path Shaders;
	u32 Stride = 0;
	ComPtr<ID3D11Device> Device;
	ComPtr<ID3D11DeviceContext> Context;
	ComPtr<ID3DBlob> CaptureVSCode, CapturePSCode, ProbeVSCode, ProbePSCode;
	ComPtr<ID3D11VertexShader> CaptureVS, ProbeVS;
	ComPtr<ID3D11PixelShader> CapturePS, ProbePS;
	ComPtr<ID3D11Texture2D> Color, Height, Depth, Output, MaskTexture;
	ComPtr<ID3D11RenderTargetView> ColorTarget, HeightTarget, OutputTarget;
	ComPtr<ID3D11DepthStencilView> DepthTarget;
	ComPtr<ID3D11ShaderResourceView> ColorView, HeightView, MaskView;
	ComPtr<ID3D11SamplerState> PointSampler, LinearSampler;
	ComPtr<ID3D11RasterizerState> RasterState;
	ComPtr<ID3D11DepthStencilState> CaptureDepth, ProbeDepth;
	ComPtr<ID3D11Buffer> VertexBuffer, IndexBuffer;
	ComPtr<ID3D11InputLayout> InputLayout;
	std::vector<ComPtr<ID3D11ShaderResourceView>> MaterialViews;
};
}

int main(int Count, char** Arguments)
{
	try
	{
		Check(Count == 2, "Pass the actual d3d11 shader directory");
		WarpFixture Fixture(Arguments[1]);
		const float Coordinate = (12.5f / Resolution) * 2.0f - 1.0f;
		const Vector Root = {Coordinate, GroundHeight(Coordinate, Coordinate), Coordinate, 0};
		Fixture.Capture(false);
		Check(Fixture.HeightError() > 0.1f, "The old window-sized viewport must reproduce the coordinate error");
		Check(Near(Fixture.Probe(1, Root), BaseColor), "The old viewport must reproduce height rejection and unchanged grass");
		Fixture.Capture(true);
		Check(Fixture.HeightError() < 0.0001f, "Correct capture must match world heights across the whole map");
		Check(Near(Fixture.Probe(1, Root), GroundColor), "Tint debug must read the captured terrain color");
		Check(Near(Fixture.Probe(0, Root, 0), BaseColor), "Strength zero must keep the original color");
		Check(!Near(Fixture.Probe(0, Root), BaseColor, 0.03f), "Strength one must visibly change the grass color");
		const Vector Sand = {0.78f, 0.64f, 0.3f, 1};
		const Vector Green = {0.14f, 0.48f, 0.08f, 1};
		Fixture.Capture(true, Sand);
		const Vector DryGrass = Fixture.Probe(0, Root);
		Check(DryGrass[0] > DryGrass[1] && DryGrass[1] > DryGrass[2] * 1.5f, "Green source grass on sand must turn yellow-brown");
		Check(std::abs(Luminance(DryGrass) - Luminance(BaseColor)) < 0.006f, "Brightness zero must retain the blade's luminance");
		TintSettings Settings;
		Settings.MaskUV = 1.0f / 6.0f;
		Check(Near(Fixture.Probe(0, Root, 1, true, Settings), BaseColor), "Transparent atlas mask must preserve source color");
		Check(Near(Fixture.Probe(1, Root, 1, true, Settings), BaseColor), "Transparent mask must also protect terrain color preview");
		Settings.MaskUV = 0.5f;
		Check(Near(Fixture.Probe(0, Root, 1, true, Settings), Fixture.Probe(0, Root, 128.0f / 255.0f)), "Half-transparent alpha mask must scale tint strength linearly");
		Settings = {};
		Check(Near(Fixture.Probe(0, Root, 1, true, Settings), DryGrass), "Opaque black RGB atlas mask must retain full tint");
		Fixture.Capture(true, Green);
		const Vector SameGrassOnGreen = Fixture.Probe(0, Root);
		Check(SameGrassOnGreen[1] > SameGrassOnGreen[0] * 1.8f && !Near(SameGrassOnGreen, DryGrass, 0.12f), "The same grass texture must follow green and sandy terrain with a strong hue change");
		Settings.Base = {0.6f, 0.44f, 0.2f, 1};
		const Vector LiveGrass = Fixture.Probe(0, Root, 1, true, Settings);
		Check(LiveGrass[1] > LiveGrass[0] * 1.8f && LiveGrass[1] > LiveGrass[2] * 1.8f, "Dry source grass on green terrain must turn green");
		Check(!Near(LiveGrass, DryGrass, 0.12f), "The same shader must produce a clearly different terrain hue");
		Settings.Base = BaseColor;
		Settings.Adjustment[1] = 0;
		const Vector Grey = Fixture.Probe(0, Root, 1, true, Settings);
		Check(Near(Grey, {Grey[0], Grey[0], Grey[0], 1}), "Saturation zero must produce a neutral result");
		Settings.Adjustment[1] = 1;
		Settings.Tips = 0;
		Settings.LocalHeight = 1;
		Check(Near(Fixture.Probe(0, Root, 1, true, Settings), BaseColor), "Tip influence zero must keep the original tips");
		Settings.Tips = 1;
		const Vector FullTip = Fixture.Probe(0, Root, 1, true, Settings);
		Settings.LocalHeight = 0;
		Check(Near(FullTip, Fixture.Probe(0, Root, 1, true, Settings)), "Full tip influence must match the root hue");
		Settings.Base = {0.6f, 0.44f, 0.2f, 1};
		Settings.GreenOnly = true;
		Check(Near(Fixture.Probe(0, Root, 1, true, Settings), Settings.Base), "Optional green-only mask must protect non-green pixels");
		Settings = {};
		Fixture.Capture(true, Sand);
		Settings.Adjustment[0] = 1;
		const Vector Bright = Fixture.Probe(0, Root, 1, true, Settings);
		Check(Luminance(Bright) > Luminance(DryGrass) * 1.2f, "Brightness control must transfer the bright sand luminance");
		Settings.Adjustment[0] = 0;
		Settings.Base = {0.08f, 0.2f, 0.04f, 1};
		const Vector FineRelief = Fixture.Probe(0, Root, 1, true, Settings);
		Check(Luminance(FineRelief) < Luminance(DryGrass) * 0.5f, "Dark blade texture detail must remain dark after recoloring");
		Settings.Adjustment[2] = 2;
		Check(Luminance(Fixture.Probe(0, Root, 1, true, Settings)) < Luminance(FineRelief), "Contrast must control the retained blade relief");
		Settings.Base = {0, 0, 0, 1};
		Check(Near(Fixture.Probe(0, Root, 1, true, Settings), {0, 0, 0, 1}), "Black blade detail must remain black and finite");
		Settings = {};
		Settings.BlendMode = 0;
		Check(!Near(Fixture.Probe(0, Root, 1, true, Settings), DryGrass, 0.03f), "Legacy soft tint must remain distinct from full hue transfer");
		Check(Near(Fixture.Probe(4, Root), {0, 1, 0, 1}), "Valid terrain lookup must be green");
		Vector Above = Root;
		Above[1] += 2;
		Check(Near(Fixture.Probe(4, Above), {1, 1, 0, 1}), "Height rejection must be yellow");
		Check(Near(Fixture.Probe(3, Above, 0, false), {1, 0, 1, 1}), "Shader diagnostic must be magenta independently of coverage and enabled state");
		Check(Near(Fixture.Probe(4, {4, 1, 4, 0}), {0, 0, 1, 1}), "Outside-map lookup must be blue");
		Check(Near(Fixture.Probe(4, Root, 1, false), {0.25f, 0.25f, 0.25f, 1}), "Disabled tint must be grey");
		Fixture.ClearCoverage();
		Check(Near(Fixture.Probe(4, Root), {1, 0, 0, 1}), "Missing coverage must be red");
		std::cout << "PASS: real D3D11 capture and tint; green grass on sand becomes dry yellow, dry grass on green terrain becomes green; brightness, saturation, contrast, tips, source detail, legacy mode and diagnostics verified\n";
		return 0;
	}
	catch (const std::exception& Error)
	{
		std::cerr << "FAIL: " << Error.what() << '\n';
		return 1;
	}
}
#endif
