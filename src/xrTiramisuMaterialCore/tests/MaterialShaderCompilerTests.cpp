#include "MaterialAsset.h"
#include "MaterialGraph.h"
#include "TiramisuMaterialShaderCompiler.h"
#include "MaterialTestHarness.h"

#include <algorithm>
#include <array>
#include <filesystem>
#include <fstream>
#include <sstream>
#include <string>

namespace
{
xr_string ReadText(const std::filesystem::path& Path)
{
	std::ifstream Stream(Path, std::ios::binary);
	std::ostringstream Text;
	Text << Stream.rdbuf();
	return Text.str();
}

FMaterialStaticParameterSet DefaultStaticParameters(const FMaterialAsset& Asset)
{
	FMaterialStaticParameterSet Result;
	for (const FMaterialParameterDefinition& Parameter : Asset.Parameters)
	{
		if (Parameter.IsStatic())
		{
			Result.emplace(Parameter.Id, Parameter.DefaultValue);
		}
	}
	return Result;
}

FMaterialSourceAssemblyResult AssembleAsset(const FMaterialAsset& Asset, const xr_string& Implementation)
{
	return AssembleMaterialShaderSource(Asset, ReadText(std::filesystem::path("gamedata/shaders/r5") / Asset.HlslTemplate.c_str()), Implementation, DefaultStaticParameters(Asset));
}

FMaterialShaderCompileResult Compile(const TiramisuMaterialShaderCompiler& Compiler, const EMaterialShaderBackend Backend, const xr_string& Source, const xr_string& Name = "compiler-test.hlsl")
{
	FMaterialShaderCompileRequest Request;
	Request.Backend = Backend;
	Request.Source = Source;
	Request.SourceName = Name;
	Request.IncludeDirectories = {"gamedata/shaders/r5", "gamedata/shaders/r5/materials"};
	return Compiler.Compile(Request);
}

FMaterialShaderCompileResult CompileShaderFile(const TiramisuMaterialShaderCompiler& Compiler, const EMaterialShaderBackend Backend, const std::filesystem::path& Path, const xr_string_view TargetProfile)
{
	FMaterialShaderCompileRequest Request;
	Request.Backend = Backend;
	Request.Source = ReadText(Path);
	Request.SourceName = Path.generic_string();
	Request.TargetProfile = xr_string(TargetProfile);
	Request.IncludeDirectories = {
		"gamedata/shaders/r5",
		"gamedata/shaders/r5/common",
		"gamedata/shaders/r5/global",
	};
	Request.Defines = {"NRI_ENABLE_DRAW_PARAMETERS_EMULATION=1"};
	return Compiler.Compile(Request);
}

void TestSourceAssembly(TiramisuMaterialTestRunner& Runner)
{
	const FMaterialAssetParseResult Parsed = ParseMaterialAssetJson(
		ReadText("gamedata/render_materials/standard_surface.material.json"), "standard_surface.material.json"
	);
	MATERIAL_CHECK(Runner, Parsed.Succeeded());
	const FMaterialSourceAssemblyResult Assembled = AssembleAsset(Parsed.Value, ReadText("gamedata/shaders/r5/materials/StandardSurface.hlsl"));
	MATERIAL_CHECK(Runner, Assembled.Succeeded());
	MATERIAL_CHECK(Runner, Assembled.Source.find("#include \"MaterialParameters.generated.hlsl\"") == xr_string::npos);
	MATERIAL_CHECK(Runner, Assembled.Source.find("#include \"MaterialImplementation.generated.hlsl\"") == xr_string::npos);
	MATERIAL_CHECK(Runner, Assembled.Source.find("struct MaterialParameters") != xr_string::npos);
	MATERIAL_CHECK(Runner, Assembled.Source.find("uint P_2136fd1d_29bd_48e9_9f4c_9ebedb470774") != xr_string::npos);
	MATERIAL_CHECK(Runner, Assembled.Source.find("float4 P_915ce004_8c2f_47ce_87c7_b4af787b835e") != xr_string::npos);
	MATERIAL_CHECK(Runner, Assembled.Source.find("MATERIAL_STATIC_P_39a456f7_50be_4de7_8b7c_60fcc35b8a92 0") != xr_string::npos);
	MATERIAL_CHECK(Runner, Assembled.Source.find("ResourceDescriptorHeap[NonUniformResourceIndex(ResourceIndex)]") != xr_string::npos);
	MATERIAL_CHECK(Runner, Assembled.Source.find("SamplerDescriptorHeap[NonUniformResourceIndex(SamplerIndex)]") != xr_string::npos);
	MATERIAL_CHECK(Runner, Assembled.Source.find("LoadMaterialParametersForInstance") != xr_string::npos);
	MATERIAL_CHECK(Runner, Assembled.Source.find("#include \"common/MaterialGpuAbi.hlsl\"") != xr_string::npos);
	MATERIAL_CHECK(Runner, Assembled.Source.find("float4 Main(MaterialValidationPixelInput Input)") != xr_string::npos);

	FMaterialStaticParameterSet Unknown = DefaultStaticParameters(Parsed.Value);
	Unknown.emplace(FMaterialParameterId{"aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"}, true);
	const FMaterialSourceAssemblyResult Rejected = AssembleMaterialShaderSource(Parsed.Value, ReadText("gamedata/shaders/r5/materials/MaterialTemplate.hlsl"), ReadText("gamedata/shaders/r5/materials/StandardSurface.hlsl"), Unknown);
	MATERIAL_CHECK(Runner, !Rejected.Succeeded());
	MATERIAL_CHECK(Runner, HasDiagnostic(Rejected.Diagnostics, "shader.unknown_static_parameter"));
}

void TestBackendCompilation(TiramisuMaterialTestRunner& Runner, const TiramisuMaterialShaderCompiler& Compiler)
{
	const FMaterialAssetParseResult Parsed = ParseMaterialAssetJson(
		ReadText("gamedata/render_materials/standard_surface.material.json"), "standard_surface.material.json"
	);
	const FMaterialSourceAssemblyResult Assembled = AssembleAsset(Parsed.Value, ReadText("gamedata/shaders/r5/materials/StandardSurface.hlsl"));

	const FMaterialShaderCompileResult Dxil = Compile(Compiler, EMaterialShaderBackend::D3D12, Assembled.Source);
	MATERIAL_CHECK(Runner, Dxil.Succeeded());
	MATERIAL_CHECK(Runner, Dxil.Bytecode.size() > 128);
	MATERIAL_CHECK(Runner, Dxil.Bytecode.size() >= 4 && std::equal(Dxil.Bytecode.begin(), Dxil.Bytecode.begin() + 4, "DXBC"));

	const FMaterialShaderCompileResult SpirV = Compile(Compiler, EMaterialShaderBackend::Vulkan, Assembled.Source);
	MATERIAL_CHECK(Runner, SpirV.Succeeded());
	constexpr xr_array<u8, 4> SpirVMagic = {0x03, 0x02, 0x23, 0x07};
	MATERIAL_CHECK(Runner, SpirV.Bytecode.size() > 128);
	MATERIAL_CHECK(Runner, SpirV.Bytecode.size() >= SpirVMagic.size() && std::equal(SpirVMagic.begin(), SpirVMagic.end(), SpirV.Bytecode.begin()));

	const FMaterialShaderCompileResult Repeated = Compile(Compiler, EMaterialShaderBackend::D3D12, Assembled.Source);
	MATERIAL_CHECK(Runner, Repeated.Succeeded());
	MATERIAL_CHECK(Runner, Repeated.Bytecode == Dxil.Bytecode);

	// Cooker labels used to contain pass/stage separators. The in-memory DXC
	// boundary must treat that string as a diagnostic name, not as another
	// physical input file.
	const FMaterialShaderCompileResult CookerStyleName = Compile(Compiler, EMaterialShaderBackend::D3D12, Assembled.Source, "standard_surface.material.json:gbuffer:pixel");
	MATERIAL_CHECK(Runner, CookerStyleName.Succeeded());
	MATERIAL_CHECK(Runner, CookerStyleName.Bytecode == Dxil.Bytecode);
}

void TestGraphAndHandWrittenParity(TiramisuMaterialTestRunner& Runner, const TiramisuMaterialShaderCompiler& Compiler)
{
	FMaterialAssetParseResult Parsed = ParseMaterialAssetJson(
		ReadText("gamedata/render_materials/example_graph.material.json"), "example_graph.material.json"
	);
	MATERIAL_CHECK(Runner, Parsed.Succeeded());
	FMaterialGraphCompileOptions Options;
	Options.Parameters = Parsed.Value.Parameters;
	Options.EmitNodeLineDirectives = false;
	const FMaterialGraphCompileResult Graph = CompileMaterialGraph(Parsed.Value.Implementation.Graph, Options);
	MATERIAL_CHECK(Runner, Graph.Succeeded());

	const xr_string HandWritten = R"(void EvaluateMaterial(
    in MaterialContext Context,
    in MaterialParameters Parameters,
    out MaterialInputs Result)
{
    Result.BaseColor = float3(1.0f, 1.0f, 1.0f);
    Result.Normal = Context.WorldNormal;
    Result.Roughness = 0.5f;
    Result.Metallic = 0.0f;
    Result.AmbientOcclusion = 1.0f;
    Result.Emissive = float3(0.0f, 0.0f, 0.0f);
    Result.Opacity = 1.0f;
    Result.OpacityMask = 1.0f;
    Result.WorldPositionOffset = float3(0.0f, 0.0f, 0.0f);
    Result.BaseColor = Parameters.P_190412be_3265_452a_94db_d4ee1a545fc8;
}
)";
	const FMaterialSourceAssemblyResult GraphSource = AssembleAsset(Parsed.Value, Graph.GeneratedHlsl);
	const FMaterialSourceAssemblyResult HandSource = AssembleAsset(Parsed.Value, HandWritten);
	MATERIAL_CHECK(Runner, GraphSource.Succeeded());
	MATERIAL_CHECK(Runner, HandSource.Succeeded());

	for (const EMaterialShaderBackend Backend : {EMaterialShaderBackend::D3D12, EMaterialShaderBackend::Vulkan})
	{
		const FMaterialShaderCompileResult GraphBlob = Compile(Compiler, Backend, GraphSource.Source, "graph-equivalent.hlsl");
		const FMaterialShaderCompileResult HandBlob = Compile(Compiler, Backend, HandSource.Source, "hand-equivalent.hlsl");
		MATERIAL_CHECK(Runner, GraphBlob.Succeeded());
		MATERIAL_CHECK(Runner, HandBlob.Succeeded());
		MATERIAL_CHECK(Runner, GraphBlob.Bytecode == HandBlob.Bytecode);
	}
}

void TestDiagnostics(TiramisuMaterialTestRunner& Runner, const TiramisuMaterialShaderCompiler& Compiler)
{
	const FMaterialShaderCompileResult Invalid = Compile(Compiler, EMaterialShaderBackend::D3D12, "float4 Main() : SV_Target0 { this_is_not_hlsl; }");
	MATERIAL_CHECK(Runner, !Invalid.Succeeded());
	MATERIAL_CHECK(Runner, HasDiagnostic(Invalid.Diagnostics, "shader.dxc_compile_failed"));
}

void TestLegacyDrawMaterialAbi(TiramisuMaterialTestRunner& Runner, const TiramisuMaterialShaderCompiler& Compiler)
{
	const xr_array ShaderFiles = {
		xr_pair{"gamedata/shaders/r5/global/scene_vertex.vs.hlsl", "vs_6_6"},
		xr_pair{"gamedata/shaders/r5/global/scene_vertex.ps.hlsl", "ps_6_6"},
		xr_pair{"gamedata/shaders/r5/global/scene_lmap.vs.hlsl", "vs_6_6"},
		xr_pair{"gamedata/shaders/r5/global/scene_lmap.ps.hlsl", "ps_6_6"},
	};
	for (const EMaterialShaderBackend Backend :
		 {EMaterialShaderBackend::D3D12, EMaterialShaderBackend::Vulkan})
	{
		for (const auto& [Path, Profile] : ShaderFiles)
		{
			const FMaterialShaderCompileResult Compiled =
				CompileShaderFile(Compiler, Backend, Path, Profile);
			MATERIAL_CHECK(Runner, Compiled.Succeeded());
			MATERIAL_CHECK(Runner, Compiled.Bytecode.size() > 128);
		}
	}
}
} // namespace

int main()
{
	TiramisuMaterialTestRunner Runner("xrMaterialShaderCompilerTests");
	const TiramisuMaterialShaderCompiler Compiler;
	MATERIAL_CHECK(Runner, Compiler.IsAvailable());
	TestSourceAssembly(Runner);
	TestBackendCompilation(Runner, Compiler);
	TestGraphAndHandWrittenParity(Runner, Compiler);
	TestLegacyDrawMaterialAbi(Runner, Compiler);
	TestDiagnostics(Runner, Compiler);
	return Runner.Finish();
}
