#include "MaterialAsset.h"
#include "MaterialGraph.h"
#include "MaterialPass.h"
#include "TiramisuMaterialShaderCompiler.h"
#include "MaterialTestHarness.h"

#include <algorithm>
#include <array>
#include <filesystem>
#include <fstream>
#include <set>
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

bool HasBackendMagic(const FMaterialShaderCompileResult& Result, const EMaterialShaderBackend Backend)
{
	constexpr xr_array<u8, 4> Dxil = {'D', 'X', 'B', 'C'};
	constexpr xr_array<u8, 4> SpirV = {0x03, 0x02, 0x23, 0x07};
	const auto& Expected = Backend == EMaterialShaderBackend::D3D12 ? Dxil : SpirV;
	return Result.Bytecode.size() >= Expected.size() && std::equal(Expected.begin(), Expected.end(), Result.Bytecode.begin());
}

void TestManifestAndRouting(TiramisuMaterialTestRunner& Runner)
{
	const xr_span Manifest = GetMaterialPassManifest();
	MATERIAL_CHECK(Runner, Manifest.size() == 7);
	xr_set<EMaterialPass> Unique;
	for (const FMaterialPassDefinition& Definition : Manifest)
	{
		MATERIAL_CHECK(Runner, Definition.Version == MaterialPassManifestVersion);
		MATERIAL_CHECK(Runner, !Definition.Name.empty());
		MATERIAL_CHECK(Runner, !Definition.ShaderSource.empty());
		MATERIAL_CHECK(Runner, !Definition.VertexFactory.empty());
		MATERIAL_CHECK(Runner, !Definition.RenderPassSignature.empty());
		MATERIAL_CHECK(Runner, Definition.Stage == EMaterialShaderStage::Pixel);
		MATERIAL_CHECK(Runner, FindMaterialVertexFactoryDefinition(Definition.VertexFactory) != nullptr);
		MATERIAL_CHECK(Runner, Unique.insert(Definition.Pass).second);
		MATERIAL_CHECK(Runner, ParseMaterialPass(Definition.Name) == Definition.Pass);
		MATERIAL_CHECK(Runner, ToString(Definition.Pass) == Definition.Name);
	}

	FMaterialAsset Asset;
	Asset.Domain = EMaterialDomain::Surface;
	Asset.BlendMode = EMaterialBlendMode::Opaque;
	MATERIAL_CHECK(Runner, GetRequiredMaterialPasses(Asset) == xr_vector<EMaterialPass>({EMaterialPass::Depth, EMaterialPass::Shadow, EMaterialPass::GBuffer}));
	Asset.BlendMode = EMaterialBlendMode::Masked;
	MATERIAL_CHECK(Runner, GetRequiredMaterialPasses(Asset).size() == 3);
	Asset.BlendMode = EMaterialBlendMode::Translucent;
	MATERIAL_CHECK(Runner, GetRequiredMaterialPasses(Asset) == xr_vector<EMaterialPass>({EMaterialPass::Forward}));
	Asset.Domain = EMaterialDomain::UI;
	MATERIAL_CHECK(Runner, GetRequiredMaterialPasses(Asset) == xr_vector<EMaterialPass>({EMaterialPass::UI}));
	Asset.Domain = EMaterialDomain::PostProcess;
	MATERIAL_CHECK(Runner, GetRequiredMaterialPasses(Asset) == xr_vector<EMaterialPass>({EMaterialPass::PostProcess}));
	Asset.Domain = EMaterialDomain::Decal;
	MATERIAL_CHECK(Runner, GetRequiredMaterialPasses(Asset) == xr_vector<EMaterialPass>({EMaterialPass::GBuffer}));
	MATERIAL_CHECK(Runner, !ParseMaterialPass("not-a-pass").has_value());

	const xr_span VertexFactories = GetMaterialVertexFactoryManifest();
	MATERIAL_CHECK(Runner, VertexFactories.size() == 2);
	xr_set<xr_string_view> UniqueVertexFactories;
	for (const FMaterialVertexFactoryDefinition& Definition : VertexFactories)
	{
		MATERIAL_CHECK(Runner, Definition.Version == MaterialVertexFactoryManifestVersion);
		MATERIAL_CHECK(Runner, !Definition.Name.empty());
		MATERIAL_CHECK(Runner, !Definition.ShaderSource.empty());
		MATERIAL_CHECK(Runner, Definition.TargetProfile == "vs_6_6");
		MATERIAL_CHECK(Runner, UniqueVertexFactories.insert(Definition.Name).second);
		MATERIAL_CHECK(Runner, FindMaterialVertexFactoryDefinition(Definition.Name) == &Definition);
	}
	MATERIAL_CHECK(Runner, FindMaterialVertexFactoryDefinition("missing") == nullptr);
}

void TestAllPassesCompile(TiramisuMaterialTestRunner& Runner)
{
	const FMaterialAssetParseResult Parsed = ParseMaterialAssetJson(
		ReadText("gamedata/render_materials/standard_surface.material.json"), "standard_surface.material.json"
	);
	MATERIAL_CHECK(Runner, Parsed.Succeeded());
	const xr_string Template = ReadText("gamedata/shaders/r5/materials/MaterialTemplate.hlsl");
	const xr_string Implementation = ReadText("gamedata/shaders/r5/materials/StandardSurface.hlsl");
	const FMaterialStaticParameterSet StaticParameters = DefaultStaticParameters(Parsed.Value);
	const TiramisuMaterialShaderCompiler Compiler;
	MATERIAL_CHECK(Runner, Compiler.IsAvailable());

	for (const FMaterialPassDefinition& Pass : GetMaterialPassManifest())
	{
		const xr_string PassSource = ReadText(std::filesystem::path("gamedata/shaders/r5") / Pass.ShaderSource);
		const FMaterialSourceAssemblyResult Source = AssembleMaterialShaderSourceForPass(
			Parsed.Value, Template, Implementation, StaticParameters, PassSource, Pass.ShaderSource
		);
		MATERIAL_CHECK(Runner, Source.Succeeded());
		MATERIAL_CHECK(Runner, Source.Source.find("#define MATERIAL_DOMAIN_SURFACE 1") != xr_string::npos);
		MATERIAL_CHECK(Runner, Source.Source.find("#define MATERIAL_BLEND_MASKED 0") != xr_string::npos);
		for (const EMaterialShaderBackend Backend : {EMaterialShaderBackend::D3D12, EMaterialShaderBackend::Vulkan})
		{
			FMaterialShaderCompileRequest Request;
			Request.Backend = Backend;
			Request.Source = Source.Source;
			Request.SourceName = xr_string(Pass.Name) + ".material.hlsl";
			Request.EntryPoint = xr_string(Pass.EntryPoint);
			Request.TargetProfile = xr_string(Pass.TargetProfile);
			Request.IncludeDirectories = {"gamedata/shaders/r5", "gamedata/shaders/r5/materials", "gamedata/shaders/r5/materials/passes"};
			const FMaterialShaderCompileResult Compiled = Compiler.Compile(Request);
			MATERIAL_CHECK(Runner, Compiled.Succeeded());
			MATERIAL_CHECK(Runner, HasBackendMagic(Compiled, Backend));
		}
	}

	FMaterialAsset Masked = Parsed.Value;
	Masked.BlendMode = EMaterialBlendMode::Masked;
	const FMaterialPassDefinition* Depth = FindMaterialPassDefinition(EMaterialPass::Depth);
	const FMaterialSourceAssemblyResult MaskedSource = AssembleMaterialShaderSourceForPass(Masked, Template, Implementation, StaticParameters, ReadText(std::filesystem::path("gamedata/shaders/r5") / Depth->ShaderSource), Depth->ShaderSource);
	MATERIAL_CHECK(Runner, MaskedSource.Succeeded());
	MATERIAL_CHECK(Runner, MaskedSource.Source.find("#define MATERIAL_BLEND_MASKED 1") != xr_string::npos);
	MATERIAL_CHECK(Runner, MaskedSource.Source.find("clip(Inputs.OpacityMask") == xr_string::npos);

	const FMaterialSourceAssemblyResult MissingPass = AssembleMaterialShaderSourceForPass(
		Parsed.Value, Template, Implementation, StaticParameters, {}, "missing-pass"
	);
	MATERIAL_CHECK(Runner, !MissingPass.Succeeded());
	MATERIAL_CHECK(Runner, HasDiagnostic(MissingPass.Diagnostics, "shader.empty_pass"));
}

void TestVertexFactoriesCompile(TiramisuMaterialTestRunner& Runner)
{
	const FMaterialAssetParseResult Parsed = ParseMaterialAssetJson(
		ReadText("gamedata/render_materials/standard_surface.material.json"), "standard_surface.material.json"
	);
	MATERIAL_CHECK(Runner, Parsed.Succeeded());
	const xr_string Template = ReadText("gamedata/shaders/r5/materials/MaterialTemplate.hlsl");
	const xr_string Implementation = ReadText("gamedata/shaders/r5/materials/StandardSurface.hlsl");
	const FMaterialStaticParameterSet StaticParameters = DefaultStaticParameters(Parsed.Value);
	const TiramisuMaterialShaderCompiler Compiler;
	MATERIAL_CHECK(Runner, Compiler.IsAvailable());

	xr_set<xr_string_view> CompiledSources;
	for (const FMaterialVertexFactoryDefinition& VertexFactory : GetMaterialVertexFactoryManifest())
	{
		if (!CompiledSources.insert(VertexFactory.ShaderSource).second)
		{
			continue;
		}
		const xr_string VertexSource = ReadText(
			std::filesystem::path("gamedata/shaders/r5") / VertexFactory.ShaderSource
		);
		const FMaterialSourceAssemblyResult Source = AssembleMaterialShaderSourceForPass(
			Parsed.Value, Template, Implementation, StaticParameters, VertexSource, VertexFactory.ShaderSource
		);
		MATERIAL_CHECK(Runner, Source.Succeeded());
		MATERIAL_CHECK(Runner, Source.Source.find("NRI_INSTANCE_ID_OFFSET") != xr_string::npos);
		MATERIAL_CHECK(Runner, Source.Source.find("EvaluateMaterial(Context, Parameters, Material)") != xr_string::npos);

		for (const EMaterialShaderBackend Backend : {EMaterialShaderBackend::D3D12, EMaterialShaderBackend::Vulkan})
		{
			FMaterialShaderCompileRequest Request;
			Request.Backend = Backend;
			Request.Source = Source.Source;
			Request.SourceName = xr_string(VertexFactory.Name) + ".material.hlsl";
			Request.EntryPoint = xr_string(VertexFactory.EntryPoint);
			Request.TargetProfile = xr_string(VertexFactory.TargetProfile);
			Request.Defines.emplace_back("MATERIAL_VERTEX_SHADER=1");
			if (Backend == EMaterialShaderBackend::D3D12)
			{
				Request.Defines.emplace_back("NRI_ENABLE_DRAW_PARAMETERS_EMULATION=1");
			}
			Request.IncludeDirectories = {"gamedata/shaders/r5", "gamedata/shaders/r5/common", "gamedata/shaders/r5/materials", "gamedata/shaders/r5/materials/passes", "gamedata/shaders/r5/materials/vertex"};
			const FMaterialShaderCompileResult Compiled = Compiler.Compile(Request);
			MATERIAL_CHECK(Runner, Compiled.Succeeded());
			MATERIAL_CHECK(Runner, HasBackendMagic(Compiled, Backend));
		}
	}
}
} // namespace

int main()
{
	TiramisuMaterialTestRunner Runner("xrMaterialPassCompilerTests");
	TestManifestAndRouting(Runner);
	TestAllPassesCompile(Runner);
	TestVertexFactoriesCompile(Runner);
	return Runner.Finish();
}
