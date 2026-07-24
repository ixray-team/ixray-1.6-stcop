#include "../MaterialPreviewCompiler.h"
#include "../MaterialPreviewAssets.h"
#include "../MaterialInstanceEditorDocument.h"

#include <MaterialGraph.h>

#include <cmath>
#include <cstring>
#include <filesystem>
#include <fstream>
#include <future>
#include <iostream>
#include <sstream>
#include <vector>

namespace
{
using namespace Tiramisu::Editor;

xr_string ReadText(const std::filesystem::path& Path)
{
    std::ifstream Input(Path, std::ios::binary);
    std::ostringstream Stream;
    Stream << Input.rdbuf();
    return Stream.str();
}

FMaterialPreviewCompileRequest RequestFor(const EMaterialShaderBackend Backend)
{
    FMaterialPreviewCompileRequest Request;
    Request.Backend = Backend;
    Request.MaterialJson = ReadText(
        "gamedata/render_materials/standard_surface.material.json");
    Request.MaterialInstanceJson = ReadText(
        "gamedata/render_materials/example_red.material-instance.json");
    Request.GeneratedHlsl = ReadText(
        "gamedata/shaders/r5/materials/StandardSurface.hlsl");
    Request.TemplateSource = ReadText(
        "gamedata/shaders/r5/materials/MaterialTemplate.hlsl");
    Request.VertexFactorySource = ReadText(
        "gamedata/shaders/r5/materials/vertex/MaterialLevelStaticVertexFactory.hlsl");
    Request.Pass = EMaterialPass::Validation;
    Request.PassSource = ReadText(
        "gamedata/shaders/r5/materials/passes/MaterialPreviewPass.hlsl");
    Request.DependencySources.push_back(ReadText(
        "gamedata/shaders/r5/materials/passes/MaterialLightingCommon.hlsl"));
    Request.IncludeDirectories = {
        "gamedata/shaders/r5", "gamedata/shaders/r5/common",
        "gamedata/shaders/r5/materials", "gamedata/shaders/r5/materials/passes",
        "gamedata/shaders/r5/materials/vertex"};
    return Request;
}

int Fail(const FMaterialPreviewCompileResult& Result, const char* Message)
{
    std::cerr << Message << '\n';
    for (const FMaterialDiagnostic& Diagnostic : Result.Diagnostics)
        std::cerr << Diagnostic.Code << ": " << Diagnostic.Message << '\n';
    return 1;
}

float ReadFloat(const FMaterialPackedParameterBlock& Block,
    const FMaterialParameterLayoutField& Field)
{
    float Value = 0.0f;
    std::memcpy(&Value, Block.Data.data() + Field.Offset, sizeof(Value));
    return Value;
}
} // namespace

int main()
{
    if (NormalizeMaterialPreviewTexturePath(
            "./gamedata\\textures\\sky\\sky_10_cube#small.DDS") !=
            "sky/sky_10_cube#small" ||
        NormalizeMaterialPreviewTexturePath(
            "$game_textures$/act/act_arm_1.tga") != "act/act_arm_1" ||
        MaterialPreviewEnvironmentAsset("Studio") !=
            "textures/sky/sky_10_cube#small" ||
        MaterialPreviewEnvironmentAsset("Neutral") !=
            "textures/sky/sky_11_cube#small" ||
        MaterialPreviewEnvironmentAsset("Outdoor") !=
            "textures/sky/sky_19_cube#small" ||
        MaterialPreviewEnvironmentAsset("unknown") !=
            MaterialPreviewEnvironmentAsset("Studio"))
    {
        std::cerr << "Material preview asset normalization or environment mapping failed\n";
        return 1;
    }

    FMaterialPreviewCompileRequest D3DRequest =
        RequestFor(EMaterialShaderBackend::D3D12);
    ResetMaterialPreviewShaderCacheForTests();
    constexpr size_t ConcurrentRequestCount = 8;
    xr_vector<std::future<FMaterialPreviewCompileResult>> ConcurrentRequests;
    ConcurrentRequests.reserve(ConcurrentRequestCount);
    for (size_t Index = 0; Index < ConcurrentRequestCount; ++Index)
    {
        ConcurrentRequests.push_back(std::async(std::launch::async,
            [D3DRequest]
            {
                return CompileMaterialPreview(D3DRequest);
            }));
    }
    FMaterialPreviewCompileResult D3D = ConcurrentRequests.front().get();
    for (size_t Index = 1; Index < ConcurrentRequests.size(); ++Index)
    {
        const FMaterialPreviewCompileResult Concurrent =
            ConcurrentRequests[Index].get();
        if (!Concurrent.Succeeded() ||
            Concurrent.PipelineKey != D3D.PipelineKey ||
            Concurrent.VertexBytecode != D3D.VertexBytecode ||
            Concurrent.PixelBytecode != D3D.PixelBytecode)
        {
            return Fail(Concurrent,
                "Concurrent identical preview compile was not coalesced");
        }
    }
    if (!D3D.Succeeded())
        return Fail(D3D, "D3D12 material preview compilation failed");
    const FMaterialPreviewShaderCacheStatistics ConcurrentCache =
        GetMaterialPreviewShaderCacheStatistics();
    if (ConcurrentCache.RequestCount != ConcurrentRequestCount ||
        ConcurrentCache.CompileCount != 1 ||
        ConcurrentCache.HitCount != ConcurrentRequestCount - 1 ||
        ConcurrentCache.EntryCount != 1)
    {
        std::cerr << "Concurrent shader cache statistics are incorrect\n";
        return 1;
    }
    if (D3D.MaterialId.Value != "128e21af-5c6f-4ec4-a2e3-8b44f90cb553" ||
        D3D.ResolvedMaterial.MasterId.Value !=
            "67e3bc21-9df5-4fc2-ab60-1ad7d02ad6e3" ||
        D3D.ParameterBlock.Resources.size() != 2)
    {
        return Fail(D3D, "Instance resolution or resource parameter packing is incorrect");
    }

    const FMaterialAssetParseResult Master = ParseMaterialAssetJson(D3DRequest.MaterialJson);
    const FMaterialParameterLayoutResult Layout =
        BuildMaterialParameterLayout(Master.Value.Parameters);
    const FMaterialParameterLayoutField* BaseColor =
        Layout.Value.Find({"915ce004-8c2f-47ce-87c7-b4af787b835e"});
    const FMaterialParameterLayoutField* Roughness =
        Layout.Value.Find({"a274b611-7391-4d5f-b08d-d9ce8255fdaf"});
    if (!BaseColor || !Roughness ||
        std::abs(ReadFloat(D3D.ParameterBlock, *BaseColor) - 1.0f) > 0.0001f ||
        std::abs(ReadFloat(D3D.ParameterBlock, *Roughness) - 0.35f) > 0.0001f)
    {
        return Fail(D3D, "Runtime instance overrides were not packed for the preview");
    }

    const FMaterialPreviewCompileResult D3DAgain = CompileMaterialPreview(D3DRequest);
    if (!D3DAgain.Succeeded() || D3DAgain.PipelineKey != D3D.PipelineKey ||
        D3DAgain.VertexBytecode != D3D.VertexBytecode ||
        D3DAgain.PixelBytecode != D3D.PixelBytecode)
    {
        return Fail(D3DAgain, "D3D12 preview output is not deterministic");
    }

    FMaterialPreviewCompileRequest RuntimeOverrideRequest = D3DRequest;
    const size_t RoughnessValue =
        RuntimeOverrideRequest.MaterialInstanceJson.find("0.35");
    if (RoughnessValue == xr_string::npos)
    {
        std::cerr << "Runtime override fixture is missing roughness\n";
        return 1;
    }
    RuntimeOverrideRequest.MaterialInstanceJson.replace(
        RoughnessValue, 4, "0.21");
    const FMaterialPreviewCompileResult RuntimeOverride =
        CompileMaterialPreview(RuntimeOverrideRequest);
    const FMaterialPreviewShaderCacheStatistics RuntimeCache =
        GetMaterialPreviewShaderCacheStatistics();
    if (!RuntimeOverride.Succeeded() ||
        RuntimeOverride.PipelineKey != D3D.PipelineKey ||
        RuntimeOverride.VertexBytecode != D3D.VertexBytecode ||
        RuntimeOverride.PixelBytecode != D3D.PixelBytecode ||
        std::abs(ReadFloat(RuntimeOverride.ParameterBlock, *Roughness) -
            0.21f) > 0.0001f ||
        RuntimeCache.CompileCount != 1)
    {
        return Fail(RuntimeOverride,
            "Runtime parameter override created a duplicate shader permutation");
    }

    FMaterialPreviewCompileRequest VulkanRequest =
        RequestFor(EMaterialShaderBackend::Vulkan);
    const FMaterialPreviewCompileResult Vulkan =
        CompileMaterialPreview(VulkanRequest);
    if (!Vulkan.Succeeded())
        return Fail(Vulkan, "Vulkan material preview compilation failed");
    if (Vulkan.PipelineKey == D3D.PipelineKey ||
        Vulkan.VertexBytecode == D3D.VertexBytecode ||
        Vulkan.PixelBytecode == D3D.PixelBytecode)
    {
        return Fail(Vulkan, "Backend-specific preview permutations were not produced");
    }

	const xr_string GraphJson = ReadText(
		"gamedata/render_materials/example_graph.material.json");
	const FMaterialAssetParseResult GraphAsset = ParseMaterialAssetJson(GraphJson);
	FMaterialGraphCompileOptions GraphOptions;
	GraphOptions.Parameters = GraphAsset.Value.Parameters;
	const FMaterialGraphCompileResult Generated = CompileMaterialGraph(
		GraphAsset.Value.Implementation.Graph, GraphOptions);
	if (!GraphAsset.Succeeded() || !Generated.Succeeded())
	{
		FMaterialPreviewCompileResult GraphFailure;
		GraphFailure.Diagnostics = Generated.Diagnostics;
		return Fail(GraphFailure, "The node graph could not generate preview HLSL");
	}
	FMaterialPreviewCompileRequest GraphRequest =
		RequestFor(EMaterialShaderBackend::D3D12);
	GraphRequest.MaterialJson = GraphJson;
	GraphRequest.MaterialInstanceJson.clear();
	// A graph asset is compiled by the same background front-end even when the
	// caller does not pre-generate HLSL.
	GraphRequest.GeneratedHlsl.clear();
	const FMaterialPreviewCompileResult GraphPreview =
		CompileMaterialPreview(GraphRequest);
	if (!GraphPreview.Succeeded() ||
		GraphPreview.MaterialId != GraphAsset.Value.Id ||
		GraphPreview.ParameterBlock.Resources.size() != 0)
	{
		return Fail(GraphPreview,
			"Graph-generated HLSL did not compile through the live preview path");
	}

    FMaterialPreviewCompileRequest ForwardRequest =
        RequestFor(EMaterialShaderBackend::D3D12);
    ForwardRequest.Pass = EMaterialPass::Forward;
    ForwardRequest.PassSource = ReadText(
        "gamedata/shaders/r5/materials/passes/MaterialForwardPass.hlsl");
    ForwardRequest.RenderPassSignature = "editor_forward:rgba8:d32";
    ForwardRequest.CompilerOptions = "editor_viewport_scene_test";
    const FMaterialPreviewCompileResult Forward =
        CompileMaterialPreview(ForwardRequest);
    if (!Forward.Succeeded() || Forward.PipelineKey == D3D.PipelineKey)
        return Fail(Forward, "D3D12 editor Forward material pass failed");

    FMaterialPreviewCompileRequest ForwardVulkanRequest = ForwardRequest;
    ForwardVulkanRequest.Backend = EMaterialShaderBackend::Vulkan;
    const FMaterialPreviewCompileResult ForwardVulkan =
        CompileMaterialPreview(ForwardVulkanRequest);
    if (!ForwardVulkan.Succeeded() ||
        ForwardVulkan.PipelineKey == Forward.PipelineKey ||
        ForwardVulkan.VertexBytecode == Forward.VertexBytecode ||
        ForwardVulkan.PixelBytecode == Forward.PixelBytecode)
    {
        return Fail(ForwardVulkan,
            "Vulkan editor Forward material permutation failed");
    }

    FMaterialPreviewCompileRequest ChangedDependency = ForwardRequest;
    ChangedDependency.DependencySources.push_back(
        "// simulated transitive include change");
    const FMaterialPreviewCompileResult Changed =
        CompileMaterialPreview(ChangedDependency);
    if (!Changed.Succeeded() || Changed.PipelineKey == Forward.PipelineKey)
        return Fail(Changed, "Transitive include did not affect the pipeline key");

    TiramisuMaterialLibrary ParentLibrary;
    if (!ParentLibrary.RegisterMaster(Master.Value).Succeeded())
        return 1;
    FMaterialInstanceAsset Middle;
    Middle.Id.Value = "preview-parent-middle";
    Middle.Name = "Preview Parent";
    Middle.Parent = Master.Value.Id.Value;
    Middle.Overrides.emplace(
        FMaterialParameterId{"a274b611-7391-4d5f-b08d-d9ce8255fdaf"}, 0.12f);
    if (!ParentLibrary.RegisterInstance(Middle).Succeeded())
        return 1;
    const FMaterialResolveResult ResolvedParent =
        ParentLibrary.Resolve(Middle.Id.Value);
    if (!ResolvedParent.Succeeded())
        return 1;

    FMaterialInstanceAsset Child;
    Child.Id.Value = "preview-parent-child";
    Child.Name = "Preview Child";
    Child.Parent = Middle.Id.Value;
    Child.Overrides.emplace(
        FMaterialParameterId{"915ce004-8c2f-47ce-87c7-b4af787b835e"},
        FFloat4{0.1f, 0.8f, 0.2f, 1.0f});
    TiramisuMaterialInstanceEditorDocument ChildDocument;
    ChildDocument.OpenInstance(Child);
    if (!ChildDocument.SetParentResolution(
            Master.Value, ResolvedParent.Value).Succeeded())
    {
        return 1;
    }

    FMaterialPreviewCompileRequest ParentChainRequest =
        RequestFor(EMaterialShaderBackend::D3D12);
    ParentChainRequest.MaterialInstanceJson =
        ChildDocument.SerializeFlattenedInstance();
    const FMaterialPreviewCompileResult ParentChainPreview =
        CompileMaterialPreview(ParentChainRequest);
    if (!ParentChainPreview.Succeeded() ||
        ParentChainPreview.MaterialId != Child.Id ||
        std::abs(ReadFloat(ParentChainPreview.ParameterBlock, *Roughness) -
            0.12f) > 0.0001f)
    {
        return Fail(ParentChainPreview,
            "Flattened parent instance chain did not reach live preview");
    }

    FMaterialPreviewCompileRequest Invalid = RequestFor(EMaterialShaderBackend::D3D12);
    Invalid.GeneratedHlsl = "void EvaluateMaterial(";
    const FMaterialPreviewCompileResult Rejected = CompileMaterialPreview(Invalid);
    if (Rejected.Succeeded() || Rejected.Diagnostics.empty())
        return Fail(Rejected, "Invalid generated HLSL was accepted");
    return 0;
}
