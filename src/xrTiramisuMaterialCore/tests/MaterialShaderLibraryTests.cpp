#include "TiramisuMaterialShaderLibrary.h"
#include "MaterialTestHarness.h"

#include <algorithm>
#include <cstdint>

namespace
{
constexpr xr_string_view VertexFactory = "level_static";
constexpr xr_string_view RenderPassSignature =
    "gbuffer:rgba8+rgba16f+rgba16f+rg16f:d24s8";

u64 PipelineKey(const EMaterialShaderBlobFormat Format)
{
    FResolvedMaterialInstance Material;
    Material.MasterId = {"master-a"};
    const FMaterialPassDefinition* Pass =
        FindMaterialPassDefinition(EMaterialPass::GBuffer);
    return MakeCookedMaterialPipelineKey(Material, *Pass,
        Format == EMaterialShaderBlobFormat::Dxil ? "d3d12" : "vulkan").StableHash();
}

FMaterialShaderBlob Blob(const EMaterialShaderBlobFormat Format, const EMaterialShaderStage Stage,
    const u8 BytecodeTag)
{
    FMaterialShaderBlob Result;
    Result.MaterialId = {"master-a"};
    Result.PipelineKey = PipelineKey(Format);
    Result.Format = Format;
    Result.EntryPoint = "Main";
    Result.Bytecode = {BytecodeTag, static_cast<u8>(BytecodeTag + 1)};
    Result.Pass = EMaterialPass::GBuffer;
    Result.Stage = Stage;
    Result.VertexFactory = VertexFactory;
    Result.RenderPassSignature = RenderPassSignature;
    return Result;
}

FMaterialBundle CompleteBundle()
{
    FMaterialAsset Master;
    Master.Id = {"master-a"};
    Master.Name = "Master A";
    Master.SourcePath = "master.json";
    Master.HlslTemplate = "materials/MaterialTemplate.hlsl";
    Master.Implementation.Source = "materials/MasterA.hlsl";

    FMaterialInstanceAsset Instance;
    Instance.Id = {"instance-a"};
    Instance.Name = "Instance A";
    Instance.SourcePath = "instance.json";
    Instance.Parent = Master.Id.Value;

    FMaterialBundle Bundle;
    Bundle.CompleteShaderSet = true;
    Bundle.Records = {
        {EMaterialBundleRecordType::Master, {"master-a"}, {"master-a"}, "master.json",
            SerializeMaterialAssetJson(Master), "", {}},
        {EMaterialBundleRecordType::FlattenedInstance, {"instance-a"}, {"master-a"},
            "instance.json", SerializeMaterialInstanceJson(Instance), "", {}},
    };
    Bundle.ShaderBlobs = {
        Blob(EMaterialShaderBlobFormat::Dxil, EMaterialShaderStage::Vertex, 1),
        Blob(EMaterialShaderBlobFormat::Dxil, EMaterialShaderStage::Pixel, 3),
        Blob(EMaterialShaderBlobFormat::SpirV, EMaterialShaderStage::Vertex, 5),
        Blob(EMaterialShaderBlobFormat::SpirV, EMaterialShaderStage::Pixel, 7),
    };
    return Bundle;
}

void TestCompleteBackendSelectionAndInstanceAlias(TiramisuMaterialTestRunner& Runner)
{
    const FMaterialShaderLibraryBuildResult Built = TiramisuMaterialShaderLibrary::Build(
        CompleteBundle(), {EMaterialShaderBlobFormat::Dxil, true});
    MATERIAL_CHECK(Runner, Built.Succeeded());
    if (!Built.Value)
        return;

    MATERIAL_CHECK(Runner, Built.Value->IsComplete());
    MATERIAL_CHECK(Runner, Built.Value->GetFormat() == EMaterialShaderBlobFormat::Dxil);
    MATERIAL_CHECK(Runner, Built.Value->GetProgramCount() == 1);
    MATERIAL_CHECK(Runner, Built.Value->ResolveMasterMaterialId({"instance-a"}) == FMaterialAssetId{"master-a"});
    const FMaterialAsset* ResolvedMaster =
        Built.Value->ResolveMasterMaterial({"instance-a"});
    MATERIAL_CHECK(Runner, ResolvedMaster != nullptr);
    MATERIAL_CHECK(Runner, ResolvedMaster && ResolvedMaster->Id == FMaterialAssetId{"master-a"});
    MATERIAL_CHECK(Runner, Built.Value->ResolveMaterial({"instance-a"}) != nullptr);
    MATERIAL_CHECK(Runner, Built.Value->ResolveMasterMaterial({"missing"}) == nullptr);

    const auto Program = Built.Value->Find(
        {"instance-a"}, PipelineKey(EMaterialShaderBlobFormat::Dxil),
        EMaterialPass::GBuffer, VertexFactory, RenderPassSignature);
    MATERIAL_CHECK(Runner, Program.has_value());
    if (Program)
    {
        MATERIAL_CHECK(Runner, Program->IsComplete());
        MATERIAL_CHECK(Runner, Program->Vertex->Format == EMaterialShaderBlobFormat::Dxil);
        MATERIAL_CHECK(Runner, Program->Vertex->Bytecode.front() == 1);
        MATERIAL_CHECK(Runner, Program->Pixel->Bytecode.front() == 3);
    }

    const auto ResolvedProgram = Built.Value->Find(
        {"instance-a"}, EMaterialPass::GBuffer, VertexFactory, RenderPassSignature);
    MATERIAL_CHECK(Runner, ResolvedProgram.has_value());
    MATERIAL_CHECK(Runner, ResolvedProgram && ResolvedProgram->IsComplete());
}

void TestSerializedLibrary(TiramisuMaterialTestRunner& Runner)
{
    const FMaterialBundleWriteResult Serialized = SerializeMaterialBundle(CompleteBundle());
    MATERIAL_CHECK(Runner, Serialized.Succeeded());
    const FMaterialShaderLibraryBuildResult Loaded = TiramisuMaterialShaderLibrary::Deserialize(
        Serialized.Data, {EMaterialShaderBlobFormat::SpirV, true});
    MATERIAL_CHECK(Runner, Loaded.Succeeded());
    if (!Loaded.Value)
        return;

    const auto Program = Loaded.Value->Find(
        {"master-a"}, PipelineKey(EMaterialShaderBlobFormat::SpirV),
        EMaterialPass::GBuffer, VertexFactory, RenderPassSignature);
    MATERIAL_CHECK(Runner, Program.has_value());
    if (Program)
    {
        MATERIAL_CHECK(Runner, Program->IsComplete());
        MATERIAL_CHECK(Runner, Program->Vertex->Format == EMaterialShaderBlobFormat::SpirV);
        MATERIAL_CHECK(Runner, Program->Pixel->Bytecode.front() == 7);
    }
}

void TestIncompleteDevelopmentGate(TiramisuMaterialTestRunner& Runner)
{
    FMaterialBundle Bundle = CompleteBundle();
    Bundle.CompleteShaderSet = false;
    std::erase_if(Bundle.ShaderBlobs, [](const FMaterialShaderBlob& Shader)
    {
        return Shader.Format == EMaterialShaderBlobFormat::Dxil && Shader.Stage == EMaterialShaderStage::Vertex;
    });

    const FMaterialShaderLibraryBuildResult Strict = TiramisuMaterialShaderLibrary::Build(
        Bundle, {EMaterialShaderBlobFormat::Dxil, true});
    MATERIAL_CHECK(Runner, !Strict.Succeeded());
    MATERIAL_CHECK(Runner, HasDiagnostic(Strict.Diagnostics, "shader_library.incomplete_bundle"));

    const FMaterialShaderLibraryBuildResult Development = TiramisuMaterialShaderLibrary::Build(
        std::move(Bundle), {EMaterialShaderBlobFormat::Dxil, false});
    MATERIAL_CHECK(Runner, Development.Succeeded());
    MATERIAL_CHECK(Runner, HasDiagnostic(Development.Diagnostics, "shader_library.missing_stage"));
    if (!Development.Value)
        return;

    MATERIAL_CHECK(Runner, !Development.Value->IsComplete());
    const auto Program = Development.Value->Find(
        {"instance-a"}, PipelineKey(EMaterialShaderBlobFormat::Dxil),
        EMaterialPass::GBuffer, VertexFactory, RenderPassSignature);
    MATERIAL_CHECK(Runner, Program.has_value());
    if (Program)
    {
        MATERIAL_CHECK(Runner, !Program->IsComplete());
        MATERIAL_CHECK(Runner, Program->Vertex == nullptr);
        MATERIAL_CHECK(Runner, Program->Pixel != nullptr);
    }
}

void TestInvalidProgramsRejected(TiramisuMaterialTestRunner& Runner)
{
    FMaterialBundle MissingStage = CompleteBundle();
    std::erase_if(MissingStage.ShaderBlobs, [](const FMaterialShaderBlob& Shader)
    {
        return Shader.Format == EMaterialShaderBlobFormat::Dxil && Shader.Stage == EMaterialShaderStage::Vertex;
    });
    const FMaterialShaderLibraryBuildResult Missing = TiramisuMaterialShaderLibrary::Build(
        std::move(MissingStage), {EMaterialShaderBlobFormat::Dxil, false});
    MATERIAL_CHECK(Runner, !Missing.Succeeded());
    MATERIAL_CHECK(Runner, HasDiagnostic(Missing.Diagnostics, "shader_library.missing_stage"));

    FMaterialBundle Duplicate = CompleteBundle();
    Duplicate.ShaderBlobs.push_back(Blob(
        EMaterialShaderBlobFormat::Dxil, EMaterialShaderStage::Pixel, 9));
    const FMaterialShaderLibraryBuildResult DuplicateResult = TiramisuMaterialShaderLibrary::Build(
        std::move(Duplicate), {EMaterialShaderBlobFormat::Dxil, true});
    MATERIAL_CHECK(Runner, !DuplicateResult.Succeeded());
    MATERIAL_CHECK(Runner, HasDiagnostic(DuplicateResult.Diagnostics, "shader_library.duplicate_stage"));

    FMaterialBundle Conflict = CompleteBundle();
    Conflict.Records.push_back({EMaterialBundleRecordType::FlattenedInstance, {"instance-a"}, {"master-b"},
        "conflict.json", "instance", "", {}});
    const FMaterialShaderLibraryBuildResult ConflictResult = TiramisuMaterialShaderLibrary::Build(
        std::move(Conflict), {EMaterialShaderBlobFormat::Dxil, true});
    MATERIAL_CHECK(Runner, !ConflictResult.Succeeded());
    MATERIAL_CHECK(Runner, HasDiagnostic(ConflictResult.Diagnostics, "shader_library.conflicting_alias"));
}

void TestMissingBackendRejected(TiramisuMaterialTestRunner& Runner)
{
    FMaterialBundle Bundle = CompleteBundle();
    std::erase_if(Bundle.ShaderBlobs, [](const FMaterialShaderBlob& Shader)
    {
        return Shader.Format == EMaterialShaderBlobFormat::SpirV;
    });
    const FMaterialShaderLibraryBuildResult Result = TiramisuMaterialShaderLibrary::Build(
        std::move(Bundle), {EMaterialShaderBlobFormat::SpirV, true});
    MATERIAL_CHECK(Runner, !Result.Succeeded());
    MATERIAL_CHECK(Runner, HasDiagnostic(Result.Diagnostics, "shader_library.missing_backend"));
}
} // namespace

int main()
{
    TiramisuMaterialTestRunner Runner("xrMaterialShaderLibraryTests");
    TestCompleteBackendSelectionAndInstanceAlias(Runner);
    TestSerializedLibrary(Runner);
    TestIncompleteDevelopmentGate(Runner);
    TestInvalidProgramsRejected(Runner);
    TestMissingBackendRejected(Runner);
    return Runner.Finish();
}
