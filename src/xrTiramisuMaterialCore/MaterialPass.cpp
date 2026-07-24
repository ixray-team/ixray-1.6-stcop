#include "MaterialPass.h"

#include <array>

namespace
{
constexpr xr_array PassManifest = {
    FMaterialPassDefinition{MaterialPassManifestVersion, EMaterialPass::Depth, "depth",
        "materials/passes/MaterialDepthPass.hlsl", "Main", "ps_6_6", "level_static", "depth:d24s8"},
    FMaterialPassDefinition{MaterialPassManifestVersion, EMaterialPass::Shadow, "shadow",
        "materials/passes/MaterialShadowPass.hlsl", "Main", "ps_6_6", "level_static", "shadow:d32"},
    FMaterialPassDefinition{MaterialPassManifestVersion, EMaterialPass::GBuffer, "gbuffer",
        "materials/passes/MaterialGBufferPass.hlsl", "Main", "ps_6_6", "level_static",
        "gbuffer:rgba8+rgba16f+rgba16f+rg16f:d24s8"},
    FMaterialPassDefinition{MaterialPassManifestVersion, EMaterialPass::Forward, "forward",
        "materials/passes/MaterialForwardPass.hlsl", "Main", "ps_6_6", "level_static", "forward:rgba16f:d24s8"},
    FMaterialPassDefinition{MaterialPassManifestVersion, EMaterialPass::UI, "ui",
        "materials/passes/MaterialUIPass.hlsl", "Main", "ps_6_6", "level_static", "ui:rgba8"},
    FMaterialPassDefinition{MaterialPassManifestVersion, EMaterialPass::PostProcess, "post_process",
        "materials/passes/MaterialPostProcessPass.hlsl", "Main", "ps_6_6", "level_static", "post_process:rgba16f"},
    FMaterialPassDefinition{MaterialPassManifestVersion, EMaterialPass::Validation, "validation",
        "materials/passes/MaterialValidationPass.hlsl", "Main", "ps_6_6", "material_validation", "validation:rgba8"},
};

constexpr xr_array VertexFactoryManifest = {
    FMaterialVertexFactoryDefinition{MaterialVertexFactoryManifestVersion,
        "level_static", "materials/vertex/MaterialLevelStaticVertexFactory.hlsl"},
    FMaterialVertexFactoryDefinition{MaterialVertexFactoryManifestVersion,
        "material_validation", "materials/vertex/MaterialLevelStaticVertexFactory.hlsl"},
};
} // namespace

xr_span<const FMaterialPassDefinition> GetMaterialPassManifest() noexcept
{
    return PassManifest;
}

const FMaterialPassDefinition* FindMaterialPassDefinition(const EMaterialPass Pass) noexcept
{
    for (const FMaterialPassDefinition& Definition : PassManifest)
        if (Definition.Pass == Pass) return &Definition;
    return nullptr;
}

xr_span<const FMaterialVertexFactoryDefinition> GetMaterialVertexFactoryManifest() noexcept
{
    return VertexFactoryManifest;
}

const FMaterialVertexFactoryDefinition* FindMaterialVertexFactoryDefinition(
    const xr_string_view Name) noexcept
{
    for (const FMaterialVertexFactoryDefinition& Definition : VertexFactoryManifest)
        if (Definition.Name == Name) return &Definition;
    return nullptr;
}

xr_vector<EMaterialPass> GetRequiredMaterialPasses(const FMaterialAsset& Asset)
{
    switch (Asset.Domain)
    {
    case EMaterialDomain::Surface:
        if (Asset.BlendMode == EMaterialBlendMode::Opaque || Asset.BlendMode == EMaterialBlendMode::Masked)
            return {EMaterialPass::Depth, EMaterialPass::Shadow, EMaterialPass::GBuffer};
        return {EMaterialPass::Forward};
    case EMaterialDomain::Decal:
        return {EMaterialPass::GBuffer};
    case EMaterialDomain::UI:
        return {EMaterialPass::UI};
    case EMaterialDomain::PostProcess:
        return {EMaterialPass::PostProcess};
    }
    return {};
}

FMaterialPipelineKey MakeCookedMaterialPipelineKey(
    const FResolvedMaterialInstance& Material, const FMaterialPassDefinition& Pass,
    const xr_string_view Backend)
{
    FMaterialPipelineKey Key;
    Key.MasterMaterial = Material.MasterId;
    Key.StaticParameters = Material.StaticParameters;
    Key.VertexFactory = Pass.VertexFactory;
    Key.RenderPassSignature = Pass.RenderPassSignature;
    Key.Backend = Backend;
    Key.ShaderModel = MaterialCookedShaderModel;
    Key.CompilerOptions = MaterialCookedCompilerOptions;
    return Key;
}

xr_string_view ToString(const EMaterialPass Pass) noexcept
{
    if (const FMaterialPassDefinition* Definition = FindMaterialPassDefinition(Pass))
        return Definition->Name;
    return "unknown";
}

xr_optional<EMaterialPass> ParseMaterialPass(const xr_string_view Value) noexcept
{
    for (const FMaterialPassDefinition& Definition : PassManifest)
        if (Definition.Name == Value) return Definition.Pass;
    return std::nullopt;
}
