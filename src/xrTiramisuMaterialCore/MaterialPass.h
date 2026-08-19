#pragma once

#include "MaterialAsset.h"
#include "MaterialRuntime.h"

#include <functional>
#include <optional>
#include <span>
#include <string_view>
#include <vector>

constexpr u32 MaterialPassManifestVersion = 1;
constexpr u32 MaterialVertexFactoryManifestVersion = 1;
inline constexpr xr_string_view MaterialCookedShaderModel = "6.6";
inline constexpr xr_string_view MaterialCookedCompilerOptions =
	"-O3;-Ges;descriptor_heap_indexing;material_gpu_abi_v4;pass_manifest_v1;vertex_factory_manifest_v1";

enum class EMaterialShaderStage : u8
{
	Vertex,
	Pixel
};

// Shader contract конкретного render pass и его допустимых material modes.
struct FMaterialPassDefinition
{
	u32 Version = MaterialPassManifestVersion;
	EMaterialPass Pass = EMaterialPass::GBuffer;
	xr_string_view Name;
	xr_string_view ShaderSource;
	xr_string_view EntryPoint = "Main";
	xr_string_view TargetProfile = "ps_6_6";
	xr_string_view VertexFactory = "level_static";
	xr_string_view RenderPassSignature;
	EMaterialShaderStage Stage = EMaterialShaderStage::Pixel;
};

// Shader contract vertex factory, участвующий в permutation key.
struct FMaterialVertexFactoryDefinition
{
	u32 Version = MaterialVertexFactoryManifestVersion;
	xr_string_view Name;
	xr_string_view ShaderSource;
	xr_string_view EntryPoint = "Main";
	xr_string_view TargetProfile = "vs_6_6";
};

// Canonical pass/vertex-factory manifest используется runtime, cooker и tests.
[[nodiscard]] xr_span<const FMaterialPassDefinition> GetMaterialPassManifest() noexcept;
[[nodiscard]] const FMaterialPassDefinition* FindMaterialPassDefinition(EMaterialPass Pass) noexcept;
[[nodiscard]] xr_span<const FMaterialVertexFactoryDefinition>
GetMaterialVertexFactoryManifest() noexcept;
[[nodiscard]] const FMaterialVertexFactoryDefinition* FindMaterialVertexFactoryDefinition(
	xr_string_view Name
) noexcept;
[[nodiscard]] xr_vector<EMaterialPass> GetRequiredMaterialPasses(const FMaterialAsset& Asset);
[[nodiscard]] FMaterialPipelineKey MakeCookedMaterialPipelineKey(
	const FResolvedMaterialInstance& Material, const FMaterialPassDefinition& Pass, xr_string_view Backend
);
[[nodiscard]] xr_string_view ToString(EMaterialPass Pass) noexcept;
[[nodiscard]] xr_optional<EMaterialPass> ParseMaterialPass(xr_string_view Value) noexcept;
