#pragma once

#include "MaterialRuntime.h"

#include <functional>
#include <span>
#include <string>
#include <string_view>
#include <vector>

constexpr u32 MaterialParameterLayoutVersion = 1;
constexpr u32 MaterialParameterBlockAlignment = 16;

// Размещение одного runtime-параметра внутри flattened GPU block.
struct FMaterialParameterLayoutField
{
	FMaterialParameterId Id;
	EMaterialParameterType Type = EMaterialParameterType::Scalar;
	u32 Offset = 0;
	u32 Size = 0;

	auto operator<=>(const FMaterialParameterLayoutField&) const = default;
};

// Детерминированный byte layout runtime-параметров master material.
struct FMaterialParameterLayout
{
	u32 Version = MaterialParameterLayoutVersion;
	u32 ByteSize = MaterialParameterBlockAlignment;
	u64 StableHash = 0;
	xr_vector<FMaterialParameterLayoutField> Fields;

	[[nodiscard]] const FMaterialParameterLayoutField* Find(const FMaterialParameterId& Id) const noexcept;
	auto operator<=>(const FMaterialParameterLayout&) const = default;
};

// Результат построения layout с диагностикой конфликтов GUID и типов.
struct FMaterialParameterLayoutResult
{
	FMaterialParameterLayout Value;
	xr_vector<FMaterialDiagnostic> Diagnostics;

	[[nodiscard]] bool Succeeded() const noexcept;
};

// Ссылка parameter block на bindless texture или sampler descriptor.
struct FMaterialParameterResourceReference
{
	FMaterialParameterId Parameter;
	EMaterialParameterType Type = EMaterialParameterType::Texture2D;
	u32 Offset = 0;
	xr_string AssetPath;

	auto operator<=>(const FMaterialParameterResourceReference&) const = default;
};

// Готовые CPU bytes и resource references одного material instance.
struct FMaterialPackedParameterBlock
{
	u64 LayoutHash = 0;
	xr_vector<u8> Data;
	xr_vector<FMaterialParameterResourceReference> Resources;
};

// Результат упаковки runtime overrides по layout master material.
struct FMaterialParameterPackResult
{
	FMaterialPackedParameterBlock Value;
	xr_vector<FMaterialDiagnostic> Diagnostics;

	[[nodiscard]] bool Succeeded() const noexcept;
};

using FMaterialResourceIndexResolver = std::function<xr_optional<FDescriptorHeapIndex>(
	EMaterialParameterType Type, xr_string_view AssetPath
)>;
using FMaterialResourceReferenceIndexResolver = std::function<xr_optional<FDescriptorHeapIndex>(
	const FMaterialParameterResourceReference& Reference
)>;

// Строит GPU layout, упаковывает values и патчит bindless resource indices.
[[nodiscard]] FMaterialParameterLayoutResult BuildMaterialParameterLayout(
	xr_span<const FMaterialParameterDefinition> Definitions
);
[[nodiscard]] FMaterialParameterPackResult PackMaterialParameters(const FMaterialParameterLayout& Layout, xr_span<const FMaterialParameterDefinition> Definitions, const FMaterialParameterMap& Values);
[[nodiscard]] FMaterialParameterPackResult PatchMaterialParameterResources(
	const FMaterialPackedParameterBlock& Source, const FMaterialResourceIndexResolver& Resolver
);
[[nodiscard]] FMaterialParameterPackResult PatchMaterialParameterResources(
	const FMaterialPackedParameterBlock& Source,
	const FMaterialResourceReferenceIndexResolver& Resolver
);

[[nodiscard]] xr_string MaterialParameterHlslFieldName(const FMaterialParameterId& Id);
[[nodiscard]] xr_string GenerateMaterialParameterHlsl(const FMaterialParameterLayout& Layout);
