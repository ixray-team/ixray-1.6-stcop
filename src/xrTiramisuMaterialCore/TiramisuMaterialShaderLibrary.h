#pragma once

#include "MaterialBundle.h"

#include <compare>
#include <cstddef>
#include <cstdint>
#include <map>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <vector>

// Ключ shader library для pipeline key, pass и shader stage.
struct FMaterialShaderProgramKey
{
	FMaterialAssetId MaterialId;
	u64 PipelineKey = 0;
	EMaterialPass Pass = EMaterialPass::GBuffer;
	xr_string VertexFactory;
	xr_string RenderPassSignature;

	auto operator<=>(const FMaterialShaderProgramKey&) const = default;
};

// Невладеющее представление bytecode, принадлежащего material shader library.
struct FMaterialShaderProgramView
{
	const FMaterialShaderBlob* Vertex = nullptr;
	const FMaterialShaderBlob* Pixel = nullptr;

	[[nodiscard]] bool IsComplete() const noexcept { return Vertex != nullptr && Pixel != nullptr; }
};

// Правила проверки полноты и допустимых backend cooked library.
struct FMaterialShaderLibraryBuildOptions
{
	EMaterialShaderBlobFormat Format = EMaterialShaderBlobFormat::Dxil;
	bool RequireCompleteShaderSet = true;
};

struct FMaterialShaderLibraryBuildResult;

// Индекс готовых DXIL/SPIR-V programs из development или cooked bundle.
class TiramisuMaterialShaderLibrary
{
public:
	// Строит индекс programs из bundle и отклоняет неполные production libraries.
	[[nodiscard]] static FMaterialShaderLibraryBuildResult Build(
		FMaterialBundle Bundle, const FMaterialShaderLibraryBuildOptions& Options
	);
	[[nodiscard]] static FMaterialShaderLibraryBuildResult Deserialize(
		xr_span<const u8> Data, const FMaterialShaderLibraryBuildOptions& Options
	);

	[[nodiscard]] EMaterialShaderBlobFormat GetFormat() const noexcept { return Format; }
	[[nodiscard]] bool IsComplete() const noexcept { return Complete; }
	[[nodiscard]] size_t GetProgramCount() const noexcept { return Programs.size(); }
	[[nodiscard]] const FMaterialBundle& GetBundle() const noexcept { return Bundle; }

	// Разрешает master/flattened instance и возвращает bytecode без копирования.
	[[nodiscard]] FMaterialAssetId ResolveMasterMaterialId(const FMaterialAssetId& MaterialId) const;
	[[nodiscard]] const FMaterialAsset* ResolveMasterMaterial(
		const FMaterialAssetId& MaterialId
	) const noexcept;
	[[nodiscard]] const FResolvedMaterialInstance* ResolveMaterial(
		const FMaterialAssetId& MaterialId
	) const noexcept;
	[[nodiscard]] xr_optional<FMaterialShaderProgramView> Find(
		const FMaterialAssetId& MaterialId, EMaterialPass Pass, xr_string_view VertexFactory, xr_string_view RenderPassSignature
	) const;
	[[nodiscard]] xr_optional<FMaterialShaderProgramView> Find(
		const FMaterialAssetId& MaterialId, u64 PipelineKey, EMaterialPass Pass, xr_string_view VertexFactory, xr_string_view RenderPassSignature
	) const;

private:
	static constexpr size_t InvalidBlobIndex = static_cast<size_t>(-1);

	// Внутреннее сопоставление shader stages с индексами blob records.
	struct FShaderStageIndices
	{
		size_t Vertex = InvalidBlobIndex;
		size_t Pixel = InvalidBlobIndex;

		[[nodiscard]] bool IsComplete() const noexcept
		{
			return Vertex != InvalidBlobIndex && Pixel != InvalidBlobIndex;
		}
	};

	EMaterialShaderBlobFormat Format = EMaterialShaderBlobFormat::Dxil;
	bool Complete = false;
	FMaterialBundle Bundle;
	xr_map<FMaterialAssetId, FMaterialAssetId> MasterAliases;
	xr_map<FMaterialAssetId, FMaterialAsset> MasterMaterials;
	xr_map<FMaterialAssetId, FResolvedMaterialInstance> ResolvedMaterials;
	xr_map<FMaterialShaderProgramKey, FShaderStageIndices> Programs;
};

// Результат построения shader library с diagnostics и completeness status.
struct FMaterialShaderLibraryBuildResult
{
	xr_optional<TiramisuMaterialShaderLibrary> Value;
	xr_vector<FMaterialDiagnostic> Diagnostics;

	[[nodiscard]] bool Succeeded() const noexcept;
};
