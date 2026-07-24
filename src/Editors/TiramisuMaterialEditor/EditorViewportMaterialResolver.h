#pragma once

#include "TiramisuMaterialEditorTypes.h"

#include <LegacyMaterialResolver.h>
#include <MaterialAsset.h>

#include <cstdint>
#include <filesystem>
#include <string>
#include <unordered_map>
#include <vector>

namespace Tiramisu::Editor
{
// Renderer-neutral описание material slot, пришедшего из native или legacy editor scene.
struct FEditorViewportLegacyMaterialSource
{
	u64 MaterialSlot = 0;
	// Непустая ссылка MaterialAsset является авторитетной; legacy shader и texture
	// names остаются только для diagnostics и не запускают поиск в compatibility table.
	xr_string MaterialAsset;
	xr_string ShaderName;
	xr_vector<xr_string> Textures;
	xr_string SurfaceName;
	bool TwoSided = false;
};

// Полный flattened material payload для компиляции editor viewport pass.
struct FEditorViewportMaterialResolution
{
	FResolvedLegacyMaterial Legacy;
	FMaterialAsset Master;
	FResolvedMaterialInstance Resolved;
	FMaterialInstanceAsset FlattenedInstance;
	xr_string CacheKey;
	bool TwoSided = false;
	xr_vector<std::filesystem::path> AssetDependencies;
	xr_vector<FMaterialDiagnostic> Diagnostics;
	// Error material остаётся renderable. Success означает наличие полного payload,
	// а не отсутствие fallback diagnostics.
	[[nodiscard]] bool Succeeded() const noexcept;
};

// Независимая от NRI библиотека для viewport и тестов. Native assets используют
// явный MaterialInstance, legacy sources — compatibility table; оба пути flatten
// через один compiler/runtime contract.
class TiramisuEditorViewportMaterialResolver
{
public:
	// Загружает material library и legacy-map; Resolve после этого не меняет state.
	[[nodiscard]] bool Load(const std::filesystem::path& MaterialRoot, xr_vector<FMaterialDiagnostic>* OutDiagnostics = nullptr);
	[[nodiscard]] bool IsLoaded() const noexcept { return Loaded; }
	[[nodiscard]] FEditorViewportMaterialResolution Resolve(
		const FEditorViewportLegacyMaterialSource& Source
	) const;

private:
	TiramisuMaterialLibrary Library;
	FLegacyMaterialMap LegacyMap;
	std::filesystem::path Root;
	std::filesystem::path LegacyMapPath;
	xr_hash_map<xr_string, std::filesystem::path> AssetPaths;
	bool Loaded = false;
};
} // namespace Tiramisu::Editor
