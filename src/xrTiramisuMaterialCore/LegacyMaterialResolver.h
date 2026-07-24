#pragma once

#include "MaterialTypes.h"

#include <optional>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

inline constexpr xr_string_view LegacyBaseTextureParameterId =
	"2136fd1d-29bd-48e9-9f4c-9ebedb470774";
inline constexpr xr_string_view LegacyLightmapTextureParameterId =
	"7449f07b-e2f1-48cf-879d-d7e84ecb97b2";

// Таблица соответствий старых shader names новым master material assets.
struct FLegacyMaterialMap
{
	u32 Version = LegacyMaterialMapVersion;
	xr_string StandardMaterial;
	xr_string ErrorMaterial;
	xr_hash_map<xr_string, xr_string> ShaderMappings;
};

// Результат чтения legacy-map вместе с диагностикой schema и paths.
struct FLegacyMaterialMapParseResult
{
	FLegacyMaterialMap Value;
	xr_vector<FMaterialDiagnostic> Diagnostics;

	[[nodiscard]] bool Succeeded() const noexcept;
};

enum class ELegacyMaterialResolution : u8
{
	ExplicitMaterial,
	LegacyMap,
	AutomaticStandard,
	ErrorMaterial
};

// Нормализованный запрос на разрешение материала старого контента.
struct FLegacyMaterialRequest
{
	xr_optional<xr_string> ExplicitMaterial;
	xr_string ShaderName;
	xr_vector<xr_string> Textures;
};

// Результат legacy fallback chain с выбранным asset и причиной выбора.
struct FResolvedLegacyMaterial
{
	ELegacyMaterialResolution Resolution = ELegacyMaterialResolution::ErrorMaterial;
	xr_string Material;
	xr_string LegacyShaderName;
	xr_string BaseTexture;
	xr_vector<xr_string> Textures;
	xr_vector<FMaterialDiagnostic> Diagnostics;
};

// Нормализует legacy names и выполняет детерминированную fallback-цепочку.
[[nodiscard]] xr_string NormalizeLegacyShaderName(xr_string_view ShaderName);
[[nodiscard]] xr_string MakeLegacyMaterialInstanceCacheKey(const FResolvedLegacyMaterial& Material);
[[nodiscard]] FMaterialParameterMap MakeLegacyMaterialRuntimeOverrides(
	const FResolvedLegacyMaterial& Material
);
[[nodiscard]] FLegacyMaterialMapParseResult ParseLegacyMaterialMapJson(xr_string_view JsonText);
[[nodiscard]] xr_string SerializeLegacyMaterialMapJson(const FLegacyMaterialMap& Map);
[[nodiscard]] FResolvedLegacyMaterial ResolveLegacyMaterial(
	const FLegacyMaterialMap& Map, const FLegacyMaterialRequest& Request
);
