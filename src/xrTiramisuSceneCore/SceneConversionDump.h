#pragma once

#include "TiramisuSceneTypes.h"

#include <cstdint>
#include <string>
#include <string_view>
#include <vector>

namespace Tiramisu::Scene
{
inline constexpr u32 LegacySceneConversionDumpVersion = 1;
inline constexpr u32 SceneConversionDumpVersion = 2;

enum class ESceneConversionStatus : u8
{
	Succeeded,
	Failed
};

// Запись dump о преобразовании legacy surface в MaterialInstance.
struct FSceneConversionMaterialMapping
{
	xr_string Surface;
	xr_string SourceKey;
	xr_string MaterialInstance;
	bool TwoSided = false;
	bool Created = false;
};

// Диагностика одного шага legacy scene conversion.
struct FSceneConversionDiagnostic
{
	xr_string Severity;
	xr_string Code;
	xr_string Message;
};

// Запись соответствия legacy source и созданного native asset.
struct FSceneConversionAssetMapping
{
	xr_string Source;
	xr_string Target;
	xr_string Dump;
	xr_string AssetId;
	xr_string TargetPayload;
};

// Audit sidecar for every legacy import. It intentionally contains no wall
// clock timestamp: the same input and converter produce a deterministic dump
// suitable for diffs and automated migration checks.
struct FSceneConversionDump
{
	u32 Version = SceneConversionDumpVersion;
	ESceneConversionStatus Status = ESceneConversionStatus::Failed;
	xr_string Importer;
	u32 ImporterVersion = 1;
	xr_string SourceType;
	xr_string SourcePath;
	xr_string SourceHash;
	xr_string TargetPath;
	xr_string TargetPayloadPath;
	xr_string TargetAssetId;
	u32 MeshCount = 0;
	u32 VertexCount = 0;
	u32 IndexCount = 0;
	u32 ComponentCount = 0;
	u32 CreatedMaterialInstances = 0;
	u32 ReusedMaterialInstances = 0;
	xr_vector<FSceneConversionAssetMapping> AssetMappings;
	xr_vector<FSceneConversionMaterialMapping> MaterialMappings;
	xr_vector<FSceneConversionDiagnostic> Diagnostics;
};

// Результат чтения versioned migration dump.
struct FSceneConversionDumpParseResult
{
	FSceneConversionDump Value;
	xr_string Diagnostic;

	[[nodiscard]] bool Succeeded() const noexcept
	{
		return Diagnostic.empty();
	}
};

// Сериализует обязательный audit dump успешной или неуспешной legacy conversion.
[[nodiscard]] xr_string SerializeSceneConversionDumpJson(
	const FSceneConversionDump& Dump
);
[[nodiscard]] FSceneConversionDumpParseResult ParseSceneConversionDumpJson(
	xr_string_view JsonText
);
[[nodiscard]] xr_string_view ToString(
	ESceneConversionStatus Status
) noexcept;
} // namespace Tiramisu::Scene
