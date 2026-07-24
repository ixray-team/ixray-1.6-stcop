#pragma once

#include <filesystem>
#include <string>
#include <vector>

class EScene;

namespace Tiramisu::Scene
{
struct FSceneConversionDiagnostic;
}

struct FLegacyLevelImportResult
{
	bool Succeeded = false;
	std::filesystem::path SourcePath;
	std::filesystem::path TargetPath;
	std::filesystem::path DumpPath;
	xr_string TargetAssetId;
	xr_vector<Tiramisu::Scene::FSceneConversionDiagnostic> Diagnostics;
};

// Converts the already loaded editor scene into the native Tiramisu render
// scene. The legacy level and referenced .object files are import-only and
// remain untouched. A deterministic audit dump is mandatory.
[[nodiscard]] FLegacyLevelImportResult ImportLoadedLegacyLevelAsset(
	const std::filesystem::path& SourcePath,
	EScene& LegacyScene,
	const std::filesystem::path& MaterialRoot,
	const std::filesystem::path& StaticMeshRoot,
	const std::filesystem::path& RenderSceneRoot
);

// Publishes the mandatory failed-conversion dump when the legacy editor scene
// could not be loaded and ImportLoadedLegacyLevelAsset therefore cannot run.
[[nodiscard]] FLegacyLevelImportResult WriteLegacyLevelLoadFailureDump(
	const std::filesystem::path& SourcePath,
	const std::filesystem::path& RenderSceneRoot,
	xr_string DiagnosticCode,
	xr_string DiagnosticMessage
);

[[nodiscard]] std::filesystem::path MakeImportedRenderScenePath(
	const std::filesystem::path& SourcePath,
	const std::filesystem::path& RenderSceneRoot
);
