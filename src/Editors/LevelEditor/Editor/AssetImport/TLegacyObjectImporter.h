#pragma once

#include <filesystem>
#include <string>
#include <vector>

namespace Tiramisu::Scene
{
struct FSceneConversionDiagnostic;
}

struct FLegacyObjectImportResult
{
	bool Succeeded = false;
	std::filesystem::path SourcePath;
	std::filesystem::path TargetPath;
	std::filesystem::path TargetPayloadPath;
	std::filesystem::path DumpPath;
	xr_string TargetAssetId;
	xr_vector<Tiramisu::Scene::FSceneConversionDiagnostic> Diagnostics;
};

// Converts an editor .object into a native Tiramisu static-mesh asset. Legacy
// files are import-only and are never overwritten. The function writes an
// audit dump for both success and failure.
[[nodiscard]] FLegacyObjectImportResult ImportLegacyObjectAsset(
	const std::filesystem::path& SourcePath,
	const std::filesystem::path& MaterialRoot,
	const std::filesystem::path& StaticMeshRoot);

[[nodiscard]] std::filesystem::path MakeImportedStaticMeshPath(
	const std::filesystem::path& SourcePath,
	const std::filesystem::path& StaticMeshRoot);
