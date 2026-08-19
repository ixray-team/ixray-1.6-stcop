#include "EditorViewportMaterialResolver.h"

#include <algorithm>
#include <fstream>
#include <iomanip>
#include <iterator>
#include <ranges>
#include <sstream>
#include <utility>

namespace Tiramisu::Editor
{
namespace
{

xr_string ReadText(const std::filesystem::path& Path)
{
	std::ifstream Input(Path, std::ios::binary);
	if (!Input)
	{
		return {};
	}
	const std::string Text{std::istreambuf_iterator<char>(Input), std::istreambuf_iterator<char>()};
	return ToXrString(Text);
}

void Append(xr_vector<FMaterialDiagnostic>& Destination, const xr_vector<FMaterialDiagnostic>& Source)
{
	Destination.insert(Destination.end(), Source.begin(), Source.end());
}

void AddError(xr_vector<FMaterialDiagnostic>& Diagnostics, xr_string Code, xr_string Message)
{
	Diagnostics.push_back({EMaterialDiagnosticSeverity::Error, std::move(Code), std::move(Message), {}, {}});
}

bool EndsWith(const std::filesystem::path& Path, const xr_string_view Suffix)
{
	return Path.filename().string().ends_with(Suffix);
}

xr_string SourceReference(const std::filesystem::path& Root, const std::filesystem::path& Path)
{
	std::error_code Error;
	const std::filesystem::path Relative =
		std::filesystem::relative(Path, Root, Error);
	return Error ? Path.generic_string() : Relative.generic_string();
}

xr_string MakeDynamicInstanceId(const u64 MaterialSlot)
{
	std::ostringstream Stream;
	Stream << "editor-legacy-" << std::hex << std::setfill('0') << std::setw(16) << MaterialSlot;
	return Stream.str();
}
} // namespace

bool FEditorViewportMaterialResolution::Succeeded() const noexcept
{
	return Master.Id.IsValid() && Resolved.MasterHandle.IsValid() &&
		   FlattenedInstance.Id.IsValid() &&
		   FlattenedInstance.Parent == Master.Id.Value;
}

bool TiramisuEditorViewportMaterialResolver::Load(
	const std::filesystem::path& MaterialRoot,
	xr_vector<FMaterialDiagnostic>* OutDiagnostics
)
{
	xr_vector<FMaterialDiagnostic> Diagnostics;
	Loaded = false;
	Root.clear();
	LegacyMapPath.clear();
	AssetPaths.clear();
	Library = {};
	LegacyMap = {};

	std::error_code Error;
	if (!std::filesystem::is_directory(MaterialRoot, Error) || Error)
	{
		AddError(Diagnostics, "editor.material_root_missing", "Material asset directory was not found: '" + ToXrString(MaterialRoot.string()) + "'.");
		if (OutDiagnostics)
		{
			*OutDiagnostics = std::move(Diagnostics);
		}
		return false;
	}

	Root = std::filesystem::weakly_canonical(MaterialRoot, Error);
	if (Error)
	{
		Root = MaterialRoot.lexically_normal();
	}
	LegacyMapPath = Root / "legacy-map.json";

	xr_vector<std::filesystem::path> Masters;
	xr_vector<std::filesystem::path> Instances;
	std::filesystem::recursive_directory_iterator Iterator(Root, std::filesystem::directory_options::skip_permission_denied, Error);
	const std::filesystem::recursive_directory_iterator End;
	while (!Error && Iterator != End)
	{
		if (Iterator->is_regular_file(Error) && !Error)
		{
			const std::filesystem::path Path = Iterator->path();
			if (EndsWith(Path, ".material-instance.json"))
			{
				Instances.push_back(Path);
			}
			else if (EndsWith(Path, ".material.json"))
			{
				Masters.push_back(Path);
			}
		}
		Iterator.increment(Error);
	}
	if (Error)
	{
		AddError(Diagnostics, "editor.material_scan_failed", "Failed to enumerate material assets under '" + ToXrString(Root.string()) + "': " + ToXrString(Error.message()));
	}
	std::ranges::sort(Masters);
	std::ranges::sort(Instances);

	for (const std::filesystem::path& Path : Masters)
	{
		const xr_string Json = ReadText(Path);
		if (Json.empty())
		{
			AddError(Diagnostics, "editor.material_read_failed", "Cannot read master material '" + ToXrString(Path.string()) + "'.");
			continue;
		}
		FMaterialAssetParseResult Parsed = ParseMaterialAssetJson(
			Json, SourceReference(Root, Path)
		);
		Append(Diagnostics, Parsed.Diagnostics);
		if (!Parsed.Succeeded())
		{
			continue;
		}
		const FMaterialAssetId Id = Parsed.Value.Id;
		FMaterialRegistrationResult Registered =
			Library.RegisterMaster(std::move(Parsed.Value));
		Append(Diagnostics, Registered.Diagnostics);
		if (Registered.Succeeded())
		{
			AssetPaths.insert_or_assign(Id.Value, Path);
		}
	}

	for (const std::filesystem::path& Path : Instances)
	{
		const xr_string Json = ReadText(Path);
		if (Json.empty())
		{
			AddError(Diagnostics, "editor.instance_read_failed", "Cannot read material instance '" + ToXrString(Path.string()) + "'.");
			continue;
		}
		FMaterialInstanceParseResult Parsed = ParseMaterialInstanceJson(
			Json, SourceReference(Root, Path)
		);
		Append(Diagnostics, Parsed.Diagnostics);
		if (!Parsed.Succeeded())
		{
			continue;
		}
		const FMaterialAssetId Id = Parsed.Value.Id;
		FMaterialRegistrationResult Registered =
			Library.RegisterInstance(std::move(Parsed.Value));
		Append(Diagnostics, Registered.Diagnostics);
		if (Registered.Succeeded())
		{
			AssetPaths.insert_or_assign(Id.Value, Path);
		}
	}

	const xr_string LegacyJson = ReadText(LegacyMapPath);
	if (LegacyJson.empty())
	{
		AddError(Diagnostics, "editor.legacy_map_read_failed", "Cannot read legacy material map '" + ToXrString(LegacyMapPath.string()) + "'.");
	}
	else
	{
		FLegacyMaterialMapParseResult Parsed =
			ParseLegacyMaterialMapJson(LegacyJson);
		Append(Diagnostics, Parsed.Diagnostics);
		if (Parsed.Succeeded())
		{
			LegacyMap = std::move(Parsed.Value);
		}
	}

	const FMaterialResolveResult Standard =
		Library.Resolve(LegacyMap.StandardMaterial);
	const FMaterialResolveResult ErrorMaterial =
		Library.Resolve(LegacyMap.ErrorMaterial);
	Loaded = !LegacyMap.StandardMaterial.empty() &&
			 !LegacyMap.ErrorMaterial.empty() && Standard.Succeeded() &&
			 ErrorMaterial.Succeeded();
	if (!Loaded)
	{
		AddError(Diagnostics, "editor.legacy_fallback_unavailable", "legacy-map.json standard/error materials are not resolvable.");
	}
	if (OutDiagnostics)
	{
		*OutDiagnostics = std::move(Diagnostics);
	}
	return Loaded;
}

FEditorViewportMaterialResolution TiramisuEditorViewportMaterialResolver::Resolve(
	const FEditorViewportLegacyMaterialSource& Source
) const
{
	FEditorViewportMaterialResolution Result;
	Result.TwoSided = Source.TwoSided;
	if (!Loaded)
	{
		AddError(Result.Diagnostics, "editor.material_resolver_unavailable", "Editor viewport material resolver is not loaded.");
		return Result;
	}

	FLegacyMaterialRequest Request;
	if (!Source.MaterialAsset.empty())
	{
		Request.ExplicitMaterial = Source.MaterialAsset;
	}
	Request.ShaderName = Source.ShaderName;
	Request.Textures = Source.Textures;
	Result.Legacy = ResolveLegacyMaterial(LegacyMap, Request);
	Append(Result.Diagnostics, Result.Legacy.Diagnostics);

	FMaterialResolveResult Resolved = Library.Resolve(Result.Legacy.Material);
	if (!Resolved.Succeeded() && Result.Legacy.Material != LegacyMap.ErrorMaterial)
	{
		Append(Result.Diagnostics, Resolved.Diagnostics);
		Result.Legacy.Resolution = ELegacyMaterialResolution::ErrorMaterial;
		Result.Legacy.Material = LegacyMap.ErrorMaterial;
		AddError(Result.Diagnostics, "editor.legacy_mapping_unresolvable", "Mapped legacy material could not be resolved; using error material.");
		Resolved = Library.Resolve(LegacyMap.ErrorMaterial);
	}
	Append(Result.Diagnostics, Resolved.Diagnostics);
	if (!Resolved.Succeeded())
	{
		return Result;
	}

	Result.Resolved = std::move(Resolved.Value);
	const FMaterialAsset* Master =
		Library.GetMaster(Result.Resolved.MasterHandle);
	if (!Master)
	{
		AddError(Result.Diagnostics, "editor.material_master_missing", "Resolved editor viewport material has no live master.");
		return Result;
	}
	Result.Master = *Master;
	Result.TwoSided |= Result.Master.TwoSided;

	// Явный asset уже содержит авторитетные flattened overrides. Legacy texture
	// metadata применяется только к compatibility fallback, иначе выбранный в
	// редакторе instance незаметно заменялся старым CSurface.
	const FMaterialParameterMap RuntimeOverrides =
		Result.Legacy.Resolution == ELegacyMaterialResolution::ExplicitMaterial
		? FMaterialParameterMap{}
		: MakeLegacyMaterialRuntimeOverrides(Result.Legacy);
	for (const auto& [ParameterId, Value] : RuntimeOverrides)
	{
		const FMaterialParameterDefinition* Definition =
			Result.Master.FindParameter(ParameterId);
		if (!Definition || Definition->IsStatic() ||
			!ValueMatchesParameterType(Value, Definition->Type))
		{
			continue;
		}
		Result.Resolved.Parameters[ParameterId] = Value;
	}

	Result.FlattenedInstance.Id.Value =
		MakeDynamicInstanceId(Source.MaterialSlot);
	Result.FlattenedInstance.Name = Source.SurfaceName.empty()
										? "Editor legacy material"
										: Source.SurfaceName;
	Result.FlattenedInstance.Parent = Result.Master.Id.Value;
	Result.FlattenedInstance.Overrides = Result.Resolved.Parameters;
	Result.FlattenedInstance.StaticOverrides = Result.Resolved.StaticParameters;

	Result.CacheKey = MakeLegacyMaterialInstanceCacheKey(Result.Legacy) +
					  (Result.TwoSided ? "two-sided" : "one-sided");
	if (Result.Legacy.Resolution != ELegacyMaterialResolution::ExplicitMaterial)
	{
		Result.AssetDependencies.push_back(LegacyMapPath);
	}
	if (const auto MasterPath = AssetPaths.find(Result.Master.Id.Value);
		MasterPath != AssetPaths.end())
	{
		Result.AssetDependencies.push_back(MasterPath->second);
	}
	for (const FMaterialAssetId& Parent : Result.Resolved.ParentChain)
	{
		if (const auto Path = AssetPaths.find(Parent.Value);
			Path != AssetPaths.end())
		{
			Result.AssetDependencies.push_back(Path->second);
		}
	}
	std::ranges::sort(Result.AssetDependencies);
	const auto Unique = std::ranges::unique(Result.AssetDependencies);
	Result.AssetDependencies.erase(Unique.begin(), Unique.end());
	return Result;
}
} // namespace Tiramisu::Editor
