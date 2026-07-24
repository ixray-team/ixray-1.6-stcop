#include "MaterialInstanceParentResolver.h"

#include <algorithm>
#include <fstream>
#include <iterator>
#include <ranges>
#include <string>
#include <string_view>
#include <unordered_map>

namespace Tiramisu::Editor
{
namespace
{

void AddError(FMaterialInstanceParentResolution& Result, xr_string Code, xr_string Message)
{
	Result.Diagnostics.push_back({EMaterialDiagnosticSeverity::Error, std::move(Code), std::move(Message), {}, {}});
}

void Append(xr_vector<FMaterialDiagnostic>& Destination, xr_vector<FMaterialDiagnostic> Source)
{
	Destination.insert(Destination.end(), std::make_move_iterator(Source.begin()), std::make_move_iterator(Source.end()));
}

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
} // namespace

bool FMaterialInstanceParentResolution::Succeeded() const noexcept
{
	return Master.Id.IsValid() && Parent.MasterHandle.IsValid() &&
		   Instance.MasterHandle.IsValid() &&
		   std::ranges::none_of(Diagnostics, [](const FMaterialDiagnostic& Diagnostic)
								{ return Diagnostic.Severity ==
										 EMaterialDiagnosticSeverity::Error; });
}

FMaterialInstanceParentResolution ResolveMaterialInstanceParent(
	const std::filesystem::path& MaterialRoot,
	const FMaterialInstanceAsset& CurrentInstance
)
{
	FMaterialInstanceParentResolution Result;
	if (CurrentInstance.Parent.empty())
	{
		AddError(Result, "editor.instance_parent_missing", "A material instance parent cannot be empty.");
		return Result;
	}

	std::error_code Error;
	if (!std::filesystem::is_directory(MaterialRoot, Error) || Error)
	{
		AddError(Result, "editor.material_root_missing", "Material asset directory was not found: '" + ToXrString(MaterialRoot.string()) + "'.");
		return Result;
	}

	xr_vector<std::filesystem::path> MasterPaths;
	xr_vector<std::filesystem::path> InstancePaths;
	std::filesystem::recursive_directory_iterator Iterator(MaterialRoot, std::filesystem::directory_options::skip_permission_denied, Error);
	const std::filesystem::recursive_directory_iterator End;
	while (!Error && Iterator != End)
	{
		if (Iterator->is_regular_file(Error) && !Error)
		{
			const std::filesystem::path Path = Iterator->path();
			if (EndsWith(Path, ".material-instance.json"))
			{
				InstancePaths.push_back(Path);
			}
			else if (EndsWith(Path, ".material.json"))
			{
				MasterPaths.push_back(Path);
			}
		}
		Iterator.increment(Error);
	}
	if (Error)
	{
		AddError(Result, "editor.material_scan_failed", "Failed to enumerate material assets under '" + ToXrString(MaterialRoot.string()) + "': " + ToXrString(Error.message()));
		return Result;
	}
	std::ranges::sort(MasterPaths);
	std::ranges::sort(InstancePaths);

	TiramisuMaterialLibrary Library;
	xr_hash_map<xr_string, std::filesystem::path> AssetPaths;
	for (const std::filesystem::path& Path : MasterPaths)
	{
		const xr_string Json = ReadText(Path);
		if (Json.empty())
		{
			AddError(Result, "editor.material_read_failed", "Cannot read master material '" + ToXrString(Path.string()) + "'.");
			continue;
		}
		FMaterialAssetParseResult Parsed = ParseMaterialAssetJson(
			Json, SourceReference(MaterialRoot, Path)
		);
		const bool ParsedSuccessfully = Parsed.Succeeded();
		Append(Result.Diagnostics, std::move(Parsed.Diagnostics));
		if (!ParsedSuccessfully)
		{
			continue;
		}
		AssetPaths.insert_or_assign(Parsed.Value.Id.Value, Path);
		FMaterialRegistrationResult Registered =
			Library.RegisterMaster(std::move(Parsed.Value));
		Append(Result.Diagnostics, std::move(Registered.Diagnostics));
	}

	for (const std::filesystem::path& Path : InstancePaths)
	{
		const xr_string Json = ReadText(Path);
		if (Json.empty())
		{
			AddError(Result, "editor.instance_read_failed", "Cannot read material instance '" + ToXrString(Path.string()) + "'.");
			continue;
		}
		FMaterialInstanceParseResult Parsed = ParseMaterialInstanceJson(
			Json, SourceReference(MaterialRoot, Path)
		);
		const bool ParsedSuccessfully = Parsed.Succeeded();
		Append(Result.Diagnostics, std::move(Parsed.Diagnostics));
		if (!ParsedSuccessfully || Parsed.Value.Id == CurrentInstance.Id)
		{
			continue;
		}
		AssetPaths.insert_or_assign(Parsed.Value.Id.Value, Path);
		FMaterialRegistrationResult Registered =
			Library.RegisterInstance(std::move(Parsed.Value));
		Append(Result.Diagnostics, std::move(Registered.Diagnostics));
	}

	FMaterialRegistrationResult CurrentRegistration =
		Library.RegisterInstance(CurrentInstance);
	const bool CurrentRegistered = CurrentRegistration.Succeeded();
	Append(Result.Diagnostics, std::move(CurrentRegistration.Diagnostics));
	if (!CurrentRegistered)
	{
		return Result;
	}

	FMaterialResolveResult Parent = Library.Resolve(CurrentInstance.Parent);
	const bool ParentResolved = Parent.Succeeded();
	Append(Result.Diagnostics, std::move(Parent.Diagnostics));
	if (!ParentResolved)
	{
		return Result;
	}

	FMaterialResolveResult Instance = Library.Resolve(CurrentInstance.Id.Value);
	const bool InstanceResolved = Instance.Succeeded();
	Append(Result.Diagnostics, std::move(Instance.Diagnostics));
	if (!InstanceResolved)
	{
		return Result;
	}

	const FMaterialAsset* Master =
		Library.GetMaster(Instance.Value.MasterHandle);
	if (!Master)
	{
		AddError(Result, "editor.instance_master_missing", "Resolved material instance does not have a live master asset.");
		return Result;
	}

	Result.Master = *Master;
	Result.Parent = std::move(Parent.Value);
	Result.Instance = std::move(Instance.Value);
	if (!CurrentInstance.SourcePath.empty())
	{
		Result.AssetDependencies.emplace_back(CurrentInstance.SourcePath.c_str());
	}
	if (const auto MasterPath = AssetPaths.find(Result.Master.Id.Value);
		MasterPath != AssetPaths.end())
	{
		Result.AssetDependencies.push_back(MasterPath->second);
	}
	for (const FMaterialAssetId& ParentId : Result.Instance.ParentChain)
	{
		if (const auto ParentPath = AssetPaths.find(ParentId.Value);
			ParentPath != AssetPaths.end())
		{
			Result.AssetDependencies.push_back(ParentPath->second);
		}
	}
	std::ranges::sort(Result.AssetDependencies);
	const auto Unique = std::ranges::unique(Result.AssetDependencies);
	Result.AssetDependencies.erase(Unique.begin(), Unique.end());
	return Result;
}
} // namespace Tiramisu::Editor
