#include "MaterialEditorAssetRouting.h"

#include <algorithm>
#include <cctype>

namespace Tiramisu::Editor
{
namespace
{
xr_string LowerFileName(const std::filesystem::path& Path)
{
	xr_string Result = Path.filename().string().c_str();
	std::ranges::transform(
		Result,
		Result.begin(),
		[](const unsigned char Character)
		{
			return static_cast<char>(std::tolower(Character));
		}
	);
	return Result;
}
} // namespace

EMaterialEditorAssetKind ClassifyMaterialEditorAsset(
	const std::filesystem::path& Path
)
{
	const xr_string FileName = LowerFileName(Path);
	if (FileName.ends_with(".material-instance.json"))
	{
		return EMaterialEditorAssetKind::MaterialInstance;
	}
	if (FileName.ends_with(".material.json"))
	{
		return EMaterialEditorAssetKind::MasterMaterial;
	}
	return EMaterialEditorAssetKind::Unsupported;
}

std::filesystem::path MakeMaterialInstancePath(
	const std::filesystem::path& MasterMaterialPath
)
{
	xr_string FileName = MasterMaterialPath.filename().string().c_str();
	const xr_string LowerName = LowerFileName(MasterMaterialPath);
	constexpr xr_string_view MasterSuffix = ".material.json";
	if (LowerName.ends_with(MasterSuffix))
	{
		FileName.resize(FileName.size() - MasterSuffix.size());
	}
	return MasterMaterialPath.parent_path() /
		(FileName + ".material-instance.json").c_str();
}

xr_vector<FMaterialEditorAssetEntry> CollectMaterialEditorAssets(
	const std::filesystem::path& MaterialRoot
)
{
	xr_vector<FMaterialEditorAssetEntry> Result;
	std::error_code Error;
	if (!std::filesystem::is_directory(MaterialRoot, Error) || Error)
	{
		return Result;
	}

	const auto Options =
		std::filesystem::directory_options::skip_permission_denied;
	std::filesystem::recursive_directory_iterator Iterator(
		MaterialRoot,
		Options,
		Error
	);
	const std::filesystem::recursive_directory_iterator End;
	while (Iterator != End)
	{
		if (Error)
		{
			Error.clear();
			Iterator.increment(Error);
			continue;
		}
		const std::filesystem::directory_entry& Entry = *Iterator;
		if (Entry.is_directory(Error) &&
			Entry.path().filename() == ".autosave")
		{
			Iterator.disable_recursion_pending();
		}
		else if (Entry.is_regular_file(Error))
		{
			const EMaterialEditorAssetKind Kind =
				ClassifyMaterialEditorAsset(Entry.path());
			if (Kind != EMaterialEditorAssetKind::Unsupported)
			{
				const std::filesystem::path Relative =
					std::filesystem::relative(
						Entry.path(), MaterialRoot, Error
					);
				if (!Error && !Relative.empty())
				{
					Result.push_back({
						Relative.lexically_normal(),
						Kind
					});
				}
			}
		}
		Error.clear();
		Iterator.increment(Error);
	}

	std::ranges::sort(
		Result,
		[](const FMaterialEditorAssetEntry& Left,
		   const FMaterialEditorAssetEntry& Right)
		{
			return Left.RelativePath.generic_string() <
				Right.RelativePath.generic_string();
		}
	);
	return Result;
}

xr_string MakeMaterialEditorPickerKey(
	const FMaterialEditorAssetEntry& Entry
)
{
	xr_string Relative = Entry.RelativePath.generic_string().c_str();
	std::ranges::replace(Relative, '/', '\\');
	const xr_string_view Prefix =
		Entry.Kind == EMaterialEditorAssetKind::MasterMaterial
		? "Materials\\"
		: Entry.Kind == EMaterialEditorAssetKind::MaterialInstance
		? "Instances\\"
		: "Unsupported\\";
	return xr_string(Prefix) + Relative;
}

xr_optional<FMaterialEditorAssetEntry> ParseMaterialEditorPickerKey(
	const xr_string_view PickerKey
)
{
	constexpr xr_string_view MaterialPrefix = "Materials\\";
	constexpr xr_string_view InstancePrefix = "Instances\\";
	EMaterialEditorAssetKind Kind = EMaterialEditorAssetKind::Unsupported;
	xr_string_view Relative;
	if (PickerKey.starts_with(MaterialPrefix))
	{
		Kind = EMaterialEditorAssetKind::MasterMaterial;
		Relative = PickerKey.substr(MaterialPrefix.size());
	}
	else if (PickerKey.starts_with(InstancePrefix))
	{
		Kind = EMaterialEditorAssetKind::MaterialInstance;
		Relative = PickerKey.substr(InstancePrefix.size());
	}
	if (Relative.empty())
	{
		return std::nullopt;
	}

	xr_string Normalized(Relative);
	std::ranges::replace(Normalized, '\\', '/');
	const std::filesystem::path Path(Normalized.c_str());
	if (ClassifyMaterialEditorAsset(Path) != Kind || Path.is_absolute())
	{
		return std::nullopt;
	}
	for (const std::filesystem::path& Part : Path)
	{
		if (Part == "..")
		{
			return std::nullopt;
		}
	}
	return FMaterialEditorAssetEntry{Path.lexically_normal(), Kind};
}
} // namespace Tiramisu::Editor
