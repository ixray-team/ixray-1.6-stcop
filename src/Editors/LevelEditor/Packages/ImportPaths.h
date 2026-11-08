#pragma once

#include "PackageStore.h"

namespace SdkPackages::ImportDetails
{
static bool IsPackageMetadata(const xr_string& Relative)
{
	if (Relative.find_first_of("/\\") != xr_string::npos)
	{
		return false;
	}
	const auto Name = Details::Lower(Relative);
	if (Name == "package.json" || Name == ".gitignore" || Name == ".gitattributes")
	{
		return true;
	}
	for (const char* Stem : {"readme", "changelog", "changes", "history", "license", "copying", "notice"})
	{
		if (Name == Stem || Name == xr_string(Stem) + ".md" || Name == xr_string(Stem) + ".txt" || Name == xr_string(Stem) + ".markdown")
		{
			return true;
		}
	}
	return false;
}

static xr_path Normalize(const xr_path& Path)
{
	xr_path Result = Path.lexically_normal();
	if (!Result.has_filename() && Result.has_relative_path())
	{
		Result = Result.parent_path();
	}
	return Result;
}

static bool IsSafe(const xr_path& Path, const xr_path& Root)
{
	std::error_code Code;
	const auto AbsoluteRoot = Normalize(std::filesystem::absolute(Root, Code));
	if (Code || !Details::SamePath(Normalize(std::filesystem::weakly_canonical(AbsoluteRoot, Code)), AbsoluteRoot) || Code)
	{
		return false;
	}
	const auto Absolute = Normalize(std::filesystem::absolute(Path, Code));
	return !Code && Details::SamePath(Normalize(std::filesystem::weakly_canonical(Absolute, Code)), Absolute) && !Code && Details::IsWithin(Absolute, AbsoluteRoot);
}

static bool IsProtected(const xr_path& Path, const PackageStore& Store)
{
	const auto Sdk = Store.GetStateRoot().parent_path();
	for (const char* Name : {"packages", "plugins", ".ixray-packages"})
	{
		const auto Root = Join(Sdk, Name);
		if (Details::SamePath(Path, Root) || Details::IsWithin(Path, Root))
		{
			return true;
		}
	}
	return false;
}

static xr_string Key(const xr_path& Path)
{
	const auto Normalized = Path.lexically_normal();
	if constexpr (xr_path::preferred_separator == '\\')
	{
		return Details::Lower(Utf8(Normalized));
	}
	return Utf8(Normalized);
}

} // namespace SdkPackages::ImportDetails
