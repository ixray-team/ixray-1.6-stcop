#include "stdafx.h"
#include "LocalSource.h"
#include "LocalArchive.h"

namespace SdkPackages::LocalSource
{
static bool FindPackageRoot(xr_path& Checkout, xr_string& Error)
{
	std::error_code Code;
	if (std::filesystem::is_regular_file(Join(Checkout, "package.json"), Code))
	{
		return true;
	}
	xr_path Wrapper;
	xr_dir_iter Iter(Checkout, Code), End;
	for (; !Code && Iter != End; Iter.increment(Code))
	{
		const auto Name = Iter->path().filename();
		if (Name == "__MACOSX" || Name == ".DS_Store")
		{
			continue;
		}
		if (!Wrapper.empty() || !Iter->is_directory(Code))
		{
			return Details::Fail(Error, "Archive must contain package.json at its root or inside one outer folder.");
		}
		Wrapper = Iter->path();
	}
	if (Code || Wrapper.empty() || !std::filesystem::is_regular_file(Join(Wrapper, "package.json"), Code) || Code)
	{
		return Details::Fail(Error, "Archive package manifest package.json was not found.");
	}
	Checkout = Wrapper;
	return true;
}

bool Prepare(PackageInfo& Info, const xr_path& Stage, xr_path& Checkout, xr_string& Error, std::stop_token Stop, const ProgressCallback& Progress)
{
	Error.clear();
	if (Stop.stop_requested())
	{
		return Details::Fail(Error, "Operation cancelled.");
	}
	if (!Details::NormalizeLocalSource(Info, Error))
	{
		return false;
	}
	if (Info.Source == PackageSource::Folder)
	{
		Checkout = FromUtf8(Info.LocalPath);
		std::error_code Code;
		return std::filesystem::is_regular_file(Join(Checkout, "package.json"), Code) || Details::Fail(Error, "Select a folder containing package.json.");
	}
	Checkout = Join(Stage, "extracted");
	const auto Path = FromUtf8(Info.LocalPath);
	if (Progress)
	{
		Progress({"Reading archive contents", Info.LocalPath});
	}
	const bool IsExtracted = ExtractArchive(Path, Checkout, Error, Stop, Progress);
	if (!IsExtracted)
	{
		return false;
	}
	return FindPackageRoot(Checkout, Error);
}
} // namespace SdkPackages::LocalSource
