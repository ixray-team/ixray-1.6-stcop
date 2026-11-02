#pragma once

#include "PackageStore.h"
#include <SDL3/SDL_filesystem.h>

namespace SdkPackages
{
class CatalogSnapshot final
{
public:
	static constexpr size_t Limit = 32 * 1024 * 1024;
	explicit CatalogSnapshot(const xr_path& Root)
		: Root(Root) {}
	~CatalogSnapshot() = default;

	bool Read(nlohmann::json& Json, xr_string& Error, bool* IsMissing = nullptr) const
	{
		Error.clear();
		if (IsMissing)
		{
			*IsMissing = false;
		}
		const auto File = Path();
		if (!Managed(File))
		{
			return Details::Fail(Error, "Catalog cache path is unsafe or inaccessible.");
		}
		std::error_code Code;
		const auto Status = std::filesystem::symlink_status(File, Code);
		if (Code == std::errc::no_such_file_or_directory || (!Code && Status.type() == std::filesystem::file_type::not_found))
		{
			if (IsMissing)
			{
				*IsMissing = true;
			}
			return Details::Fail(Error, "Catalog cache does not exist yet.");
		}
		if (Code || !std::filesystem::is_regular_file(Status))
		{
			return Details::Fail(Error, "No safe catalog snapshot is available.");
		}
		return Details::ReadJson(File, Json, Error, Limit);
	}

	bool Write(const nlohmann::json& Json, xr_string& Error) const
	{
		if (Json.dump().size() > Limit || !Managed(Path()))
		{
			return Details::Fail(Error, "Catalog snapshot exceeds its limit or its path is unsafe.");
		}
		PackageStore Store(Root);
		xr_path Stage;
		if (!Store.CreateStage(Stage, Error))
		{
			return false;
		}
		Details::ScopedDirectory Cleanup(Stage);
		const auto Pending = Join(Stage, "catalog.json");
		const auto Target = Path();
		if (!Details::WriteJson(Pending, Json, Error))
		{
			return false;
		}
		std::error_code Code;
		if (std::filesystem::file_size(Pending, Code) > Limit || Code)
		{
			return Details::Fail(Error, "Formatted catalog snapshot exceeds its limit.");
		}
		const auto Status = std::filesystem::symlink_status(Target, Code);
		const bool DoesExist = !Code && std::filesystem::exists(Status);
		if ((Code && Code != std::errc::no_such_file_or_directory) || (DoesExist && !std::filesystem::is_regular_file(Status)))
		{
			return Details::Fail(Error, "Catalog snapshot is not a regular file.");
		}
		for (u32 Attempt = 0; Attempt < 16; ++Attempt)
		{
			if (SDL_RenamePath(Utf8(Pending).c_str(), Utf8(Target).c_str()))
			{
				return true;
			}
			// Brief filesystem readers can temporarily prevent replacement on Windows.
			SDL_Delay(2);
		}
		Error = xr_string("Cannot replace catalog snapshot; previous data retained. ") + SDL_GetError();
		return false;
	}

private:
	xr_path Path() const { return Join(Join(Root, ".ixray-packages"), "catalog.json"); }
	bool Managed(const xr_path& File) const
	{
		std::error_code Code;
		const auto Resolved = std::filesystem::weakly_canonical(File, Code);
		if (Code)
		{
			return false;
		}
		const auto Absolute = std::filesystem::absolute(File, Code).lexically_normal();
		return !Code && Details::SamePath(Resolved, Absolute) && Details::IsWithin(File, Root);
	}
	xr_path Root;
};
} // namespace SdkPackages
