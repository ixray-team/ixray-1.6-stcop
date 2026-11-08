#pragma once

#include "../../../xrCore/xrCore.h"
#include "PackagePresentation.h"
#include "PackageProgress.h"
#include "../../../xrCore/Platform/FileLock.h"
#include <nlohmann/json.hpp>
#include <array>
#include <fstream>
#include <iomanip>
#include <stop_token>

namespace SdkPackages
{
inline xr_path FromUtf8(const xr_string& Text)
{
	return std::filesystem::path(std::u8string(reinterpret_cast<const char8_t*>(Text.data()), Text.size()));
}

inline xr_string Utf8(const xr_path& Path)
{
	if (!Presentation::IsUtf8Path(Path))
	{
		return "<invalid Unicode path>";
	}
	const auto Text = Path.u8string();
	xr_string Result;
	Result.assign(reinterpret_cast<const char*>(Text.data()), Text.size());
	return Result;
}

inline xr_path Join(const xr_path& Parent, const xr_path& Child)
{
	xr_path Result(Parent);
	Result /= static_cast<const std::filesystem::path&>(Child);
	return Result;
}

inline xr_path Join(const xr_path& Parent, const xr_string& Child)
{
	return Join(Parent, FromUtf8(Child));
}
inline xr_path Join(const xr_path& Parent, const char* Child)
{
	return Join(Parent, FromUtf8(Child));
}

enum class PackageKind
{
	Plugin,
	Assets
};

enum class PackageSource
{
	GitHub,
	Folder,
	Archive
};

struct PackageInfo
{
	PackageCardMetadata Card;
	xr_string Id;
	xr_string Name;
	xr_string Version;
	xr_string Description;
	xr_string Author;
	xr_string Url;
	xr_string Ref;
	xr_string Commit;
	xr_string LocalPath;
	PackageSource Source = PackageSource::GitHub;
	PackageKind Kind = PackageKind::Plugin;
};

namespace Details
{
const char* SourceName(PackageSource Source);

bool ParseSource(const xr_string& Name, PackageSource& Source);

const char* KindName(PackageKind Kind);

bool ParseKind(const xr_string& Name, PackageKind& Kind);

bool Fail(xr_string& Error, const char* Message);

bool IsAlphaNumeric(char Value);

xr_string Lower(const xr_string& Value);

bool IsId(const xr_string& Id);

bool IsRef(const xr_string& Ref);

bool ReadString(const nlohmann::json& Json, const char* Key, xr_string& Value, bool IsRequired = false, size_t MaxSize = 2048);

bool ReadJson(const xr_path& Path, nlohmann::json& Json, xr_string& Error, size_t Limit = 64 * 1024);

bool WriteJson(const xr_path& Path, const nlohmann::json& Json, xr_string& Error);

bool IsWithin(const xr_path& Path, const xr_path& Parent);

bool SamePath(const xr_path& First, const xr_path& Second);

bool IsArchivePath(const xr_path& Path);

bool NormalizeLocalSource(PackageInfo& Info, xr_string& Error);

bool Files(const xr_path& Root, xr_vector<xr_string>& Result, xr_string& Error, bool IgnoreRepository = false, std::stop_token Stop = {});

bool CopyBytes(std::istream& Input, std::ostream& Output, xr_string& Error, std::stop_token Stop, const ProgressCallback& Progress = {});

bool Copy(const xr_path& Source, const xr_path& Target, xr_string& Error, bool IgnoreRepository = false, std::stop_token Stop = {}, const ProgressCallback& Progress = {});

bool Equal(const xr_path& First, const xr_path& Second, xr_string& Error, std::stop_token Stop = {});

class ScopedDirectory final
{
public:
	explicit ScopedDirectory(const xr_path& Path)
		: Path(Path) {}
	ScopedDirectory(const ScopedDirectory&) = delete;
	ScopedDirectory& operator=(const ScopedDirectory&) = delete;
	~ScopedDirectory()
	{
		if (!Keep)
		{
			std::error_code Code;
			std::filesystem::remove_all(Path, Code);
		}
	}
	xr_path Path;
	bool Keep = false;
};
} // namespace Details

class PackageStore final
{
public:
	explicit PackageStore(const xr_path& SdkRoot);
	~PackageStore() = default;

	static bool ParseGitHubUrl(const xr_string& Input, xr_string& Url, xr_string& Id, xr_string& Error);

	static bool ParseManifest(const nlohmann::json& Json, PackageInfo& Info, xr_string& ContentName, xr_string& Error);

	bool InstallCheckout(const xr_path& Checkout, const PackageInfo& Source, bool IsUpdate, xr_string& Error, PackageInfo* InstalledInfo = nullptr, bool RequireKindMatch = false, std::stop_token Stop = {}, const ProgressCallback& Progress = {}) const;

	bool Remove(const xr_string& Id, xr_string& Error, std::stop_token Stop = {}) const;

	bool Recover(xr_string& Error) const;

	xr_vector<PackageInfo> List(xr_string& Error) const;

	bool ReadDocuments(const xr_string& Id, PackageDocuments& Documents, xr_string& Error) const;

	bool CreateStage(xr_path& Stage, xr_string& Error) const;

	xr_path GetStateRoot() const { return Join(Root, ".ixray-packages"); }
	xr_path GetDestination(const PackageInfo& Info) const;

private:
	static bool RetireTransaction(const xr_path& Stage);

	bool RecoverTransaction(const xr_path& Stage, xr_string& Error) const;

	bool Managed(const xr_path& Path, xr_string& Error) const;

	static nlohmann::json ToJson(const PackageInfo& Info);

	bool ReadRecord(const xr_string& Id, PackageInfo& Info, xr_string& Error) const;

	static bool Manifest(const xr_path& Checkout, PackageInfo& Info, xr_path& Content, xr_string& Error, std::stop_token Stop = {});

	xr_path Root;
};
} // namespace SdkPackages
