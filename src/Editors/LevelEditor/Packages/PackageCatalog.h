#pragma once

#include "RemoteDocuments.h"
#include "CatalogSnapshot.h"
#include "PackageThread.h"

namespace SdkPackages
{
enum class CatalogChannel
{
	Official,
	Community,
	Other
};
enum class CatalogView
{
	All,
	My,
	Official,
	Community
};

enum class PackageUpdateState
{
	Unknown,
	Current,
	Available,
	Local
};

struct CatalogPackage
{
	PackageInfo Package;
	PackageDocuments Documents;
	CatalogChannel Channel = CatalogChannel::Official;
	xr_string Category;
	xr_string MetadataError;
	bool IsMetadataAvailable = true;
};

struct PackageRow
{
	PackageInfo Package;
	PackageDocuments Documents;
	xr_string BundledDocumentsPath;
	CatalogChannel Channel = CatalogChannel::Other;
	xr_string Key;
	xr_string Category;
	xr_string CatalogVersion;
	xr_string CatalogCommit;
	xr_string MetadataError;
	bool IsMetadataAvailable = true;
	bool IsInstalled = false;
	bool HasCatalogEntry = false;
	bool HasSourceConflict = false;
};

struct CatalogSettings
{
	xr_string Repository = "https://github.com/kola24511/ixray-packages";
	xr_string Ref;
	xr_string File = "sdk-packages.json";
};

class PackageCatalog final
{
public:
	explicit PackageCatalog(const xr_path& SdkRoot, const xr_path& SettingsFile = {});
	~PackageCatalog() = default;

	static bool Parse(const nlohmann::json& Json, const xr_path&, xr_vector<CatalogPackage>& Result, xr_string& Error);

	static xr_vector<PackageRow> Merge(const xr_vector<CatalogPackage>& Catalog, const xr_vector<PackageInfo>& Installed);

	static PackageUpdateState UpdateStatus(const PackageRow& Row, const xr_string& CheckedCommit = {});

	static bool Matches(const PackageRow& Row, CatalogView View, const xr_string& Search);

	bool Load(xr_vector<CatalogPackage>& Result, xr_string& Error) const;

	// Fetch may run concurrently; progress callbacks are serialized by Refresh.
	using ManifestFetcher = std::function<bool(const PackageInfo&, nlohmann::json&, PackageDocuments&, xr_string&, xr_string&, const nlohmann::json*, std::stop_token, const ProgressCallback&)>;
	static constexpr size_t MaxConcurrentPackages = 4;
	bool Refresh(const nlohmann::json& Index, const ManifestFetcher& Fetch, xr_string& Error, std::stop_token Stop = {}, const ProgressCallback& Progress = {}, const PackageThread::Launcher& Launch = {}) const;

	bool RememberCheckout(const PackageInfo& Info, const xr_path& Checkout, xr_string& Error) const;

	template <typename TReader>
	static bool FetchRevision(const PackageInfo& Source, const xr_string& Commit, const nlohmann::json* Cached, TReader&& Read, nlohmann::json& Manifest, PackageDocuments& Documents, xr_string& Error, std::stop_token Stop = {})
	{
		Error.clear();
		if (Stop.stop_requested())
		{
			return Details::Fail(Error, "Catalog synchronization cancelled.");
		}
		if (!GitSource::IsCommit(Commit))
		{
			return Details::Fail(Error, "Catalog metadata requires an exact package revision.");
		}
		xr_string CachedUrl, CachedRef;
		CatalogPackage Previous;
		Previous.Package = Source;
		xr_string CacheError;
		if (Cached && Presentation::Text(*Cached, "url", CachedUrl, 512) && Presentation::Text(*Cached, "ref", CachedRef, 200) &&
			Details::Lower(CachedUrl) == Details::Lower(Source.Url) && CachedRef == Source.Ref && Cached->contains("documents") &&
			ReadRecord(*Cached, Previous, CacheError) && Details::Lower(Previous.Package.Commit) == Details::Lower(Commit) && Previous.Documents.Notice.empty())
		{
			Manifest = (*Cached)["manifest"];
			Documents = std::move(Previous.Documents);
			return true;
		}
		xr_string Bytes;
		bool IsMissing = false;
		if (!Read("package.json", Bytes, IsMissing))
		{
			return Details::Fail(Error, "Cannot download the package manifest.");
		}
		if (Stop.stop_requested())
		{
			return Details::Fail(Error, "Catalog synchronization cancelled.");
		}
		Manifest = nlohmann::json::parse(Bytes.begin(), Bytes.end(), nullptr, false);
		auto Candidate = Source;
		xr_string Content;
		if (!PackageStore::ParseManifest(Manifest, Candidate, Content, Error) || !Presentation::ParseDocuments(Manifest, Documents, Error))
		{
			return false;
		}
		return RemoteDocuments::Fetch(Documents, Read, Stop, Cached, Commit) || Details::Fail(Error, "Catalog synchronization cancelled.");
	}

	bool ReadSettings(CatalogSettings& Settings, xr_string& Error) const;

	bool Sync(xr_string& Error, std::stop_token Stop = {}, const ProgressCallback& Progress = {}) const;

private:
	static bool FetchManifest(const PackageInfo& Source, nlohmann::json& Manifest, PackageDocuments& Documents, xr_string& Commit, xr_string& Error, const nlohmann::json* Cached, std::stop_token Stop, const ProgressCallback& Progress);

	static const nlohmann::json* FindRecord(const nlohmann::json& Records, const PackageInfo& Source);

	static bool ReadRecord(const nlohmann::json& Record, CatalogPackage& Entry, xr_string& Error);

	static void CheckIds(xr_vector<CatalogPackage>& Entries);

	static bool SameSource(const PackageInfo& First, const PackageInfo& Second);
	xr_path Root;
	xr_path SettingsFile;
};
} // namespace SdkPackages
