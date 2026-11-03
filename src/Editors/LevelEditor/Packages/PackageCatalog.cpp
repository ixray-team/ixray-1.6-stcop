#include "stdafx.h"
#include "PackageCatalog.h"

namespace SdkPackages
{
PackageCatalog::PackageCatalog(const xr_path& SdkRoot, const xr_path& SettingsFile)
	: SettingsFile(SettingsFile)
{
	std::error_code Code;
	Root = std::filesystem::weakly_canonical(SdkRoot, Code);
	if (Code)
	{
		Root = std::filesystem::absolute(SdkRoot, Code).lexically_normal();
	}
}

bool PackageCatalog::Parse(const nlohmann::json& Json, const xr_path&, xr_vector<CatalogPackage>& Result, xr_string& Error)
{
	Error.clear();
	Result.clear();
	if (!Json.is_object() || Json.size() != 2 || !Json.contains("official") || !Json.contains("community"))
	{
		return Details::Fail(Error, "Use official/community arrays of repository links, without schemaVersion or package descriptions.");
	}
	xr_hash_set<xr_string> Seen;
	for (const auto Channel : {CatalogChannel::Official, CatalogChannel::Community})
	{
		const auto& Items = Json[Channel == CatalogChannel::Official ? "official" : "community"];
		if (!Items.is_array() || Items.size() > 128)
		{
			return Details::Fail(Error, "Catalog channels must contain at most 128 repository links.");
		}
		for (const auto& Item : Items)
		{
			CatalogPackage Entry;
			Entry.Channel = Channel;
			Entry.IsMetadataAvailable = false;
			xr_string Url;
			if (Item.is_string())
			{
				Url = Item.get_ref<const nlohmann::json::string_t&>().c_str();
				if (Url.size() != Item.get_ref<const nlohmann::json::string_t&>().size())
				{
					return Details::Fail(Error, "Invalid catalog repository URL.");
				}
			}
			else if (Item.is_object() && Item.contains("url") && Item.size() == (Item.contains("ref") ? 2 : 1) && Presentation::Text(Item, "url", Url, 512) && Presentation::Text(Item, "ref", Entry.Package.Ref, 200))
			{
			}
			else
			{
				return Details::Fail(Error, "A catalog entry must be a repository URL or an object with only url and optional ref.");
			}
			if (!PackageStore::ParseGitHubUrl(Url, Entry.Package.Url, Entry.Package.Id, Error) || !Details::IsRef(Entry.Package.Ref) ||
				!Seen.insert(Details::Lower(Entry.Package.Url) + "#" + Entry.Package.Ref).second)
			{
				return Details::Fail(Error, "Invalid or duplicate catalog repository/revision.");
			}
			Entry.Package.Source = PackageSource::GitHub;
			Entry.Package.Name = Entry.Package.Url.substr(xr_string("https://github.com/").size());
			Entry.Package.Name.resize(Entry.Package.Name.size() - 4);
			Entry.Package.Version = "unavailable";
			Entry.MetadataError = "Sync catalog to load this package's package.json.";
			Result.push_back(std::move(Entry));
		}
	}
	return Json.dump().size() <= 65536 || Details::Fail(Error, "Catalog index exceeds 64 KiB.");
}

xr_vector<PackageRow> PackageCatalog::Merge(const xr_vector<CatalogPackage>& Catalog, const xr_vector<PackageInfo>& Installed)
{
	xr_vector<PackageRow> Result;
	xr_hash_set<xr_string> Matched;
	for (const auto& Entry : Catalog)
	{
		PackageRow Row;
		Row.Package = Entry.Package;
		Row.Documents = Entry.Documents;
		if (Entry.Package.Source == PackageSource::Folder)
		{
			Row.BundledDocumentsPath = Entry.Package.LocalPath;
		}
		Row.Channel = Entry.Channel;
		Row.Key = Entry.Package.Source == PackageSource::GitHub ? "catalog:" + Details::Lower(Entry.Package.Url) + "#" + Entry.Package.Ref : "bundled:" + Entry.Package.Id;
		Row.Category = Entry.Category;
		Row.IsMetadataAvailable = Entry.IsMetadataAvailable;
		Row.MetadataError = Entry.MetadataError;
		Row.CatalogVersion = Entry.IsMetadataAvailable ? Entry.Package.Version : "";
		Row.CatalogCommit = Entry.IsMetadataAvailable ? Entry.Package.Commit : "";
		Row.HasCatalogEntry = true;
		const auto Existing = std::find_if(Installed.begin(), Installed.end(), [&Entry](const auto& Info)
										   { return Entry.IsMetadataAvailable ? Info.Id == Entry.Package.Id : Info.Source == PackageSource::GitHub && Details::Lower(Info.Url) == Details::Lower(Entry.Package.Url) && Info.Ref == Entry.Package.Ref; });
		if (Existing != Installed.end())
		{
			if (SameSource(*Existing, Entry.Package) || !Entry.IsMetadataAvailable)
			{
				Row.Package = *Existing;
				Row.IsInstalled = true;
				Row.IsMetadataAvailable = true;
				if (Entry.IsMetadataAvailable && Existing->Version == Entry.Package.Version)
				{
					auto& Card = Row.Package.Card;
					const auto& Published = Entry.Package.Card;
					if (Card.SourceUrl.empty())
					{
						Card.SourceUrl = Published.SourceUrl;
					}
					if (Card.License.empty())
					{
						Card.License = Published.License;
					}
					if (Card.WebsiteUrl.empty())
					{
						Card.WebsiteUrl = Published.WebsiteUrl;
					}
					if (Card.DocumentationUrl.empty())
					{
						Card.DocumentationUrl = Published.DocumentationUrl;
					}
					if (Card.ChangelogUrl.empty())
					{
						Card.ChangelogUrl = Published.ChangelogUrl;
					}
				}
				if (!Entry.IsMetadataAvailable || Existing->Version != Entry.Package.Version ||
					(Entry.Package.Source == PackageSource::GitHub && !Entry.Package.Commit.empty() && Details::Lower(Existing->Commit) != Details::Lower(Entry.Package.Commit)))
				{
					Row.Documents = {};
					Row.BundledDocumentsPath.clear();
				}
				Matched.insert(Existing->Id);
			}
			else
			{
				Row.HasSourceConflict = true;
			}
		}
		Result.push_back(std::move(Row));
	}
	for (const auto& Info : Installed)
	{
		if (!Matched.contains(Info.Id))
		{
			PackageRow Row;
			Row.Package = Info;
			Row.Key = "installed:" + Info.Id;
			Row.IsInstalled = true;
			Result.push_back(std::move(Row));
		}
	}
	return Result;
}

PackageUpdateState PackageCatalog::UpdateStatus(const PackageRow& Row, const xr_string& CheckedCommit)
{
	if (!Row.IsInstalled || Row.HasSourceConflict)
	{
		return PackageUpdateState::Unknown;
	}
	if (Row.Package.Source != PackageSource::GitHub)
	{
		return PackageUpdateState::Local;
	}
	const auto Latest = !CheckedCommit.empty() ? CheckedCommit : Row.MetadataError.empty() ? Row.CatalogCommit
																						   : xr_string{};
	if (!GitSource::IsCommit(Latest) || !GitSource::IsCommit(Row.Package.Commit))
	{
		return PackageUpdateState::Unknown;
	}
	return Details::Lower(Latest) == Details::Lower(Row.Package.Commit) ? PackageUpdateState::Current : PackageUpdateState::Available;
}

bool PackageCatalog::Matches(const PackageRow& Row, CatalogView View, const xr_string& Search)
{
	if ((View == CatalogView::My && !Row.IsInstalled) || (View == CatalogView::Official && Row.Channel != CatalogChannel::Official) || (View == CatalogView::Community && Row.Channel != CatalogChannel::Community))
	{
		return false;
	}
	const auto& Info = Row.Package;
	return Search.empty() || Details::Lower(Info.Name + " " + Info.Id + " " + Info.Description + " " + Info.Author + " " + Row.Category).find(Details::Lower(Search)) != xr_string::npos;
}

bool PackageCatalog::Load(xr_vector<CatalogPackage>& Result, xr_string& Error) const
{
	Error.clear();
	Result.clear();
	nlohmann::json Snapshot;
	xr_string CacheError;
	xr_vector<CatalogPackage> References;
	bool IsMissing = false;
	if (!CatalogSnapshot(Root).Read(Snapshot, CacheError, &IsMissing))
	{
		if (IsMissing)
		{
			return true;
		}
		Error = "Cannot read catalog cache: " + Utf8(Join(Join(Root, ".ixray-packages"), "catalog.json")) + ". " + CacheError + " Use Sync catalog to download it again; check file access if the error persists.";
		return false;
	}
	if (!Snapshot.is_object() || !Snapshot.contains("index") || !Snapshot.contains("manifests") || !Snapshot["manifests"].is_array() || Snapshot["manifests"].size() > 256 || !Parse(Snapshot["index"], Root, References, CacheError))
	{
		Error = "Catalog cache has an invalid or obsolete format. Use Sync catalog to rebuild it. " + CacheError;
		return false;
	}
	bool IsDamaged = false;
	for (auto& Reference : References)
	{
		const auto* Record = FindRecord(Snapshot["manifests"], Reference.Package);
		xr_string RecordError, Notice;
		const bool HasNotice = Record && Presentation::Text(*Record, "error", Notice, 2048) && !Notice.empty();
		auto Candidate = Reference;
		if (Record && ReadRecord(*Record, Candidate, RecordError))
		{
			Reference = std::move(Candidate);
		}
		else if (HasNotice)
		{
			Reference.MetadataError = Notice;
		}
		else
		{
			IsDamaged = true;
			Reference.MetadataError = "Cached package metadata is missing or invalid. Use Sync catalog to download it again.";
		}
		Result.push_back(std::move(Reference));
	}
	CheckIds(Result);
	return !IsDamaged || Details::Fail(Error, "Catalog cache contains missing or invalid package records. Use Sync catalog to rebuild it. Valid cards are still available.");
}

bool PackageCatalog::Refresh(const nlohmann::json& Index, const ManifestFetcher& Fetch, xr_string& Error, std::stop_token Stop, const ProgressCallback& Progress, const PackageThread::Launcher& Launch) const
{
	xr_vector<CatalogPackage> References;
	if (!Parse(Index, Root, References, Error))
	{
		return false;
	}
	nlohmann::json Previous;
	xr_string CacheError;
	if (!CatalogSnapshot(Root).Read(Previous, CacheError) || !Previous.is_object() || !Previous.contains("manifests") || !Previous["manifests"].is_array() || Previous["manifests"].size() > 256)
	{
		Previous = {{"manifests", nlohmann::json::array()}};
	}
	const auto& CachedRecords = Previous["manifests"];
	xr_vector<nlohmann::json> Records(References.size());
	xrCriticalSection StateLock;
	size_t Next = 0, Completed = 0, Active = 0, Failed = 0;
	std::stop_source WorkStop;
	std::stop_callback Cancel(Stop, [&]()
							  { WorkStop.request_stop(); });
	const auto Token = WorkStop.get_token();
	// StateLock also serializes notifications so older counters cannot overwrite newer ones.
	const auto PublishLambda = [&](const PackageProgress& Input)
	{
		auto Value = Input;
		Value.PackageCount = References.size();
		Value.PackagesCompleted = Completed;
		Value.ActivePackages = Active;
		if (Progress)
		{
			Progress(Value);
		}
	};
	const auto WorkLambda = [&]()
	{
		for (;;)
		{
			size_t Position;
			{
				xrCriticalSectionGuard Guard(StateLock);
				if (Token.stop_requested() || Next == References.size())
				{
					return;
				}
				Position = Next++;
				++Active;
				PublishLambda({"Checking package revision", References[Position].Package.Url, Position + 1});
			}
			const auto& Reference = References[Position];
			const ProgressCallback ReportLambda = [&](const PackageProgress& Input)
			{
				xrCriticalSectionGuard Guard(StateLock);
				auto Value = Input;
				Value.PackageIndex = Position + 1;
				PublishLambda(Value);
			};
			nlohmann::json Manifest;
			PackageDocuments Documents;
			xr_string Commit, FetchError;
			const auto* Cached = FindRecord(CachedRecords, Reference.Package);
			const bool HasFetched = !Token.stop_requested() && Fetch(Reference.Package, Manifest, Documents, Commit, FetchError, Cached, Token, ReportLambda);
			nlohmann::json Record = {{"url", Reference.Package.Url.c_str()}, {"ref", Reference.Package.Ref.c_str()}, {"commit", Commit.c_str()}, {"manifest", Manifest}, {"documents", Presentation::ToJson(Documents)}};
			auto Candidate = Reference;
			const bool IsValid = HasFetched && ReadRecord(Record, Candidate, FetchError);
			if (!IsValid)
			{
				if (Cached && ReadRecord(*Cached, Candidate, FetchError))
				{
					Record = *Cached;
					Record["error"] = "Package metadata could not be refreshed. Showing the last cached manifest.";
				}
				else
				{
					Record["manifest"] = nullptr;
					Record["error"] = "Cannot load a valid package.json from this repository. Sync again after checking the package and its revision.";
				}
			}
			{
				xrCriticalSectionGuard Guard(StateLock);
				--Active;
				if (Token.stop_requested())
				{
					return;
				}
				Records[Position] = std::move(Record);
				++Completed;
				Failed += !IsValid;
				PublishLambda({"Package metadata ready", Reference.Package.Url, Position + 1});
			}
		}
	};
	xr_array<PackageThread, MaxConcurrentPackages> Workers;
	const auto JoinWorkersLambda = [&]()
	{
		for (auto& Worker : Workers)
		{
			Worker.Join();
		}
	};

	const auto Count = std::min(MaxConcurrentPackages, References.size());
	const PackageThread::Callback JobLambda = [&](std::stop_token)
	{ WorkLambda(); };
	for (size_t Number = 0; Number < Count && !Token.stop_requested(); ++Number)
	{
		if (!Workers[Number].Start(JobLambda, "SDK catalog", Error, Launch))
		{
			WorkStop.request_stop();
			JoinWorkersLambda();
			return false;
		}
	}
	JoinWorkersLambda();


	if (Token.stop_requested())
	{
		return Details::Fail(Error, "Catalog synchronization cancelled.");
	}
	PublishLambda({"Saving catalog cache", {}, References.size()});
	if (Token.stop_requested())
	{
		return Details::Fail(Error, "Catalog synchronization cancelled.");
	}
	if (!CatalogSnapshot(Root).Write({{"index", Index}, {"manifests", Records}}, Error))
	{
		return false;
	}
	Error = Failed ? "Some package manifests are unavailable; cached cards are retained when possible." : "";
	return true;
}

bool PackageCatalog::RememberCheckout(const PackageInfo& Info, const xr_path& Checkout, xr_string& Error) const
{
	nlohmann::json Snapshot;
	xr_string ReadError;
	if (!CatalogSnapshot(Root).Read(Snapshot, ReadError) || !Snapshot.is_object() || !Snapshot.contains("manifests") || !Snapshot["manifests"].is_array())
	{
		return true;
	}
	for (auto& Record : Snapshot["manifests"])
	{
		xr_string Url, Ref;
		if (!Record.is_object() || !Presentation::Text(Record, "url", Url, 512) || !Presentation::Text(Record, "ref", Ref, 200) || Details::Lower(Url) != Details::Lower(Info.Url) || Ref != Info.Ref)
		{
			continue;
		}
		nlohmann::json Manifest;
		if (!Details::ReadJson(Join(Checkout, "package.json"), Manifest, Error))
		{
			return false;
		}
		nlohmann::json Updated = {{"url", Info.Url.c_str()}, {"ref", Info.Ref.c_str()}, {"commit", Info.Commit.c_str()}, {"manifest", Manifest}};
		CatalogPackage Candidate;
		Candidate.Package = Info;
		if (!ReadRecord(Updated, Candidate, Error) || Candidate.Package.Id != Info.Id || Candidate.Package.Kind != Info.Kind)
		{
			return Details::Fail(Error, "Installed manifest does not match its catalog source.");
		}
		xr_string Content;
		if (!PackageStore::ParseManifest(Manifest, Candidate.Package, Content, Error))
		{
			return false;
		}
		PackageDocuments Documents;
		Presentation::Discover(Checkout, Join(Checkout, Content.c_str()), Documents);
		Updated["documents"] = Presentation::ToJson(Documents);
		Record = std::move(Updated);
		return CatalogSnapshot(Root).Write(Snapshot, Error);
	}
	return true;
}

bool PackageCatalog::ReadSettings(CatalogSettings& Settings, xr_string& Error) const
{
	Settings = {};
	Error.clear();
	std::error_code Code;
	const bool DoesExist = !SettingsFile.empty() && std::filesystem::exists(SettingsFile, Code);
	if (Code)
	{
		return Details::Fail(Error, ("Cannot inspect Package Manager settings: " + Utf8(SettingsFile)).c_str());
	}
	if (DoesExist)
	{
		nlohmann::json Config;
		if (!Details::ReadJson(SettingsFile, Config, Error))
		{
			return Details::Fail(Error, ("Invalid Package Manager settings: " + Utf8(SettingsFile)).c_str());
		}
		for (const auto& [Key, Value] : Config.items())
		{
			if (Key != "repository" && Key != "ref" && Key != "file")
			{
				return Details::Fail(Error, ("Unknown Package Manager setting: " + xr_string(Key.c_str())).c_str());
			}
		}
		if ((Config.contains("repository") && !Presentation::Text(Config, "repository", Settings.Repository, 512)) ||
			(Config.contains("ref") && !Presentation::Text(Config, "ref", Settings.Ref, 200)) ||
			(Config.contains("file") && !Presentation::Text(Config, "file", Settings.File, 2048)))
		{
			return Details::Fail(Error, ("Invalid Package Manager settings: " + Utf8(SettingsFile)).c_str());
		}
	}
	xr_string Canonical, Id;
	if (!PackageStore::ParseGitHubUrl(Settings.Repository, Canonical, Id, Error) || !Details::IsRef(Settings.Ref) || !HttpSource::RelativeFile(Settings.File))
	{
		return Details::Fail(Error, "Package Manager settings require a GitHub repository, a valid ref and a relative catalog file.");
	}
	Settings.Repository = std::move(Canonical);
	return true;
}

bool PackageCatalog::Sync(xr_string& Error, std::stop_token Stop, const ProgressCallback& Progress) const
{
	CatalogSettings Settings;
	xr_string Commit, Bytes;
	if (!ReadSettings(Settings, Error))
	{
		return false;
	}
	if (Progress)
	{
		Progress({"Checking catalog revision", Settings.Repository});
	}
	if (!GitSource::ResolveRevision(Settings.Repository, Settings.Ref, Commit, Error, Stop))
	{
		return false;
	}
	if (Progress)
	{
		Progress({"Downloading catalog index", Settings.File});
	}
	if (!HttpSource::ReadRaw(Settings.Repository, Commit, Settings.File, Bytes, Error, Stop))
	{
		return false;
	}
	const auto Index = nlohmann::json::parse(Bytes.begin(), Bytes.end(), nullptr, false);
	return Refresh(Index, FetchManifest, Error, Stop, Progress);
}

bool PackageCatalog::FetchManifest(const PackageInfo& Source, nlohmann::json& Manifest, PackageDocuments& Documents, xr_string& Commit, xr_string& Error, const nlohmann::json* Cached, std::stop_token Stop, const ProgressCallback& Progress)
{
	if (!GitSource::ResolveRevision(Source.Url, Source.Ref, Commit, Error, Stop))
	{
		return false;
	}
	const auto ReadLambda = [&](const char* File, xr_string& Text, bool& IsMissing)
	{
		if (Progress)
		{
			Progress({"Downloading package metadata", Source.Url + " / " + File});
		}
		xr_string DocumentError;
		return HttpSource::ReadRaw(Source.Url, Commit, File, Text, DocumentError, Stop, &IsMissing);
	};
	return FetchRevision(Source, Commit, Cached, ReadLambda, Manifest, Documents, Error, Stop);
}

const nlohmann::json* PackageCatalog::FindRecord(const nlohmann::json& Records, const PackageInfo& Source)
{
	for (const auto& Record : Records)
	{
		xr_string Url, Ref;
		if (Record.is_object() && Presentation::Text(Record, "url", Url, 512) && Presentation::Text(Record, "ref", Ref, 200) && Details::Lower(Url) == Details::Lower(Source.Url) && Ref == Source.Ref)
		{
			return &Record;
		}
	}
	return nullptr;
}

bool PackageCatalog::ReadRecord(const nlohmann::json& Record, CatalogPackage& Entry, xr_string& Error)
{
	xr_string Commit, Content;
	if (!Record.is_object() || !Presentation::Text(Record, "commit", Commit, 64) || !GitSource::IsCommit(Commit) || !Record.contains("manifest") || !Record["manifest"].is_object() || Record["manifest"].dump().size() > 65536 ||
		!PackageStore::ParseManifest(Record["manifest"], Entry.Package, Content, Error) ||
		!Presentation::Text(Record["manifest"], "category", Entry.Category, 2048) || !Presentation::ParseDocuments(Record["manifest"], Entry.Documents, Error) ||
		!Presentation::Text(Record, "error", Entry.MetadataError, 2048))
	{
		return Details::Fail(Error, "Invalid cached or fetched SDK package manifest.");
	}
	if (Record.contains("documents") && !Presentation::ParseDocuments(Record["documents"], Entry.Documents, Error))
	{
		return false;
	}
	Entry.Package.Commit = Commit;
	Entry.IsMetadataAvailable = true;
	return true;
}

void PackageCatalog::CheckIds(xr_vector<CatalogPackage>& Entries)
{
	xr_hash_set<xr_string> Seen;
	for (auto& Entry : Entries)
	{
		if (Entry.IsMetadataAvailable && !Seen.insert(Entry.Package.Id).second)
		{
			Entry.IsMetadataAvailable = false;
			Entry.MetadataError = "Another package declares the same ID. Resolve the catalog conflict before installing this package.";
		}
	}
}

bool PackageCatalog::SameSource(const PackageInfo& First, const PackageInfo& Second)
{
	if (First.Source != Second.Source || First.Kind != Second.Kind)
	{
		return false;
	}
	return First.Source == PackageSource::GitHub ? Details::Lower(First.Url) == Details::Lower(Second.Url) && First.Ref == Second.Ref : Details::SamePath(FromUtf8(First.LocalPath), FromUtf8(Second.LocalPath));
}
} // namespace SdkPackages
