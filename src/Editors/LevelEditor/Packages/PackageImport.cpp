#include "stdafx.h"
#include "PackageImport.h"
#include "ImportTransaction.h"

namespace SdkPackages
{
const char* ImportOutcomeName(ImportOutcome Outcome)
{
	switch (Outcome)
	{
		case ImportOutcome::Added: return "Added";
		case ImportOutcome::Replaced: return "Replaced";
		case ImportOutcome::Identical: return "Identical";
		case ImportOutcome::Kept: return "Kept";
		case ImportOutcome::Skipped: return "Skipped";
		case ImportOutcome::RolledBack: return "Rolled back";
		case ImportOutcome::Failed: return "Failed";
	}
	return "Unknown";
}

xr_vector<ImportRoot> PackageImporter::CaptureRoots()
{
	xr_vector<ImportRoot> Roots = {
		{"rawdata", "$server_data_root$"},
		{"rawdata/objects", "$objects$"},
		{"rawdata/levels", "$maps$"},
		{"rawdata/groups", "$groups$"},
		{"rawdata/textures", "$textures$"},
		{"rawdata/sounds", "$sounds$"},
		{"rawdata/particles", "$game_particles$"},
		{"gamedata", "$game_data$"},
		{"gamedata/meshes", "$game_meshes$"},
		{"gamedata/anims", "$game_anims$"},
		{"gamedata/levels", "$game_levels$"},
		{"gamedata/textures", "$game_textures$"},
		{"gamedata/sounds", "$game_sounds$"},
		{"gamedata/scripts", "$game_scripts$"},
		{"gamedata/configs", "$game_config$"},
		{"gamedata/shaders", "$game_shaders$"},
		{"gamedata/spawns", "$game_spawn$"},
		{"gamedata/ai", "$game_ai$"},
		{"gamedata/fonts", "$game_fonts$"},
		{"import", "$import$"}
	};
	// An empty prefix captures active level metadata without mapping package imports to it.
	Roots.push_back({"", "$level$"});
	for (auto& Root : Roots)
	{
		if (FS.path_exist(Root.Alias.c_str()))
		{
			string_path Path;
			FS.update_path(Path, Root.Alias.c_str(), "");
			std::error_code Code;
			const auto Absolute = std::filesystem::absolute(Path, Code);
			if (!Code)
			{
				Root.Destination = ImportDetails::Normalize(std::filesystem::weakly_canonical(ImportDetails::Normalize(Absolute), Code));
			}
			if (Code)
			{
				Root.Destination.clear();
			}
		}
	}
	return Roots;
}

void PackageImporter::RefreshFilesystem(const xr_vector<xr_path>& Imported)
{
	const auto Roots = CaptureRoots();
	xr_hash_set<xr_string> Aliases;
	for (const auto& File : Imported)
	{
		const ImportRoot* Match = nullptr;
		for (const auto& Root : Roots)
		{
			if (!Root.Destination.empty() && Details::IsWithin(File, Root.Destination) && (!Match || Root.Destination.native().size() > Match->Destination.native().size()))
			{
				Match = &Root;
			}
		}
		if (Match)
		{
			Aliases.insert(Match->Alias);
		}
	}
	for (const auto& Alias : Aliases)
	{
		xr_path Directory = FromUtf8(FS.get_path(Alias.c_str())->m_Path);
		Directory.make_preferred();
		auto Path = Details::Lower(Utf8(Directory));
		if (!Path.empty())
		{
			if (Path.back() != Platform::kPreferredSeparator[0])
			{
				Path += Platform::kPreferredSeparator;
			}
			FS.rescan_path(Path.c_str(), true, false);
		}
	}
}

bool PackageImporter::Prepare(const PackageInfo& Info, const xr_vector<ImportRoot>& Roots, ImportPlan& Plan, xr_string& Error, std::stop_token Stop, const ProgressCallback& Progress) const
{
	Error.clear();
	const auto LockPath = Join(Store.GetStateRoot(), "transaction.lock");
	if (!ImportDetails::IsSafe(LockPath, Store.GetStateRoot()))
	{
		return Details::Fail(Error, "Package storage path is unsafe or inaccessible.");
	}
	Platform::FileLock Lock(LockPath);
	if (!Lock.IsLocked())
	{
		return Details::Fail(Error, "SDK package storage is busy. Retry after the other operation finishes.");
	}
	return BuildPlan(Info, Roots, Plan, Error, Stop, Progress);
}

namespace ImportDetails
{
static bool ScanFile(const xr_path& Source, const PackageStore& Store, const xr_vector<ImportRoot>& Roots, ImportFile& File, xr_string& Error, std::stop_token Stop)
{
	auto& Relative = File.Relative;
	std::replace(Relative.begin(), Relative.end(), '\\', '/');
	const auto Lower = Details::Lower(Relative);
	const ImportRoot* Match = nullptr;
	for (const auto& Root : Roots)
	{
		if (Lower.starts_with(Root.Prefix + "/") && (!Match || Root.Prefix.size() > Match->Prefix.size()))
		{
			Match = &Root;
		}
	}
	std::error_code Code;
	bool HasFingerprints = false;
	if (!Match)
	{
		File.Notice = "No destination: use rawdata/, gamedata/ or import/ in the package.";
	}
	else
	{
		File.Alias = Match->Alias;
		if (Match->Destination.empty())
		{
			File.Notice = "The destination alias is missing or inaccessible in fs.ltx.";
		}
		else
		{
			File.Destination = Join(Match->Destination, Relative.substr(Match->Prefix.size() + 1)).lexically_normal();
			if (!ImportDetails::IsSafe(File.Destination, Match->Destination) || ImportDetails::IsProtected(File.Destination, Store))
			{
				File.Notice = "The destination is unsafe or belongs to managed package storage.";
			}
			else
			{
				const auto Status = std::filesystem::symlink_status(File.Destination, Code);
				if (Code && Code != std::errc::no_such_file_or_directory)
				{
					File.Notice = "Cannot inspect the destination.";
				}
				else if (!Code && std::filesystem::exists(Status))
				{
					if (!std::filesystem::is_regular_file(Status))
					{
						File.Notice = "The destination is not a regular file; kept unchanged.";
					}
					else
					{
						xr_string InspectionError;
						const auto Result = ImportDetails::ReadComparedFingerprints(Join(Source, Relative), File.Destination, File.SourceStamp, File.DestinationStamp, InspectionError, Stop);
						if (Result == ImportDetails::ComparisonResult::SourceError)
						{
							Error = InspectionError;
							return false;
						}
						if (Result == ImportDetails::ComparisonResult::DestinationError)
						{
							File.Notice = InspectionError;
						}
						else
						{
							HasFingerprints = true;
							File.Status = Result == ImportDetails::ComparisonResult::Equal ? ImportFile::State::Identical : ImportFile::State::Changed;
						}
					}
				}
				else
				{
					File.Status = ImportFile::State::Missing;
					File.IsSelected = true;
				}
			}
		}
	}
	if (!HasFingerprints && !ImportDetails::ReadFingerprint(Join(Source, Relative), File.SourceStamp, Error, Stop))
	{
		return false;
	}
	File.Size = File.SourceStamp.Size;
	return true;
}
} // namespace ImportDetails

bool PackageImporter::BuildPlan(const PackageInfo& Info, const xr_vector<ImportRoot>& Roots, ImportPlan& Plan, xr_string& Error, std::stop_token Stop, const ProgressCallback& Progress) const
{
	Plan = {};
	const auto Installed = Store.List(Error);
	if (!Error.empty())
	{
		return false;
	}
	const auto Package = std::find_if(Installed.begin(), Installed.end(), [&](const auto& Candidate)
									  { return Candidate.Id == Info.Id; });
	if (Package == Installed.end() || Package->Kind != PackageKind::Assets || Info.Kind != PackageKind::Assets || Package->Commit != Info.Commit || Package->Version != Info.Version || Package->Source != Info.Source || Package->Url != Info.Url || Package->Ref != Info.Ref || Package->LocalPath != Info.LocalPath)
	{
		return Details::Fail(Error, "The downloaded asset package changed or is no longer available. Reopen Import.");
	}
	const auto Source = Store.GetDestination(*Package);
	if (!ImportDetails::IsSafe(Join(Source, "package-import-check"), Source))
	{
		return Details::Fail(Error, "The downloaded asset directory is unsafe or inaccessible.");
	}
	xr_vector<xr_string> Files;
	if (!Details::Files(Source, Files, Error, false, Stop))
	{
		return false;
	}
	if (Files.size() > 100000)
	{
		return Details::Fail(Error, "The asset package exceeds the 100000 file import limit.");
	}
	Files.erase(std::remove_if(Files.begin(), Files.end(), ImportDetails::IsPackageMetadata), Files.end());
	Plan.Package = *Package;
	Plan.Roots = Roots;
	PackageProgress Scan{"Scanning asset files"};
	Scan.FilesTotal = Files.size();
	if (Progress)
	{
		Progress(Scan);
	}
	Plan.Files.resize(Files.size());
	xr_atomic_u32 Next{0};
	xr_atomic_bool HasFailed{false};
	xrCriticalSection StateLock;
	size_t FirstError = Files.size();
	const auto ScanLambda = [&](size_t)
	{
		while (!HasFailed.load() && !Stop.stop_requested())
		{
			const size_t Index = Next.fetch_add(1);
			if (Index >= Files.size())
			{
				return;
			}
			auto& File = Plan.Files[Index];
			File.Relative = Files[Index];
			xr_string FileError;
			if (!ImportDetails::ScanFile(Source, Store, Roots, File, FileError, Stop))
			{
				HasFailed.store(true);
				xrCriticalSectionGuard Guard(StateLock);
				if (Index < FirstError)
				{
					FirstError = Index;
					Error = FileError;
				}
				return;
			}
			// Serialize notifications so older counters cannot overwrite newer progress.
			xrCriticalSectionGuard Guard(StateLock);
			++Scan.FilesCompleted;
			if (Progress && (Scan.FilesCompleted % 128 == 0 || Scan.FilesCompleted == Files.size()))
			{
				Scan.Detail = File.Relative;
				Progress(Scan);
			}
		}
	};
	constexpr size_t MaxScanTasks = 4;
	const size_t TaskCount = std::min({MaxScanTasks, Files.size(), std::max<size_t>(1, xr_max_concurrency())});
	if (TaskCount == 1)
	{
		ScanLambda(0);
	}
	else
	{
		xr_parallel_for(size_t{0}, TaskCount, ScanLambda);
	}
	if (Stop.stop_requested())
	{
		return Details::Fail(Error, "Asset import cancelled.");
	}
	if (HasFailed.load())
	{
		return false;
	}
	// Resolve collisions after every task has finished; result order follows the package file list.
	xr_hash_map<xr_string, size_t> Destinations;
	for (size_t Index = 0; Index < Plan.Files.size(); ++Index)
	{
		if (Stop.stop_requested())
		{
			return Details::Fail(Error, "Asset import cancelled.");
		}
		auto& File = Plan.Files[Index];
		if (File.Destination.empty())
		{
			continue;
		}
		const auto [Previous, IsUnique] = Destinations.emplace(ImportDetails::Key(File.Destination), Index);
		if (!IsUnique)
		{
			auto& Other = Plan.Files[Previous->second];
			Other.Status = File.Status = ImportFile::State::Blocked;
			Other.Notice = File.Notice = "Multiple package files have the same destination.";
			Other.IsSelected = File.IsSelected = false;
		}
	}
	return true;
}

bool PackageImporter::Execute(const ImportPlan& Plan, ImportReport& Report, xr_string& Error, std::stop_token Stop, const ProgressCallback& Progress) const
{
	Error.clear();
	Report = {};
	Report.PackageName = Plan.Package.Name;
	const auto RunLambda = [&]() -> bool
	{
		const auto LockPath = Join(Store.GetStateRoot(), "transaction.lock");
		if (!ImportDetails::IsSafe(LockPath, Store.GetStateRoot()))
		{
			return Details::Fail(Error, "Package storage path is unsafe or inaccessible.");
		}
		Platform::FileLock Lock(LockPath);
		if (!Lock.IsLocked())
		{
			return Details::Fail(Error, "SDK package storage is busy. Retry after the other operation finishes.");
		}
		ImportPlan Current;
		if (!BuildPlan(Plan.Package, Plan.Roots, Current, Error, Stop, Progress))
		{
			return false;
		}
		xr_hash_map<xr_string, size_t> Available;
		for (size_t Index = 0; Index < Current.Files.size(); ++Index)
		{
			const auto& File = Current.Files[Index];
			Available.emplace(File.Relative, Index);
			ImportResultFile Item;
			Item.Relative = File.Relative;
			Item.Destination = File.Destination;
			if (File.Status == ImportFile::State::Identical)
			{
				Item.Outcome = ImportOutcome::Identical;
				Item.Reason = "Contents are identical; no copy needed.";
			}
			else if (File.Status == ImportFile::State::Changed)
			{
				Item.Outcome = ImportOutcome::Kept;
				Item.Reason = "Different existing file; replacement was not selected.";
			}
			else
			{
				Item.Reason = File.Notice.empty() ? "Not selected." : File.Notice;
			}
			Report.Files.push_back(std::move(Item));
		}
		xr_vector<size_t> Selected;
		xr_hash_set<xr_string> Seen;
		PackageProgress State{"Importing assets"};
		for (const auto& File : Plan.Files)
		{
			if (!File.IsSelected)
			{
				continue;
			}
			const auto Actual = Available.find(File.Relative);
			if (Actual == Available.end() || !Seen.insert(File.Relative).second)
			{
				return Details::Fail(Error, "The selected files changed. Reopen Import to review them.");
			}
			const auto& Fresh = Current.Files[Actual->second];
			if (!File.CanSelect() || !Fresh.CanSelect() || File.Status != Fresh.Status || File.SourceStamp != Fresh.SourceStamp || File.DestinationStamp != Fresh.DestinationStamp || !Details::SamePath(File.Destination, Fresh.Destination) || File.Size != Fresh.Size || File.Size > std::numeric_limits<u64>::max() - State.BytesTotal)
			{
				return Details::Fail(Error, "The selected assets or destinations changed. Reopen Import to review the files.");
			}
			Selected.push_back(Actual->second);
			State.BytesTotal += File.Size;
			auto& Item = Report.Files[Actual->second];
			Item.Outcome = ImportOutcome::Skipped;
			Item.Reason = "Not imported: the operation did not finish.";
		}
		if (Selected.empty())
		{
			return Details::Fail(Error, "Select missing or different asset files to import.");
		}
		State.FilesTotal = Selected.size();
		ImportDetails::ImportTransaction Writes(Report, Error);
		if (!Writes.Open(Store.GetStateRoot()))
		{
			return false;
		}
		const auto CopyLambda = [&]() -> bool
		{
			for (const auto Index : Selected)
			{
				const auto& File = Current.Files[Index];
				auto& Item = Report.Files[Index];
				Item.Outcome = ImportOutcome::Failed;
				Item.Reason = "The file could not be imported.";
				if (Stop.stop_requested())
				{
					return Details::Fail(Error, "Asset import cancelled.");
				}
				const auto Root = std::find_if(Current.Roots.begin(), Current.Roots.end(), [&](const auto& Candidate)
											   { return Candidate.Alias == File.Alias && Details::Lower(File.Relative).starts_with(Candidate.Prefix + "/"); });
				const auto Source = Join(Store.GetDestination(Current.Package), File.Relative);
				ImportFile::Fingerprint SourceStamp;
				if (Root == Current.Roots.end() || !ImportDetails::IsSafe(Source, Store.GetDestination(Current.Package)) || !ImportDetails::ReadFingerprint(Source, SourceStamp, Error, Stop) || SourceStamp != File.SourceStamp)
				{
					return Details::Fail(Error, "A downloaded asset changed. Reopen Import before copying it.");
				}
				State.Detail = File.Relative;
				State.Stage = File.Status == ImportFile::State::Changed ? "Backing up changed asset" : "Importing assets";
				const ProgressCallback BackupProgressLambda = [&](const PackageProgress& Value)
				{
					if (Progress)
					{
						auto BackupProgress = State;
						BackupProgress.BytesCompleted = Value.BytesCompleted;
						BackupProgress.BytesTotal = File.DestinationStamp.Size;
						Progress(BackupProgress);
					}
				};
				if (!Writes.BeginFile(File, Root->Destination, Index, Stop, BackupProgressLambda))
				{
					Item.Reason = Error;
					return false;
				}
				std::ifstream Input(Source, std::ios::binary);
				// Exclusive creation also protects files created after the preview was checked.
				std::ofstream Output(File.Destination, std::ios::binary | std::ios::out | std::ios::noreplace);
				if (!Output.is_open())
				{
					Item.Reason = "Cannot create the destination; an existing file was preserved.";
					return Details::Fail(Error, Item.Reason.c_str());
				}
				Writes.TrackOutput();
				State.Stage = "Importing assets";
				const auto Previous = State.BytesCompleted;
				const ProgressCallback FileProgressLambda = [&](const PackageProgress& Value)
				{
					State.BytesCompleted = Previous + Value.BytesCompleted;
					if (Progress)
					{
						Progress(State);
					}
				};
				if (!Input || !Details::CopyBytes(Input, Output, Error, Stop, FileProgressLambda))
				{
					Item.Reason = Error.empty() ? "Cannot read a downloaded asset." : Error;
					return Details::Fail(Error, Item.Reason.c_str());
				}
				Output.close();
				ImportFile::Fingerprint Copied;
				if (!Output || State.BytesCompleted - Previous != File.Size || !ImportDetails::ReadFingerprint(File.Destination, Copied, Error, Stop) || Copied.Size != File.SourceStamp.Size || Copied.Crc != File.SourceStamp.Crc)
				{
					return Details::Fail(Error, "Cannot finish importing an asset; the source may have changed.");
				}
				Writes.CompleteOutput(Copied);
				Item.Outcome = File.Status == ImportFile::State::Changed ? ImportOutcome::Replaced : ImportOutcome::Added;
				Item.Reason = File.Status == ImportFile::State::Changed ? "Replaced the selected file; original retained in backups." : "Copied a missing file.";
				State.Stage = "Imported asset";
				++State.FilesCompleted;
				if (Progress)
				{
					Progress(State);
				}
			}
			return !Stop.stop_requested() || Details::Fail(Error, "Asset import cancelled.");
		};
		return Writes.Finish(CopyLambda());
	};
	Report.IsSuccess = RunLambda();
	Report.Error = Error;
	return Report.IsSuccess;
}
} // namespace SdkPackages
