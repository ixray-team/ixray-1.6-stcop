#pragma once

#include "PackageImport.h"
#include "ImportPaths.h"

namespace SdkPackages::ImportDetails
{
// CRC and timestamps detect changes to a preview; equality is checked byte for byte.
static bool ReadFingerprint(const xr_path& Path, ImportFile::Fingerprint& Stamp, xr_string& Error, std::stop_token Stop)
{
	Stamp = {};
	std::error_code Code;
	Stamp.Modified = std::filesystem::last_write_time(Path, Code);
	if (Code)
	{
		return Details::Fail(Error, "Cannot inspect an asset file.");
	}
	std::ifstream Input(Path, std::ios::binary);
	if (!Input)
	{
		return Details::Fail(Error, "Cannot read an asset file.");
	}
	xr_array<char, 65536> Buffer{};
	while (Input)
	{
		if (Stop.stop_requested())
		{
			return Details::Fail(Error, "Asset import cancelled.");
		}
		Input.read(Buffer.data(), Buffer.size());
		const auto Count = static_cast<size_t>(Input.gcount());
		Stamp.Crc = crc32(Buffer.data(), Count, Stamp.Crc);
		Stamp.Size += Count;
	}
	if (!Input.eof() || Stamp.Modified != std::filesystem::last_write_time(Path, Code) || Code)
	{
		return Details::Fail(Error, "An asset file cannot be read or changed during scanning. Reopen Import.");
	}
	return true;
}

enum class ComparisonResult
{
	Equal,
	Different,
	SourceError,
	DestinationError
};

static ComparisonResult ReadComparedFingerprints(const xr_path& Source, const xr_path& Destination, ImportFile::Fingerprint& SourceStamp, ImportFile::Fingerprint& DestinationStamp, xr_string& Error, std::stop_token Stop)
{
	SourceStamp = {};
	DestinationStamp = {};
	std::error_code Code;
	SourceStamp.Modified = std::filesystem::last_write_time(Source, Code);
	if (Code)
	{
		Error = "Cannot inspect an asset file.";
		return ComparisonResult::SourceError;
	}
	std::ifstream Left(Source, std::ios::binary);
	if (!Left)
	{
		Error = "Cannot read an asset file.";
		return ComparisonResult::SourceError;
	}
	DestinationStamp.Modified = std::filesystem::last_write_time(Destination, Code);
	if (Code)
	{
		Error = "Cannot inspect an asset file.";
		return ComparisonResult::DestinationError;
	}
	std::ifstream Right(Destination, std::ios::binary);
	if (!Right)
	{
		Error = "Cannot read an asset file.";
		return ComparisonResult::DestinationError;
	}
	xr_array<char, 65536> A{}, B{};
	bool IsEqual = true;
	while (Left || Right)
	{
		if (Stop.stop_requested())
		{
			Error = "Asset import cancelled.";
			return ComparisonResult::SourceError;
		}
		size_t LeftCount = 0, RightCount = 0;
		if (Left)
		{
			Left.read(A.data(), A.size());
			LeftCount = static_cast<size_t>(Left.gcount());
			SourceStamp.Crc = crc32(A.data(), LeftCount, SourceStamp.Crc);
			SourceStamp.Size += LeftCount;
		}
		if (!Left.eof() && Left.fail())
		{
			Error = "An asset file cannot be read or changed during scanning. Reopen Import.";
			return ComparisonResult::SourceError;
		}
		if (Right)
		{
			Right.read(B.data(), B.size());
			RightCount = static_cast<size_t>(Right.gcount());
			DestinationStamp.Crc = crc32(B.data(), RightCount, DestinationStamp.Crc);
			DestinationStamp.Size += RightCount;
		}
		if (!Right.eof() && Right.fail())
		{
			Error = "An asset file cannot be read or changed during scanning. Reopen Import.";
			return ComparisonResult::DestinationError;
		}
		IsEqual = IsEqual && LeftCount == RightCount && std::equal(A.begin(), A.begin() + LeftCount, B.begin());
	}
	if (SourceStamp.Modified != std::filesystem::last_write_time(Source, Code) || Code)
	{
		Error = "An asset file cannot be read or changed during scanning. Reopen Import.";
		return ComparisonResult::SourceError;
	}
	if (DestinationStamp.Modified != std::filesystem::last_write_time(Destination, Code) || Code)
	{
		Error = "An asset file cannot be read or changed during scanning. Reopen Import.";
		return ComparisonResult::DestinationError;
	}
	return IsEqual ? ComparisonResult::Equal : ComparisonResult::Different;
}

static bool EqualContents(const xr_path& First, const xr_path& Second, bool& IsEqual, xr_string& Error, std::stop_token Stop)
{
	std::ifstream Left(First, std::ios::binary), Right(Second, std::ios::binary);
	if (!Left || !Right)
	{
		return Details::Fail(Error, "Cannot compare existing asset files.");
	}
	xr_array<char, 65536> A{}, B{};
	IsEqual = false;
	do
	{
		if (Stop.stop_requested())
		{
			return Details::Fail(Error, "Asset import cancelled.");
		}
		Left.read(A.data(), A.size());
		Right.read(B.data(), B.size());
		if ((!Left.eof() && Left.fail()) || (!Right.eof() && Right.fail()))
		{
			return Details::Fail(Error, "Cannot compare existing asset files.");
		}
		if (Left.gcount() != Right.gcount() || !std::equal(A.begin(), A.begin() + Left.gcount(), B.begin()))
		{
			return true;
		}
	} while (Left.gcount());
	IsEqual = true;
	return true;
}

class ImportTransaction final
{
public:
	ImportTransaction(ImportReport& Report, xr_string& Error)
		: Report(Report), Error(Error) {}
	ImportTransaction(const ImportTransaction&) = delete;
	ImportTransaction& operator=(const ImportTransaction&) = delete;
	~ImportTransaction()
	{
		if (!HasFinished)
		{
			Rollback();
		}
	}
	bool Open(const xr_path& StateRoot)
	{
		const auto Parent = Join(StateRoot, "import-history");
		std::error_code Code;
		if (!IsSafe(Parent, StateRoot))
		{
			return Details::Fail(Error, "The import history directory is unsafe.");
		}
		std::filesystem::create_directories(Parent, Code);
		if (Code)
		{
			return Details::Fail(Error, "Cannot create import history.");
		}
		for (u32 Attempt = 0; Attempt < 10; ++Attempt)
		{
			const auto Directory = Join(Parent, xr_string("import-") + std::to_string(SDL_GetPerformanceCounter()).c_str());
			if (std::filesystem::create_directory(Directory, Code))
			{
				Report.Path = Join(Directory, "report.json");
				Report.BackupDirectory = Join(Directory, "backups");
				return SaveReport();
			}
			if (Code)
			{
				break;
			}
		}
		return Details::Fail(Error, "Cannot create an import report directory.");
	}
	bool BeginFile(const ImportFile& File, const xr_path& Root, size_t Index, std::stop_token Stop, const ProgressCallback& Progress)
	{
		std::error_code Code;
		Entries.push_back({File.Destination, Root, Index});
		auto& Entry = Entries.back();
		if (!IsSafe(File.Destination, Root) || !CreateParents(File.Destination.parent_path()) || !IsSafe(File.Destination, Root))
		{
			return Details::Fail(Error, "Cannot safely create the asset destination.");
		}
		if (File.Status != ImportFile::State::Changed)
		{
			return true;
		}
		ImportFile::Fingerprint Current;
		if (!ReadFingerprint(File.Destination, Current, Error, Stop) || Current != File.DestinationStamp)
		{
			return Details::Fail(Error, "An existing file changed after the preview. Reopen Import before replacing it.");
		}
		Entry.Backup = Join(Report.BackupDirectory, File.Relative);
		if (!IsSafe(Entry.Backup, Report.BackupDirectory) || !CreateParents(Entry.Backup.parent_path()))
		{
			return Details::Fail(Error, "Cannot safely create an asset backup.");
		}
		std::ifstream Input(File.Destination, std::ios::binary);
		std::ofstream Output(Entry.Backup, std::ios::binary | std::ios::out | std::ios::noreplace);
		if (!Input || !Output || !Details::CopyBytes(Input, Output, Error, Stop, Progress))
		{
			return Details::Fail(Error, "Cannot back up an existing asset. The original is kept unchanged.");
		}
		Input.close();
		Output.close();
		bool IsEqual = false;
		if (!Output || !EqualContents(File.Destination, Entry.Backup, IsEqual, Error, Stop) || !IsEqual || !ReadFingerprint(File.Destination, Current, Error, Stop) || Current != File.DestinationStamp)
		{
			return Details::Fail(Error, "An asset backup could not be verified. The original is kept unchanged.");
		}
		Report.Files[Index].Backup = Entry.Backup;
		Entry.Modified = Current.Modified;
		// A small restore record survives interruption without rewriting the full report for each backup.
		const nlohmann::json Restore = {{"file", File.Relative.c_str()}, {"destination", Utf8(File.Destination).c_str()}, {"backup", Utf8(Entry.Backup).c_str()}};
		const auto RestorePath = Join(Report.Path.parent_path(), xr_string("restore-") + std::to_string(Index).c_str() + ".json");
		if (!Details::WriteJson(RestorePath, Restore, Error) || Stop.stop_requested())
		{
			return Details::Fail(Error, "Asset import cancelled or its backup report could not be saved.");
		}
		if (!IsSafe(File.Destination, Root) || !std::filesystem::remove(File.Destination, Code) || Code)
		{
			return Details::Fail(Error, "Cannot replace an existing asset. Its backup is retained.");
		}
		Entry.WasRemoved = true;
		return true;
	}
	void TrackOutput() { Entries.back().HasOutput = true; }
	void CompleteOutput(const ImportFile::Fingerprint& Stamp)
	{
		Entries.back().OutputStamp = Stamp;
		Entries.back().HasCompletedOutput = true;
	}
	bool Finish(bool IsSuccess)
	{
		if (!IsSuccess)
		{
			Rollback();
		}
		Report.IsSuccess = IsSuccess;
		Report.Error = Error;
		if (!SaveReport())
		{
			if (IsSuccess)
			{
				Rollback();
			}
			Report.IsSuccess = false;
			Report.Error = Error;
			SaveReport();
		}
		HasFinished = true;
		return Report.IsSuccess;
	}

private:
	struct Entry
	{
		xr_path Destination;
		xr_path Root;
		size_t Index;
		xr_path Backup;
		std::filesystem::file_time_type Modified;
		ImportFile::Fingerprint OutputStamp;
		bool HasOutput = false;
		bool HasCompletedOutput = false;
		bool WasRemoved = false;
	};
	bool CreateParents(const xr_path& Directory)
	{
		std::error_code Code;
		xr_vector<xr_path> Missing;
		for (xr_path Current = Directory; !std::filesystem::exists(Current, Code); Current = Current.parent_path())
		{
			if (Code || Current.empty() || Current == Current.parent_path())
			{
				return Details::Fail(Error, "Cannot inspect the asset destination directory.");
			}
			Missing.push_back(Current);
		}
		if (Code)
		{
			return Details::Fail(Error, "Cannot inspect the asset destination directory.");
		}
		for (auto Iter = Missing.rbegin(); Iter != Missing.rend(); ++Iter)
		{
			if (std::filesystem::create_directory(*Iter, Code))
			{
				Directories.push_back(*Iter);
			}
			if (Code)
			{
				return Details::Fail(Error, "Cannot create the asset destination directory.");
			}
		}
		return true;
	}
	void Rollback()
	{
		for (auto Iter = Entries.rbegin(); Iter != Entries.rend(); ++Iter)
		{
			auto& File = Report.Files[Iter->Index];
			std::error_code Code;
			bool IsRestored = IsSafe(Iter->Destination, Iter->Root);
			bool HasConflict = false;
			if (IsRestored && Iter->HasCompletedOutput)
			{
				const auto Status = std::filesystem::symlink_status(Iter->Destination, Code);
				if (Code == std::errc::no_such_file_or_directory || (!Code && !std::filesystem::exists(Status)))
				{
					Iter->HasOutput = false;
				}
				else
				{
					ImportFile::Fingerprint Current;
					xr_string InspectionError;
					HasConflict = Code || !std::filesystem::is_regular_file(Status) || !ReadFingerprint(Iter->Destination, Current, InspectionError, {}) || Current != Iter->OutputStamp;
					IsRestored = !HasConflict;
				}
			}
			if (IsRestored && Iter->HasOutput)
			{
				std::filesystem::remove(Iter->Destination, Code);
				IsRestored = !Code;
			}
			if (IsRestored && Iter->WasRemoved)
			{
				IsRestored = std::filesystem::copy_file(Iter->Backup, Iter->Destination, std::filesystem::copy_options::none, Code) && !Code;
				if (IsRestored)
				{
					std::filesystem::last_write_time(Iter->Destination, Iter->Modified, Code);
				}
			}
			if (!IsRestored)
			{
				File.Outcome = ImportOutcome::Failed;
				File.Reason = HasConflict ? "File changed after copying; preserved in place. Inspect its retained original backup before restoring manually." : "Automatic rollback failed. Inspect the destination and any retained backup.";
				Error += " Rollback could not restore " + Utf8(Iter->Destination) + ". Inspect the import report and backups.";
			}
			else if (Iter->HasOutput || Iter->WasRemoved)
			{
				File.Outcome = ImportOutcome::RolledBack;
				File.Reason = Iter->WasRemoved ? "Operation did not finish; the original was restored." : "Operation did not finish; the new file was removed.";
			}
		}
		for (auto Iter = Directories.rbegin(); Iter != Directories.rend(); ++Iter)
		{
			std::error_code Code;
			// Only empty directories created by this operation are removed.
			std::filesystem::remove(*Iter, Code);
		}
		Entries.clear();
	}
	bool SaveReport() const
	{
		if (Report.Path.empty())
		{
			return Details::Fail(Error, "The import report is unavailable.");
		}
		nlohmann::json Data = {{"package", Report.PackageName.c_str()}, {"success", Report.IsSuccess}, {"error", Report.Error.c_str()}, {"files", nlohmann::json::array()}};
		for (const auto& File : Report.Files)
		{
			Data["files"].push_back({{"file", File.Relative.c_str()}, {"destination", Utf8(File.Destination).c_str()}, {"backup", Utf8(File.Backup).c_str()}, {"outcome", ImportOutcomeName(File.Outcome)}, {"reason", File.Reason.c_str()}});
		}
		return Details::WriteJson(Report.Path, Data, Error);
	}
	ImportReport& Report;
	xr_string& Error;
	xr_vector<Entry> Entries;
	xr_vector<xr_path> Directories;
	bool HasFinished = false;
};
} // namespace SdkPackages::ImportDetails
