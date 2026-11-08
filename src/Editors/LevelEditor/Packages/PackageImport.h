#pragma once

#include "PackageStore.h"

namespace SdkPackages
{
struct ImportRoot
{
	xr_string Prefix;
	xr_string Alias;
	xr_path Destination;
};

struct ImportFile
{
	enum class State
	{
		Missing,
		Identical,
		Changed,
		Blocked
	};
	struct Fingerprint
	{
		u64 Size = 0;
		u32 Crc = 0;
		std::filesystem::file_time_type Modified;
		bool operator==(const Fingerprint&) const = default;
	};
	xr_string Relative;
	xr_string Alias;
	xr_path Destination;
	xr_string Notice;
	u64 Size = 0;
	Fingerprint SourceStamp;
	Fingerprint DestinationStamp;
	State Status = State::Blocked;
	bool IsSelected = false;
	bool CanSelect() const { return Notice.empty() && (Status == State::Missing || Status == State::Changed); }
};

struct ImportPlan
{
	PackageInfo Package;
	xr_vector<ImportRoot> Roots;
	xr_vector<ImportFile> Files;
};

enum class ImportOutcome
{
	Added,
	Replaced,
	Identical,
	Kept,
	Skipped,
	RolledBack,
	Failed
};

const char* ImportOutcomeName(ImportOutcome Outcome);

struct ImportResultFile
{
	xr_string Relative;
	xr_path Destination;
	xr_path Backup;
	ImportOutcome Outcome = ImportOutcome::Skipped;
	xr_string Reason;
};

struct ImportReport
{
	xr_string PackageName;
	xr_vector<ImportResultFile> Files;
	xr_path Path;
	xr_path BackupDirectory;
	xr_string Error;
	bool IsSuccess = false;
};

class PackageImporter final
{
public:
	explicit PackageImporter(const PackageStore& Store)
		: Store(Store) {}
	~PackageImporter() = default;
	// Capture the active filesystem aliases on the editor thread.
	static xr_vector<ImportRoot> CaptureRoots();
	static void RefreshFilesystem(const xr_vector<xr_path>& Imported);
	bool Prepare(const PackageInfo& Info, const xr_vector<ImportRoot>& Roots, ImportPlan& Plan, xr_string& Error, std::stop_token Stop = {}, const ProgressCallback& Progress = {}) const;
	bool Execute(const ImportPlan& Plan, ImportReport& Report, xr_string& Error, std::stop_token Stop = {}, const ProgressCallback& Progress = {}) const;

private:
	bool BuildPlan(const PackageInfo& Info, const xr_vector<ImportRoot>& Roots, ImportPlan& Plan, xr_string& Error, std::stop_token Stop, const ProgressCallback& Progress) const;
	const PackageStore& Store;
};
} // namespace SdkPackages
