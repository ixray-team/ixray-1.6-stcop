#pragma once

#include "PackageStore.h"
#include <atomic>
#include "PackageThread.h"

namespace SdkPackages
{
class PackageCatalog;
class PackageOperations final
{
public:
	enum class Action
	{
		Install,
		Update,
		Remove,
		SyncCatalog,
		CheckUpdates
	};
	struct Result
	{
		Action Operation = Action::Install;
		PackageInfo Package;
		xr_string Message;
		xr_string CheckedCommit;
		bool IsSuccess = false;
	};
	PackageOperations(PackageStore& Store, PackageCatalog& Catalog, const PackageThread::Launcher& Launch = {});
	~PackageOperations();
	bool Start(Action Operation, const PackageInfo& Info, bool IsFromCatalog, xr_string& Error);
	bool Poll(Result& Output);
	void Cancel();
	PackageProgress GetProgress() const;

private:
	void ReportProgress(const PackageProgress& Value);
	mutable xrCriticalSection ProgressLock;
	PackageProgress CurrentProgress;
	PackageStore& Store;
	PackageCatalog& Catalog;
	PackageThread::Launcher Launch;
	PackageThread Worker;
	xr_atomic_bool IsComplete{false};
	bool IsRunning = false;
	Result Pending;
};
} // namespace SdkPackages
