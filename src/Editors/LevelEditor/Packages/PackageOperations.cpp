#include "stdafx.h"
#include "PackageOperations.h"
#include "PackageCatalog.h"
#include "LocalSource.h"

namespace SdkPackages
{
PackageOperations::PackageOperations(PackageStore& Store, PackageCatalog& Catalog, const PackageThread::Launcher& Launch)
	: Store(Store), Catalog(Catalog), Launch(Launch) {}
PackageOperations::~PackageOperations()
{
	Cancel();
	Worker.Join();
}
PackageProgress PackageOperations::GetProgress() const
{
	xrCriticalSectionGuard Guard(ProgressLock);
	return CurrentProgress;
}
void PackageOperations::ReportProgress(const PackageProgress& Value)
{
	xrCriticalSectionGuard Guard(ProgressLock);
	CurrentProgress = Value;
}
void PackageOperations::Cancel()
{
	Worker.Cancel();
}
bool PackageOperations::Poll(Result& Output)
{
	if (!IsComplete.exchange(false))
	{
		return false;
	}
	Worker.Join();
	IsRunning = false;
	ReportProgress({});
	Output = std::move(Pending);
	return true;
}
bool PackageOperations::Start(Action Operation, const PackageInfo& SourceInfo, bool IsFromCatalog, xr_string& StartError)
{
	StartError.clear();
	if (IsRunning)
	{
		return Details::Fail(StartError, "A package operation is already running.");
	}
	IsRunning = true;
	ReportProgress({"Starting package operation", SourceInfo.Name});

	auto JobLambda = [this, Operation, Info = SourceInfo, IsFromCatalog](std::stop_token Stop) mutable
	{
		xr_string Error, CheckedCommit, CatalogWarning;
		const ProgressCallback ProgressLambda = [this](const PackageProgress& Value)
		{ ReportProgress(Value); };
		bool IsSuccess = false;

		if (Operation == Action::SyncCatalog)
		{
			IsSuccess = Catalog.Sync(Error, Stop, ProgressLambda);
		}
		else if (Operation == Action::CheckUpdates)
		{
			ProgressLambda({"Checking package revision", Info.Url});
			IsSuccess = SdkPackages::GitSource::ResolveRevision(Info.Url, Info.Ref, CheckedCommit, Error, Stop);
		}
		else if (Operation == Action::Remove)
		{
			ProgressLambda({"Checking and removing package files", Info.Name});
			IsSuccess = Store.Remove(Info.Id, Error, Stop);
		}
		else
		{
			xr_path Stage;
			if (Store.CreateStage(Stage, Error))
			{
				SdkPackages::Details::ScopedDirectory Cleanup(Stage);
				xr_path Checkout = SdkPackages::Join(Stage, "repository");
				const xr_string ExpectedCommit = IsFromCatalog && Operation == Action::Install ? Info.Commit : xr_string{};
				ProgressLambda({Info.Source == PackageSource::GitHub ? "Downloading package repository" : Info.Source == PackageSource::Zip ? "Extracting ZIP package"
																																			: "Reading local package",
								Info.Source == PackageSource::GitHub ? Info.Url : Info.LocalPath});
				const bool IsPrepared = Info.Source == SdkPackages::PackageSource::GitHub ? SdkPackages::GitSource::Checkout(Info.Url, Info.Ref, Checkout, Info.Commit, Error, Stop, ExpectedCommit) : SdkPackages::LocalSource::Prepare(Info, Stage, Checkout, Error, Stop);
				if (IsPrepared && !Stop.stop_requested())
				{
					IsSuccess = Store.InstallCheckout(Checkout, Info, Operation == Action::Update, Error, &Info, IsFromCatalog, Stop, ProgressLambda);
					if (IsSuccess && Info.Source == SdkPackages::PackageSource::GitHub)
					{
						ProgressLambda({"Updating package documentation cache", Info.Name});
						Catalog.RememberCheckout(Info, Checkout, CatalogWarning);
					}
				}
			}
		}

		if (!IsSuccess && Error.empty() && Stop.stop_requested())
		{
			Error = "Operation cancelled.";
		}
		Result Output;
		Output.Operation = Operation;
		Output.Package = std::move(Info);
		Output.CheckedCommit = std::move(CheckedCommit);
		Output.IsSuccess = IsSuccess;
		Output.Message = Error;
		if (IsSuccess)
		{
			switch (Operation)
			{
				case Action::CheckUpdates: Output.Message = Details::Lower(Output.Package.Commit) == Details::Lower(Output.CheckedCommit) ? "Package is up to date." : "A different package revision is available."; break;
				case Action::Install: Output.Message = "Package installed."; break;
				case Action::Update: Output.Message = "Package updated."; break;
				case Action::Remove: Output.Message = "Package removed."; break;
				case Action::SyncCatalog: Output.Message = Error.empty() ? "Catalog synchronized." : "Catalog synchronized. " + Error; break;
			}
		}
		if (!CatalogWarning.empty())
		{
			Output.Message += " Catalog cache could not be refreshed: " + CatalogWarning;
		}
		Pending = std::move(Output);
		IsComplete = true;
	};
	if (!Worker.Start(JobLambda, "SDK packages", StartError, Launch))
	{
		IsRunning = false;
		ReportProgress({});
		return false;
	}

	return true;
}
} // namespace SdkPackages
