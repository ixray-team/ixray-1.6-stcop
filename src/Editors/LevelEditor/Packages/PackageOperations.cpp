#include "stdafx.h"
#include "PackageOperations.h"
#include "PackageCatalog.h"
#include "LocalSource.h"
#include "ObjectComparison.h"
#include "TextureComparison.h"
#include "ImportPaths.h"

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
bool PackageOperations::Start(Action Operation, const PackageInfo& SourceInfo, bool IsFromCatalog, xr_string& StartError, const ImportPlan& ImportRequest, const xr_vector<ImportRoot>& ImportRoots, size_t FileIndex)
{
	StartError.clear();
	if (IsRunning)
	{
		return Details::Fail(StartError, "A package operation is already running.");
	}
	IsRunning = true;
	ReportProgress({"Starting package operation", SourceInfo.Name});

	auto JobLambda = [this, Operation, Info = SourceInfo, IsFromCatalog, ImportRequest, ImportRoots, FileIndex](std::stop_token Stop) mutable
	{
		xr_string Error, CheckedCommit, CatalogWarning;
		ImportPlan PreparedImport;
		ImportReport ImportSummary;
		DependencyReport Dependencies;
		AssetComparison Comparison;
		bool HasDependencyResult = false;
		const ProgressCallback ProgressLambda = [this](const PackageProgress& Value)
		{ ReportProgress(Value); };
		bool IsSuccess = false;

		if (Operation == Action::CompareImportFile)
		{
			if (FileIndex < ImportRequest.Files.size() && Info.Id == ImportRequest.Package.Id && Info.Kind == PackageKind::Assets)
			{
				const auto& File = ImportRequest.Files[FileIndex];
				const auto Root = std::find_if(ImportRequest.Roots.begin(), ImportRequest.Roots.end(), [&](const auto& Entry)
											   { return Entry.Alias == File.Alias && !Entry.Destination.empty(); });
				const auto SourceRoot = Store.GetDestination(ImportRequest.Package);
				const auto Source = Join(SourceRoot, File.Relative);
				if (Root != ImportRequest.Roots.end() && ImportDetails::IsSafe(File.Destination, Root->Destination) && !ImportDetails::IsProtected(File.Destination, Store) && ImportDetails::IsSafe(Source, SourceRoot))
				{
					const auto Extension = Details::Lower(Utf8(Source.extension()));
					if (Extension == ".object")
					{
						IsSuccess = ObjectComparison::Read(File.Destination, Source, Comparison, Error, Stop, ProgressLambda);
					}
					else if (Extension == ".thm" || Extension == ".dds" || Extension == ".tga")
					{
						IsSuccess = TextureComparison::Read(File.Destination, Source, Comparison, Error, Stop, ProgressLambda);
					}
					else
					{
						Comparison.Notices.push_back("Content comparison supports .object, texture .thm, .dds and .tga files. Other formats show file information only.");
						IsSuccess = true;
					}
				}
			}
			if (!IsSuccess && Error.empty())
			{
				Error = "The comparison file is unavailable or its path is unsafe. Reopen Import.";
			}
		}
		else if (Operation == Action::PrepareImport)
		{
			IsSuccess = PackageImporter(Store).Prepare(Info, ImportRoots, PreparedImport, Error, Stop, ProgressLambda);
		}
		else if (Operation == Action::CheckImportDependencies)
		{
			HasDependencyResult = IsSuccess = PackageDependencyChecker(Store).Check(ImportRequest, Dependencies, Error, Stop, ProgressLambda);
		}
		else if (Operation == Action::SelectImportDependencies)
		{
			PreparedImport = ImportRequest;
			HasDependencyResult = IsSuccess = PackageDependencyChecker(Store).SelectMissing(PreparedImport, Dependencies, Error, Stop, ProgressLambda);
		}
		else if (Operation == Action::Import)
		{
			IsSuccess = PackageImporter(Store).Execute(ImportRequest, ImportSummary, Error, Stop, ProgressLambda);
		}
		else if (Operation == Action::SyncCatalog)
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
				ProgressLambda({Info.Source == PackageSource::GitHub ? "Downloading package repository" : Info.Source == PackageSource::Archive ? "Extracting package archive"
																																				: "Reading local package",
								Info.Source == PackageSource::GitHub ? Info.Url : Info.LocalPath});
				const bool IsPrepared = Info.Source == SdkPackages::PackageSource::GitHub ? SdkPackages::GitSource::Checkout(Info.Url, Info.Ref, Checkout, Info.Commit, Error, Stop, ExpectedCommit) : SdkPackages::LocalSource::Prepare(Info, Stage, Checkout, Error, Stop, ProgressLambda);
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
		Output.Import = std::move(PreparedImport);
		Output.ImportSummary = std::move(ImportSummary);
		Output.Dependencies = std::move(Dependencies);
		Output.Comparison = std::move(Comparison);
		Output.HasDependencyResult = HasDependencyResult;
		Output.IsSuccess = IsSuccess;
		Output.Message = Error;
		if (IsSuccess)
		{
			switch (Operation)
			{
				case Action::CheckUpdates: Output.Message = Details::Lower(Output.Package.Commit) == Details::Lower(Output.CheckedCommit) ? "Package is up to date." : "A different package revision is available."; break;
				case Action::Install: Output.Message = Output.Package.Kind == PackageKind::Assets ? "Package downloaded. Choose Import to add its assets to the SDK." : "Package installed."; break;
				case Action::Update: Output.Message = Output.Package.Kind == PackageKind::Assets ? "Download updated. Imported files are unchanged." : "Package updated."; break;
				case Action::Remove: Output.Message = Output.Package.Kind == PackageKind::Assets ? "Download removed. Imported files are unchanged." : "Package removed."; break;
				case Action::SyncCatalog: Output.Message = Error.empty() ? "Catalog synchronized." : "Catalog synchronized. " + Error; break;
				case Action::PrepareImport: Output.Message = "Select the asset files to import."; break;
				case Action::CheckImportDependencies: Output.Message = "Level dependency check finished."; break;
				case Action::CompareImportFile: Output.Message = "File content comparison finished."; break;
				case Action::SelectImportDependencies: Output.Message = "Available dependencies selected. Review the list before importing."; break;
				case Action::Import: Output.Message = "Selected assets imported. The downloaded package is unchanged."; break;
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
