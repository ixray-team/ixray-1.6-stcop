#pragma once

#include "PackageImport.h"

namespace SdkPackages
{
enum class DependencyKind
{
	Object,
	Texture,
	ScenePart
};

enum class DependencyStatus
{
	Missing,
	NotSelected,
	InSdk,
	Selected
};

struct DependencyFile
{
	DependencyKind Kind = DependencyKind::Object;
	DependencyStatus Status = DependencyStatus::Missing;
	xr_string Name;
	xr_string RequiredBy;
	xr_string PackageRelative;
	xr_path Destination;
	xr_string Reason;
};

enum class DependencyWarningKind
{
	IncompleteCheck,
	EmptyBumpReference
};

struct DependencyWarning
{
	xr_string File;
	xr_string Message;
	xr_string RequiredBy;
	xr_path Source;
	xr_string Remedy;
	DependencyWarningKind Kind = DependencyWarningKind::IncompleteCheck;
};

struct DependencyReport
{
	xr_vector<DependencyFile> Files;
	xr_vector<DependencyWarning> Warnings;
	size_t Levels = 0;
};

class PackageDependencyChecker final
{
public:
	explicit PackageDependencyChecker(const PackageStore& Store)
		: Store(Store) {}
	~PackageDependencyChecker() = default;
	bool Check(const ImportPlan& Plan, DependencyReport& Report, xr_string& Error, std::stop_token Stop = {}, const ProgressCallback& Progress = {}) const;
	bool SelectMissing(ImportPlan& Plan, DependencyReport& Report, xr_string& Error, std::stop_token Stop = {}, const ProgressCallback& Progress = {}) const;

private:
	const PackageStore& Store;
};
} // namespace SdkPackages
