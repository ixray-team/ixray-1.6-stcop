#include "stdafx.h"
#include "PackageDependencies.h"
#include "ImportPaths.h"

namespace SdkPackages
{
bool PackageDependencyChecker::SelectMissing(ImportPlan& Plan, DependencyReport& Report, xr_string& Error, std::stop_token Stop, const ProgressCallback& Progress) const
{
	Report = {};
	Error.clear();
	if (Stop.stop_requested())
	{
		return Details::Fail(Error, "Dependency selection cancelled.");
	}
	// Publish the new selection only after every pass succeeds.
	auto Working = Plan;
	xr_hash_map<xr_string, ImportFile*> Candidates;
	for (auto& File : Working.Files)
	{
		if (File.CanSelect() && !File.Destination.empty())
		{
			Candidates.emplace(ImportDetails::Key(File.Destination), &File);
		}
	}
	for (;;)
	{
		DependencyReport Current;
		if (!Check(Working, Current, Error, Stop, Progress))
		{
			return false;
		}
		if (Stop.stop_requested())
		{
			return Details::Fail(Error, "Dependency selection cancelled.");
		}
		size_t Selected = 0;
		// Apply scene parts and metadata before choosing textures derived from their settings.
		const bool HasMetadata = std::any_of(Current.Files.begin(), Current.Files.end(), [](const auto& File)
											 { return File.Status == DependencyStatus::NotSelected && File.Kind == DependencyKind::ScenePart; });
		for (const auto& Dependency : Current.Files)
		{
			if (Stop.stop_requested())
			{
				return Details::Fail(Error, "Dependency selection cancelled.");
			}
			if (Dependency.Status != DependencyStatus::NotSelected || Dependency.PackageRelative.empty() || Dependency.Destination.empty())
			{
				continue;
			}
			const auto Found = Candidates.find(ImportDetails::Key(Dependency.Destination));
			if (Found != Candidates.end() && Found->second->Relative == Dependency.PackageRelative && !Found->second->IsSelected)
			{
				if (Found->second->Status != ImportFile::State::Missing)
				{
					return Details::Fail(Error, "SDK files changed after the preview. Reopen Import before selecting dependencies.");
				}
				if (HasMetadata && Dependency.Kind != DependencyKind::ScenePart)
				{
					continue;
				}
				Found->second->IsSelected = true;
				++Selected;
			}
		}
		if (Selected == 0)
		{
			if (Stop.stop_requested())
			{
				return Details::Fail(Error, "Dependency selection cancelled.");
			}
			Plan = std::move(Working);
			Report = std::move(Current);
			return true;
		}
		if (Progress)
		{
			Progress({"Selecting missing dependencies", Plan.Package.Name});
		}
		// Each pass adds files; selected metadata may reveal more references on the next pass.
	}
}
} // namespace SdkPackages
