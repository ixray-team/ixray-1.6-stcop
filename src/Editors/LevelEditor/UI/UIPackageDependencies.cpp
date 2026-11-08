#include "stdafx.h"
#include "UIPackageManager.h"
#include "UIPackageDependencies.h"

namespace PackageDependenciesUI
{
static void DrawWarningTooltip(const SdkPackages::DependencyWarning& Warning)
{
	ImGui::BeginTooltip();
	ImGui::PushTextWrapPos(ImGui::GetFontSize() * 65);
	ImGui::TextWrapped("%s", Warning.Message.c_str());
	if (!Warning.Source.empty())
	{
		ImGui::TextWrapped("Source file: %s", SdkPackages::Utf8(Warning.Source).c_str());
	}
	if (!Warning.RequiredBy.empty())
	{
		ImGui::TextWrapped("Required by: %s", Warning.RequiredBy.c_str());
	}
	if (!Warning.Remedy.empty())
	{
		ImGui::Separator();
		ImGui::TextWrapped("%s", Warning.Remedy.c_str());
	}
	ImGui::PopTextWrapPos();
	ImGui::EndTooltip();
}
} // namespace PackageDependenciesUI

void UIPackageManager::InvalidateImportDependencies()
{
	IsDependencyCheckCurrent = false;
	DependencyCheckError.clear();
}

void PackageDependenciesUI::Draw(const SdkPackages::DependencyReport& Report, bool IsCurrent, const xr_string& Error, bool& ShowOnlyProblems, float Height)
{
	const float HeaderStart = ImGui::GetCursorPosY();
	ImGui::TextWrapped("Checks scene/model references, terrain masks, bump/detail metadata and animation frames. Metadata follows textures.ltx, runtime THM and the active level. Other shader-generated textures and game resources are outside this check.");
	if (!IsCurrent)
	{
		ImGui::TextWrapped("The selection changed or the check did not finish. Run the check again to refresh the list.");
	}
	if (!Error.empty())
	{
		ImGui::TextWrapped("%s", Error.c_str());
	}
	if (std::any_of(Report.Warnings.begin(), Report.Warnings.end(), [](const auto& Warning)
					{ return Warning.Kind == SdkPackages::DependencyWarningKind::EmptyBumpReference; }))
	{
		ImGui::TextWrapped("Bump not assigned: the THM is readable, but bump mapping is enabled without a texture name. Choose a bump texture or disable Bump in the texture editor. Hover the row to see the exact THM path.");
	}
	ImGui::Checkbox("Show only dependency problems", &ShowOnlyProblems);
	xr_vector<size_t> Rows;
	for (size_t Index = 0; Index < Report.Files.size(); ++Index)
	{
		const auto Status = Report.Files[Index].Status;
		if (!ShowOnlyProblems || Status == SdkPackages::DependencyStatus::Missing || Status == SdkPackages::DependencyStatus::NotSelected)
		{
			Rows.push_back(Index);
		}
	}
	for (size_t Index = 0; Index < Report.Warnings.size(); ++Index)
	{
		Rows.push_back(Report.Files.size() + Index);
	}
	if (Rows.empty())
	{
		ImGui::TextWrapped(!IsCurrent ? "No current dependency results." : Report.Levels == 0 ? "Select a .level file or scene parts to check their object and texture dependencies."
																							  : "No missing dependencies found among the supported references.");
		return;
	}
	if (ImGui::BeginTable("Report", 4, ImGuiTableFlags_Resizable | ImGuiTableFlags_BordersInnerV | ImGuiTableFlags_RowBg | ImGuiTableFlags_ScrollY | ImGuiTableFlags_PadOuterX, {0, std::max(80.0f, Height - (ImGui::GetCursorPosY() - HeaderStart))}))
	{
		ImGui::TableSetupColumn("Resource", ImGuiTableColumnFlags_WidthStretch, 0.28f);
		ImGui::TableSetupColumn("Status", ImGuiTableColumnFlags_WidthStretch, 0.12f);
		ImGui::TableSetupColumn("Required by", ImGuiTableColumnFlags_WidthStretch, 0.26f);
		ImGui::TableSetupColumn("Details", ImGuiTableColumnFlags_WidthStretch, 0.34f);
		ImGui::TableSetupScrollFreeze(0, 1);
		ImGui::TableHeadersRow();
		ImGuiListClipper Clipper;
		Clipper.Begin(static_cast<int>(Rows.size()));
		while (Clipper.Step())
		{
			for (int Row = Clipper.DisplayStart; Row < Clipper.DisplayEnd; ++Row)
			{
				const auto Index = Rows[Row];
				const bool IsWarning = Index >= Report.Files.size();
				const auto* File = IsWarning ? nullptr : &Report.Files[Index];
				const auto* Warning = IsWarning ? &Report.Warnings[Index - Report.Files.size()] : nullptr;
				ImGui::TableNextRow();
				ImGui::TableSetColumnIndex(0);
				ImGui::TextUnformatted(IsWarning ? Warning->File.c_str() : File->Name.c_str());
				if (ImGui::IsItemHovered())
				{
					if (Warning)
					{
						DrawWarningTooltip(*Warning);
					}
					else if (File->PackageRelative.empty())
					{
						ImGui::SetTooltip("Destination: %s", SdkPackages::Utf8(File->Destination).c_str());
					}
					else
					{
						ImGui::SetTooltip("Destination: %s\nPackage file: %s", SdkPackages::Utf8(File->Destination).c_str(), File->PackageRelative.c_str());
					}
				}
				ImGui::TableSetColumnIndex(1);
				const char* Status = (Warning && Warning->Kind == SdkPackages::DependencyWarningKind::EmptyBumpReference ? "Bump not assigned" : "Could not check");
				if (File)
				{
					switch (File->Status)
					{
						case SdkPackages::DependencyStatus::Missing: Status = "Missing"; break;
						case SdkPackages::DependencyStatus::NotSelected: Status = "Not selected"; break;
						case SdkPackages::DependencyStatus::InSdk: Status = "In SDK"; break;
						case SdkPackages::DependencyStatus::Selected: Status = "Selected"; break;
					}
				}
				ImGui::TextUnformatted(Status);
				if (Warning && ImGui::IsItemHovered())
				{
					DrawWarningTooltip(*Warning);
				}
				ImGui::TableSetColumnIndex(2);
				ImGui::TextUnformatted(File ? File->RequiredBy.c_str() : Warning->RequiredBy.empty() ? "-"
																									 : Warning->RequiredBy.c_str());
				if (ImGui::IsItemHovered())
				{
					if (Warning)
					{
						DrawWarningTooltip(*Warning);
					}
					else
					{
						ImGui::SetTooltip("%s", File->RequiredBy.c_str());
					}
				}
				ImGui::TableSetColumnIndex(3);
				const auto& Reason = File ? File->Reason : Warning->Message;
				ImGui::TextUnformatted(Reason.c_str());
				if (ImGui::IsItemHovered())
				{
					if (Warning)
					{
						DrawWarningTooltip(*Warning);
					}
					else
					{
						ImGui::SetTooltip("%s", Reason.c_str());
					}
				}
			}
		}
		ImGui::EndTable();
	}
}

void UIPackageManager::DrawImportDependencies(float Height)
{
	PackageDependenciesUI::Draw(ImportDependencies, IsDependencyCheckCurrent, DependencyCheckError, ShowOnlyDependencyProblems, Height);
}
