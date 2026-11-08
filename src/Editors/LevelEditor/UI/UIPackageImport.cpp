#include "stdafx.h"
#include "UIPackageManager.h"
#include "../Editor/Utils/ContentView.h"
#include "../../xrEUI/ModernUI.h"
#include <cmath>
#include <cfloat>
#include <chrono>

namespace PackageImportWindow
{
static const char* FileIcon(const xr_string& Relative)
{
	const auto Name = SdkPackages::Details::Lower(Relative);
	const auto Position = Name.find_last_of('.');
	const xr_string_view Extension = Position == xr_string::npos ? xr_string_view{} : xr_string_view(Name.data() + Position, Name.size() - Position);
	if (Extension == ".level")
	{
		return "level";
	}
	if (Extension == ".object" || Extension == ".group")
	{
		return "object";
	}
	if (Extension == ".ltx" || Extension == ".thm")
	{
		return "thm";
	}
	for (const char* Type : {"ogg", "wav", "seq", "tga", "dll", "exe", "efd", "widget"})
	{
		if (Extension.size() == xr_strlen(Type) + 1 && Extension.substr(1) == Type)
		{
			return Type;
		}
	}
	if (Extension == ".dds" || Extension == ".png" || Extension == ".jpg" || Extension == ".jpeg" || Extension == ".bmp")
	{
		return "image";
	}
	if (Extension == ".cmd" || Extension == ".bat")
	{
		return "cmd";
	}
	return "file";
}

static float IconSize()
{
	return ImGui::GetTextLineHeightWithSpacing();
}

static int LabelPadding()
{
	return static_cast<int>(std::ceil((IconSize() + ImGui::GetStyle().ItemInnerSpacing.x) / std::max(1.0f, ImGui::CalcTextSize(" ").x)));
}

static void DrawIcon(const char* Type, const ImVec2& Position, float Size)
{
	if (const auto Icon = CContentView::GetTypeIcon(Type))
	{
		const ImVec2 Maximum = {Position.x + Size, Position.y + Size};
		const auto Tint = XRay::ImGui::GetEditorColor(XRay::ImGui::EEditorColors::ContentIconTint);
		ImGui::GetWindowDrawList()->AddImage(Icon, Position, Maximum, {0, 0}, {1, 1}, ImGui::GetColorU32(Tint.Value));
	}
}

static void ComparisonText(const xr_string& Text)
{
	auto Line = Text;
	for (auto& Letter : Line)
	{
		if (Letter == '\n' || Letter == '\r' || Letter == '\t')
		{
			Letter = ' ';
		}
	}
	ImGui::TextUnformatted(Line.c_str());
	if (ImGui::IsItemHovered())
	{
		ImGui::BeginTooltip();
		ImGui::PushTextWrapPos(ImGui::GetFontSize() * 65);
		ImGui::TextUnformatted(Text.c_str());
		ImGui::PopTextWrapPos();
		ImGui::EndTooltip();
	}
}

static bool CanSelect(const SdkPackages::ImportFile& File, bool CanReplace)
{
	return File.CanSelect() && (File.Status != SdkPackages::ImportFile::State::Changed || CanReplace);
}
static const char* Status(const SdkPackages::ImportFile& File)
{
	if (!File.Notice.empty())
	{
		return "Unavailable";
	}
	switch (File.Status)
	{
		case SdkPackages::ImportFile::State::Missing: return "New";
		case SdkPackages::ImportFile::State::Identical: return "Already in SDK";
		case SdkPackages::ImportFile::State::Changed: return "Different";
		case SdkPackages::ImportFile::State::Blocked: return "Unavailable";
	}
	return "Unavailable";
}

static xr_string ModifiedTime(std::filesystem::file_time_type Value)
{
	const auto Time = std::chrono::floor<std::chrono::seconds>(std::chrono::clock_cast<std::chrono::system_clock>(Value));
	const auto Day = std::chrono::floor<std::chrono::days>(Time);
	const std::chrono::year_month_day Date(Day);
	const std::chrono::hh_mm_ss Clock(Time - Day);
	string64 Result;
	xr_sprintf(Result, "%04d-%02u-%02u %02u:%02u:%02u UTC", static_cast<int>(Date.year()), static_cast<unsigned>(Date.month()), static_cast<unsigned>(Date.day()), static_cast<unsigned>(Clock.hours().count()), static_cast<unsigned>(Clock.minutes().count()), static_cast<unsigned>(Clock.seconds().count()));
	return Result;
}

static void DrawFileInformation(const SdkPackages::ImportFile& File, const xr_path& SourceRoot)
{
	if (ImGui::BeginTable("FileComparison", 3, ImGuiTableFlags_BordersInnerV | ImGuiTableFlags_RowBg | ImGuiTableFlags_PadOuterX))
	{
		ImGui::TableSetupColumn("Property", ImGuiTableColumnFlags_WidthFixed, 110);
		ImGui::TableSetupColumn("Current in SDK", ImGuiTableColumnFlags_WidthStretch);
		ImGui::TableSetupColumn("From package", ImGuiTableColumnFlags_WidthStretch);
		ImGui::TableHeadersRow();
		ImGui::TableNextRow();
		ImGui::TableSetColumnIndex(0);
		ImGui::TextUnformatted("Size");
		for (int Column = 1; Column <= 2; ++Column)
		{
			const auto& Stamp = Column == 1 ? File.DestinationStamp : File.SourceStamp;
			ImGui::TableSetColumnIndex(Column);
			ImGui::TextWrapped("%llu bytes (%.2f MiB)", static_cast<unsigned long long>(Stamp.Size), static_cast<double>(Stamp.Size) / (1024 * 1024));
		}
		ImGui::TableNextRow();
		ImGui::TableSetColumnIndex(0);
		ImGui::TextUnformatted("Modified (UTC)");
		ImGui::TableSetColumnIndex(1);
		ImGui::TextWrapped("%s", ModifiedTime(File.DestinationStamp.Modified).c_str());
		ImGui::TableSetColumnIndex(2);
		ImGui::TextWrapped("%s", ModifiedTime(File.SourceStamp.Modified).c_str());
		ImGui::TableNextRow();
		ImGui::TableSetColumnIndex(0);
		ImGui::TextUnformatted("CRC32");
		ImGui::TableSetColumnIndex(1);
		ImGui::Text("%08X", static_cast<unsigned>(File.DestinationStamp.Crc));
		ImGui::TableSetColumnIndex(2);
		ImGui::Text("%08X", static_cast<unsigned>(File.SourceStamp.Crc));
		ImGui::TableNextRow();
		ImGui::TableSetColumnIndex(0);
		ImGui::TextUnformatted("Location");
		ImGui::TableSetColumnIndex(1);
		ImGui::TextWrapped("%s", SdkPackages::Utf8(File.Destination).c_str());
		ImGui::TableSetColumnIndex(2);
		ImGui::TextWrapped("%s", SdkPackages::Utf8(SdkPackages::Join(SourceRoot, File.Relative)).c_str());
		ImGui::EndTable();
	}
	ImGui::Separator();
	const float TextPadding = ImGui::GetStyle().WindowPadding.x;
	ImGui::Indent(TextPadding);
	ImGui::PushTextWrapPos(ImGui::GetWindowWidth() - TextPadding);
	ImGui::TextWrapped("Asset files have no recorded version number. These values were captured during the import scan; modification dates are not release versions.");
	if (File.SourceStamp.Size == File.DestinationStamp.Size && File.SourceStamp.Crc == File.DestinationStamp.Crc)
	{
		ImGui::TextWrapped("The contents differ despite matching size and CRC32. Equality is checked byte by byte.");
	}
	ImGui::PopTextWrapPos();
	ImGui::Unindent(TextPadding);
}

static void DrawTooltip(const SdkPackages::ImportFile& File)
{
	ImGui::BeginTooltip();
	ImGui::PushTextWrapPos(ImGui::GetFontSize() * 55);
	ImGui::TextUnformatted(File.Relative.c_str());
	if (!File.Destination.empty())
	{
		ImGui::TextWrapped("Destination: %s", SdkPackages::Utf8(File.Destination).c_str());
	}
	if (!File.Notice.empty())
	{
		ImGui::TextWrapped("%s", File.Notice.c_str());
	}
	else if (File.Status == SdkPackages::ImportFile::State::Changed)
	{
		ImGui::TextUnformatted("Click Different to compare the SDK file with the package file.");
	}
	ImGui::PopTextWrapPos();
	ImGui::EndTooltip();
}
} // namespace PackageImportWindow


void UIPackageManager::BuildImportTree()
{
	ImportTree = {};
	for (size_t Index = 0; Index < ImportPlan.Files.size(); ++Index)
	{
		if (!ShowExistingImportFiles && ImportPlan.Files[Index].Status == SdkPackages::ImportFile::State::Identical)
		{
			continue;
		}
		auto* Node = &ImportTree;
		Node->Files.push_back(Index);
		const auto& Relative = ImportPlan.Files[Index].Relative;
		size_t Begin = 0;
		for (auto End = Relative.find('/'); End != xr_string::npos; End = Relative.find('/', Begin))
		{
			Node = &Node->Folders[Relative.substr(Begin, End - Begin)];
			Node->Files.push_back(Index);
			Begin = End + 1;
		}
		Node->Leaves.push_back(Index);
	}
}

void UIPackageManager::DrawImportNode(ImportNode& Node)
{
	for (auto& [Name, Folder] : Node.Folders)
	{
		ImGui::PushID(Name.c_str());
		ImGui::TableNextRow();
		ImGui::TableSetColumnIndex(0);
		size_t Available = 0, Selected = 0;
		for (const auto Index : Folder.Files)
		{
			const auto& File = ImportPlan.Files[Index];
			Available += PackageImportWindow::CanSelect(File, CanReplaceImportFiles);
			Selected += File.IsSelected;
		}
		bool IsSelected = Available && Selected == Available;
		ImGui::BeginDisabled(Available == 0);
		if (ImGui::Checkbox("##SelectFolder", &IsSelected))
		{
			InvalidateImportDependencies();
			for (const auto Index : Folder.Files)
			{
				auto& File = ImportPlan.Files[Index];
				File.IsSelected = PackageImportWindow::CanSelect(File, CanReplaceImportFiles) && IsSelected;
			}
		}
		ImGui::EndDisabled();
		ImGui::SameLine();
		const auto Position = ImGui::GetCursorScreenPos();
		const bool IsOpen = ImGui::TreeNodeEx("##Folder", ImGuiTreeNodeFlags_DefaultOpen, "%*s%s", PackageImportWindow::LabelPadding(), "", Name.c_str());
		const float Size = PackageImportWindow::IconSize();
		PackageImportWindow::DrawIcon("Folder", {Position.x + ImGui::GetTreeNodeToLabelSpacing(), ImGui::GetItemRectMin().y + (ImGui::GetItemRectSize().y - Size) * 0.5f}, Size);
		if (IsOpen)
		{
			ImGui::TableSetColumnIndex(0);
			DrawImportNode(Folder);
			ImGui::TableSetColumnIndex(0);
			ImGui::TreePop();
		}
		ImGui::PopID();
	}
	for (const auto Index : Node.Leaves)
	{
		auto& File = ImportPlan.Files[Index];
		ImGui::PushID(static_cast<int>(Index));
		ImGui::TableNextRow();
		ImGui::TableSetColumnIndex(0);
		ImGui::BeginDisabled(!PackageImportWindow::CanSelect(File, CanReplaceImportFiles));
		const auto Position = File.Relative.find_last_of('/');
		const auto Name = Position == xr_string::npos ? File.Relative : File.Relative.substr(Position + 1);
		if (ImGui::Checkbox("##SelectFile", &File.IsSelected))
		{
			InvalidateImportDependencies();
		}
		ImGui::SameLine();
		ImGui::SetCursorPosX(ImGui::GetCursorPosX() + ImGui::GetTreeNodeToLabelSpacing());
		const auto IconPosition = ImGui::GetCursorScreenPos();
		ImGui::SetCursorPosX(ImGui::GetCursorPosX() + PackageImportWindow::LabelPadding() * ImGui::CalcTextSize(" ").x);
		ImGui::TextUnformatted(Name.c_str());
		const float Size = PackageImportWindow::IconSize();
		PackageImportWindow::DrawIcon(PackageImportWindow::FileIcon(File.Relative), {IconPosition.x, ImGui::GetItemRectMin().y + (ImGui::GetItemRectSize().y - Size) * 0.5f}, Size);
		ImGui::EndDisabled();
		if (ImGui::IsItemHovered(ImGuiHoveredFlags_AllowWhenDisabled))
		{
			PackageImportWindow::DrawTooltip(File);
		}
		ImGui::TableSetColumnIndex(1);
		const ImVec4 Color = !File.Notice.empty() || File.Status == SdkPackages::ImportFile::State::Blocked ? ImVec4(1, 0.5f, 0.45f, 1) : File.Status == SdkPackages::ImportFile::State::Missing ? ImVec4(0.65f, 0.85f, 0.72f, 1)
																																	  : File.Status == SdkPackages::ImportFile::State::Changed	 ? ImVec4(1, 0.8f, 0.45f, 1)
																																																 : ImGui::GetStyleColorVec4(ImGuiCol_TextDisabled);
		const bool CanCompare = File.Notice.empty() && File.Status == SdkPackages::ImportFile::State::Changed;
		if (CanCompare)
		{
			ImGui::PushStyleColor(ImGuiCol_TextLink, Color);
			if (ImGui::TextLink(PackageImportWindow::Status(File)))
			{
				ComparisonIndex = Index;
				ImportComparison = {};
				ComparisonError.clear();
				ShowOnlyComparisonDifferences = true;
				ShouldOpenImportComparison = true;
				Start(Action::CompareImportFile, ImportPlan.Package);
				if (!IsBusy)
				{
					ComparisonError = Message;
				}
			}
			ImGui::PopStyleColor();
		}
		else
		{
			ImGui::TextColored(Color, "%s", PackageImportWindow::Status(File));
		}
		if (ImGui::IsItemHovered())
		{
			PackageImportWindow::DrawTooltip(File);
		}
		ImGui::PopID();
	}
}

void UIPackageManager::DrawImportDetails(bool HasLevelSelection, float Height)
{
	ImGui::BeginChild("ImportDetails", {0, Height}, ImGuiChildFlags_Borders);
	ImGui::TextWrapped("Dependency checks are optional. Import copies the selected files without running this check.");
	if (HasLevelSelection)
	{
		ImGui::BeginDisabled(IsBusy);
		if (ImGui::Button("Check dependencies"))
		{
			Start(Action::CheckImportDependencies, ImportPlan.Package);
		}
		const bool HasUnselected = IsDependencyCheckCurrent && std::any_of(ImportDependencies.Files.begin(), ImportDependencies.Files.end(), [](const auto& File)
																		   { return File.Status == SdkPackages::DependencyStatus::NotSelected; });
		if (HasUnselected)
		{
			ImGui::SameLine();
			if (ImGui::Button("Select dependencies"))
			{
				Start(Action::SelectImportDependencies, ImportPlan.Package);
			}
		}
		ImGui::EndDisabled();
		ImGui::Separator();
		DrawImportDependencies(std::max(80.0f, ImGui::GetContentRegionAvail().y));
	}
	else if (!DependencyCheckError.empty())
	{
		ImGui::TextWrapped("%s", DependencyCheckError.c_str());
	}
	ImGui::EndChild();
}

void UIPackageManager::DrawImportDialog()
{
	if (ShouldOpenImportDialog)
	{
		ImGui::OpenPopup("Import assets");
		ShouldOpenImportDialog = false;
	}
	if (!ImGui::IsPopupOpen("Import assets"))
	{
		return;
	}
	ImGui::SetNextWindowSize({900, 650}, ImGuiCond_FirstUseEver);
	ImGui::SetNextWindowSizeConstraints({660, 480}, {FLT_MAX, FLT_MAX});
	const float TextPadding = ImGui::GetStyle().WindowPadding.x;
	ImGui::PushStyleVar(ImGuiStyleVar_WindowPadding, ImVec2(0, ImGui::GetStyle().WindowPadding.y));
	const bool IsVisible = ImGui::BeginPopupModal("Import assets", nullptr);
	ImGui::PopStyleVar();
	if (!IsVisible)
	{
		return;
	}
	if (ShouldCloseImportDialog)
	{
		ShouldCloseImportDialog = false;
		ImGui::CloseCurrentPopup();
		ImGui::EndPopup();
		return;
	}
	if (ShouldShowImportDependencies)
	{
		IsImportDetailsExpanded = true;
		ShouldShowImportDependencies = false;
	}
	ImGui::Indent(TextPadding);
	ImGui::PushTextWrapPos(ImGui::GetWindowWidth() - TextPadding);
	ImGui::TextUnformatted(ImportPlan.Package.Name.c_str());
	xr_array<size_t, 4> Counts{};
	bool HasLevelSelection = false;
	size_t Selected = 0;
	u64 Bytes = 0;
	for (const auto& File : ImportPlan.Files)
	{
		++Counts[static_cast<size_t>(File.Status)];
		if (File.IsSelected)
		{
			++Selected;
			Bytes += File.Size;
			const auto Lower = SdkPackages::Details::Lower(File.Relative);
			HasLevelSelection |= File.Alias == "$maps$" && (Lower.ends_with(".level") || Lower.find(".part") != xr_string::npos);
		}
	}
	ImGui::BeginDisabled(IsBusy);
	if (ImGui::SmallButton("Select all"))
	{
		for (auto& File : ImportPlan.Files)
		{
			File.IsSelected = PackageImportWindow::CanSelect(File, CanReplaceImportFiles);
		}
		InvalidateImportDependencies();
	}
	ImGui::SameLine();
	if (ImGui::SmallButton("Select none"))
	{
		for (auto& File : ImportPlan.Files)
		{
			File.IsSelected = false;
		}
		InvalidateImportDependencies();
	}
	ImGui::EndDisabled();
	ImGui::SameLine();
	ImGui::TextDisabled("Selected: %zu files, %.2f MiB", Selected, static_cast<double>(Bytes) / (1024 * 1024));
	ImGui::BeginDisabled(IsBusy);
	if (Counts[1])
	{
		if (ImGui::Checkbox("Show existing files", &ShowExistingImportFiles))
		{
			BuildImportTree();
		}
		ImGui::SameLine();
		ImGui::TextDisabled("(%zu)", Counts[1]);
		ImGui::SameLine();
	}
	if (Counts[2])
	{
		if (ImGui::Checkbox("Replace different files", &CanReplaceImportFiles))
		{
			InvalidateImportDependencies();
			if (!CanReplaceImportFiles)
			{
				for (auto& File : ImportPlan.Files)
				{
					if (File.Status == SdkPackages::ImportFile::State::Changed)
					{
						File.IsSelected = false;
					}
				}
			}
		}
		if (ImGui::IsItemHovered())
		{
			ImGui::SetTooltip("Only explicitly selected files are replaced. Originals are backed up.");
		}
		ImGui::SameLine();
	}
	ImGui::EndDisabled();
	if (ImGui::SmallButton("Details"))
	{
		IsImportDetailsExpanded = !IsImportDetailsExpanded;
	}
	if (Counts[3])
	{
		ImGui::TextColored(ImVec4(1, 0.5f, 0.45f, 1), "Cannot import: %zu files. Hover their status for details.", Counts[3]);
	}
	ImGui::PopTextWrapPos();
	ImGui::Unindent(TextPadding);
	ImGui::Separator();
	const float Footer = ImGui::GetFrameHeightWithSpacing() + 8 + (IsBusy ? ImGui::GetTextLineHeightWithSpacing() + ImGui::GetFrameHeightWithSpacing() : 0);
	if (IsImportDetailsExpanded)
	{
		const float Height = std::clamp((ImGui::GetContentRegionAvail().y - Footer) * 0.45f, 100.0f, 260.0f);
		DrawImportDetails(HasLevelSelection, Height);
	}
	const float Height = std::max(80.0f, ImGui::GetContentRegionAvail().y - Footer);
	if (ImportTree.Files.empty())
	{
		ImGui::BeginChild("ImportEmpty", {0, Height}, ImGuiChildFlags_AlwaysUseWindowPadding);
		ImGui::Spacing();
		ImGui::TextWrapped((ImportPlan.Files.empty() ? "This package has no asset files to import." : "All asset files are already in the SDK. No import is needed."));
		ImGui::EndChild();
	}
	else
	{
		ImGui::BeginDisabled(IsBusy);
		if (ImGui::BeginTable("ImportFilesCompact", 2, ImGuiTableFlags_Resizable | ImGuiTableFlags_BordersInnerV | ImGuiTableFlags_RowBg | ImGuiTableFlags_ScrollY | ImGuiTableFlags_PadOuterX, {0, Height}))
		{
			ImGui::TableSetupColumn("Package files", ImGuiTableColumnFlags_WidthStretch);
			ImGui::TableSetupColumn("Status", ImGuiTableColumnFlags_WidthFixed, 120);
			ImGui::TableSetupScrollFreeze(0, 1);
			ImGui::TableHeadersRow();
			DrawImportNode(ImportTree);
			ImGui::EndTable();
		}
		ImGui::EndDisabled();
	}
	ImGui::Indent(TextPadding);
	if (IsBusy)
	{
		const auto Progress = Operations.GetProgress();
		ImGui::TextUnformatted(Progress.Stage.empty() ? Message.c_str() : Progress.Stage.c_str());
		const float Fraction = Progress.BytesTotal ? std::min(1.0f, static_cast<float>(Progress.BytesCompleted) / Progress.BytesTotal) : Progress.FilesTotal ? std::min(1.0f, static_cast<float>(Progress.FilesCompleted) / Progress.FilesTotal)
																																							 : -static_cast<float>(ImGui::GetTime());
		ImGui::ProgressBar(Fraction, {std::max(1.0f, ImGui::GetContentRegionAvail().x - TextPadding), 0}, Fraction < 0 ? "Working..." : nullptr);
		if (!Progress.Detail.empty() && ImGui::IsItemHovered())
		{
			ImGui::SetTooltip("%s", Progress.Detail.c_str());
		}
	}
	ImGui::BeginDisabled(IsBusy || Selected == 0);
	if (ImGui::Button("Import"))
	{
		Start(Action::Import, ImportPlan.Package);
		if (!IsBusy)
		{
			ImportReport = {};
			ImportReport.PackageName = ImportPlan.Package.Name;
			ImportReport.Error = Message;
			ShouldOpenImportReport = true;
			ImGui::CloseCurrentPopup();
		}
	}
	ImGui::EndDisabled();
	ImGui::SameLine();
	if (ImGui::Button("Cancel"))
	{
		if (IsBusy)
		{
			Operations.Cancel();
		}
		ImGui::CloseCurrentPopup();
	}
	ImGui::Unindent(TextPadding);
	DrawImportComparison();
	ImGui::EndPopup();
}

void UIPackageManager::DrawImportComparison()
{
	const char* Title = "File comparison###ImportFileComparison";
	if (ComparisonIndex >= ImportPlan.Files.size())
	{
		return;
	}
	if (ShouldOpenImportComparison)
	{
		ImGui::OpenPopup(Title);
		ShouldOpenImportComparison = false;
	}
	if (!ImGui::IsPopupOpen(Title))
	{
		return;
	}
	ImGui::SetNextWindowSize({std::min(1000.0f, ImGui::GetMainViewport()->WorkSize.x - 40), std::min(600.0f, ImGui::GetMainViewport()->WorkSize.y - 60)}, ImGuiCond_FirstUseEver);
	ImGui::SetNextWindowSizeConstraints({400, 300}, {FLT_MAX, FLT_MAX});
	const float TextPadding = ImGui::GetStyle().WindowPadding.x;
	ImGui::PushStyleVar(ImGuiStyleVar_WindowPadding, ImVec2(0, ImGui::GetStyle().WindowPadding.y));
	bool IsOpen = true;
	const bool IsVisible = ImGui::BeginPopupModal(Title, &IsOpen);
	ImGui::PopStyleVar();
	if (!IsVisible)
	{
		if (!IsOpen && IsBusy)
		{
			Operations.Cancel();
		}
		return;
	}
	const auto& File = ImportPlan.Files[ComparisonIndex];
	xr_vector<size_t> Rows;
	ImGui::Indent(TextPadding);
	ImGui::PushTextWrapPos(ImGui::GetWindowWidth() - TextPadding);
	ImGui::TextWrapped("%s", File.Relative.c_str());
	ImGui::TextWrapped("Package: %s / %s", ImportPlan.Package.Name.c_str(), ImportPlan.Package.Version.c_str());
	if (IsBusy)
	{
		const auto Progress = Operations.GetProgress();
		ImGui::TextUnformatted("Comparing file contents");
		ImGui::ProgressBar(-static_cast<float>(ImGui::GetTime()), {-1, 0}, "Working...");
	}
	else if (!ComparisonError.empty())
	{
		ImGui::TextWrapped("%s", ComparisonError.c_str());
	}
	else
	{
		for (const auto& Notice : ImportComparison.Notices)
		{
			ImGui::TextWrapped("%s", Notice.c_str());
		}
		if (ImportComparison.IsSupported)
		{
			ImGui::Checkbox("Show only differences", &ShowOnlyComparisonDifferences);
			for (size_t Index = 0; Index < ImportComparison.Properties.size(); ++Index)
			{
				if (!ShowOnlyComparisonDifferences || ImportComparison.Properties[Index].IsDifferent)
				{
					Rows.push_back(Index);
				}
			}
			if (Rows.empty())
			{
				ImGui::TextWrapped("Decoded settings match. The file layout or ordering may still differ.");
			}
		}
	}
	ImGui::PopTextWrapPos();
	ImGui::Unindent(TextPadding);
	const float TableHeight = std::max(80.0f, ImGui::GetContentRegionAvail().y - ImGui::GetFrameHeightWithSpacing() * 2);
	if (!Rows.empty() && ImGui::BeginTable("ObjectSettingsComparison", 3, ImGuiTableFlags_Resizable | ImGuiTableFlags_BordersInnerV | ImGuiTableFlags_RowBg | ImGuiTableFlags_ScrollY | ImGuiTableFlags_PadOuterX, {0, TableHeight}))
	{
		ImGui::TableSetupColumn("Property", ImGuiTableColumnFlags_WidthStretch, 0.4f);
		ImGui::TableSetupColumn("Current in SDK", ImGuiTableColumnFlags_WidthStretch, 0.3f);
		ImGui::TableSetupColumn("From package", ImGuiTableColumnFlags_WidthStretch, 0.3f);
		ImGui::TableSetupScrollFreeze(0, 1);
		ImGui::TableHeadersRow();
		ImGuiListClipper Clipper;
		Clipper.Begin(static_cast<int>(Rows.size()));
		while (Clipper.Step())
		{
			for (int Index = Clipper.DisplayStart; Index < Clipper.DisplayEnd; ++Index)
			{
				const auto& Property = ImportComparison.Properties[Rows[Index]];
				ImGui::TableNextRow();
				ImGui::TableSetColumnIndex(0);
				if (Property.IsDifferent)
				{
					ImGui::PushStyleColor(ImGuiCol_Text, ImVec4(1, 0.8f, 0.45f, 1));
				}
				const xr_string Label = xr_string(Property.Section.c_str()) + (Property.Owner.empty() ? xr_string{} : ": " + Property.Owner) + " / " + Property.Name.c_str();
				PackageImportWindow::ComparisonText(Label);
				if (Property.IsDifferent)
				{
					ImGui::PopStyleColor();
				}
				for (int Column = 1; Column <= 2; ++Column)
				{
					ImGui::TableSetColumnIndex(Column);
					const bool IsPresent = Column == 1 ? Property.HasCurrent : Property.HasImported;
					const auto& Value = Column == 1 ? Property.Current : Property.Imported;
					const char* Text = !IsPresent ? "Not stored" : Value.empty()																  ? "Empty value"
															   : Property.IsRawData && Property.IsDifferent && Column == 2 && Property.HasCurrent ? "Changed stored data"
																																				  : Value.c_str();
					PackageImportWindow::ComparisonText(Text);
				}
			}
		}
		ImGui::EndTable();
	}
	if (ImGui::CollapsingHeader("File information"))
	{
		PackageImportWindow::DrawFileInformation(File, Store.GetDestination(ImportPlan.Package));
	}
	ImGui::Indent(TextPadding);
	if (ImGui::Button("Close"))
	{
		if (IsBusy)
		{
			Operations.Cancel();
		}
		ImGui::CloseCurrentPopup();
	}
	ImGui::Unindent(TextPadding);
	ImGui::EndPopup();
}

void UIPackageManager::DrawImportReport()
{
	if (ShouldOpenImportReport)
	{
		ImGui::OpenPopup("Import report");
		ShouldOpenImportReport = false;
	}
	if (!ImGui::IsPopupOpen("Import report"))
	{
		return;
	}
	ImGui::SetNextWindowSize({1080, 620}, ImGuiCond_FirstUseEver);
	ImGui::SetNextWindowSizeConstraints({760, 420}, {FLT_MAX, FLT_MAX});
	if (ImGui::BeginPopupModal("Import report", nullptr))
	{
		ImGui::TextUnformatted(ImportReport.PackageName.c_str());
		ImGui::TextUnformatted(ImportReport.IsSuccess ? "Import completed." : "Import did not complete.");
		xr_array<size_t, 7> Counts{};
		xr_vector<size_t> Rows;
		for (size_t Index = 0; Index < ImportReport.Files.size(); ++Index)
		{
			const auto& File = ImportReport.Files[Index];
			++Counts[static_cast<size_t>(File.Outcome)];
			if (File.Outcome != SdkPackages::ImportOutcome::Identical)
			{
				Rows.push_back(Index);
			}
		}
		ImGui::TextWrapped("%zu added / %zu replaced / %zu identical / %zu kept / %zu skipped / %zu rolled back / %zu failed", Counts[0], Counts[1], Counts[2], Counts[3], Counts[4], Counts[5], Counts[6]);
		if (!ImportReport.Error.empty())
		{
			ImGui::TextWrapped("%s", ImportReport.Error.c_str());
		}
		if (!ImportReport.Path.empty())
		{
			ImGui::TextWrapped("Report and backups: %s", SdkPackages::Utf8(ImportReport.Path.parent_path()).c_str());
		}
		if (Counts[static_cast<size_t>(SdkPackages::ImportOutcome::Identical)])
		{
			ImGui::TextDisabled("Identical files are omitted from the list.");
		}
		const float Footer = ImGui::GetFrameHeightWithSpacing();
		if (ImGui::BeginTable("ImportResults", 3, ImGuiTableFlags_Resizable | ImGuiTableFlags_BordersInnerV | ImGuiTableFlags_RowBg | ImGuiTableFlags_ScrollY | ImGuiTableFlags_PadOuterX, {0, std::max(100.0f, ImGui::GetContentRegionAvail().y - Footer)}))
		{
			ImGui::TableSetupColumn("Package file", ImGuiTableColumnFlags_WidthStretch, 0.4f);
			ImGui::TableSetupColumn("Result", ImGuiTableColumnFlags_WidthStretch, 0.12f);
			ImGui::TableSetupColumn("Reason", ImGuiTableColumnFlags_WidthStretch, 0.48f);
			ImGui::TableSetupScrollFreeze(0, 1);
			ImGui::TableHeadersRow();
			ImGuiListClipper Clipper;
			Clipper.Begin(static_cast<int>(Rows.size()));
			while (Clipper.Step())
			{
				for (int Index = Clipper.DisplayStart; Index < Clipper.DisplayEnd; ++Index)
				{
					const auto& File = ImportReport.Files[Rows[Index]];
					ImGui::TableNextRow();
					ImGui::TableSetColumnIndex(0);
					ImGui::TextUnformatted(File.Relative.c_str());
					if (ImGui::IsItemHovered())
					{
						ImGui::SetTooltip("Destination: %s\nBackup: %s", SdkPackages::Utf8(File.Destination).c_str(), File.Backup.empty() ? "-" : SdkPackages::Utf8(File.Backup).c_str());
					}
					ImGui::TableSetColumnIndex(1);
					ImGui::TextUnformatted(SdkPackages::ImportOutcomeName(File.Outcome));
					ImGui::TableSetColumnIndex(2);
					ImGui::TextUnformatted(File.Reason.c_str());
					if (ImGui::IsItemHovered())
					{
						ImGui::SetTooltip("%s", File.Reason.c_str());
					}
				}
			}
			ImGui::EndTable();
		}
		if (ImGui::Button("Close"))
		{
			ImGui::CloseCurrentPopup();
		}
		ImGui::EndPopup();
	}
}
