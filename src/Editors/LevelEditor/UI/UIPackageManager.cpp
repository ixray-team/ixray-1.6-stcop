#include "stdafx.h"
#include "UIPackageManager.h"
#include "UIPackageCard.h"
#include "../Packages/PackageMarkdown.h"
#include "../Plugins/PluginManager.h"
#include "../Plugins/PluginsUIRun.h"
#include "../Editor/Utils/ContentView.h"
#include <cfloat>
#include <SDL3/SDL_dialog.h>

UIPackageManager* UIPackageManager::Form = nullptr;

namespace PackageWindow
{
static xr_path SdkRoot()
{
	string_path Path;
	FS.update_path(Path, "$fs_root$", "");
	std::error_code Code;
	return std::filesystem::absolute(Path, Code).lexically_normal();
}

static xr_path SettingsFile()
{
	string_path Path;
	FS.update_path(Path, "$app_data_root$", "package-manager.json");
	std::error_code Code;
	return std::filesystem::absolute(Path, Code).lexically_normal();
}

static xr_string RevisionKey(const SdkPackages::PackageInfo& Info)
{
	return SdkPackages::Details::Lower(Info.Url) + "#" + Info.Ref;
}

static float TextPadding()
{
	return ImGui::GetFontSize() * 0.6f;
}

static bool PaddedSelectable(const char* Label, bool IsSelected, float Height = 0)
{
	const auto Position = ImGui::GetCursorScreenPos();
	const float PaddingY = ImGui::GetStyle().CellPadding.y;
	ImGui::PushID(Label);
	const bool IsPressed = ImGui::Selectable("##PaddedRow", IsSelected, ImGuiSelectableFlags_None, {0, Height > 0 ? Height : ImGui::GetTextLineHeight() + PaddingY * 2});
	ImGui::PopID();
	const auto Bounds = ImGui::GetItemRectMax();
	const float Padding = TextPadding();
	auto* DrawList = ImGui::GetWindowDrawList();
	DrawList->PushClipRect({Position.x + Padding, Position.y}, {std::max(Position.x + Padding, Bounds.x - Padding), Bounds.y}, true);
	DrawList->AddText({Position.x + Padding, Position.y + PaddingY}, ImGui::GetColorU32(ImGuiCol_Text), Label);
	DrawList->PopClipRect();
	return IsPressed;
}

static void BeginPane(const char* Name, const ImVec2& Size, bool PadSides = false)
{
	const auto Padding = ImGui::GetStyle().WindowPadding;
	ImGui::PushStyleVar(ImGuiStyleVar_WindowPadding, ImVec2(PadSides ? Padding.x : 0, Padding.y));
	ImGui::BeginChild(Name, Size, ImGuiChildFlags_AlwaysUseWindowPadding);
	ImGui::PopStyleVar();
}

static const char* KindLabel(SdkPackages::PackageKind Kind)
{
	switch (Kind)
	{
		case SdkPackages::PackageKind::Plugin: return "SDK plugin";
		case SdkPackages::PackageKind::Assets: return "Assets";
	}
	return "Package";
}

static const char* SourceLabel(SdkPackages::PackageSource Source)
{
	switch (Source)
	{
		case SdkPackages::PackageSource::GitHub: return "GitHub";
		case SdkPackages::PackageSource::Folder: return "Local folder";
		case SdkPackages::PackageSource::Archive: return "ZIP / 7z archive";
	}
	return "Package source";
}
} // namespace PackageWindow

UIPackageManager::UIPackageManager()
	: Store(PackageWindow::SdkRoot()), Catalog(PackageWindow::SdkRoot(), PackageWindow::SettingsFile()), Operations(Store, Catalog)
{
	SetStateName("PackageManager");
	xr_string RecoveryError;
	const bool HasRecovered = Store.Recover(RecoveryError);
	Refresh();
	if (!HasRecovered)
	{
		Message = RecoveryError;
		IsSucceeded = false;
	}
}

UIPackageManager::~UIPackageManager() = default;

void UIPackageManager::Show()
{
	if (!Form)
	{
		Form = new UIPackageManager;
	}
	Form->bOpen = true;
}

void UIPackageManager::Update()
{
	if (Form)
	{
		Form->Poll();
		if (!Form->IsClosed())
		{
			Form->Draw();
		}
	}
}

void UIPackageManager::Destroy()
{
	xr_delete(Form);
}

void UIPackageManager::Refresh(bool ResetChecks)
{
	if (ResetChecks)
	{
		CheckedCommits.clear();
	}
	CardDocumentsKey.clear();
	xr_string Error;
	Installed = Store.List(Error);
	xr_vector<SdkPackages::CatalogPackage> Entries;
	xr_string CatalogError;
	Catalog.Load(Entries, CatalogError);
	Packages = SdkPackages::PackageCatalog::Merge(Entries, Installed);
	if (!Error.empty() || !CatalogError.empty())
	{
		const auto& LoadError = !Error.empty() ? Error : CatalogError;
		if (!ResetChecks && !Message.empty())
		{
			Message += " ";
			Message += LoadError;
		}
		else
		{
			Message = LoadError;
		}
		IsSucceeded = false;
	}
}

void UIPackageManager::Poll()
{
	PollLocalSource();
	SdkPackages::PackageOperations::Result Result;
	if (Operations.Poll(Result))
	{
		Message = Result.Message;
		if (Result.IsSuccess && Result.Operation == Action::SyncCatalog)
		{
			CheckedCommits.clear();
		}
		const auto& Info = Result.Package;
		if (Result.Operation == Action::CompareImportFile)
		{
			ImportComparison = std::move(Result.Comparison);
			ComparisonError = Result.IsSuccess ? xr_string{} : Result.Message;
		}
		if ((Result.IsSuccess || Result.Operation == Action::CheckUpdates) && Result.Operation != Action::SyncCatalog && Result.Operation != Action::CompareImportFile && Info.Source == SdkPackages::PackageSource::GitHub)
		{
			const auto Key = PackageWindow::RevisionKey(Info);
			const xr_string Commit = Result.Operation == Action::CheckUpdates ? Result.CheckedCommit : Result.Operation == Action::Remove ? xr_string{}
																																		  : Info.Commit;
			if (Result.IsSuccess && !Commit.empty())
			{
				CheckedCommits[Key] = Commit;
			}
			else
			{
				CheckedCommits.erase(Key);
			}
		}
		IsSucceeded = Result.IsSuccess;
		const bool HasChanged = Result.IsSuccess && (Result.Operation == Action::Install || Result.Operation == Action::Update || Result.Operation == Action::Remove);
		if (Result.IsSuccess && Result.Operation == Action::PrepareImport)
		{
			ImportPlan = std::move(Result.Import);
			CanReplaceImportFiles = false;
			ShowExistingImportFiles = false;
			IsImportDetailsExpanded = false;
			ShouldCloseImportDialog = false;
			ShouldOpenImportComparison = false;
			ComparisonIndex = std::numeric_limits<size_t>::max();
			ImportComparison = {};
			ComparisonError.clear();
			ImportDependencies = {};
			IsDependencyCheckCurrent = false;
			DependencyCheckError.clear();
			ShouldShowImportDependencies = false;
			BuildImportTree();
			ShouldOpenImportDialog = true;
		}
		if (Result.Operation == Action::CheckImportDependencies || Result.Operation == Action::SelectImportDependencies)
		{
			if (Result.IsSuccess && Result.Operation == Action::SelectImportDependencies)
			{
				ImportPlan = std::move(Result.Import);
			}
			ImportDependencies = std::move(Result.Dependencies);
			IsDependencyCheckCurrent = Result.HasDependencyResult;
			DependencyCheckError = Result.IsSuccess ? xr_string{} : Result.Message;
			ShouldShowImportDependencies = true;
		}
		if (Result.Operation == Action::Import && !Result.ImportSummary.PackageName.empty())
		{
			ImportReport = std::move(Result.ImportSummary);
			ShouldCloseImportDialog = true;
			ShouldOpenImportReport = !ImportReport.IsSuccess;
			size_t ImportedFiles = 0;
			xr_vector<xr_path> Changed;
			for (const auto& File : ImportReport.Files)
			{
				if (File.Outcome == SdkPackages::ImportOutcome::Added || File.Outcome == SdkPackages::ImportOutcome::Replaced)
				{
					++ImportedFiles;
				}
				if (File.Outcome == SdkPackages::ImportOutcome::Added || File.Outcome == SdkPackages::ImportOutcome::Replaced || File.Outcome == SdkPackages::ImportOutcome::Failed)
				{
					if (!File.Destination.empty())
					{
						Changed.push_back(File.Destination);
					}
				}
			}
			if (ImportReport.IsSuccess)
			{
				string128 Summary;
				xr_sprintf(Summary, "Imported %zu files.", ImportedFiles);
				Message = Summary;
			}
			if (!Changed.empty())
			{
				SdkPackages::PackageImporter::RefreshFilesystem(Changed);
				CContentView::NotifyFilesChanged();
			}
			if (!ImportReport.Path.empty())
			{
				Msg("* [Packages] Import report: %s", SdkPackages::Utf8(ImportReport.Path).c_str());
			}
		}
		NeedsPluginRefresh |= HasChanged && Info.Kind == SdkPackages::PackageKind::Plugin;
		const bool HasInstalledPackage = Result.IsSuccess && Result.Operation == Action::Install;
		if (HasInstalledPackage)
		{
			SelectedKey.clear();
			ActiveView = SdkPackages::CatalogView::All;
			Search[0] = '\0';
		}
		IsBusy = false;
		if (Result.Operation != Action::CompareImportFile)
		{
			Refresh(false);
		}
		if (HasInstalledPackage)
		{
			const auto Selected = std::find_if(Packages.begin(), Packages.end(), [&](const auto& Row)
											   { return Row.IsInstalled && Row.Package.Id == Info.Id; });
			if (Selected != Packages.end())
			{
				SelectedKey = Selected->Key;
			}
		}
		Msg("%s [Packages] %s", IsSucceeded ? "*" : "!", Message.c_str());
	}
	// Reinitialization invalidates pointers retained by open plugin windows.
	if (NeedsPluginRefresh && !IsBusy && !EContext.UI->HasWindow<CPluginUIRun>())
	{
		CPluginsManagers::Instance().Reinit();
		NeedsPluginRefresh = false;
	}
}

void UIPackageManager::Start(Action Operation, const SdkPackages::PackageInfo& Info, bool IsFromCatalog)
{
	if (IsBusy)
	{
		return;
	}
	xr_string Error;
	if (!Operations.Start(Operation, Info, IsFromCatalog, Error, (Operation == Action::Import || Operation == Action::CheckImportDependencies || Operation == Action::SelectImportDependencies || Operation == Action::CompareImportFile) ? ImportPlan : SdkPackages::ImportPlan{}, Operation == Action::PrepareImport ? SdkPackages::PackageImporter::CaptureRoots() : xr_vector<SdkPackages::ImportRoot>{}, Operation == Action::CompareImportFile ? ComparisonIndex : std::numeric_limits<size_t>::max()))
	{
		Message = Error;
		IsSucceeded = false;
		return;
	}
	IsBusy = true;
	IsSucceeded = true;
	Message = Operation == Action::CompareImportFile ? "Comparing file contents" : Operation == Action::SelectImportDependencies   ? "Selecting level dependencies..."
																			   : Operation == Action::CheckImportDependencies	   ? "Checking level dependencies..."
																			   : Operation == Action::PrepareImport				   ? "Preparing asset import..."
																			   : Operation == Action::Import					   ? "Importing selected assets..."
																			   : Operation == Action::SyncCatalog				   ? "Synchronizing SDK catalog..."
																			   : Operation == Action::CheckUpdates				   ? "Checking for package updates..."
																			   : Operation == Action::Remove					   ? "Removing package..."
																			   : Info.Source == SdkPackages::PackageSource::GitHub ? "Downloading package from GitHub..."
																																   : "Reading local package...";
}

void UIPackageManager::Draw()
{
	const auto& Style = ImGui::GetStyle();
	ImGui::PushStyleVar(ImGuiStyleVar_WindowPadding, ImVec2(std::max(10.0f, Style.WindowPadding.x), std::max(8.0f, Style.WindowPadding.y)));
	ImGui::PushStyleVar(ImGuiStyleVar_CellPadding, ImVec2(std::max(6.0f, Style.CellPadding.x), std::max(3.0f, Style.CellPadding.y)));
	ImGui::SetNextWindowSize({1180, 720}, ImGuiCond_FirstUseEver);
	ImGui::SetNextWindowSizeConstraints({940, 520}, {FLT_MAX, FLT_MAX});
	if (ImGui::Begin("Package Manager", &bOpen))
	{
		DrawToolbar();
		ImGui::Separator();
		const float FooterHeight = ImGui::GetTextLineHeightWithSpacing() * 4 + ImGui::GetStyle().WindowPadding.y * 2;
		ImGui::BeginChild("PackageBrowser", {0, -FooterHeight});
		const float PaneHeight = ImGui::GetContentRegionAvail().y;
		ImGui::PushStyleVar(ImGuiStyleVar_CellPadding, ImVec2(0, 0));
		const bool HasColumns = ImGui::BeginTable("PackageColumns", 3, ImGuiTableFlags_Resizable | ImGuiTableFlags_BordersInnerV | ImGuiTableFlags_SizingFixedFit | ImGuiTableFlags_NoPadOuterX);
		if (HasColumns)
		{
			ImGui::TableSetupColumn("Navigation", ImGuiTableColumnFlags_WidthFixed, 175);
			ImGui::TableSetupColumn("Packages", ImGuiTableColumnFlags_WidthFixed, 300);
			ImGui::TableSetupColumn("Details", ImGuiTableColumnFlags_WidthStretch);
			ImGui::TableNextRow();
		}
		ImGui::PopStyleVar();
		if (HasColumns)
		{
			ImGui::TableSetColumnIndex(0);
			PackageWindow::BeginPane("PackageNavigation", {0, PaneHeight});
			DrawNavigation();
			ImGui::EndChild();
			ImGui::TableSetColumnIndex(1);
			PackageWindow::BeginPane("PackageList", {0, PaneHeight});
			DrawList();
			ImGui::EndChild();
			ImGui::TableSetColumnIndex(2);
			PackageWindow::BeginPane("PackageDetails", {0, PaneHeight}, true);
			DrawDetails();
			ImGui::EndChild();
			ImGui::EndTable();
		}
		ImGui::EndChild();
		ImGui::Separator();
		PackageWindow::BeginPane("PackageStatus", {0, 0});
		if (IsBusy)
		{
			const auto Progress = Operations.GetProgress();
			ImGui::TextUnformatted(Progress.Stage.empty() ? Message.c_str() : Progress.Stage.c_str());
			if (Progress.PackageCount)
			{
				ImGui::SameLine();
				ImGui::TextDisabled("(%zu / %zu completed; %zu active)", Progress.PackagesCompleted, Progress.PackageCount, Progress.ActivePackages);
			}
			else if (Progress.FilesTotal)
			{
				ImGui::SameLine();
				ImGui::TextDisabled("(%zu / %zu files)", Progress.FilesCompleted, Progress.FilesTotal);
			}
			if (!Progress.Detail.empty())
			{
				ImGui::TextDisabled("%s", Progress.Detail.c_str());
				if (ImGui::IsItemHovered())
				{
					ImGui::SetTooltip("%s", Progress.Detail.c_str());
				}
			}
			if (Progress.BytesTotal)
			{
				string128 Label;
				const float Fraction = std::min(1.0f, static_cast<float>(Progress.BytesCompleted) / static_cast<float>(Progress.BytesTotal));
				xr_sprintf(Label, "%.0f%% | %.2f / %.2f MiB", Fraction * 100, static_cast<double>(Progress.BytesCompleted) / (1024 * 1024), static_cast<double>(Progress.BytesTotal) / (1024 * 1024));
				ImGui::ProgressBar(Fraction, {-1, 0}, Label);
			}
			else if (Progress.FilesTotal)
			{
				const float Fraction = std::min(1.0f, static_cast<float>(Progress.FilesCompleted) / static_cast<float>(Progress.FilesTotal));
				ImGui::ProgressBar(Fraction, {-1, 0});
			}
			else
			{
				ImGui::ProgressBar(-static_cast<float>(ImGui::GetTime()), {-1, 0}, "Working...");
			}
		}
		else if (Message.empty())
		{
			ImGui::TextDisabled("%zu packages available / %zu in your SDK library", Packages.size(), Installed.size());
		}
		else
		{
			ImGui::PushStyleColor(ImGuiCol_Text, IsSucceeded ? ImVec4(0.65f, 0.85f, 0.72f, 1) : ImVec4(1, 0.5f, 0.45f, 1));
			ImGui::TextWrapped("%s", Message.c_str());
			ImGui::PopStyleColor();
		}
		if (NeedsPluginRefresh)
		{
			ImGui::TextWrapped("Close open plugin windows to refresh the Plugins menu.");
		}
		ImGui::EndChild();
		if (ShouldOpenSourceMenu)
		{
			ImGui::OpenPopup("PackageSources");
			ShouldOpenSourceMenu = false;
		}
		DrawSourceMenu();
		if (ShouldOpenAddDialog)
		{
			ImGui::OpenPopup("Add package");
			ShouldOpenAddDialog = false;
		}
		DrawAddDialog();
		DrawImportDialog();
		DrawImportReport();
	}
	ImGui::End();
	ImGui::PopStyleVar(2);
}

void UIPackageManager::DrawToolbar()
{
	ImGui::BeginDisabled(IsBusy);
	if (ImGui::Button("+ Add package"))
	{
		ShouldOpenSourceMenu = true;
	}
	ImGui::SameLine();
	if (ImGui::Button("Sync catalog"))
	{
		Start(Action::SyncCatalog, {});
	}
	ImGui::EndDisabled();
	if (!ImportReport.PackageName.empty() && !IsBusy)
	{
		ImGui::SameLine();
		if (ImGui::Button("Last import report"))
		{
			ShouldOpenImportReport = true;
		}
	}
	if (IsBusy)
	{
		ImGui::SameLine();
		if (ImGui::Button("Cancel operation"))
		{
			Operations.Cancel();
		}
	}
}

void UIPackageManager::DrawNavigation()
{
	ImGui::SetCursorPosX(PackageWindow::TextPadding());
	ImGui::TextDisabled("LIBRARY");
	ImGui::Spacing();
	for (const auto View : {SdkPackages::CatalogView::All, SdkPackages::CatalogView::My, SdkPackages::CatalogView::Official, SdkPackages::CatalogView::Community})
	{
		if (View == SdkPackages::CatalogView::Official)
		{
			ImGui::Spacing();
			ImGui::Separator();
			ImGui::Spacing();
			ImGui::SetCursorPosX(PackageWindow::TextPadding());
			ImGui::TextDisabled("SOURCES");
		}
		const char* Label = View == SdkPackages::CatalogView::All ? "All packages" : View == SdkPackages::CatalogView::My	  ? "My packages"
																				 : View == SdkPackages::CatalogView::Official ? "Official"
																															  : "Community";
		if (PackageWindow::PaddedSelectable(Label, ActiveView == View))
		{
			ActiveView = View;
		}
	}
	ImGui::Spacing();
	ImGui::Separator();
	ImGui::Spacing();
	ImGui::SetCursorPosX(PackageWindow::TextPadding());
	ImGui::PushTextWrapPos(ImGui::GetCursorPosX() + ImGui::GetContentRegionAvail().x - PackageWindow::TextPadding());
	ImGui::TextWrapped("Packages are shared across projects using this SDK.");
	ImGui::PopTextWrapPos();
}

bool UIPackageManager::Matches(const SdkPackages::PackageRow& Row) const
{
	return SdkPackages::PackageCatalog::Matches(Row, ActiveView, Search);
}

void UIPackageManager::DrawList()
{
	ImGui::SetNextItemWidth(-1);
	ImGui::InputTextWithHint("##PackageSearch", "Search packages", Search, sizeof(Search));
	if (ImGui::SmallButton(IsSortDescending ? "Name Z-A" : "Name A-Z"))
	{
		IsSortDescending = !IsSortDescending;
	}
	xr_vector<const SdkPackages::PackageRow*> Visible;
	for (const auto& Row : Packages)
	{
		if (Matches(Row))
		{
			Visible.push_back(&Row);
		}
	}
	std::sort(Visible.begin(), Visible.end(), [this](const auto* Left, const auto* Right)
			  {
		const auto LeftName = SdkPackages::Details::Lower(Left->Package.Name);
		const auto RightName = SdkPackages::Details::Lower(Right->Package.Name);
		return IsSortDescending ? LeftName > RightName : LeftName < RightName; });
	if (std::none_of(Visible.begin(), Visible.end(), [this](const auto* Row)
					 { return Row->Key == SelectedKey; }))
	{
		SelectedKey = Visible.empty() ? "" : Visible.front()->Key;
	}
	ImGui::SameLine();
	ImGui::TextDisabled("%zu packages", Visible.size());
	ImGui::Separator();
	ImGui::BeginChild("PackageRows", {0, 0});
	const float RowHeight = ImGui::GetTextLineHeightWithSpacing() * 2 + 8;
	for (const auto* Row : Visible)
	{
		const auto& Info = Row->Package;
		ImGui::PushID(Row->Key.c_str());
		const float RowY = ImGui::GetCursorPosY();
		if (PackageWindow::PaddedSelectable(Info.Name.c_str(), SelectedKey == Row->Key, RowHeight))
		{
			SelectedKey = Row->Key;
		}
		ImGui::SetCursorPosY(RowY + ImGui::GetStyle().CellPadding.y + ImGui::GetTextLineHeightWithSpacing());
		ImGui::SetCursorPosX(PackageWindow::TextPadding());
		if (!Row->IsMetadataAvailable)
		{
			ImGui::TextDisabled("Manifest unavailable");
		}
		else
		{
			ImGui::TextDisabled("%s / %s", Info.Version.c_str(), Row->IsInstalled ? (Info.Kind == SdkPackages::PackageKind::Assets ? (Info.Source == SdkPackages::PackageSource::GitHub ? "Downloaded" : "Added") : "Installed") : Row->HasSourceConflict ? "ID conflict"
																																																														  : "Available");
		}
		ImGui::SetCursorPosY(RowY + RowHeight + ImGui::GetStyle().ItemSpacing.y);
		ImGui::PopID();
	}
	if (Visible.empty())
	{
		ImGui::SetCursorPosX(PackageWindow::TextPadding());
		ImGui::PushTextWrapPos(ImGui::GetCursorPosX() + ImGui::GetContentRegionAvail().x - PackageWindow::TextPadding());
		ImGui::TextWrapped(Search[0] ? "No packages match this search." : ActiveView == SdkPackages::CatalogView::My	  ? "Your library is empty. Browse All packages or add one with +."
																	  : ActiveView == SdkPackages::CatalogView::Community ? "No community packages in this catalog yet."
																														  : "No packages in this view.");
		ImGui::PopTextWrapPos();
	}
	ImGui::EndChild();
}

void UIPackageManager::DrawDetails()
{
	const auto Selected = std::find_if(Packages.begin(), Packages.end(), [this](const auto& Row)
									   { return Row.Key == SelectedKey; });
	if (Selected == Packages.end() || !Matches(*Selected))
	{
		ImGui::Spacing();
		ImGui::TextUnformatted("Your SDK library");
		ImGui::Separator();
		ImGui::TextWrapped("Browse SDK tools and asset collections, or add a package from GitHub, a local folder or archive.");
		return;
	}
	const auto& Row = *Selected;
	const auto& Info = Row.Package;
	LoadCardDocuments(Row);
	ImFont* HeadingFont = GUIManager->GetHeadingFont();
	ImGui::PushFont(HeadingFont, HeadingFont ? HeadingFont->LegacySize : ImGui::GetStyle().FontSizeBase * 2.0f);
	ImGui::TextWrapped("%s", Info.Name.c_str());
	ImGui::PopFont();
	ImGui::TextDisabled("%s", Row.IsMetadataAvailable ? Info.Id.c_str() : "Package manifest unavailable");
	if (Row.IsMetadataAvailable)
	{
		ImGui::Text("%s / %s", Info.Version.c_str(), PackageWindow::KindLabel(Info.Kind));
	}
	PackageCardUI::DrawMetadata(Row, !CardDocuments.License.empty(), CardDocumentsError);
	if (!Row.MetadataError.empty())
	{
		ImGui::PushStyleColor(ImGuiCol_Text, ImVec4(1, 0.75f, 0.45f, 1));
		ImGui::TextWrapped("%s", Row.MetadataError.c_str());
		ImGui::PopStyleColor();
	}
	if (!Row.IsMetadataAvailable)
	{
		return;
	}
	if (Row.IsInstalled && !Row.CatalogVersion.empty() && Row.CatalogVersion != Info.Version)
	{
		ImGui::Text("Catalog version: %s", Row.CatalogVersion.c_str());
	}
	ImGui::Spacing();
	ImGui::BeginDisabled(IsBusy || Row.HasSourceConflict);
	const auto Checked = CheckedCommits.find(PackageWindow::RevisionKey(Info));
	const auto UpdateState = SdkPackages::PackageCatalog::UpdateStatus(Row, Checked != CheckedCommits.end() ? Checked->second : xr_string{});
	const bool IsCurrent = Row.IsInstalled && UpdateState == SdkPackages::PackageUpdateState::Current;
	const bool HasCheckedRevision = Checked != CheckedCommits.end() && SdkPackages::GitSource::IsCommit(Checked->second);
	const bool IsUnknown = Row.IsInstalled && UpdateState == SdkPackages::PackageUpdateState::Unknown && !HasCheckedRevision;
	const bool IsAsset = Info.Kind == SdkPackages::PackageKind::Assets;
	if (IsAsset && Row.IsInstalled)
	{
		if (ImGui::Button("Import"))
		{
			Start(Action::PrepareImport, Info);
		}
	}
	if (!Row.IsInstalled || !IsAsset || Info.Source == SdkPackages::PackageSource::GitHub)
	{
		if (IsAsset && Row.IsInstalled)
		{
			ImGui::SameLine();
		}
		const char* ActionLabel = !Row.IsInstalled ? (IsAsset ? "Download" : "Install package") : IsCurrent																										   ? "Up to date"
																							  : IsUnknown																										   ? "Check updates"
																							  : (UpdateState == SdkPackages::PackageUpdateState::Local || UpdateState == SdkPackages::PackageUpdateState::Unknown) ? (IsAsset ? "Reload download" : "Reload package")
																																																				   : (IsAsset ? "Download update" : "Update package");
		ImGui::BeginDisabled(IsCurrent);
		if (ImGui::Button(ActionLabel))
		{
			Start(!Row.IsInstalled ? Action::Install : IsUnknown ? Action::CheckUpdates
																 : Action::Update,
				  Info,
				  Row.HasCatalogEntry);
		}
		ImGui::EndDisabled();
		if (IsCurrent && ImGui::IsItemHovered(ImGuiHoveredFlags_AllowWhenDisabled))
		{
			ImGui::SetTooltip("Installed revision matches the last catalog sync or update check.");
		}
	}
	if (Row.IsInstalled && Info.Source == SdkPackages::PackageSource::GitHub && !IsUnknown)
	{
		ImGui::SameLine();
		if (ImGui::Button("Check updates"))
		{
			Start(Action::CheckUpdates, Info);
		}
	}
	if (Row.IsInstalled)
	{
		ImGui::SameLine();
		if (ImGui::Button(IsAsset && Info.Source == SdkPackages::PackageSource::GitHub ? "Remove download" : "Remove package"))
		{
			ImGui::OpenPopup("Remove managed package?");
		}
	}
	if (ImGui::BeginPopupModal("Remove managed package?", nullptr, ImGuiWindowFlags_AlwaysAutoResize))
	{
		ImGui::Text("Remove %s?", Info.Name.c_str());
		ImGui::TextWrapped("%s", IsAsset ? "The downloaded package will be removed. Imported files are kept." : "Only this package's directory will be removed. Locally changed files prevent removal.");
		if (ImGui::Button("Remove"))
		{
			Start(Action::Remove, Info);
			ImGui::CloseCurrentPopup();
		}
		ImGui::SameLine();
		if (ImGui::Button("Cancel"))
		{
			ImGui::CloseCurrentPopup();
		}
		ImGui::EndPopup();
	}
	ImGui::EndDisabled();
	if (Row.HasSourceConflict)
	{
		ImGui::TextWrapped("Another source with this ID is installed. Manage it in My packages before installing this catalog entry.");
	}
	ImGui::Spacing();
	if (ImGui::BeginTabBar("PackageInformation"))
	{
		if (ImGui::BeginTabItem("Overview"))
		{
			ImGui::Spacing();
			const char* Channel = Row.Channel == SdkPackages::CatalogChannel::Official ? "Official" : Row.Channel == SdkPackages::CatalogChannel::Community ? "Community"
																																							: "Custom source";
			ImGui::TextDisabled("%s / %s", Channel, Row.IsInstalled ? (IsAsset ? (Info.Source == SdkPackages::PackageSource::GitHub ? "Downloaded" : "Added") : "Installed") : "Available");
			if (!Row.Category.empty())
			{
				ImGui::Text("Category: %s", Row.Category.c_str());
			}
			ImGui::Spacing();
			if (!Info.Description.empty())
			{
				ImGui::TextWrapped("%s", Info.Description.c_str());
			}
			else if (CardDocuments.Readme.empty())
			{
				ImGui::TextWrapped("No description provided by this package.");
			}
			if (!CardDocuments.Readme.empty())
			{
				ImGui::Spacing();
				ImGui::Separator();
				ImGui::Spacing();
				if (!CardDocumentsSource.empty())
				{
					ImGui::TextWrapped("%s", CardDocumentsSource.c_str());
					ImGui::Spacing();
				}
				PackageCardUI::DrawDocument(SdkPackages::Markdown::Overview(CardDocuments.Readme), "");
			}
			ImGui::Spacing();
			ImGui::Separator();
			ImGui::Spacing();
			if (Info.Kind == SdkPackages::PackageKind::Assets)
			{
				ImGui::TextWrapped("Download to your shared SDK library, then choose Import to select files for the current SDK. Download updates and removal keep imported files unchanged.");
			}
			else if (Info.Kind == SdkPackages::PackageKind::Plugin)
			{
				ImGui::TextWrapped("Extends the SDK. Installed tools appear in the Plugins menu.");
			}
			ImGui::EndTabItem();
		}
		for (int Document = 0; Document < 2; ++Document)
		{
			const char* Label = Document == 0 ? "Changelog" : "License";
			if (ImGui::BeginTabItem(Label))
			{
				ImGui::Spacing();
				if (!CardDocumentsSource.empty())
				{
					ImGui::TextWrapped("%s", CardDocumentsSource.c_str());
					ImGui::Separator();
					ImGui::Spacing();
				}
				const auto& Text = Document == 0 ? CardDocuments.Changelog : CardDocuments.License;
				PackageCardUI::DrawDocument(Text, "No document preview is available. Sync the catalog to refresh repository documentation.", true);
				ImGui::EndTabItem();
			}
		}
		ImGui::EndTabBar();
	}
	if (!CardDocuments.Notice.empty())
	{
		ImGui::Separator();
		ImGui::TextWrapped("%s", CardDocuments.Notice.c_str());
	}
	if (!CardDocumentsError.empty())
	{
		ImGui::TextColored(ImVec4(1, 0.5f, 0.45f, 1), "%s", CardDocumentsError.c_str());
	}
}

void UIPackageManager::DrawSourceMenu()
{
	if (ImGui::BeginPopup("PackageSources"))
	{
		for (auto Source : {SdkPackages::PackageSource::GitHub, SdkPackages::PackageSource::Folder, SdkPackages::PackageSource::Archive})
		{
			const char* Label = Source == SdkPackages::PackageSource::GitHub ? "Install from GitHub..." : Source == SdkPackages::PackageSource::Folder ? "Install from folder..."
																																					   : "Install from archive...";
			if (ImGui::MenuItem(Label))
			{
				AddSource = Source;
				ShouldOpenAddDialog = true;
			}
		}
		ImGui::EndPopup();
	}
}

void UIPackageManager::DrawAddDialog()
{
	ImGui::SetNextWindowSize({600, 340}, ImGuiCond_FirstUseEver);
	ImGui::SetNextWindowSizeConstraints({480, 280}, {FLT_MAX, FLT_MAX});
	if (ImGui::BeginPopupModal("Add package"))
	{
		ImGui::BeginDisabled(LocalSelection != nullptr);
		const bool IsGitHub = AddSource == SdkPackages::PackageSource::GitHub;
		ImGui::TextUnformatted(PackageWindow::SourceLabel(AddSource));
		ImGui::Separator();
		if (IsGitHub)
		{
			ImGui::TextUnformatted("Repository URL");
			ImGui::SetNextItemWidth(-1);
			ImGui::InputTextWithHint("##PackageRepository", "https://github.com/owner/repository", RepositoryUrl, sizeof(RepositoryUrl));
			ImGui::TextUnformatted("Branch or tag (optional)");
			ImGui::SetNextItemWidth(-1);
			ImGui::InputTextWithHint("##PackageRevision", "Default branch", Revision, sizeof(Revision));
			ImGui::TextUnformatted("Package type without a manifest");
			const char* Kinds[] = {"SDK plugin", "Assets"};
			ImGui::SetNextItemWidth(-1);
			ImGui::Combo("##PackageType", &SelectedKind, Kinds, 2);
			ImGui::TextDisabled("Public GitHub repositories / Git required");
		}
		else
		{
			ImGui::TextUnformatted(AddSource == SdkPackages::PackageSource::Folder ? "Package folder or package.json" : "ZIP or 7z file");
			const float BrowseWidth = ImGui::CalcTextSize("Browse...").x + ImGui::GetStyle().FramePadding.x * 2;
			ImGui::SetNextItemWidth(-BrowseWidth - ImGui::GetStyle().ItemSpacing.x);
			ImGui::InputText("##LocalPackagePath", LocalPath, sizeof(LocalPath));
			ImGui::SameLine();
			if (ImGui::Button("Browse..."))
			{
				ChooseLocalSource();
			}
			ImGui::TextWrapped("The package must contain package.json. The original folder or archive is kept in place.");
			if (!LocalSourceError.empty())
			{
				ImGui::TextWrapped("%s", LocalSourceError.c_str());
			}
		}
		ImGui::Spacing();
		ImGui::Separator();
		ImGui::BeginDisabled(IsBusy || (IsGitHub ? RepositoryUrl[0] == '\0' : LocalPath[0] == '\0'));
		if (ImGui::Button(AddSource == SdkPackages::PackageSource::GitHub && SelectedKind == 1 ? "Download package" : "Add package"))
		{
			SdkPackages::PackageInfo Info;
			Info.Url = RepositoryUrl;
			Info.Ref = Revision;
			Info.Kind = SelectedKind == 1 ? SdkPackages::PackageKind::Assets : SdkPackages::PackageKind::Plugin;
			Info.Source = AddSource;
			if (!IsGitHub)
			{
				Info.LocalPath = LocalPath;
			}
			Start(Action::Install, Info);
			ImGui::CloseCurrentPopup();
		}
		ImGui::EndDisabled();
		ImGui::SameLine();
		if (ImGui::Button("Cancel"))
		{
			ImGui::CloseCurrentPopup();
		}
		ImGui::EndDisabled();
		ImGui::EndPopup();
	}
}

void UIPackageManager::ChooseLocalSource()
{
	if (LocalSelection)
	{
		return;
	}
	LocalSourceError.clear();
	LocalSelection = xr_make_shared<SourceSelection>();
	auto Context = xr_make_unique<xr_shared_ptr<SourceSelection>>(LocalSelection);
	const auto SelectionLambda = [](void* UserData, const char* const* Files, int)
	{
		xr_unique_ptr<xr_shared_ptr<SourceSelection>> Owner(static_cast<xr_shared_ptr<SourceSelection>*>(UserData));
		auto& Selection = **Owner;
		if (!Files)
		{
			Selection.Error = SDL_GetError();
		}
		else if (Files[0])
		{
			Selection.Path = Files[0];
		}
		Selection.IsReady.store(true, std::memory_order_release);
	};
	xr_path InitialPath = LocalPath[0] && SdkPackages::Presentation::IsUtf8(LocalPath) ? SdkPackages::FromUtf8(LocalPath) : PackageWindow::SdkRoot();
	std::error_code Code;
	if (!std::filesystem::is_directory(InitialPath, Code))
	{
		InitialPath = InitialPath.parent_path();
	}
	if (InitialPath.empty() || !std::filesystem::is_directory(InitialPath, Code))
	{
		InitialPath = PackageWindow::SdkRoot();
	}
	const auto Initial = SdkPackages::Utf8(InitialPath);
	auto* Window = SDL_GetWindowFromID(static_cast<SDL_WindowID>(reinterpret_cast<uintptr_t>(ImGui::GetMainViewport()->PlatformHandle)));
	if (AddSource == SdkPackages::PackageSource::Folder)
	{
		SDL_ShowOpenFolderDialog(SelectionLambda, Context.release(), Window, Initial.c_str(), false);
	}
	else
	{
		static const SDL_DialogFileFilter Filters[] = {{"ZIP / 7z", "zip;7z"}};
		SDL_ShowOpenFileDialog(SelectionLambda, Context.release(), Window, Filters, 1, Initial.c_str(), false);
	}
}

void UIPackageManager::PollLocalSource()
{
	if (!LocalSelection || !LocalSelection->IsReady.load(std::memory_order_acquire))
	{
		return;
	}
	LocalSourceError = LocalSelection->Error;
	if (!LocalSelection->Path.empty())
	{
		if (LocalSelection->Path.size() < sizeof(LocalPath))
		{
			xr_strcpy(LocalPath, LocalSelection->Path.c_str());
		}
		else
		{
			LocalSourceError = "Package path is too long.";
		}
	}
	LocalSelection.reset();
}
