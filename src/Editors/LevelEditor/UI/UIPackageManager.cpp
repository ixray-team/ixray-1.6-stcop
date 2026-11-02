#include "stdafx.h"
#include "UIPackageManager.h"
#include "UIPackageCard.h"
#include "../Packages/PackageMarkdown.h"
#include "../Plugins/PluginManager.h"
#include "../Plugins/PluginsUIRun.h"
#include <cfloat>

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
	ImGui::PushID(Label);
	const bool IsPressed = ImGui::Selectable("##PaddedRow", IsSelected, ImGuiSelectableFlags_None, {0, Height});
	ImGui::PopID();
	const auto Bounds = ImGui::GetItemRectMax();
	const float Padding = TextPadding();
	auto* DrawList = ImGui::GetWindowDrawList();
	DrawList->PushClipRect({Position.x + Padding, Position.y}, {std::max(Position.x + Padding, Bounds.x - Padding), Bounds.y}, true);
	DrawList->AddText({Position.x + Padding, Position.y}, ImGui::GetColorU32(ImGuiCol_Text), Label);
	DrawList->PopClipRect();
	return IsPressed;
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
		case SdkPackages::PackageSource::Zip: return "ZIP archive";
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
	SdkPackages::PackageOperations::Result Result;
	if (Operations.Poll(Result))
	{
		Message = Result.Message;
		if (Result.IsSuccess && Result.Operation == Action::SyncCatalog)
		{
			CheckedCommits.clear();
		}
		const auto& Info = Result.Package;
		if ((Result.IsSuccess || Result.Operation == Action::CheckUpdates) && Result.Operation != Action::SyncCatalog && Info.Source == SdkPackages::PackageSource::GitHub)
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
		const bool HasChanged = Result.IsSuccess && Result.Operation != Action::SyncCatalog && Result.Operation != Action::CheckUpdates;
		NeedsPluginRefresh |= HasChanged && Info.Kind == SdkPackages::PackageKind::Plugin;
		const bool HasInstalledPackage = Result.IsSuccess && Result.Operation == Action::Install;
		if (HasInstalledPackage)
		{
			SelectedKey.clear();
			ActiveView = SdkPackages::CatalogView::All;
			Search[0] = '\0';
		}
		IsBusy = false;
		Refresh(false);
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
	if (NeedsPluginRefresh && !EContext.UI->HasWindow<CPluginUIRun>())
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
	if (!Operations.Start(Operation, Info, IsFromCatalog, Error))
	{
		Message = Error;
		IsSucceeded = false;
		return;
	}
	IsBusy = true;
	IsSucceeded = true;
	Message = Operation == Action::SyncCatalog ? "Synchronizing SDK catalog..." : Operation == Action::CheckUpdates				  ? "Checking for package updates..."
																			  : Operation == Action::Remove						  ? "Removing package..."
																			  : Info.Source == SdkPackages::PackageSource::GitHub ? "Downloading package from GitHub..."
																																  : "Reading local package...";
}

void UIPackageManager::Draw()
{
	ImGui::SetNextWindowSize({1180, 720}, ImGuiCond_FirstUseEver);
	ImGui::SetNextWindowSizeConstraints({940, 520}, {FLT_MAX, FLT_MAX});
	if (ImGui::Begin("Package Manager", &bOpen))
	{
		DrawToolbar();
		ImGui::Separator();
		const float FooterHeight = ImGui::GetTextLineHeightWithSpacing() * 4;
		ImGui::BeginChild("PackageBrowser", {0, -FooterHeight});
		const float PaneHeight = ImGui::GetContentRegionAvail().y;
		if (ImGui::BeginTable("PackageColumns", 3, ImGuiTableFlags_Resizable | ImGuiTableFlags_BordersInnerV | ImGuiTableFlags_SizingFixedFit))
		{
			ImGui::TableSetupColumn("Navigation", ImGuiTableColumnFlags_WidthFixed, 175);
			ImGui::TableSetupColumn("Packages", ImGuiTableColumnFlags_WidthFixed, 300);
			ImGui::TableSetupColumn("Details", ImGuiTableColumnFlags_WidthStretch);
			ImGui::TableNextRow();
			ImGui::TableSetColumnIndex(0);
			ImGui::BeginChild("PackageNavigation", {0, PaneHeight});
			DrawNavigation();
			ImGui::EndChild();
			ImGui::TableSetColumnIndex(1);
			ImGui::BeginChild("PackageList", {0, PaneHeight});
			DrawList();
			ImGui::EndChild();
			ImGui::TableSetColumnIndex(2);
			ImGui::BeginChild("PackageDetails", {0, PaneHeight});
			DrawDetails();
			ImGui::EndChild();
			ImGui::EndTable();
		}
		ImGui::EndChild();
		ImGui::Separator();
		ImGui::BeginChild("PackageStatus", {0, 0});
		if (IsBusy)
		{
			const auto Progress = Operations.GetProgress();
			ImGui::TextUnformatted(Progress.Stage.empty() ? Message.c_str() : Progress.Stage.c_str());
			if (Progress.PackageCount)
			{
				ImGui::SameLine();
				ImGui::TextDisabled("(%zu / %zu completed; %zu active)", Progress.PackagesCompleted, Progress.PackageCount, Progress.ActivePackages);
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
				xr_sprintf(Label, "%.2f / %.2f MiB", static_cast<double>(Progress.BytesCompleted) / (1024 * 1024), static_cast<double>(Progress.BytesTotal) / (1024 * 1024));
				const float Fraction = std::min(1.0f, static_cast<float>(Progress.BytesCompleted) / static_cast<float>(Progress.BytesTotal));
				ImGui::ProgressBar(Fraction, {-1, 0}, Label);
			}
		}
		else if (Message.empty())
		{
			ImGui::TextDisabled("%zu packages available / %zu installed in this SDK", Packages.size(), Installed.size());
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
	}
	ImGui::End();
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
	ImGui::SameLine();
	ImGui::TextDisabled("Shared SDK library");
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
		ImGui::SetCursorPosY(RowY + ImGui::GetTextLineHeightWithSpacing());
		ImGui::SetCursorPosX(PackageWindow::TextPadding());
		if (!Row->IsMetadataAvailable)
		{
			ImGui::TextDisabled("Manifest unavailable");
		}
		else
		{
			ImGui::TextDisabled("%s / %s", Info.Version.c_str(), Row->IsInstalled ? "Installed" : Row->HasSourceConflict ? "ID conflict"
																														 : "Available");
		}
		ImGui::SetCursorPosY(RowY + RowHeight + ImGui::GetStyle().ItemSpacing.y);
		ImGui::PopID();
	}
	if (Visible.empty())
	{
		ImGui::SetCursorPosX(PackageWindow::TextPadding());
		ImGui::TextWrapped(Search[0] ? "No packages match this search." : ActiveView == SdkPackages::CatalogView::My	  ? "No packages installed. Browse All packages or add one with +."
																	  : ActiveView == SdkPackages::CatalogView::Community ? "No community packages in this catalog yet."
																														  : "No packages in this view.");
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
		ImGui::TextWrapped("Browse SDK tools and asset collections, or add a package from GitHub, a local folder or ZIP.");
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
	const char* ActionLabel = !Row.IsInstalled ? "Install package" : IsCurrent																										  ? "Up to date"
																 : IsUnknown																										  ? "Check updates"
																 : (UpdateState == SdkPackages::PackageUpdateState::Local || UpdateState == SdkPackages::PackageUpdateState::Unknown) ? "Reload package"
																																													  : "Update package";
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
		if (ImGui::Button("Remove package"))
		{
			ImGui::OpenPopup("Remove managed package?");
		}
	}
	if (ImGui::BeginPopupModal("Remove managed package?", nullptr, ImGuiWindowFlags_AlwaysAutoResize))
	{
		ImGui::Text("Remove %s?", Info.Name.c_str());
		ImGui::TextUnformatted("Only this package's directory will be removed. Locally changed files prevent removal.");
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
			ImGui::TextDisabled("%s / %s", Channel, Row.IsInstalled ? "Installed" : "Available");
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
				ImGui::TextWrapped("Stored in the shared SDK library after installation. Asset importing into projects will be added later.");
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
		for (auto Source : {SdkPackages::PackageSource::GitHub, SdkPackages::PackageSource::Folder, SdkPackages::PackageSource::Zip})
		{
			const char* Label = Source == SdkPackages::PackageSource::GitHub ? "Install from GitHub..." : Source == SdkPackages::PackageSource::Folder ? "Install from folder..."
																																					   : "Install from ZIP...";
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
			ImGui::TextUnformatted(AddSource == SdkPackages::PackageSource::Folder ? "Package folder or package.json" : "ZIP file");
			const float BrowseWidth = ImGui::CalcTextSize("Browse...").x + ImGui::GetStyle().FramePadding.x * 2;
			ImGui::SetNextItemWidth(-BrowseWidth - ImGui::GetStyle().ItemSpacing.x);
			ImGui::InputText("##LocalPackagePath", LocalPath, sizeof(LocalPath));
			ImGui::SameLine();
			if (ImGui::Button("Browse..."))
			{
				xr_path Initial = SdkPackages::Presentation::IsUtf8(LocalPath) ? SdkPackages::FromUtf8(LocalPath) : PackageWindow::SdkRoot();
				std::error_code Code;
				if (!std::filesystem::is_directory(Initial, Code))
				{
					Initial = Initial.parent_path();
				}
				if (Initial.empty() || !std::filesystem::is_directory(Initial, Code))
				{
					Initial = PackageWindow::SdkRoot();
				}
				LoadBrowser(Initial);
				ImGui::OpenPopup("Select package source");
			}
			ImGui::TextWrapped("The package must contain package.json. The original folder or ZIP is kept in place.");
			DrawLocalBrowser();
		}
		ImGui::Spacing();
		ImGui::Separator();
		ImGui::BeginDisabled(IsBusy || (IsGitHub ? RepositoryUrl[0] == '\0' : LocalPath[0] == '\0'));
		if (ImGui::Button("Install package"))
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
		ImGui::EndPopup();
	}
}

void UIPackageManager::LoadBrowser(const xr_path& Directory)
{
	std::error_code Code;
	const xr_path Resolved = std::filesystem::canonical(Directory, Code);
	if (Code || !std::filesystem::is_directory(Resolved, Code) || Code)
	{
		BrowserError = "Cannot open this directory.";
		return;
	}
	const auto Text = SdkPackages::Utf8(Resolved);
	if (Text.size() >= sizeof(BrowserLocation))
	{
		BrowserError = "Directory path is too long.";
		return;
	}
	BrowserDirectory = Resolved;
	BrowserSelection.clear();
	BrowserError.clear();
	xr_strcpy(BrowserLocation, Text.c_str());
	BrowserEntries.clear();
	xr_dir_iter Iter(Resolved, Code), End;
	for (; !Code && Iter != End; Iter.increment(Code))
	{
		const auto Status = Iter->symlink_status(Code);
		if (Code)
		{
			break;
		}
		if (std::filesystem::is_symlink(Status) || !SdkPackages::Presentation::IsUtf8Path(Iter->path()))
		{
			continue;
		}
		const bool IsDirectory = std::filesystem::is_directory(Status);
		if (IsDirectory || (AddSource == SdkPackages::PackageSource::Zip && std::filesystem::is_regular_file(Status) && SdkPackages::Details::Lower(SdkPackages::Utf8(Iter->path().extension())) == ".zip"))
		{
			BrowserEntries.push_back({Iter->path(), IsDirectory});
		}
		if (BrowserEntries.size() >= 10000)
		{
			BrowserError = "Directory list is limited to 10000 entries. Enter a more specific path above.";
			break;
		}
	}
	if (Code)
	{
		BrowserError = "Some directory entries could not be read.";
	}
	std::sort(BrowserEntries.begin(), BrowserEntries.end(), [](const auto& Left, const auto& Right)
			  {
		if (Left.IsDirectory != Right.IsDirectory) { return Left.IsDirectory; }
		return SdkPackages::Details::Lower(SdkPackages::Utf8(Left.Path)) < SdkPackages::Details::Lower(SdkPackages::Utf8(Right.Path)); });
}

void UIPackageManager::DrawLocalBrowser()
{
	ImGui::SetNextWindowSize({720, 520}, ImGuiCond_FirstUseEver);
	ImGui::SetNextWindowSizeConstraints({620, 420}, {FLT_MAX, FLT_MAX});
	if (ImGui::BeginPopupModal("Select package source"))
	{
		ImGui::SetNextItemWidth(-125);
		const bool ShouldOpenPath = ImGui::InputText("##BrowserDirectory", BrowserLocation, sizeof(BrowserLocation), ImGuiInputTextFlags_EnterReturnsTrue);
		ImGui::SameLine();
		if (ImGui::Button("Open") || ShouldOpenPath)
		{
			if (SdkPackages::Presentation::IsUtf8(BrowserLocation))
			{
				LoadBrowser(SdkPackages::FromUtf8(BrowserLocation));
			}
			else
			{
				BrowserError = "Directory path must be valid UTF-8.";
			}
		}
		ImGui::SameLine();
		if (ImGui::Button("Up") && BrowserDirectory.has_parent_path())
		{
			LoadBrowser(BrowserDirectory.parent_path());
		}
		ImGui::TextDisabled("Double-click a folder to open it.");
		ImGui::Separator();
		ImGui::BeginChild("LocalPackageEntries", {0, -100}, ImGuiChildFlags_Borders);
		xr_path NextDirectory;
		for (const auto& Entry : BrowserEntries)
		{
			const xr_string Name = (Entry.IsDirectory ? xr_string("[Folder] ") : xr_string()) + SdkPackages::Utf8(static_cast<const std::filesystem::path&>(Entry.Path).filename());
			ImGui::PushID(SdkPackages::Utf8(Entry.Path).c_str());
			if (ImGui::Selectable(Name.c_str(), !Entry.IsDirectory && BrowserSelection == Entry.Path, ImGuiSelectableFlags_AllowDoubleClick))
			{
				if (Entry.IsDirectory && ImGui::IsMouseDoubleClicked(ImGuiMouseButton_Left))
				{
					NextDirectory = Entry.Path;
				}
				if (!Entry.IsDirectory)
				{
					BrowserSelection = Entry.Path;
				}
			}
			ImGui::PopID();
		}
		ImGui::EndChild();
		if (!NextDirectory.empty())
		{
			LoadBrowser(NextDirectory);
		}
		if (!BrowserError.empty())
		{
			ImGui::TextWrapped("%s", BrowserError.c_str());
		}
		const bool IsFolder = AddSource == SdkPackages::PackageSource::Folder;
		std::error_code Code;
		const bool HasManifest = std::filesystem::is_regular_file(SdkPackages::Join(BrowserDirectory, "package.json"), Code);
		ImGui::TextWrapped("%s", IsFolder ? (HasManifest ? "Package manifest found in this folder." : "Open the package folder containing package.json.") : "Select a ZIP archive from this folder.");
		const auto Selected = IsFolder ? BrowserDirectory : BrowserSelection;
		ImGui::BeginDisabled(IsFolder ? !HasManifest : Selected.empty());
		if (ImGui::Button(IsFolder ? "Use this folder" : "Use ZIP archive"))
		{
			const auto Path = SdkPackages::Utf8(Selected);
			if (Path.size() < sizeof(LocalPath))
			{
				xr_strcpy(LocalPath, Path.c_str());
				ImGui::CloseCurrentPopup();
			}
			else
			{
				BrowserError = "Package path is too long.";
			}
		}
		ImGui::EndDisabled();
		ImGui::SameLine();
		if (ImGui::Button("Cancel"))
		{
			ImGui::CloseCurrentPopup();
		}
		ImGui::EndPopup();
	}
}
