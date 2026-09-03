#include "stdafx.h"
#include "UIGitWindow.h"
#include "Editor/Utils/GitIntegration.h"
#include "Editor/Utils/GitLFSConfig.h"

#include "IconsFontAwesome6.h"

UIGitWindow::UIGitWindow()
	: IEditorWnd()
{
	CommitMessage[0] = '\0';
}

UIGitWindow::~UIGitWindow()
{
}

UIGitWindow* UIGitWindow::Form = nullptr;

void UIGitWindow::Update()
{
	if (Form)
	{
		if (!Form->IsClosed())
		{
			Form->BeginDraw();
			Form->Draw();
			Form->EndDraw();
		}
		else
		{
			xr_delete(Form);
		}
	}
}

void UIGitWindow::Show()
{
	if (Form == nullptr)
	{
		Form = new UIGitWindow();
	}

	Form->bOpen = true;
}

void UIGitWindow::Close()
{
	xr_delete(Form);
}

void UIGitWindow::ResetBegin()
{
}

void UIGitWindow::ResetEnd()
{
}

const char* UIGitWindow::GitStatusIcon(const EGitFileInfo& Info)
{
	if (Info.IsDirectory)
	{
		return ICON_FA_FILE_LINES;
	}

	switch (Info.Status)
	{
		case EGitFileStatus::Modified:
			return ICON_FA_FILE_PEN;
		case EGitFileStatus::Added:
			return ICON_FA_FILE_CIRCLE_PLUS;
		case EGitFileStatus::Deleted:
			return ICON_FA_FILE_CIRCLE_MINUS;
		case EGitFileStatus::Renamed:
		case EGitFileStatus::Copied:
			return ICON_FA_ARROW_RIGHT_ARROW_LEFT;
		case EGitFileStatus::Untracked:
			return ICON_FA_FILE_CIRCLE_QUESTION;
		case EGitFileStatus::Ignored:
			return ICON_FA_BAN;
		case EGitFileStatus::Conflicted:
			return ICON_FA_CIRCLE_EXCLAMATION;
		default:
			return ICON_FA_FILE_LINES;
	}
}

void UIGitWindow::Draw()
{
	if (!bOpen)
	{
		return;
	}

	ImGui::SetNextWindowSize(ImVec2(900, 600), ImGuiCond_FirstUseEver);
	ImGui::SetNextWindowPos(ImGui::GetMainViewport()->GetCenter(), ImGuiCond_FirstUseEver, ImVec2(0.5f, 0.5f));

	if (ImGui::Begin("Git Integration", &bOpen, ImGuiWindowFlags_MenuBar))
	{
		if (ImGui::BeginMenuBar())
		{
			if (ImGui::BeginMenu("File"))
			{
				if (ImGui::MenuItem("Close"))
				{
					bOpen = false;
				}
				ImGui::EndMenu();
			}

			if (ImGui::BeginMenu("View"))
			{
				if (ImGui::MenuItem("Refresh"))
				{
					if (Git)
					{
						Git->RefreshStatus(true);
					}
				}
				ImGui::EndMenu();
			}

			ImGui::EndMenuBar();
		}

		if (Git && Git->IsRepository)
		{
			ImGui::Text(ICON_FA_CODE_BRANCH " %s", Git->RepositoryName.c_str());
			ImGui::SameLine();
			ImGui::TextDisabled("(%s)", Git->Branch.c_str());

			if (Git->Ahead > 0 || Git->Behind > 0)
			{
				ImGui::SameLine();
				if (Git->Ahead > 0)
				{
					ImGui::TextColored(ImVec4(1.0f, 0.8f, 0.0f, 1.0f), "(+%d)", Git->Ahead);
				}
				if (Git->Behind > 0)
				{
					ImGui::SameLine();
					ImGui::TextColored(ImVec4(1.0f, 0.4f, 0.0f, 1.0f), "(-%d)", Git->Behind);
				}
			}

			ImGui::SameLine();
			ImGui::TextDisabled("|");
			ImGui::SameLine();
			DrawTaskStatus();
			ImGui::Separator();

			DrawToolbar();
			ImGui::Separator();

			DrawRemotesAndBranches();
			ImGui::Separator();

			// Reserve space for the commit row + collapsed History/LFS sections at the bottom
			const float CommitRowH = ImGui::GetFrameHeightWithSpacing() + ImGui::GetStyle().ItemSpacing.y;
			const float SectionHeaderH = ImGui::GetFrameHeightWithSpacing() + ImGui::GetStyle().ItemSpacing.y;
			const float MiddleH = std::max(200.0f, ImGui::GetContentRegionAvail().y - CommitRowH - SectionHeaderH * 2.0f);

			DrawFilesPanel(0.0f, MiddleH);
			DrawCommitRow();

			if (ImGui::CollapsingHeader("History"))
			{
				DrawHistory();
			}

			if (ImGui::CollapsingHeader("LFS"))
			{
				DrawLFSSettings();
			}

			DrawAddRemoteDialog();
			DrawNewBranchDialog();
		}
		else
		{
			ImGui::Text("Git repository not found or not initialized.");
			ImGui::TextWrapped("Run `git init` in the game data root, then press Retry.");
			if (XRay::ImGui::ToolbarButton("##git_retry", "Retry", nullptr, ImVec2(0, 0), ImDrawFlags_RoundCornersAll))
			{
				if (Git)
				{
					Git->RefreshStatus(true);
				}
			}
			if (ImGui::IsItemHovered())
			{
				ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
			}
		}

		ImGui::End();
	}
}

void UIGitWindow::DrawToolbar()
{
	// Same look as the Select/Add/Move action buttons: a joined group with
	// rounded outer corners, tooltip and hand cursor on hover.
	auto ToolbarBtn = [&](const char* id, const char* label, const char* tooltip, ImDrawFlags flags, auto&& OnClick)
	{
		if (XRay::ImGui::ToolbarButton(id, label, nullptr, ImVec2(0, 0), flags))
		{
			OnClick();
		}

		if (ImGui::IsItemHovered())
		{
			ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
			ImGui::SetTooltip("%s", tooltip);
		}
	};

	ImGui::PushStyleVar(ImGuiStyleVar_ItemSpacing, ImVec2(0, 0));
	ToolbarBtn
	(
		"##git_push", ICON_FA_UPLOAD " Push", "Push commits to remote", ImDrawFlags_RoundCornersLeft, 
		[]()
		{
			if (Git)
			{
				Git->Push();
			}
		}
	);
	ImGui::SameLine();

	ToolbarBtn
	(
		"##git_pull", ICON_FA_DOWNLOAD " Pull", "Pull commits from remote", ImDrawFlags_RoundCornersNone,
		[]()
		{
			if (Git)
			{
				Git->Pull();
			}
		}
	);
	ImGui::SameLine();

	ToolbarBtn
	(
		"##git_fetch", ICON_FA_ARROWS_ROTATE " Fetch", "Fetch from remote without merging", ImDrawFlags_RoundCornersNone, 
		[]()
		{
			if (Git)
			{ 
				Git->Fetch();
			} 
		}
	);
	ImGui::SameLine();

	ToolbarBtn
	(
		"##git_refresh", "Refresh", "Refresh git status", ImDrawFlags_RoundCornersRight, 
		[]()
		{
			if (Git)
			{
				Git->RefreshStatus(true);
			}
		}
	);

	ImGui::PopStyleVar();
}

void UIGitWindow::DrawFilesPanel(float Width, float Height)
{
	if (!Git)
	{
		return;
	}

	xr_vector<std::pair<xr_string, EGitFileInfo>> Files;
	Git->GetFiles(Files);
	std::sort(Files.begin(), Files.end(), [](const auto& A, const auto& B)
			  { return A.first < B.first; });

	if (ImGui::BeginChild("##git_files", ImVec2(Width, Height), ImGuiChildFlags_Border))
	{
		const s64 Now = std::chrono::steady_clock::now().time_since_epoch().count();
		const s64 PendingTimeout = std::chrono::duration_cast<std::chrono::steady_clock::duration>(std::chrono::seconds(15)).count();

		auto ResolveStaged = [&](const xr_string& FilePath, bool Actual) -> bool
		{
			auto It = PendingStage.find(FilePath);
			if (It == PendingStage.end())
			{
				return Actual;
			}

			if (It->second.first == Actual || Now - It->second.second > PendingTimeout)
			{
				PendingStage.erase(It);
				return Actual;
			}

			return It->second.first;
		};

		bool AllStaged = !Files.empty();
		for (const auto& [MarkedPath, MarkedInfo] : Files)
		{
			if (!ResolveStaged(MarkedPath, MarkedInfo.Staged))
			{
				AllStaged = false;
				break;
			}
		}

		char MarkAllLabel[64];
		xr_sprintf(MarkAllLabel, "Files (%d) Mark all", static_cast<int>(Files.size()));
		if (ImGui::Checkbox(MarkAllLabel, &AllStaged))
		{
			for (const auto& [MarkedPath, MarkedInfo] : Files)
			{
				PendingStage[MarkedPath] = {AllStaged, Now};
			}

			if (AllStaged)
			{
				Git->StageAll();
			}
			else
			{
				Git->UnstageAll();
			}
		}

		ImGui::Separator();

		if (ImGui::BeginTable("##git_files_table", 3, ImGuiTableFlags_ScrollY | ImGuiTableFlags_RowBg | ImGuiTableFlags_SizingStretchProp))
		{
			ImGui::TableSetupColumn("##mark", ImGuiTableColumnFlags_WidthFixed, 28.0f);
			ImGui::TableSetupColumn("##file", ImGuiTableColumnFlags_WidthStretch);
			ImGui::TableSetupColumn("##status", ImGuiTableColumnFlags_WidthFixed, 28.0f);

			ImGuiListClipper Clipper;
			Clipper.Begin(static_cast<int>(Files.size()));
			while (Clipper.Step())
			{
				for (int Index = Clipper.DisplayStart; Index < Clipper.DisplayEnd; ++Index)
				{
					const auto& [Path, Info] = Files[Index];

					ImGui::PushID(Index);
					ImGui::TableNextRow();

					// Checkbox: stage/unstage the file
					ImGui::TableSetColumnIndex(0);
					bool Staged = ResolveStaged(Path, Info.Staged);
					if (ImGui::Checkbox("##stage", &Staged))
					{
						PendingStage[Path] = {Staged, Now};

						xr_string Absolute = Git->RepositoryRoot + "/" + Path;
						if (Staged)
						{
							Git->Stage(xr_path(Absolute.c_str()));
						}
						else
						{
							Git->Unstage(xr_path(Absolute.c_str()));
						}
					}

					ImGui::TableSetColumnIndex(1);
					ImGui::TextUnformatted(Path.c_str());
					if (ImGui::IsItemHovered(ImGuiHoveredFlags_DelayShort))
					{
						ImGui::SetTooltip("%s\nStaged: %s\nLFS: %s", GitStatusText(Info), Info.Staged ? "yes" : "no", Info.Lfs ? "yes" : "no");
					}

					ImGui::TableSetColumnIndex(2);
					const char* Icon = GitStatusIcon(Info);
					const float IconW = ImGui::CalcTextSize(Icon).x;
					ImGui::SetCursorPosX(ImGui::GetCursorPosX() + ImGui::GetContentRegionAvail().x - IconW);
					ImGui::TextColored(ImGui::ColorConvertU32ToFloat4(GitStatusColor(Info)), "%s", Icon);

					ImGui::PopID();
				}
			}
			Clipper.End();

			ImGui::EndTable();
		}
	}
	ImGui::EndChild();
}

void UIGitWindow::DrawTaskStatus()
{
	if (!Git)
	{
		return;
	}

	xr_string CurrentTask = Git->TaskName;
	const EGitTaskState CurrentState = Git->TaskState.load();

	ImVec4 StateColor;
	const char* StateText;
	switch (CurrentState)
	{
		case EGitTaskState::Running:
			StateColor = ImVec4(0.0f, 0.5f, 1.0f, 1.0f);
			StateText = "Running";
			break;
		case EGitTaskState::Succeeded:
			StateColor = ImVec4(0.0f, 0.8f, 0.0f, 1.0f);
			StateText = "Succeeded";
			break;
		case EGitTaskState::Failed:
			StateColor = ImVec4(1.0f, 0.0f, 0.0f, 1.0f);
			StateText = "Failed";
			break;
		case EGitTaskState::Idle:
		default:
			StateColor = ImVec4(0.5f, 0.5f, 0.5f, 1.0f);
			StateText = "Idle";
			break;
	}

	ImGui::Text("Task: %s", CurrentTask.empty() ? "Idle" : CurrentTask.c_str());

	const xr_string StatusLabel = xr_string("Status: ") + StateText;
	const float StatusW = ImGui::CalcTextSize(StatusLabel.c_str()).x;
	const float RightX = ImGui::GetContentRegionMax().x - StatusW;

	if (RightX > ImGui::GetCursorPosX())
	{
		ImGui::SameLine(RightX);
	}
	else
	{
		ImGui::SameLine();
	}

	ImGui::TextColored(StateColor, "%s", StatusLabel.c_str());
}

void UIGitWindow::DrawRemotesAndBranches()
{
	if (!Git)
	{
		return;
	}

	xr_vector<xr_string> Remotes;
	xr_vector<xr_string> Branches;
	Git->GetRemotes(Remotes);
	Git->GetBranches(Branches);

	xr_string CurrentRemote;
	xr_string CurrentBranch;
	{
		xrSRWLockGuard Guard(Git->Mutex, true);
		CurrentRemote = Git->SelectedRemote;
		CurrentBranch = Git->Branch;
	}

	ImGui::Text("Remote:");
	ImGui::SameLine();
	ImGui::SetNextItemWidth(140.0f);
	if (ImGui::BeginCombo("##git_remote", CurrentRemote.empty() ? "<none>" : CurrentRemote.c_str()))
	{
		for (const auto& Remote : Remotes)
		{
			const bool Selected = Remote == CurrentRemote;
			if (ImGui::Selectable(Remote.c_str(), Selected))
			{
				xrSRWLockGuard Guard(Git->Mutex);
				Git->SelectedRemote = Remote;
			}
			if (Selected)
			{
				ImGui::SetItemDefaultFocus();
			}
		}
		ImGui::EndCombo();
	}

	ImGui::SameLine();
	if (XRay::ImGui::ToolbarButton("##git_add_remote", ICON_FA_PLUS, nullptr, ImVec2(0, 0), ImDrawFlags_RoundCornersAll))
	{
		ImGui::OpenPopup("Add Remote");
	}
	if (ImGui::IsItemHovered(ImGuiHoveredFlags_DelayShort))
	{
		ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
		ImGui::SetTooltip("Add remote");
	}

	ImGui::SameLine();
	ImGui::Text("Branch:");
	ImGui::SameLine();
	ImGui::SetNextItemWidth(180.0f);
	if (ImGui::BeginCombo("##git_branch", CurrentBranch.empty() ? "<none>" : CurrentBranch.c_str()))
	{
		for (const auto& BranchName : Branches)
		{
			const bool Selected = BranchName == CurrentBranch;
			if (ImGui::Selectable(BranchName.c_str(), Selected))
			{
				if (!Selected)
				{
					Git->CheckoutBranch(BranchName.c_str());
				}
			}
			if (Selected)
			{
				ImGui::SetItemDefaultFocus();
			}
		}
		ImGui::EndCombo();
	}
	if (ImGui::IsItemHovered(ImGuiHoveredFlags_DelayShort))
	{
		ImGui::SetTooltip("Switch branch (checkout)");
	}

	ImGui::SameLine();
	if (XRay::ImGui::ToolbarButton("##git_new_branch", ICON_FA_PLUS, nullptr, ImVec2(0, 0), ImDrawFlags_RoundCornersAll))
	{
		ImGui::OpenPopup("New Branch");
	}
	if (ImGui::IsItemHovered(ImGuiHoveredFlags_DelayShort))
	{
		ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
		ImGui::SetTooltip("Create branch");
	}
}

void UIGitWindow::DrawCommitRow()
{
	if (!Git)
	{
		return;
	}

	const bool CanCommit = strlen(CommitMessage) > 0;

	ImGui::SetNextItemWidth(std::max(100.0f, ImGui::GetContentRegionAvail().x - 110.0f));
	ImGui::InputTextWithHint("##git_commit_msg", "Commit name", CommitMessage, sizeof(CommitMessage));

	ImGui::SameLine();

	if (!CanCommit)
	{
		ImGui::BeginDisabled();
	}

	if (XRay::ImGui::ToolbarButton("##git_commit", "Commit", nullptr, ImVec2(100.0f, 0), ImDrawFlags_RoundCornersAll))
	{
		Git->Commit(CommitMessage);
		CommitMessage[0] = '\0';
	}

	if (ImGui::IsItemHovered())
	{
		ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
		ImGui::SetTooltip("Commit staged files");
	}

	if (!CanCommit)
	{
		ImGui::EndDisabled();
	}
}

void UIGitWindow::DrawHistory()
{
	if (!Git)
	{
		return;
	}

	xr_vector<SGitCommit> Commits;
	Git->GetHistory(Commits);

	ImGui::Text("%d commits", static_cast<int>(Commits.size()));
	ImGui::SameLine();
	if (XRay::ImGui::ToolbarButton("##git_history_refresh", "Refresh", nullptr, ImVec2(0, 0), ImDrawFlags_RoundCornersAll))
	{
		Git->RefreshStatus(true);
	}
	if (ImGui::IsItemHovered(ImGuiHoveredFlags_DelayShort))
	{
		ImGui::SetTooltip("Refresh history");
	}

	ImGui::Separator();

	if (ImGui::BeginChild("##git_history", ImVec2(0, 220.0f), ImGuiChildFlags_Border))
	{
		if (ImGui::BeginTable("##git_history_table", 2,
			ImGuiTableFlags_ScrollY | ImGuiTableFlags_RowBg | ImGuiTableFlags_SizingStretchProp))
		{
			ImGui::TableSetupColumn("##hash", ImGuiTableColumnFlags_WidthFixed, 70.0f);
			ImGui::TableSetupColumn("##subject", ImGuiTableColumnFlags_WidthStretch);

			ImGuiListClipper Clipper;
			Clipper.Begin(static_cast<int>(Commits.size()));
			while (Clipper.Step())
			{
				for (int Index = Clipper.DisplayStart; Index < Clipper.DisplayEnd; ++Index)
				{
					const SGitCommit& Commit = Commits[Index];

					ImGui::PushID(Index);
					ImGui::TableNextRow();

					ImGui::TableSetColumnIndex(0);
					if (ImGui::Selectable(Commit.ShortHash.c_str(), false, ImGuiSelectableFlags_SpanAllColumns))
					{
						ImGui::SetClipboardText(Commit.Hash.c_str());
					}
					if (ImGui::IsItemHovered(ImGuiHoveredFlags_DelayShort))
					{
						ImGui::SetTooltip("Click to copy full hash");
					}

					ImGui::TableSetColumnIndex(1);
					ImGui::TextUnformatted(Commit.Subject.c_str());
					if (ImGui::IsItemHovered(ImGuiHoveredFlags_DelayShort))
					{
						ImGui::SetTooltip("%s (%s)\n%s", Commit.Author.c_str(), Commit.Date.c_str(), Commit.Hash.c_str());
					}

					ImGui::PopID();
				}
			}
			Clipper.End();

			ImGui::EndTable();
		}
	}
	ImGui::EndChild();
}

void UIGitWindow::DrawAddRemoteDialog()
{
	if (!Git)
	{
		return;
	}

	if (ImGui::BeginPopupModal("Add Remote", nullptr, ImGuiWindowFlags_AlwaysAutoResize))
	{
		ImGui::SetNextItemWidth(300.0f);
		ImGui::InputTextWithHint("##git_new_remote_name", "Name (e.g. origin)", NewRemoteName, sizeof(NewRemoteName));
		ImGui::SetNextItemWidth(300.0f);
		ImGui::InputTextWithHint("##git_new_remote_url", "URL", NewRemoteUrl, sizeof(NewRemoteUrl));

		const bool CanAdd = strlen(NewRemoteName) > 0 && strlen(NewRemoteUrl) > 0;
		if (!CanAdd)
		{
			ImGui::BeginDisabled();
		}
		if (XRay::ImGui::ToolbarButton("##git_add_remote_confirm", "Add", nullptr, ImVec2(120.0f, 0), ImDrawFlags_RoundCornersAll))
		{
			Git->AddRemote(NewRemoteName, NewRemoteUrl);
			NewRemoteName[0] = '\0';
			NewRemoteUrl[0] = '\0';
			ImGui::CloseCurrentPopup();
		}
		if (!CanAdd)
		{
			ImGui::EndDisabled();
		}

		ImGui::SameLine();
		if (XRay::ImGui::ToolbarButton("##git_add_remote_cancel", "Cancel", nullptr, ImVec2(120.0f, 0), ImDrawFlags_RoundCornersAll))
		{
			NewRemoteName[0] = '\0';
			NewRemoteUrl[0] = '\0';
			ImGui::CloseCurrentPopup();
		}

		ImGui::EndPopup();
	}
}

void UIGitWindow::DrawNewBranchDialog()
{
	if (!Git)
	{
		return;
	}

	if (ImGui::BeginPopupModal("New Branch", nullptr, ImGuiWindowFlags_AlwaysAutoResize))
	{
		ImGui::SetNextItemWidth(300.0f);
		ImGui::InputTextWithHint("##git_new_branch_name", "Branch name", NewBranchName, sizeof(NewBranchName));
		ImGui::Checkbox("Switch to it", &NewBranchCheckout);

		const bool CanCreate = strlen(NewBranchName) > 0;
		if (!CanCreate)
		{
			ImGui::BeginDisabled();
		}
		if (XRay::ImGui::ToolbarButton("##git_new_branch_confirm", "Create", nullptr, ImVec2(120.0f, 0), ImDrawFlags_RoundCornersAll))
		{
			Git->CreateBranch(NewBranchName, NewBranchCheckout);
			NewBranchName[0] = '\0';
			ImGui::CloseCurrentPopup();
		}
		if (!CanCreate)
		{
			ImGui::EndDisabled();
		}

		ImGui::SameLine();
		if (XRay::ImGui::ToolbarButton("##git_new_branch_cancel", "Cancel", nullptr, ImVec2(120.0f, 0), ImDrawFlags_RoundCornersAll))
		{
			NewBranchName[0] = '\0';
			ImGui::CloseCurrentPopup();
		}

		ImGui::EndPopup();
	}
}

void UIGitWindow::DrawLFSSettings()
{
	CGitLFSConfig& Config = CGitLFSConfig::Instance();

	bool AutoTrack = Config.AutoTrackEnabled;
	if (ImGui::Checkbox("Auto-track files", &AutoTrack))
	{
		Config.AutoTrackEnabled = AutoTrack;
		Config.Save();
	}

	ImGui::Separator();
	ImGui::Text("File Patterns:");

	if (ImGui::BeginTable("LFSPatterns", 3, ImGuiTableFlags_Resizable | ImGuiTableFlags_ScrollY))
	{
		ImGui::TableSetupColumn("Pattern");
		ImGui::TableSetupColumn("Description");
		ImGui::TableSetupColumn("Enabled");
		ImGui::TableHeadersRow();

		for (const auto& Pattern : Config.Patterns)
		{
			ImGui::TableNextRow();
			ImGui::TableSetColumnIndex(0);
			ImGui::Text("%s", Pattern.Pattern.c_str());

			ImGui::TableSetColumnIndex(1);
			ImGui::Text("%s", Pattern.Description.c_str());

			ImGui::TableSetColumnIndex(2);
			bool Enabled = Pattern.Enabled;
			if (ImGui::Checkbox(("##enable_" + Pattern.Pattern).c_str(), &Enabled))
			{
				Config.SetPatternEnabled(Pattern.Pattern, Enabled);
				Config.Save();
			}
		}

		ImGui::EndTable();
	}
}
