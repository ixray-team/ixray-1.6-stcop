#include "stdafx.h"
#include "UILevelSelector.h"

#include "Editor/Utils/GitIntegration.h"
#include "IconsFontAwesome7.h"

#include <ctime>
#include <filesystem>

UILevelSelector* UILevelSelector::Form = nullptr;

namespace LevelSelector
{
	// Matches CLevelAutoBackup: $temp$\autobackup\<level>_YYYY-MM-DD_HH-MM-SS.level
	constexpr const char* BackupFolder = "autobackup\\";
	constexpr const char* BackupExt = ".level";
	constexpr size_t BackupStampSize = 19;

	const ImVec4 BackupColor = ImVec4(0.95f, 0.65f, 0.25f, 1.0f);
	const ImVec4 GitRemoteColor = ImVec4(0.40f, 0.70f, 1.00f, 1.0f);

	// A restored backup is loaded as is, then renamed to the original level
	struct PendingRestore
	{
		xr_string Backup;
		xr_string Original;
	};

	PendingRestore g_PendingRestore;

	// Comparison key: the editor FS is case insensitive and keeps names in lower case,
	// the recent list and the last file name may not
	xr_string MakePathKey(xr_string Path)
	{
		if (IsUTF8(Path.c_str()))
		{
			Path = Platform::UTF8_to_CP1251(Path);
		}

		std::replace(Path.begin(), Path.end(), '/', '\\');
		xr_strlwr(Path);
		return Path;
	}

	bool ContainsNoCase(const xr_string& Text, const char* Pattern)
	{
		if (Pattern[0] == '\0')
		{
			return true;
		}

		xr_string Lower = Text;
		xr_string Needle = Pattern;
		xr_strlwr(Lower);
		xr_strlwr(Needle);
		return Lower.find(Needle) != xr_string::npos;
	}

	xr_string FormatSize(long Size)
	{
		string64 Text;
		if (Size >= 1024 * 1024)
		{
			xr_sprintf(Text, "%.1f MB", float(Size) / (1024.0f * 1024.0f));
		}
		else
		{
			xr_sprintf(Text, "%.1f KB", float(Size) / 1024.0f);
		}
		return Text;
	}

	xr_string FormatTime(time_t Time)
	{
		if (Time == 0)
		{
			return "";
		}

		std::tm LocalTime = {};
#ifdef _WIN32
		localtime_s(&LocalTime, &Time);
#else
		localtime_r(&Time, &LocalTime);
#endif
		string64 Text;
		std::strftime(Text, sizeof(Text), "%d.%m.%Y  %H:%M", &LocalTime);
		return Text;
	}

	xr_string FormatAge(time_t Time)
	{
		const s64 Seconds = s64(std::time(nullptr)) - s64(Time);
		string64 Text;
		if (Seconds < 60)
		{
			xr_strcpy(Text, "just now");
		}
		else if (Seconds < 3600)
		{
			xr_sprintf(Text, "%d min ago", int(Seconds / 60));
		}
		else if (Seconds < 86400)
		{
			xr_sprintf(Text, "%d h ago", int(Seconds / 3600));
		}
		else
		{
			xr_sprintf(Text, "%d d ago", int(Seconds / 86400));
		}
		return Text;
	}

	// YYYY-MM-DD_HH-MM-SS, local time
	time_t ParseStamp(const char* Stamp)
	{
		std::tm Time = {};
		if (sscanf(Stamp, "%d-%d-%d_%d-%d-%d", &Time.tm_year, &Time.tm_mon, &Time.tm_mday, &Time.tm_hour, &Time.tm_min, &Time.tm_sec) != 6)
		{
			return 0;
		}

		Time.tm_year -= 1900;
		Time.tm_mon -= 1;
		Time.tm_isdst = -1;
		return std::mktime(&Time);
	}

	ImU32 MutedColor()
	{
		return ImGui::GetColorU32(ImGuiCol_TextDisabled);
	}

	// Small rounded label, returns its width
	float DrawBadge(ImDrawList* Draw, ImVec2 Pos, const ImVec4& Color, const char* Text)
	{
		const ImVec2 Size = ImGui::CalcTextSize(Text);
		const ImVec2 Pad(6.0f, 1.0f);
		ImVec4 Back = Color;
		Back.w = 0.18f;
		Draw->AddRectFilled(Pos, ImVec2(Pos.x + Size.x + Pad.x * 2.0f, Pos.y + Size.y + Pad.y * 2.0f), ImGui::GetColorU32(Back), 4.0f);
		Draw->AddText(ImVec2(Pos.x + Pad.x, Pos.y + Pad.y), ImGui::GetColorU32(Color), Text);
		return Size.x + Pad.x * 2.0f;
	}

	bool HasGit()
	{
		return Git != nullptr && Git->IsRepository;
	}

	// Parses `git log --name-only --format=@@<hash>\t<author>\t<time>\t<subject>`
	xr_hash_map<xr_string, UILevelSelector::GitRemoteChange> ParseGitLog(const xr_string& Out)
	{
		xr_hash_map<xr_string, UILevelSelector::GitRemoteChange> Changes;
		UILevelSelector::GitRemoteChange Commit;

		size_t Begin = 0;
		while (Begin < Out.size())
		{
			size_t End = Out.find('\n', Begin);
			if (End == xr_string::npos)
			{
				End = Out.size();
			}

			xr_string Line = Out.substr(Begin, End - Begin);
			Begin = End + 1;
			if (!Line.empty() && Line.back() == '\r')
			{
				Line.pop_back();
			}
			if (Line.empty())
			{
				continue;
			}

			if (Line.starts_with("@@"))
			{
				Commit = {};
				size_t From = 2;
				xr_string Parts[3];
				int Count = 0;
				for (; Count < 3; ++Count)
				{
					const size_t Tab = Line.find('\t', From);
					if (Tab == xr_string::npos)
					{
						break;
					}
					Parts[Count] = Line.substr(From, Tab - From);
					From = Tab + 1;
				}

				if (Count == 3)
				{
					Commit.ShortHash = Parts[0];
					Commit.Author = Parts[1];
					Commit.Time = time_t(atoll(Parts[2].c_str()));
					Commit.Subject = Line.substr(From);
				}
				continue;
			}

			std::replace(Line.begin(), Line.end(), '\\', '/');
			xr_strlwr(Line);

			// The log is newest first: the first commit names the change, the rest only count
			UILevelSelector::GitRemoteChange& Change = Changes[Line];
			if (Change.Commits == 0)
			{
				Change = Commit;
			}
			++Change.Commits;
		}

		return Changes;
	}
}

using namespace LevelSelector;

void UILevelSelector::Update()
{
	if (!g_PendingRestore.Backup.empty() && !LUI->LoaderEvent.IsBusy())
	{
		// The loader names the scene after the backup: point it back to the original level
		if (MakePathKey(LTools->m_LastFileName.c_str()) == MakePathKey(g_PendingRestore.Backup))
		{
			LTools->m_LastFileName = g_PendingRestore.Original.c_str();
			Scene->m_RTFlags.set(EScene::flRT_Unsaved | EScene::flRT_Modified, TRUE);
			Msg("* Restored autobackup '%s' into '%s'. Save the level to keep it.", g_PendingRestore.Backup.c_str(), g_PendingRestore.Original.c_str());
		}
		g_PendingRestore = {};
	}

	if (Form == nullptr)
	{
		return;
	}

	if (!Form->IsClosed())
	{
		Form->Draw();
	}
	else
	{
		xr_delete(Form);
	}
}

void UILevelSelector::Show()
{
	if (Form == nullptr)
	{
		Form = new UILevelSelector();
	}

	Form->bOpen = true;
	Form->FocusFilter = true;
	Form->ScrollToSelected = true;
	Form->Refresh();
}

void UILevelSelector::Close()
{
	xr_delete(Form);
}

void UILevelSelector::Refresh()
{
	Levels.clear();
	Folders.clear();
	Selected = -1;

	xr_hash_set<xr_string> RecentSet;
	for (const xr_string& Recent : EPrefs->scene_recent_list)
	{
		RecentSet.insert(MakePathKey(Recent));
	}

	string_path MapsRoot;
	FS.update_path(MapsRoot, _maps_, "");
	const xr_string Current = MakePathKey(LTools->m_LastFileName.c_str());

	FS_FileSet Files;
	FS.file_list(Files, _maps_, FS_ListFiles | FS_ClampExt, "*.level");

	for (const FS_File& File : Files)
	{
		LevelEntry& Entry = Levels.emplace_back();
		Entry.Name = File.name + BackupExt;
		Entry.FullPath = xr_string(MapsRoot) + Entry.Name;
		Entry.Size = File.size;
		Entry.Modified = File.time_write;
		Entry.SizeText = FormatSize(File.size);
		Entry.TimeText = FormatTime(File.time_write);

		const xr_string Key = MakePathKey(Entry.FullPath);
		Entry.Recent = RecentSet.contains(Key);

		if (HasGit())
		{
			Git->BuildRepositoryKey(xr_path(Entry.FullPath), Entry.GitKey);
		}

		const xr_string Utf8 = Platform::ANSI_TO_UTF8(File.name);
		const size_t Slash = Utf8.find_last_of("\\/");
		if (Slash != xr_string::npos)
		{
			Entry.Folder = Utf8.substr(0, Slash);
			Entry.Display = Utf8.substr(Slash + 1);
		}
		else
		{
			Entry.Display = Utf8;
		}

		if (std::find(Folders.begin(), Folders.end(), Entry.Folder) == Folders.end())
		{
			Folders.push_back(Entry.Folder);
		}

		if (Key == Current)
		{
			Selected = int(Levels.size()) - 1;
		}
	}

	std::sort(Folders.begin(), Folders.end());
	if (View == EView::Folder && FolderIndex >= int(Folders.size()))
	{
		View = EView::All;
	}

	ScanBackups();
	StartGitScan();
}

void UILevelSelector::ScanBackups()
{
	string_path Folder;
	FS.update_path(Folder, _temp_, BackupFolder);

	// Level name -> newest backup; the stamp sorts by time
	xr_hash_map<xr_string, xr_string> Newest;
	const size_t Tail = BackupStampSize + xr_strlen(BackupExt);

	std::error_code Error;
	for (const std::filesystem::directory_entry& It : std::filesystem::directory_iterator(Folder, Error))
	{
		if (!It.is_regular_file(Error))
		{
			continue;
		}

		xr_string Name = It.path().filename().string().c_str();
		xr_strlwr(Name);
		if (Name.size() <= Tail + 1 || !Name.ends_with(BackupExt) || Name[Name.size() - Tail - 1] != '_')
		{
			continue;
		}

		xr_string& Best = Newest[Name.substr(0, Name.size() - Tail - 1)];
		if (Best < Name)
		{
			Best = Name;
		}
	}

	for (LevelEntry& Entry : Levels)
	{
		Entry.BackupPath.clear();
		Entry.BackupTime = 0;

		xr_string LevelName = EFS.ExtractFileName(Entry.FullPath.c_str());
		xr_strlwr(LevelName);

		auto It = Newest.find(LevelName);
		if (It == Newest.end())
		{
			continue;
		}

		// Only a backup written after the last save is worth restoring
		const time_t Stamp = ParseStamp(It->second.c_str() + It->second.size() - Tail);
		if (Stamp > Entry.Modified)
		{
			Entry.BackupPath = xr_string(Folder) + It->second;
			Entry.BackupTime = Stamp;
		}
	}
}

void UILevelSelector::StartGitScan()
{
	if (!HasGit())
	{
		return;
	}

	{
		xrCriticalSectionGuard Guard(GitLock);
		if (GitScanning)
		{
			return;
		}
		GitScanning = true;
		GitResolved = false;
		GitUpstream = Git->Upstream;
	}

	if (GitWorker.joinable())
	{
		GitWorker.join();
	}

	const xr_string Root = Git->RepositoryRoot;
	GitWorker = std::jthread([this, Root]()
	{
		// Upstream commits missing in HEAD, newest first. Uses the last fetch, no network access.
		const CGitIntegration::SExecResult Result = Git->Execute("-c core.quotepath=off log HEAD..@{u} --name-only \"--format=@@%h%x09%an%x09%ct%x09%s\"", Root);

		xr_hash_map<xr_string, GitRemoteChange> Changes;
		if (Result.ExitCode == 0)
		{
			Changes = ParseGitLog(Result.Output);
		}

		xrCriticalSectionGuard Guard(GitLock);
		GitChanges = std::move(Changes);
		GitScanning = false;
	});
}

void UILevelSelector::ResolveGitChanges()
{
	xrCriticalSectionGuard Guard(GitLock);
	if (GitResolved || GitScanning)
	{
		return;
	}
	GitResolved = true;

	for (LevelEntry& Entry : Levels)
	{
		Entry.HasGitChange = false;
		Entry.GitChange = {};
		if (Entry.GitKey.empty())
		{
			continue;
		}

		// The level file and its part folder: <level>.level and <level>/...
		const xr_string PartPrefix = Entry.GitKey.substr(0, Entry.GitKey.size() - xr_strlen(BackupExt)) + "/";
		for (const auto& [Key, Change] : GitChanges)
		{
			if (Key != Entry.GitKey && !Key.starts_with(PartPrefix))
			{
				continue;
			}

			const u32 Commits = std::max(Entry.GitChange.Commits, Change.Commits);
			if (!Entry.HasGitChange || Change.Time > Entry.GitChange.Time)
			{
				Entry.GitChange = Change;
			}
			Entry.GitChange.Commits = Commits;
			Entry.HasGitChange = true;
		}
	}
}

bool UILevelSelector::PassFilter(const LevelEntry& Entry) const
{
	switch (View)
	{
	case EView::Recent:
		if (!Entry.Recent)
		{
			return false;
		}
		break;
	case EView::Backup:
		if (Entry.BackupPath.empty())
		{
			return false;
		}
		break;
	case EView::Git:
		if (!Entry.HasGitChange)
		{
			return false;
		}
		break;
	case EView::Folder:
		if (FolderIndex < 0 || FolderIndex >= int(Folders.size()) || Entry.Folder != Folders[FolderIndex])
		{
			return false;
		}
		break;
	default:
		break;
	}

	return ContainsNoCase(Entry.Display, Filter) || ContainsNoCase(Entry.Folder, Filter);
}

void UILevelSelector::OpenLevel(const xr_string& FullPath)
{
	bOpen = false;
	ExecCommand(COMMAND_LOAD, FullPath);
}

void UILevelSelector::RestoreBackup(const LevelEntry& Entry)
{
	if (Entry.BackupPath.empty())
	{
		return;
	}

	g_PendingRestore.Backup = Entry.BackupPath;
	g_PendingRestore.Original = Entry.FullPath;
	OpenLevel(Entry.BackupPath);
}

void UILevelSelector::Browse()
{
	xr_string FileName = LTools->m_LastFileName.c_str();
	if (EFS.GetOpenName(_maps_, FileName, false, 0, -1, "*.level;*.tmp"))
	{
		OpenLevel(FileName);
	}
}

void UILevelSelector::DrawSidebar()
{
	auto Item = [this](const char* Icon, const char* Label, EView ItemView, int ItemFolder, int Count, const ImVec4* Accent)
	{
		ImGui::PushID(int(ItemView) * 10000 + ItemFolder);
		const bool Active = View == ItemView && (ItemView != EView::Folder || FolderIndex == ItemFolder);
		const ImVec2 Pos = ImGui::GetCursorScreenPos();
		const float Height = ImGui::GetTextLineHeight() + 10.0f;
		if (ImGui::Selectable("##view", Active, 0, ImVec2(0, Height)))
		{
			View = ItemView;
			FolderIndex = ItemFolder;
			ScrollToSelected = true;
		}

		ImDrawList* Draw = ImGui::GetWindowDrawList();
		const float TextY = Pos.y + (Height - ImGui::GetTextLineHeight()) * 0.5f;
		const float Right = Pos.x + ImGui::GetContentRegionAvail().x;
		const ImU32 TextColor = ImGui::GetColorU32(ImGuiCol_Text);
		Draw->AddText(ImVec2(Pos.x + 8.0f, TextY), Accent != nullptr ? ImGui::GetColorU32(*Accent) : TextColor, Icon);
		Draw->AddText(ImVec2(Pos.x + 30.0f, TextY), TextColor, Label);

		string16 CountText;
		xr_sprintf(CountText, "%d", Count);
		const float CountW = ImGui::CalcTextSize(CountText).x;
		const ImU32 CountColor = (Accent != nullptr && Count > 0) ? ImGui::GetColorU32(*Accent) : MutedColor();
		Draw->AddText(ImVec2(Right - CountW - 8.0f, TextY), CountColor, CountText);
		ImGui::PopID();
	};

	int RecentCount = 0;
	int BackupCount = 0;
	int GitCount = 0;
	for (const LevelEntry& Entry : Levels)
	{
		RecentCount += Entry.Recent ? 1 : 0;
		BackupCount += Entry.BackupPath.empty() ? 0 : 1;
		GitCount += Entry.HasGitChange ? 1 : 0;
	}

	Item(ICON_FA_LAYER_GROUP, "All levels", EView::All, -1, int(Levels.size()), nullptr);
	Item(ICON_FA_CLOCK_ROTATE_LEFT, "Recent", EView::Recent, -1, RecentCount, nullptr);
	Item(ICON_FA_LIFE_RING, "Newer backup", EView::Backup, -1, BackupCount, &BackupColor);
	if (HasGit())
	{
		Item(ICON_FA_CODE_BRANCH, "Newer in Git", EView::Git, -1, GitCount, &GitRemoteColor);
	}

	ImGui::Dummy(ImVec2(0, 4.0f));
	ImGui::TextDisabled("  FOLDERS");
	ImGui::Dummy(ImVec2(0, 2.0f));

	for (int Index = 0; Index < int(Folders.size()); ++Index)
	{
		const int Count = int(std::count_if(Levels.begin(), Levels.end(), [&](const LevelEntry& Entry) { return Entry.Folder == Folders[Index]; }));
		const char* Label = Folders[Index].empty() ? "(root)" : Folders[Index].c_str();
		Item(ICON_FA_FOLDER, Label, EView::Folder, Index, Count, nullptr);
	}
}

void UILevelSelector::DrawDetails(const LevelEntry& Entry) const
{
	ImGui::BeginTooltip();
	ImGui::TextUnformatted(Entry.Display.c_str());
	ImGui::TextDisabled("%s", Platform::ANSI_TO_UTF8(Entry.FullPath).c_str());

	if (!Entry.BackupPath.empty())
	{
		ImGui::Separator();
		ImGui::TextColored(BackupColor, ICON_FA_LIFE_RING "  Autobackup is newer than the saved level");
		ImGui::TextDisabled("Backup: %s (%s)", FormatTime(Entry.BackupTime).c_str(), FormatAge(Entry.BackupTime).c_str());
		ImGui::TextDisabled("Saved:  %s", Entry.TimeText.c_str());
	}

	if (Entry.HasGitChange)
	{
		ImGui::Separator();
		ImGui::TextColored(GitRemoteColor, ICON_FA_CODE_BRANCH "  %u newer commit(s) in %s", Entry.GitChange.Commits, GitUpstream.empty() ? "upstream" : GitUpstream.c_str());
		ImGui::TextDisabled("%s  %s, %s", Entry.GitChange.ShortHash.c_str(), Entry.GitChange.Author.c_str(), FormatAge(Entry.GitChange.Time).c_str());
		ImGui::TextUnformatted(Entry.GitChange.Subject.c_str());
		ImGui::TextDisabled("Pull in the Git window to get it");
	}

	ImGui::EndTooltip();
}

bool UILevelSelector::DrawList(const xr_vector<int>& Visible)
{
	bool OpenRequested = false;
	ImDrawList* Draw = ImGui::GetWindowDrawList();
	const float LineH = ImGui::GetTextLineHeight();
	const float RowH = LineH * 2.0f + 14.0f;
	const float Pad = 12.0f;

	for (int Index : Visible)
	{
		const LevelEntry& Entry = Levels[Index];
		const bool IsSelected = Index == Selected;

		ImGui::PushID(Index);
		const ImVec2 Pos = ImGui::GetCursorScreenPos();
		const float Width = ImGui::GetContentRegionAvail().x;
		if (ImGui::Selectable("##level", IsSelected, ImGuiSelectableFlags_AllowDoubleClick, ImVec2(0, RowH)))
		{
			Selected = Index;
			if (ImGui::IsMouseDoubleClicked(ImGuiMouseButton_Left))
			{
				OpenRequested = true;
			}
		}

		if (ImGui::IsItemHovered(ImGuiHoveredFlags_DelayNormal) && (!Entry.BackupPath.empty() || Entry.HasGitChange))
		{
			DrawDetails(Entry);
		}

		if (ImGui::BeginPopupContextItem("##row"))
		{
			Selected = Index;
			if (ImGui::MenuItem(ICON_FA_FOLDER_OPEN "  Open"))
			{
				OpenRequested = true;
			}

			ImGui::BeginDisabled(Entry.BackupPath.empty());
			if (ImGui::MenuItem(ICON_FA_LIFE_RING "  Restore newer backup"))
			{
				RestoreBackup(Entry);
			}
			if (ImGui::MenuItem(ICON_FA_EYE "  Open backup as is"))
			{
				OpenLevel(Entry.BackupPath);
			}
			ImGui::EndDisabled();
			ImGui::EndPopup();
		}

		if (IsSelected && ScrollToSelected)
		{
			ImGui::SetScrollHereY(0.5f);
			ScrollToSelected = false;
		}

		// Accent bar on the selected row
		if (IsSelected)
		{
			Draw->AddRectFilled(Pos, ImVec2(Pos.x + 3.0f, Pos.y + RowH), ImGui::GetColorU32(ImGuiCol_CheckMark), 2.0f);
		}

		const float TextX = Pos.x + Pad + 26.0f;
		const float TopY = Pos.y + 7.0f;
		const float BottomY = TopY + LineH + 1.0f;
		const ImU32 IconColor = Entry.Recent ? ImGui::GetColorU32(ImGuiCol_CheckMark) : MutedColor();

		Draw->AddText(ImVec2(Pos.x + Pad, Pos.y + (RowH - LineH) * 0.5f), IconColor, Entry.Recent ? ICON_FA_CLOCK_ROTATE_LEFT : ICON_FA_MAP);
		Draw->AddText(ImVec2(TextX, TopY), ImGui::GetColorU32(ImGuiCol_Text), Entry.Display.c_str());

		// State badges after the name
		float BadgeX = TextX + ImGui::CalcTextSize(Entry.Display.c_str()).x + 10.0f;
		const float BadgeY = TopY - 1.0f;
		if (!Entry.BackupPath.empty())
		{
			const xr_string Text = xr_string(ICON_FA_LIFE_RING " backup ") + FormatAge(Entry.BackupTime);
			BadgeX += DrawBadge(Draw, ImVec2(BadgeX, BadgeY), BackupColor, Text.c_str()) + 6.0f;
		}
		if (Entry.HasGitChange)
		{
			string64 Text;
			xr_sprintf(Text, ICON_FA_CODE_BRANCH " %u new in git", Entry.GitChange.Commits);
			BadgeX += DrawBadge(Draw, ImVec2(BadgeX, BadgeY), GitRemoteColor, Text) + 6.0f;
		}
		if (!Entry.GitKey.empty() && HasGit())
		{
			const EGitFileInfo Info = Git->GetFileInfo(xr_path(Entry.FullPath));
			if (Info.Status != EGitFileStatus::Unmodified)
			{
				DrawBadge(Draw, ImVec2(BadgeX, BadgeY), ImGui::ColorConvertU32ToFloat4(GitStatusColor(Info)), GitStatusText(Info));
			}
		}

		const char* FolderText = Entry.Folder.empty() ? "$maps$" : Entry.Folder.c_str();
		Draw->AddText(ImVec2(TextX, BottomY), MutedColor(), FolderText);

		const float TimeW = ImGui::CalcTextSize(Entry.TimeText.c_str()).x;
		const float SizeW = ImGui::CalcTextSize(Entry.SizeText.c_str()).x;
		Draw->AddText(ImVec2(Pos.x + Width - TimeW - Pad, TopY), MutedColor(), Entry.TimeText.c_str());
		Draw->AddText(ImVec2(Pos.x + Width - SizeW - Pad, BottomY), MutedColor(), Entry.SizeText.c_str());

		ImGui::PopID();
	}

	if (Visible.empty())
	{
		const char* Text = Levels.empty() ? "No levels found in $maps$" : "Nothing matches the filter";
		const ImVec2 Avail = ImGui::GetContentRegionAvail();
		const ImVec2 Size = ImGui::CalcTextSize(Text);
		ImGui::SetCursorPos(ImVec2((Avail.x - Size.x) * 0.5f, ImGui::GetCursorPosY() + 40.0f));
		ImGui::TextDisabled("%s", Text);
	}

	return OpenRequested;
}

void UILevelSelector::DrawFooter(int VisibleCount, bool& OpenRequested, bool& RestoreRequested)
{
	const bool HasSelection = Selected >= 0 && Selected < int(Levels.size());
	const bool CanRestore = HasSelection && !Levels[Selected].BackupPath.empty();
	const float ButtonW = 100.0f;
	const float RestoreW = 160.0f;
	const float Spacing = ImGui::GetStyle().ItemSpacing.x;

	bool GitBusy = false;
	{
		xrCriticalSectionGuard Guard(GitLock);
		GitBusy = GitScanning;
	}

	ImGui::AlignTextToFramePadding();
	if (HasSelection)
	{
		const LevelEntry& Entry = Levels[Selected];
		if (!Entry.BackupPath.empty())
		{
			ImGui::TextColored(BackupColor, ICON_FA_LIFE_RING "  Autobackup from %s is newer", FormatTime(Entry.BackupTime).c_str());
		}
		else if (Entry.HasGitChange)
		{
			ImGui::TextColored(GitRemoteColor, ICON_FA_CODE_BRANCH "  Newer in %s: %s", GitUpstream.c_str(), Entry.GitChange.Subject.c_str());
		}
		else
		{
			ImGui::TextDisabled("$maps$\\%s%s%s.level", Entry.Folder.c_str(), Entry.Folder.empty() ? "" : "\\", Entry.Display.c_str());
		}
	}
	else if (GitBusy)
	{
		ImGui::TextDisabled(ICON_FA_CODE_BRANCH "  Checking Git...");
	}
	else
	{
		ImGui::TextDisabled("%d of %d levels", VisibleCount, int(Levels.size()));
	}

	const float ButtonsW = ButtonW * 2.0f + Spacing + (CanRestore ? RestoreW + Spacing : 0.0f);
	ImGui::SameLine(ImGui::GetWindowContentRegionMax().x - ButtonsW);

	if (CanRestore)
	{
		ImGui::PushStyleColor(ImGuiCol_Text, BackupColor);
		if (ImGui::Button(ICON_FA_LIFE_RING "  Restore backup", ImVec2(RestoreW, 0.0f)))
		{
			RestoreRequested = true;
		}
		ImGui::PopStyleColor();
		ImGui::SetItemTooltip("Load the newer autobackup into this level. Save to overwrite it.");
		ImGui::SameLine();
	}

	if (ImGui::Button("Cancel", ImVec2(ButtonW, 0.0f)))
	{
		bOpen = false;
	}

	ImGui::SameLine();
	ImGui::BeginDisabled(!HasSelection);
	ImGui::PushStyleColor(ImGuiCol_Button, ImGui::GetColorU32(ImGuiCol_ButtonActive));
	if (ImGui::Button(ICON_FA_FOLDER_OPEN "  Open", ImVec2(ButtonW, 0.0f)))
	{
		OpenRequested = true;
	}
	ImGui::PopStyleColor();
	ImGui::EndDisabled();
}

void UILevelSelector::Draw()
{
	if (!bOpen)
	{
		return;
	}

	ResolveGitChanges();

	ImGui::SetNextWindowSize(ImVec2(860, 580), ImGuiCond_FirstUseEver);
	ImGui::SetNextWindowPos(ImGui::GetMainViewport()->GetCenter(), ImGuiCond_Appearing, ImVec2(0.5f, 0.5f));
	ImGui::PushStyleVar(ImGuiStyleVar_WindowPadding, ImVec2(12.0f, 12.0f));
	ImGui::PushStyleVar(ImGuiStyleVar_ItemSpacing, ImVec2(8.0f, 6.0f));
	ImGui::PushStyleVar(ImGuiStyleVar_FramePadding, ImVec2(8.0f, 6.0f));
	ImGui::PushStyleVar(ImGuiStyleVar_FrameRounding, 4.0f);

	if (!ImGui::Begin(ICON_FA_FOLDER_OPEN "  Open Level###LevelSelector", &bOpen, ImGuiWindowFlags_NoCollapse | ImGuiWindowFlags_NoDocking))
	{
		ImGui::End();
		ImGui::PopStyleVar(4);
		return;
	}

	const bool Focused = ImGui::IsWindowFocused(ImGuiFocusedFlags_RootAndChildWindows);
	if (Focused && ImGui::IsKeyPressed(ImGuiKey_Escape))
	{
		bOpen = false;
	}

	// Search row
	if (FocusFilter)
	{
		ImGui::SetKeyboardFocusHere();
		FocusFilter = false;
	}

	const char* SortLabels[] = {"Name", "Modified", "Size"};
	const float SortW = 130.0f;
	const float RefreshW = ImGui::GetFrameHeight();
	const float BrowseW = ImGui::CalcTextSize(ICON_FA_FOLDER_OPEN "  Browse...").x + ImGui::GetStyle().FramePadding.x * 2.0f;
	const float Spacing = ImGui::GetStyle().ItemSpacing.x;

	ImGui::SetNextItemWidth(ImGui::GetContentRegionAvail().x - SortW - RefreshW - BrowseW - Spacing * 3.0f);
	const bool FilterEnter = ImGui::InputTextWithHint("##filter", ICON_FA_MAGNIFYING_GLASS "  Search levels...", Filter, sizeof(Filter), ImGuiInputTextFlags_EnterReturnsTrue);
	if (ImGui::IsItemEdited())
	{
		ScrollToSelected = true;
	}

	ImGui::SameLine();
	ImGui::SetNextItemWidth(SortW);
	int SortIndex = int(SortBy);
	if (ImGui::Combo("##sort", &SortIndex, SortLabels, IM_ARRAYSIZE(SortLabels)))
	{
		SortBy = ESort(SortIndex);
		ScrollToSelected = true;
	}
	ImGui::SetItemTooltip("Sort by");

	ImGui::SameLine();
	if (ImGui::Button(ICON_FA_ARROWS_ROTATE, ImVec2(RefreshW, 0)))
	{
		Refresh();
	}
	ImGui::SetItemTooltip("Rescan $maps$, autobackups and Git");

	ImGui::SameLine();
	if (ImGui::Button(ICON_FA_FOLDER_OPEN "  Browse..."))
	{
		Browse();
	}

	// Filter and sort
	xr_vector<int> VisibleRows;
	VisibleRows.reserve(Levels.size());
	for (int Index = 0; Index < int(Levels.size()); ++Index)
	{
		if (PassFilter(Levels[Index]))
		{
			VisibleRows.push_back(Index);
		}
	}

	std::stable_sort(VisibleRows.begin(), VisibleRows.end(), [this](int A, int B)
	{
		const LevelEntry& L = Levels[A];
		const LevelEntry& R = Levels[B];
		switch (SortBy)
		{
		case ESort::Modified:
			return std::max(L.Modified, L.BackupTime) > std::max(R.Modified, R.BackupTime);
		case ESort::Size:
			return L.Size > R.Size;
		default:
			return xr_strcmp(L.Display.c_str(), R.Display.c_str()) < 0;
		}
	});

	bool OpenRequested = false;
	if (FilterEnter && !VisibleRows.empty())
	{
		if (std::find(VisibleRows.begin(), VisibleRows.end(), Selected) == VisibleRows.end())
		{
			Selected = VisibleRows.front();
		}
		OpenRequested = true;
	}

	// Arrow keys move through the visible rows
	const bool KeyDown = ImGui::IsKeyPressed(ImGuiKey_DownArrow);
	const bool KeyUp = ImGui::IsKeyPressed(ImGuiKey_UpArrow);
	if (Focused && !VisibleRows.empty() && (KeyDown || KeyUp))
	{
		auto It = std::find(VisibleRows.begin(), VisibleRows.end(), Selected);
		const int Pos = It == VisibleRows.end() ? -1 : int(It - VisibleRows.begin());
		const int NewPos = std::clamp(KeyDown ? Pos + 1 : (Pos < 0 ? 0 : Pos - 1), 0, int(VisibleRows.size()) - 1);
		if (NewPos != Pos)
		{
			Selected = VisibleRows[NewPos];
			ScrollToSelected = true;
		}
	}

	// Body: views and folders on the left, levels on the right
	const float FooterH = ImGui::GetFrameHeight() + ImGui::GetStyle().ItemSpacing.y * 2.0f + 1.0f;
	const float BodyH = ImGui::GetContentRegionAvail().y - FooterH;

	ImGui::PushStyleColor(ImGuiCol_ChildBg, ImGui::GetColorU32(ImGuiCol_FrameBg, 0.5f));
	ImGui::PushStyleVar(ImGuiStyleVar_ChildRounding, 6.0f);
	ImGui::PushStyleVar(ImGuiStyleVar_WindowPadding, ImVec2(6.0f, 6.0f));
	ImGui::PushStyleVar(ImGuiStyleVar_ItemSpacing, ImVec2(4.0f, 2.0f));

	if (ImGui::BeginChild("##views", ImVec2(210.0f, BodyH), ImGuiChildFlags_AlwaysUseWindowPadding))
	{
		DrawSidebar();
	}
	ImGui::EndChild();

	ImGui::SameLine(0.0f, 8.0f);
	if (ImGui::BeginChild("##list", ImVec2(0.0f, BodyH), ImGuiChildFlags_AlwaysUseWindowPadding))
	{
		OpenRequested |= DrawList(VisibleRows);
	}
	ImGui::EndChild();

	ImGui::PopStyleVar(3);
	ImGui::PopStyleColor();

	if (Focused && !FilterEnter && (ImGui::IsKeyPressed(ImGuiKey_Enter) || ImGui::IsKeyPressed(ImGuiKey_KeypadEnter)))
	{
		OpenRequested = true;
	}

	bool RestoreRequested = false;
	DrawFooter(int(VisibleRows.size()), OpenRequested, RestoreRequested);

	ImGui::End();
	ImGui::PopStyleVar(4);

	// Actions run after the window is closed: loading clears the scene
	const bool HasSelection = Selected >= 0 && Selected < int(Levels.size());
	if (!bOpen || !HasSelection)
	{
		return;
	}

	if (RestoreRequested)
	{
		RestoreBackup(Levels[Selected]);
	}
	else if (OpenRequested)
	{
		OpenLevel(Levels[Selected].FullPath);
	}
}
