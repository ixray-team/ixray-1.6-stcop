#include "stdafx.h"
#include "LevelAutoBackup.h"
#include "IconsFontAwesome7.h"
#include <SDL3/SDL.h>
#include <ctime>
#include <filesystem>

CLevelAutoBackup g_LevelAutoBackup;

static constexpr const char* BackupFolder = "autobackup\\";
static constexpr const char* BackupExt = ".level";
// YYYY-MM-DD_HH-MM-SS
static constexpr size_t BackupStampSize = 19;
// Next check while the scene can not be read
static constexpr u32 BackupWaitMs = 1000;

bool CLevelAutoBackup::IsCurrentToolLocked() const
{
	return Busy && LockedTool == LTools->CurrentClassID();
}

void CLevelAutoBackup::BeginToolWidgets() const
{
	const bool Locked = IsCurrentToolLocked();
	if (Locked)
	{
		ImGui::TextDisabled(ICON_FA_FLOPPY_DISK " Writing level backup...");
	}
	ImGui::BeginDisabled(Locked);
}

void CLevelAutoBackup::EndToolWidgets() const
{
	ImGui::EndDisabled();
}

void CLevelAutoBackup::Update()
{
	const u64 Now = SDL_GetTicks();
	// The game of Play In Editor changes the objects the backup reads
	if (Scene->IsPlayInEditor())
	{
		Cancel();
		Requested = false;
		NextTicks = 0;
		return;
	}

	if (Busy)
	{
		if (!ToolSaved)
		{
			return;
		}

		Task.wait();
		LockedTool = OBJCLASS_DUMMY;
		if (Pending.empty())
		{
			Finish(Now);
		}
		else
		{
			LockNextTool();
		}
		return;
	}

	if (Requested)
	{
		if (!IsNeeded())
		{
			Requested = false;
		}
		else if (CanStart())
		{
			Requested = false;
			Start(Now);
		}
		return;
	}

	if (NextTicks == 0)
	{
		NextTicks = Now + IntervalMs;
		return;
	}

	if (Now < NextTicks)
	{
		return;
	}

	if (!IsNeeded())
	{
		NextTicks = Now + IntervalMs;
		return;
	}

	if (!CanStart())
	{
		NextTicks = Now + BackupWaitMs;
		return;
	}

	Start(Now);
}

void CLevelAutoBackup::Cancel()
{
	if (!Busy)
	{
		return;
	}

	Task.wait();
	Busy = false;
	LockedTool = OBJCLASS_DUMMY;
	ToolSaved = true;
	Pending.clear();

	std::error_code Error;
	std::filesystem::remove_all(PartPrefix.c_str(), Error);

	NextTicks = SDL_GetTicks() + RetryMs;
	Msg("~ Level backup cancelled: the scene changed while it was written");
}

bool CLevelAutoBackup::IsNeeded() const
{
	return Scene->IsUnsaved() && Scene->GetModifyCount() != SavedModifyCount;
}

bool CLevelAutoBackup::CanStart() const
{
	return EContext.UI->GetEState() == esEditScene && Scene->valid() && !Scene->locked() && !Scene->IsPlayInEditor() && !LUI->LoaderEvent.IsBusy();
}

void CLevelAutoBackup::Start(u64 Now)
{
	LevelName = LTools->m_LastFileName.empty() ? xr_string("untitled") : EFS.ExtractFileName(LTools->m_LastFileName.c_str());
	xr_strlwr(LevelName);

	string64 Stamp;
	const std::time_t Time = std::time(nullptr);
	std::strftime(Stamp, sizeof(Stamp), "%Y-%m-%d_%H-%M-%S", std::localtime(&Time));

	string_path Path;
	const xr_string Name = BackupFolder + LevelName + "_" + Stamp + BackupExt;
	FS.update_path(Path, _temp_, Name.c_str());
	FileName = Path;
	PartPrefix = Scene->LevelPartPath(Path);

	Pending.clear();
	for (SceneToolsMapPairIt It = Scene->FirstTool(); It != Scene->LastTool(); ++It)
	{
		if (EScene::IsSavedTool(It->first, It->second))
		{
			Pending.push_back({It->first, It->second});
		}
	}

	StartModifyCount = Scene->GetModifyCount();
	StartTicks = Now;
	Busy = true;
	ToolSaved = true;
	LockNextTool();
}

bool CLevelAutoBackup::IsToolInUse(ObjClassID ID)
{
	if (ID != LTools->CurrentClassID())
	{
		return false;
	}

	// A drag, a gizmo or a property field that edits the tool right now
	return EContext.UI->IsMouseCaptured() || imManipulator.m_active || ImGui::IsAnyItemActive();
}

void CLevelAutoBackup::LockNextTool()
{
	auto It = std::find_if(Pending.begin(), Pending.end(), [](const PendingTool& Tool) { return !IsToolInUse(Tool.ID); });
	if (It == Pending.end())
	{
		// Every tool left is being edited: try again on the next frame
		return;
	}

	ESceneToolBase* Tool = It->Tool;
	LockedTool = It->ID;
	Pending.erase(It);

	ToolSaved = false;
	Task.run
	(
		[this, Tool, PartName = EScene::ToolPartName(Tool, PartPrefix)]()
		{
			Scene->SaveToolPart(Tool, PartName.c_str(), SaveCache);
			ToolSaved = true;
		}
	);
}

void CLevelAutoBackup::Finish(u64 Now)
{
	// The level header last: a backup without it is incomplete
	CInifile Ini(FileName.c_str(), false, false, false);
	Scene->SaveMainLTX(Ini);
	Ini.save_as();
	GWorldPartition.SaveLevelState(PartPrefix.c_str());

	Busy = false;
	SavedModifyCount = StartModifyCount;
	NextTicks = Now + IntervalMs;

	RemoveOldBackups();

	const float Seconds = float(Now - StartTicks) / 1000.0f;
	Msg("* Level backup saved: %s (%.2f sec)", FileName.c_str(), Seconds);

	string512 Text;
	xr_sprintf(Text, "%s (%.1f sec)", Platform::ANSI_TO_UTF8(EFS.ExtractFileName(FileName.c_str()) + BackupExt).c_str(), Seconds);
	EContext.UI->Toasts.Push(EToastType::Success, "Level backup saved", Text);
}

bool CLevelAutoBackup::IsBackupName(const xr_string& Name) const
{
	const xr_string Prefix = LevelName + "_";
	return Name.size() == Prefix.size() + BackupStampSize + xr_strlen(BackupExt) && Name.starts_with(Prefix) && Name.ends_with(BackupExt);
}

void CLevelAutoBackup::RemoveOldBackups() const
{
	string_path Folder;
	FS.update_path(Folder, _temp_, BackupFolder);

	std::error_code Error;
	xr_vector<xr_string> Backups;
	for (const std::filesystem::directory_entry& Entry : std::filesystem::directory_iterator(Folder, Error))
	{
		xr_string Name = Entry.path().filename().string().c_str();
		xr_strlwr(Name);
		if (Entry.is_regular_file(Error) && IsBackupName(Name))
		{
			Backups.push_back(Name);
		}
	}

	// The stamp sorts by time
	std::sort(Backups.begin(), Backups.end());
	for (size_t Index = 0; Index + KeepCount < Backups.size(); ++Index)
	{
		const xr_string Path = Folder + Backups[Index];
		std::filesystem::remove(Path.c_str(), Error);
		std::filesystem::remove_all(Scene->LevelPartPath(Path.c_str()).c_str(), Error);
	}
}
