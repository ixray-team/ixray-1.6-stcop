#pragma once

class ESceneToolBase;

// Periodic copy of the unsaved level into $temp$\autobackup. The tools are written one by one by a
// worker thread while the editor keeps running. The tool being written is locked: its viewport
// controls, gizmo, tool panel and properties do not edit it until the worker moves on. Changes that
// touch several tools at once (adding or removing objects, groups, renames, undo, loading, building)
// cancel a running backup, which starts again a little later.
class CLevelAutoBackup
{
public:
	// Main thread, once per frame: starts a backup when it is due and hands the lock to the next tool
	void Update();
	// Main thread: stops a running backup, waits for the tool being written and removes the
	// incomplete copy
	void Cancel();
	// Backup as soon as the scene can be read, even without changes
	void RequestNow() { Requested = true; }

	bool IsBusy() const { return Busy; }
	bool IsToolLocked(ObjClassID ID) const { return Busy && LockedTool == ID; }
	// The tool selected in the editor is being written
	bool IsCurrentToolLocked() const;

	// Around widgets that edit the selected tool: disabled, with a note, while it is written
	void BeginToolWidgets() const;
	void EndToolWidgets() const;

	u32 IntervalMs = 3 * 60 * 1000;
	// Next try after a cancel
	u32 RetryMs = 10 * 1000;
	// Backups kept per level
	u32 KeepCount = 10;

private:
	struct PendingTool
	{
		ObjClassID ID = OBJCLASS_DUMMY;
		ESceneToolBase* Tool = nullptr;
	};

	bool IsNeeded() const;
	bool CanStart() const;
	void Start(u64 Now);
	void LockNextTool();
	void Finish(u64 Now);
	void RemoveOldBackups() const;
	bool IsBackupName(const xr_string& Name) const;
	static bool IsToolInUse(ObjClassID ID);

	xr_task_group Task;
	xr_atomic_bool ToolSaved = true;
	// Used by the worker only
	CMemoryWriter SaveCache;

	bool Busy = false;
	bool Requested = false;
	ObjClassID LockedTool = OBJCLASS_DUMMY;
	xr_vector<PendingTool> Pending;

	xr_string LevelName;
	// Main file and part folder of the running backup
	xr_string FileName;
	xr_string PartPrefix;

	u32 StartModifyCount = 0;
	u32 SavedModifyCount = 0;
	u64 StartTicks = 0;
	u64 NextTicks = 0;
};

extern CLevelAutoBackup g_LevelAutoBackup;
