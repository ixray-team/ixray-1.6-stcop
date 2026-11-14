#pragma once

#include "EditorWnd.h"

#include <thread>

// File -> Open dialog of the scene view: lists every level under $maps$,
// with newer autobackups and newer upstream Git commits
class UILevelSelector final : public IEditorWnd
{
public:
	// Upstream commit that changed a level and is not in HEAD yet
	struct GitRemoteChange
	{
		xr_string ShortHash;
		xr_string Author;
		xr_string Subject;
		time_t Time = 0;
		u32 Commits = 0;
	};

	struct LevelEntry
	{
		xr_string Name;     // Relative to $maps$, CP1251
		xr_string FullPath; // CP1251
		xr_string Display;  // UTF-8
		xr_string Folder;   // UTF-8
		xr_string SizeText;
		xr_string TimeText;
		long Size = 0;
		time_t Modified = 0;
		bool Recent = false;

		// Newest autobackup written after the level file, empty when none
		xr_string BackupPath;
		time_t BackupTime = 0;

		// Repository key of the level file, empty outside a repository
		xr_string GitKey;
		bool HasGitChange = false;
		GitRemoteChange GitChange;
	};

	// Sidebar selection
	enum class EView : u8
	{
		All,
		Recent,
		Backup,
		Git,
		Folder
	};

	enum class ESort : u8
	{
		Name,
		Modified,
		Size
	};

	virtual void Draw() override;

	static void Update();
	static void Show();
	static void Close();
	static IC bool IsOpen() { return Form != nullptr; }

private:
	void Refresh();
	void ScanBackups();
	void StartGitScan();
	void ResolveGitChanges();
	void OpenLevel(const xr_string& FullPath);
	void RestoreBackup(const LevelEntry& Entry);
	void Browse();
	bool PassFilter(const LevelEntry& Entry) const;
	void DrawSidebar();
	bool DrawList(const xr_vector<int>& Visible);
	void DrawDetails(const LevelEntry& Entry) const;
	void DrawFooter(int VisibleCount, bool& OpenRequested, bool& RestoreRequested);

	xr_vector<LevelEntry> Levels;
	xr_vector<xr_string> Folders;
	char Filter[256] = {};
	int Selected = -1;
	EView View = EView::All;
	int FolderIndex = -1; // Index in Folders when View is Folder
	ESort SortBy = ESort::Name;
	bool FocusFilter = true;
	bool ScrollToSelected = true;

	// Written by GitWorker under GitLock
	mutable xrCriticalSection GitLock;
	xr_hash_map<xr_string, GitRemoteChange> GitChanges;
	bool GitScanning = false;
	bool GitResolved = true;
	xr_string GitUpstream;
	// Declared last: joins before the members it writes are destroyed
	std::jthread GitWorker;

	static UILevelSelector* Form;
};
