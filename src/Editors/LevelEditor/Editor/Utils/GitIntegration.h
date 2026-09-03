#pragma once

#include <atomic>
#include <chrono>
#include <deque>
#include <functional>
#include <thread>

enum class EGitFileStatus : u8
{
	Unmodified,
	Modified,
	Added,
	Deleted,
	Renamed,
	Copied,
	Untracked,
	Ignored,
	Conflicted
};

enum class EGitTaskState : u8
{
	Idle,
	Running,
	Succeeded,
	Failed
};

struct EGitFileInfo
{
	EGitFileStatus Status = EGitFileStatus::Unmodified;
	bool Staged = false;
	bool Unstaged = false;
	bool Lfs = false;
	bool IsDirectory = false;
};

struct SGitCommit
{
	xr_string Hash;
	xr_string ShortHash;
	xr_string Author;
	xr_string Date;
	xr_string Subject;
};

class CGitIntegration final
{
public:
	struct SExecResult
	{
		int ExitCode = -1;
		xr_string Output;
	};

	struct STask
	{
		xr_string Name;
		bool RefreshLfsAfter = false;
		std::function<void(SExecResult&)> Task;
	};

	void Initialize();
	void Shutdown();

	bool DetectRepository();
	void RunTask(const char* Name, bool RefreshLfsAfter, const std::function<void(SExecResult&)>& Task);
	void ProcessQueue();
	static void LogTaskResult(const char* Name, const SExecResult& Result, bool Succeeded);
	SExecResult Execute(const xr_string& Arguments, const xr_string& WorkDir) const;
	void UpdateStatusData();
	void UpdateLfsFiles();
	void UpdateHistory();
	void UpdateBranchesAndRemotes();
	xr_string ResolveRemote() const;
	bool BuildRepositoryKey(const xr_path& Path, xr_string& Out) const;

	void GetFiles(xr_vector<std::pair<xr_string, EGitFileInfo>>& Out) const;
	EGitFileInfo GetFileInfo(const xr_path& Path) const;
	void GetHistory(xr_vector<SGitCommit>& Out) const;
	void GetBranches(xr_vector<xr_string>& Out) const;
	void GetRemotes(xr_vector<xr_string>& Out) const;

	void RefreshStatus(bool Force = false);
	void CheckoutBranch(const xr_string& BranchName);
	void AddRemote(const xr_string& Name, const xr_string& Url);
	void CreateBranch(const xr_string& BranchName, bool Checkout);
	void Stage(const xr_path& Path);
	void StageAll();
	void Unstage(const xr_path& Path);
	void UnstageAll();
	void Discard(const xr_path& Path);
	void Commit(const xr_string& Message);
	void Push();
	void Pull();
	void Fetch();
	void TrackWithLfs(const xr_path& Path);
	void AutoTrackFileWithLfs(const xr_path& Path);
	void ProcessFileForLFS(const xr_path& Path);

	mutable xrSRWLock Mutex;
	xr_hash_map<xr_string, EGitFileInfo> Files;
	xr_hash_map<xr_string, bool> DirtyDirectories;
	xr_hash_map<xr_string, bool> LfsFiles;
	xr_vector<SGitCommit> History;
	xr_vector<xr_string> Branches;
	xr_vector<xr_string> Remotes;
	xr_string SelectedRemote;
	std::deque<STask> TaskQueue;
	xr_vector<std::jthread> Workers;

	xr_string RepositoryRoot;
	xr_string RepositoryName;
	xr_string Branch;
	xr_string Upstream;
	xr_string TaskName;
	u32 Ahead = 0;
	u32 Behind = 0;

	std::atomic_bool Busy{ false };
	std::atomic<EGitTaskState> TaskState{ EGitTaskState::Idle };
	// True when HEAD resolves (false for a fresh `git init` without commits,
	// where `git reset HEAD` doesn't work and rm --cached must be used instead).
	// Defaults to true: the reset path is the safe one if status hasn't run yet.
	std::atomic_bool HasCommits{ true };
	std::atomic<s64> LastRefreshTick{ 0 };
	std::atomic_bool NeedsRefresh{ false };

	bool IsRepository = false;
	bool LfsAvailable = false;
};

extern CGitIntegration* Git;

ImU32 GitStatusColor(const EGitFileInfo& Info);
const char* GitStatusText(const EGitFileInfo& Info);
