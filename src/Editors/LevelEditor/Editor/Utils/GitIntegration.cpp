#include "stdafx.h"
#include "GitIntegration.h"
#include "GitLFSConfig.h"

#include <fstream>

static xr_string TrimSpaces(const xr_string& Value)
{
	size_t Start = Value.find_first_not_of(" \t\r\n");
	if (Start == xr_string::npos)
	{
		return {};
	}

	size_t End = Value.find_last_not_of(" \t\r\n");
	return Value.substr(Start, End - Start + 1);
}

static xr_string NormalizeKey(const xr_string& Value)
{
	xr_string Result;
	Result.reserve(Value.size());
	for (char Character : Value)
	{
		if (Character == '\\')
		{
			Character = '/';
		}

		Result.push_back(static_cast<char>(tolower(static_cast<unsigned char>(Character))));
	}

	while (Result.size() > 1 && Result.back() == '/')
	{
		Result.pop_back();
	}

	return Result;
}

static xr_string ToNativePath(const xr_string& Value)
{
	xr_string Result = Value;
	std::replace(Result.begin(), Result.end(), '/', '\\');
	return Result;
}

static xr_string QuoteArgument(const xr_string& Value)
{
	return "\"" + Value + "\"";
}

ImU32 GitStatusColor(const EGitFileInfo& Info)
{
	if (Info.Status == EGitFileStatus::Conflicted)
	{
		return IM_COL32(0xC8, 0x58, 0xC8, 0xFF);
	}

	if (Info.Status == EGitFileStatus::Deleted)
	{
		return Info.Staged && !Info.Unstaged ? IM_COL32(0x59, 0xC1, 0x6A, 0xFF) : IM_COL32(0xE0, 0x53, 0x4E, 0xFF);
	}

	if (Info.Status == EGitFileStatus::Added)
	{
		return IM_COL32(0x59, 0xC1, 0x6A, 0xFF);
	}

	if (Info.Status == EGitFileStatus::Untracked)
	{
		return IM_COL32(0x8A, 0x93, 0xA6, 0xFF);
	}

	if (Info.Status == EGitFileStatus::Renamed || Info.Status == EGitFileStatus::Copied)
	{
		return IM_COL32(0x53, 0xA8, 0xE0, 0xFF);
	}

	if (Info.Status == EGitFileStatus::Modified)
	{
		return Info.Staged && !Info.Unstaged ? IM_COL32(0x59, 0xC1, 0x6A, 0xFF) : IM_COL32(0xE2, 0xB0, 0x3C, 0xFF);
	}

	if (Info.Status == EGitFileStatus::Ignored)
	{
		return IM_COL32(0x60, 0x60, 0x60, 0x99);
	}

	return IM_COL32(0x59, 0xC1, 0x6A, 0x99);
}

const char* GitStatusText(const EGitFileInfo& Info)
{
	if (Info.IsDirectory)
	{
		return "Contains changes";
	}

	switch (Info.Status)
	{
		case EGitFileStatus::Modified:
			return Info.Staged && !Info.Unstaged ? "Modified (staged)" : "Modified";
		case EGitFileStatus::Added:
			return "Added";
		case EGitFileStatus::Deleted:
			return Info.Staged && !Info.Unstaged ? "Deleted (staged)" : "Deleted";
		case EGitFileStatus::Renamed:
			return "Renamed";
		case EGitFileStatus::Copied:
			return "Copied";
		case EGitFileStatus::Untracked:
			return "Untracked";
		case EGitFileStatus::Ignored:
			return "Ignored";
		case EGitFileStatus::Conflicted:
			return "Conflicted";
		default:
			return "Unmodified";
	}
}

CGitIntegration* Git = nullptr;

bool CGitIntegration::DetectRepository()
{
	SExecResult Version = Execute("--version", ".");
	if (Version.ExitCode != 0)
	{
		return IsRepository;
	}

	string_path FsRoot = {};
	FS.update_path(FsRoot, "$fs_root$", "");

	SExecResult TopLevel = Execute("rev-parse --show-toplevel", FsRoot);
	if (TopLevel.ExitCode != 0)
	{
		return IsRepository;
	}

	xr_string Root = ToNativePath(TrimSpaces(TopLevel.Output));
	if (Root.empty())
	{
		return IsRepository;
	}

	{
		xrSRWLockGuard Guard(Mutex);
		RepositoryRoot = Root;

		size_t NameStart = RepositoryRoot.find_last_of("\\/");
		RepositoryName = NameStart == xr_string::npos ? RepositoryRoot : RepositoryRoot.substr(NameStart + 1);
	}

	IsRepository = true;
	return true;
}

void CGitIntegration::Initialize()
{
	VERIFY(!Git);
	Git = this;

	if (!DetectRepository())
	{
		return;
	}

	SExecResult LfsVersion = Execute("lfs version", RepositoryRoot);
	LfsAvailable = LfsVersion.ExitCode == 0;

	RunTask("Refresh Status", true, [](SExecResult&) {});
}

void CGitIntegration::Shutdown()
{
	NeedsRefresh = false;

	for (std::jthread& Worker : Workers)
	{
		if (Worker.joinable())
		{
			Worker.join();
		}
	}

	Workers.clear();
}

void CGitIntegration::GetFiles(xr_vector<std::pair<xr_string, EGitFileInfo>>& Out) const
{
	xrSRWLockGuard Guard(Mutex, true);
	Out.reserve(Files.size());
	for (const auto& [Path, Info] : Files)
	{
		Out.emplace_back(Path, Info);
	}
}

void CGitIntegration::GetHistory(xr_vector<SGitCommit>& Out) const
{
	xrSRWLockGuard Guard(Mutex, true);
	Out = History;
}

void CGitIntegration::GetBranches(xr_vector<xr_string>& Out) const
{
	xrSRWLockGuard Guard(Mutex, true);
	Out = Branches;
}

void CGitIntegration::GetRemotes(xr_vector<xr_string>& Out) const
{
	xrSRWLockGuard Guard(Mutex, true);
	Out = Remotes;
}

xr_string CGitIntegration::ResolveRemote() const
{
	xrSRWLockGuard Guard(Mutex, true);

	const auto FindRemote = [&](const xr_string& Name) -> bool
	{
		for (const auto& Remote : Remotes)
		{
			if (Remote == Name)
			{
				return true;
			}
		}
		return false;
	};

	if (!SelectedRemote.empty() && FindRemote(SelectedRemote))
	{
		return SelectedRemote;
	}

	if (FindRemote("origin"))
	{
		return "origin";
	}

	if (!Remotes.empty())
	{
		return Remotes.front();
	}

	return {};
}

EGitFileInfo CGitIntegration::GetFileInfo(const xr_path& Path) const
{
	xr_string Key;
	if (!BuildRepositoryKey(Path, Key))
	{
		return {};
	}

	xrSRWLockGuard Guard(Mutex, true);

	if (auto Iterator = Files.find(Key); Iterator != Files.end())
	{
		return Iterator->second;
	}

	if (DirtyDirectories.contains(Key))
	{
		EGitFileInfo Info;
		Info.Status = EGitFileStatus::Modified;
		Info.IsDirectory = true;
		return Info;
	}

	if (LfsFiles.contains(Key))
	{
		EGitFileInfo Info;
		Info.Lfs = true;
		return Info;
	}

	return {};
}

void CGitIntegration::RefreshStatus(bool Force)
{
	// The repository may have been created after the editor started
	// (e.g. user ran `git init` while LevelEditor was running),
	// so try to (re-)detect it instead of giving up forever.
	if (!IsRepository)
	{
		const auto Now = std::chrono::steady_clock::now().time_since_epoch().count();
		const auto Throttle = std::chrono::duration_cast<std::chrono::steady_clock::duration>(std::chrono::seconds(3)).count();

		if (!Force && Now - LastRefreshTick.load() < Throttle)
		{
			return;
		}

		LastRefreshTick = Now;

		if (!DetectRepository())
		{
			return;
		}

		SExecResult LfsVersion = Execute("lfs version", RepositoryRoot);
		LfsAvailable = LfsVersion.ExitCode == 0;

		RunTask("Refresh Status", true, [](SExecResult&) {});
		return;
	}

	const auto Now = std::chrono::steady_clock::now().time_since_epoch().count();
	const auto Throttle = std::chrono::duration_cast<std::chrono::steady_clock::duration>(std::chrono::seconds(3)).count();

	if (!Force && Now - LastRefreshTick.load() < Throttle)
	{
		return;
	}

	LastRefreshTick = Now;
	RunTask("Refresh Status", false, [](SExecResult&) {});
}

void CGitIntegration::Stage(const xr_path& Path)
{
	if (!IsRepository)
	{
		return;
	}

	xr_string Relative;
	if (!BuildRepositoryKey(Path, Relative))
	{
		return;
	}

	RunTask("Stage", false, [this, Relative](SExecResult& Result)
			{ Result = Execute("add -- " + QuoteArgument(Relative), RepositoryRoot); });
}

void CGitIntegration::StageAll()
{
	if (!IsRepository)
	{
		return;
	}

	RunTask("Stage All", false, [this](SExecResult& Result)
			{ Result = Execute("add -A", RepositoryRoot); });
}

void CGitIntegration::Unstage(const xr_path& Path)
{
	if (!IsRepository)
	{
		return;
	}

	xr_string Relative;
	if (!BuildRepositoryKey(Path, Relative))
	{
		return;
	}

	// `git reset HEAD` fails in a fresh repository without commits
	// ("Failed to resolve 'HEAD' as a valid ref"); there everything staged
	// is new, so `git rm --cached` is the correct unstage.
	RunTask("Unstage", false, [this, Relative](SExecResult& Result)
			{
		if (HasCommits.load()){
			Result = Execute("reset -q HEAD -- " + QuoteArgument(Relative), RepositoryRoot);
}
		else{
			Result = Execute("rm --cached -- " + QuoteArgument(Relative), RepositoryRoot);
} });
}

void CGitIntegration::UnstageAll()
{
	if (!IsRepository)
	{
		return;
	}

	RunTask("Unstage All", false, [this](SExecResult& Result)
			{
		if (HasCommits.load()){
			Result = Execute("reset -q HEAD", RepositoryRoot);
}
		else{
			Result = Execute("rm -r --cached .", RepositoryRoot);
} });
}

void CGitIntegration::Discard(const xr_path& Path)
{
	if (!IsRepository)
	{
		return;
	}

	xr_string Relative;
	EGitFileInfo Info = GetFileInfo(Path);
	if (!BuildRepositoryKey(Path, Relative))
	{
		return;
	}

	RunTask("Discard", false, [this, Relative, Info](SExecResult& Result)
			{
		if (Info.Status == EGitFileStatus::Untracked){
			Result = Execute("clean -f -- " + QuoteArgument(Relative), RepositoryRoot);
}
		else{
			Result = Execute("checkout -- " + QuoteArgument(Relative), RepositoryRoot);
} });
}

void CGitIntegration::Commit(const xr_string& Message)
{
	if (!IsRepository || Message.empty())
	{
		return;
	}

	string_path LogDir = {};
	FS.update_path(LogDir, "$logs$", "");

	xr_string MessageFile = xr_string(LogDir) + "LevelEditor_CommitMessage.txt";
	{
		std::ofstream Stream(MessageFile.c_str(), std::ios::binary | std::ios::trunc);
		if (!Stream.is_open())
		{
			return;
		}

		Stream << Message.c_str();
	}

	RunTask("Commit", false, [this, MessageFile](SExecResult& Result)
			{
		Result = Execute("commit --file " + QuoteArgument(MessageFile), RepositoryRoot);
		std::remove(MessageFile.c_str()); });
}

void CGitIntegration::Push()
{
	if (!IsRepository)
	{
		return;
	}

	const xr_string Remote = ResolveRemote();

	RunTask("Push", false, [this, Remote](SExecResult& Result)
			{
				xr_string Args = "push";
				if (!Remote.empty())
				{
					Args += " " + Remote;
				}
				Result = Execute(Args, RepositoryRoot);
			});
}

void CGitIntegration::Pull()
{
	if (!IsRepository)
	{
		return;
	}

	const xr_string Remote = ResolveRemote();

	xr_string CurrentBranch;
	bool Detached = false;
	{
		xrSRWLockGuard Guard(Mutex, true);
		CurrentBranch = Branch;
		Detached = CurrentBranch == "(detached)";
	}

	RunTask("Pull", false, [this, Remote, CurrentBranch, Detached](SExecResult& Result)
			{
				xr_string Args = "pull --no-edit";
				if (!Remote.empty() && !Detached && !CurrentBranch.empty())
				{
					Args += " " + Remote + " " + QuoteArgument(CurrentBranch);
				}
				Result = Execute(Args, RepositoryRoot);
			});
}

void CGitIntegration::Fetch()
{
	if (!IsRepository)
	{
		return;
	}

	const xr_string Remote = ResolveRemote();

	RunTask("Fetch", false, [this, Remote](SExecResult& Result)
			{
				xr_string Args = "fetch --prune";
				if (!Remote.empty())
				{
					Args += " " + Remote;
				}
				Result = Execute(Args, RepositoryRoot);
			});
}

void CGitIntegration::CheckoutBranch(const xr_string& BranchName)
{
	if (!IsRepository || BranchName.empty())
	{
		return;
	}

	RunTask("Checkout", true, [this, BranchName](SExecResult& Result)
			{ Result = Execute("checkout " + QuoteArgument(BranchName), RepositoryRoot); });
}

void CGitIntegration::AddRemote(const xr_string& Name, const xr_string& Url)
{
	if (!IsRepository || Name.empty() || Url.empty())
	{
		return;
	}

	RunTask("Add Remote", true, [this, Name, Url](SExecResult& Result)
			{ Result = Execute("remote add " + QuoteArgument(Name) + " " + QuoteArgument(Url), RepositoryRoot); });
}

void CGitIntegration::CreateBranch(const xr_string& BranchName, bool Checkout)
{
	if (!IsRepository || BranchName.empty())
	{
		return;
	}

	RunTask("Create Branch", true, [this, BranchName, Checkout](SExecResult& Result)
			{
				if (Checkout)
				{
					Result = Execute("checkout -b " + QuoteArgument(BranchName), RepositoryRoot);
				}
				else
				{
					Result = Execute("branch " + QuoteArgument(BranchName), RepositoryRoot);
				}
			});
}

void CGitIntegration::TrackWithLfs(const xr_path& Path)
{
	if (!IsRepository || !LfsAvailable)
	{
		return;
	}

	xr_string Relative;
	if (!BuildRepositoryKey(Path, Relative))
	{
		return;
	}

	RunTask("LFS Track", true, [this, Relative](SExecResult& Result)
			{
		Result = Execute("lfs track " + QuoteArgument(Relative), RepositoryRoot);

		if (Result.ExitCode == 0)
		{
			SExecResult AddResult = Execute("add .gitattributes", RepositoryRoot);
			if (AddResult.ExitCode != 0){
				Result = AddResult;
}
		} });
}

void CGitIntegration::AutoTrackFileWithLfs(const xr_path& Path)
{
	if (!IsRepository || !LfsAvailable)
	{
		return;
	}

	xr_string FilePath = Path.xstring();

	// Check if file should be tracked with LFS based on configuration
	if (!CGitLFSConfig::Instance().ShouldTrackWithLFS(FilePath))
	{
		return;
	}

	// Check if file is already tracked with LFS
	EGitFileInfo Info = GetFileInfo(Path);
	if (Info.Lfs)
	{
		return;
	}

	// Track the file with LFS
	TrackWithLfs(Path);
}

void CGitIntegration::ProcessFileForLFS(const xr_path& Path)
{
	if (!IsRepository || !LfsAvailable)
	{
		return;
	}

	// Check if file should be tracked with LFS based on configuration
	xr_string FilePath = Path.xstring();
	if (!CGitLFSConfig::Instance().ShouldTrackWithLFS(FilePath))
	{
		return;
	}

	// Check if file exists and is not already tracked with LFS
	EGitFileInfo Info = GetFileInfo(Path);
	if (Info.Lfs || Info.Status == EGitFileStatus::Unmodified)
	{
		return;
	}

	// Track the file with LFS if it's newly added or modified
	if (Info.Status == EGitFileStatus::Added || Info.Status == EGitFileStatus::Modified || Info.Status == EGitFileStatus::Untracked)
	{
		TrackWithLfs(Path);
	}
}

void CGitIntegration::RunTask(const char* Name, bool RefreshLfsAfter, const std::function<void(SExecResult&)>& Task)
{
	{
		xrSRWLockGuard Guard(Mutex);
		STask Queued;
		Queued.Name = Name;
		Queued.RefreshLfsAfter = RefreshLfsAfter;
		Queued.Task = Task;
		TaskQueue.push_back(std::move(Queued));
	}

	// If a worker is already running it will pick the task up from the queue.
	// Otherwise this thread becomes the worker owner.
	bool Expected = false;
	if (!Busy.compare_exchange_strong(Expected, true))
	{
		NeedsRefresh = true;
		return;
	}

	Workers.emplace_back([this]()
						   { ProcessQueue(); });
}

void CGitIntegration::LogTaskResult(const char* Name, const SExecResult& Result, bool Succeeded)
{
	if (Succeeded)
	{
		Msg("* [Git] %s: OK", Name);
	}
	else
	{
		Msg("! [Git] %s: failed (exit %d)", Name, Result.ExitCode);
	}

	if (Result.Output.empty())
	{
		return;
	}

	// Output may contain embedded NULs (e.g. `status -z`); log text only.
	size_t TextLen = Result.Output.find('\0');
	if (TextLen == xr_string::npos)
	{
		TextLen = Result.Output.size();
	}

	size_t Start = 0;
	while (Start < TextLen)
	{
		size_t End = Result.Output.find('\n', Start);
		if (End == xr_string::npos || End > TextLen)
		{
			End = TextLen;
		}

		xr_string Line = TrimSpaces(Result.Output.substr(Start, End - Start));
		if (!Line.empty())
		{
			Msg("* [Git] %s", Line.c_str());
		}

		Start = End + 1;
	}
}

void CGitIntegration::ProcessQueue()
{ // Drain the queue; re-acquire the worker role if new tasks arrived
	// after the queue was observed empty (enqueue/drain race window).
	while (true)
	{
		while (true)
		{
			STask Current;
			{
				xrSRWLockGuard Guard(Mutex);
				if (TaskQueue.empty())
				{
					break;
				}

				Current = std::move(TaskQueue.front());
				TaskQueue.pop_front();

				TaskName = Current.Name;
				TaskState = EGitTaskState::Running;
			}

			SExecResult Result;
			Current.Task(Result);

			const bool Succeeded = Result.ExitCode == 0;
			{
				xrSRWLockGuard Guard(Mutex);
				TaskState = Succeeded ? EGitTaskState::Succeeded : EGitTaskState::Failed;
			}

			// Route output to the shared editor log (Msg is thread-safe).
			LogTaskResult(Current.Name.c_str(), Result, Succeeded);

			if (IsRepository)
			{
				UpdateStatusData();

				if (Current.RefreshLfsAfter)
				{
					UpdateLfsFiles();
				}

				UpdateHistory();
				UpdateBranchesAndRemotes();
			}

			if (NeedsRefresh.exchange(false))
			{
				UpdateStatusData();
			}
		}

		Busy = false;

		bool HasWork = false;
		{
			xrSRWLockGuard Guard(Mutex);
			HasWork = !TaskQueue.empty();
		}

		if (!HasWork)
		{
			return;
		}

		bool Expected = false;
		if (!Busy.compare_exchange_strong(Expected, true))
		{
			return; // another worker picked the remaining tasks up
		}
	}
}

CGitIntegration::SExecResult CGitIntegration::Execute(const xr_string& Arguments, const xr_string& WorkDir) const
{
	SExecResult Result;

	if (Arguments.empty())
	{
		return Result;
	}

	const char* WorkDirectory = WorkDir.empty() ? nullptr : WorkDir.c_str();

	SECURITY_ATTRIBUTES SecurityAttributes{sizeof(SECURITY_ATTRIBUTES), nullptr, TRUE};

	HANDLE StdOutRead = nullptr;
	HANDLE StdOutWrite = nullptr;
	if (!CreatePipe(&StdOutRead, &StdOutWrite, &SecurityAttributes, 0))
	{
		return Result;
	}

	SetHandleInformation(StdOutRead, HANDLE_FLAG_INHERIT, 0);

	HANDLE StdInRead = CreateFileA("NUL", GENERIC_READ, FILE_SHARE_READ | FILE_SHARE_WRITE, &SecurityAttributes, OPEN_EXISTING, 0, nullptr);

	STARTUPINFOA StartupInfo{};
	StartupInfo.cb = sizeof(StartupInfo);
	StartupInfo.dwFlags = STARTF_USESTDHANDLES | STARTF_USESHOWWINDOW;
	StartupInfo.wShowWindow = SW_HIDE;
	StartupInfo.hStdInput = StdInRead;
	StartupInfo.hStdOutput = StdOutWrite;
	StartupInfo.hStdError = StdOutWrite;

	PROCESS_INFORMATION ProcessInfo{};
	xr_string CommandLine = "git.exe " + Arguments;

	BOOL Launched = CreateProcessA(nullptr, CommandLine.data(), nullptr, nullptr, TRUE, CREATE_NO_WINDOW, nullptr, WorkDirectory, &StartupInfo, &ProcessInfo);
	CloseHandle(StdOutWrite);

	if (!Launched)
	{
		CloseHandle(StdOutRead);
		if (StdInRead != INVALID_HANDLE_VALUE)
		{
			CloseHandle(StdInRead);
		}

		Result.Output = "Failed to start git.exe. Make sure git is installed and available in PATH.";
		return Result;
	}

	if (StdInRead != INVALID_HANDLE_VALUE)
	{
		CloseHandle(StdInRead);
	}

	std::string Buffer;
	char Chunk[4096];
	DWORD Read = 0;
	while (ReadFile(StdOutRead, Chunk, sizeof(Chunk), &Read, nullptr) && Read > 0)
	{
		Buffer.append(Chunk, Read);
	}

	CloseHandle(StdOutRead);

	WaitForSingleObject(ProcessInfo.hProcess, INFINITE);

	DWORD ExitCode = 0;
	GetExitCodeProcess(ProcessInfo.hProcess, &ExitCode);
	CloseHandle(ProcessInfo.hProcess);
	CloseHandle(ProcessInfo.hThread);

	Result.ExitCode = static_cast<int>(ExitCode);
	// NOTE: must preserve embedded NULs: `git status -z` separates entries with '\0'.
	// Assigning via `const char*` would truncate the output at the first entry.
	Result.Output.assign(Buffer.data(), Buffer.size());
	return Result;
}

void CGitIntegration::UpdateStatusData()
{
	xr_string WorkDir;
	{
		xrSRWLockGuard Guard(Mutex);
		WorkDir = RepositoryRoot;
	}

	SExecResult Result = Execute("status --porcelain=v1 -b -z -uall --no-renames", WorkDir);

	xr_hash_map<xr_string, EGitFileInfo> NewFiles;
	xr_hash_map<xr_string, bool> NewDirectories;
	xr_string NewBranch;
	xr_string NewUpstream;
	u32 NewAhead = 0;
	u32 NewBehind = 0;
	bool NewHasCommits = true;

	if (Result.ExitCode == 0)
	{
		size_t Start = 0;
		while (Start < Result.Output.size())
		{
			size_t End = Result.Output.find('\0', Start);
			if (End == xr_string::npos)
			{
				End = Result.Output.size();
			}

			xr_string Token = Result.Output.substr(Start, End - Start);
			Start = End + 1;

			if (Token.empty())
			{
				continue;
			}

			if (Token.starts_with("##"))
			{
				xr_string Header = TrimSpaces(Token.substr(2));
				if (Header.StartWith("HEAD (no branch)"))
				{
					NewBranch = "(detached)";
				}
				else if (Header.StartWith("No commits yet on "))
				{
					// Fresh `git init` repository without any commit:
					// header looks like "No commits yet on master"
					NewBranch = TrimSpaces(Header.substr(sizeof("No commits yet on ") - 1));
					NewHasCommits = false;
				}
				else
				{
					size_t Dots = Header.find("...");
					size_t Bracket = Header.find('[');
					size_t UpstreamEnd = Bracket == xr_string::npos ? Header.size() : Bracket;

					if (Dots != xr_string::npos)
					{
						NewBranch = TrimSpaces(Header.substr(0, Dots));
						NewUpstream = TrimSpaces(Header.substr(Dots + 3, UpstreamEnd - Dots - 3));
					}
					else
					{
						NewBranch = TrimSpaces(Header.substr(0, UpstreamEnd));
					}

					if (Bracket != xr_string::npos)
					{
						xr_string State = Header.substr(Bracket + 1, Header.find(']', Bracket) - Bracket - 1);
						size_t AheadPosition = State.find("ahead ");
						size_t BehindPosition = State.find("behind ");

						if (AheadPosition != xr_string::npos)
						{
							NewAhead = static_cast<u32>(atoi(State.c_str() + AheadPosition + 6));
						}

						if (BehindPosition != xr_string::npos)
						{
							NewBehind = static_cast<u32>(atoi(State.c_str() + BehindPosition + 7));
						}
					}
				}

				continue;
			}

			if (Token.size() < 4)
			{
				continue;
			}

			const char IndexStatus = Token[0];
			const char WorkTreeStatus = Token[1];
			xr_string FilePath = Token.substr(3);

			if (WorkTreeStatus == '!')
			{
				continue;
			}

			EGitFileInfo Info;
			Info.Staged = IndexStatus != ' ' && IndexStatus != '?';
			Info.Unstaged = WorkTreeStatus != ' ';
			Info.Lfs = false;

			if (IndexStatus == 'U' || WorkTreeStatus == 'U' ||
				(IndexStatus == 'D' && WorkTreeStatus == 'D') ||
				(IndexStatus == 'A' && WorkTreeStatus == 'A'))
			{
				Info.Status = EGitFileStatus::Conflicted;
			}
			else if (WorkTreeStatus == '?')
			{
				Info.Status = EGitFileStatus::Untracked;
				Info.Unstaged = true;
			}
			else if (WorkTreeStatus == 'D' || IndexStatus == 'D')
			{
				Info.Status = EGitFileStatus::Deleted;
			}
			else if (IndexStatus == 'A' || WorkTreeStatus == 'A')
			{
				Info.Status = EGitFileStatus::Added;
			}
			else if (IndexStatus == 'R' || WorkTreeStatus == 'R')
			{
				Info.Status = EGitFileStatus::Renamed;
			}
			else if (IndexStatus == 'C' || WorkTreeStatus == 'C')
			{
				Info.Status = EGitFileStatus::Copied;
			}
			else
			{
				Info.Status = EGitFileStatus::Modified;
			}

			xr_string Key = NormalizeKey(FilePath);
			if (Key.empty())
			{
				continue;
			}

			NewFiles[Key] = Info;

			size_t Slash = Key.find('/');
			while (Slash != xr_string::npos)
			{
				NewDirectories[Key.substr(0, Slash)] = true;
				Slash = Key.find('/', Slash + 1);
			}
		}
	}

	{
		xrSRWLockGuard Guard(Mutex);
		Files.swap(NewFiles);
		DirtyDirectories.swap(NewDirectories);
		Branch = std::move(NewBranch);
		Upstream = std::move(NewUpstream);
		Ahead = NewAhead;
		Behind = NewBehind;
		LastRefreshTick = std::chrono::steady_clock::now().time_since_epoch().count();
		if (Result.ExitCode == 0)
		{
			HasCommits = NewHasCommits;
		}
	}

	if (!LfsFiles.empty())
	{
		xrSRWLockGuard Guard(Mutex);
		for (auto& [Path, Info] : Files)
		{
			Info.Lfs = LfsFiles.contains(Path);
		}
	}
}

void CGitIntegration::UpdateLfsFiles()
{
	xr_string WorkDir;
	{
		xrSRWLockGuard Guard(Mutex);
		WorkDir = RepositoryRoot;
	}

	SExecResult Result = Execute("lfs ls-files -n", WorkDir);

	xr_hash_map<xr_string, bool> NewLfsFiles;
	if (Result.ExitCode == 0)
	{
		size_t LineStart = 0;
		while (LineStart < Result.Output.size())
		{
			size_t LineEnd = Result.Output.find_first_of("\r\n", LineStart);
			if (LineEnd == xr_string::npos)
			{
				LineEnd = Result.Output.size();
			}

			xr_string Line = TrimSpaces(Result.Output.substr(LineStart, LineEnd - LineStart));
			LineStart = LineEnd + 1;

			if (!Line.empty())
			{
				NewLfsFiles[NormalizeKey(Line)] = true;
			}
		}
	}

	xrSRWLockGuard Guard(Mutex);
	LfsFiles.swap(NewLfsFiles);

	for (auto& [Path, Info] : Files)
	{
		Info.Lfs = LfsFiles.contains(Path);
	}
}

void CGitIntegration::UpdateHistory()
{
	xr_string WorkDir;
	{
		xrSRWLockGuard Guard(Mutex, true);
		WorkDir = RepositoryRoot;
	}

	SExecResult Result = Execute("log -n 50 --date=short --pretty=format:%H%x1f%h%x1f%an%x1f%ad%x1f%s%x1e", WorkDir);

	xr_vector<SGitCommit> NewHistory;
	if (Result.ExitCode == 0)
	{
		size_t Start = 0;
		while (Start < Result.Output.size())
		{
			size_t End = Result.Output.find('\x1e', Start);
			if (End == xr_string::npos)
			{
				End = Result.Output.size();
			}

			xr_string Record = TrimSpaces(Result.Output.substr(Start, End - Start));
			Start = End + 1;

			if (Record.empty())
			{
				continue;
			}

			xr_vector<xr_string> Fields;
			size_t FieldStart = 0;
			while (FieldStart <= Record.size())
			{
				size_t FieldEnd = Record.find('\x1f', FieldStart);
				if (FieldEnd == xr_string::npos)
				{
					Fields.push_back(Record.substr(FieldStart));
					break;
				}

				Fields.push_back(Record.substr(FieldStart, FieldEnd - FieldStart));
				FieldStart = FieldEnd + 1;
			}

			if (Fields.size() != 5)
			{
				continue;
			}

			SGitCommit Commit;
			Commit.Hash = Fields[0];
			Commit.ShortHash = Fields[1];
			Commit.Author = Fields[2];
			Commit.Date = Fields[3];
			Commit.Subject = Fields[4];
			NewHistory.push_back(std::move(Commit));
		}
	}

	xrSRWLockGuard Guard(Mutex);
	History.swap(NewHistory);
}

void CGitIntegration::UpdateBranchesAndRemotes()
{
	xr_string WorkDir;
	{
		xrSRWLockGuard Guard(Mutex, true);
		WorkDir = RepositoryRoot;
	}

	const auto ParseLines = [](const xr_string& Output, xr_vector<xr_string>& Out)
	{
		size_t Start = 0;
		while (Start < Output.size())
		{
			size_t End = Output.find('\n', Start);
			if (End == xr_string::npos)
			{
				End = Output.size();
			}

			xr_string Line = TrimSpaces(Output.substr(Start, End - Start));
			Start = End + 1;

			if (!Line.empty())
			{
				Out.push_back(Line);
			}
		}
	};

	SExecResult RemotesResult = Execute("remote", WorkDir);
	SExecResult BranchesResult = Execute("branch --format=%(refname:short)", WorkDir);

	xr_vector<xr_string> NewRemotes;
	xr_vector<xr_string> NewBranches;
	if (RemotesResult.ExitCode == 0)
	{
		ParseLines(RemotesResult.Output, NewRemotes);
	}
	if (BranchesResult.ExitCode == 0)
	{
		ParseLines(BranchesResult.Output, NewBranches);
	}

	xrSRWLockGuard Guard(Mutex);
	Remotes.swap(NewRemotes);
	Branches.swap(NewBranches);

	// Keep the selection valid: prefer the previous choice, then origin, then first.
	const auto HasRemote = [&](const xr_string& Name) -> bool
	{
		for (const auto& Remote : Remotes)
		{
			if (Remote == Name)
			{
				return true;
			}
		}
		return false;
	};

	if (!SelectedRemote.empty() && HasRemote(SelectedRemote))
	{
		return;
	}

	if (HasRemote("origin"))
	{
		SelectedRemote = "origin";
	}
	else if (!Remotes.empty())
	{
		SelectedRemote = Remotes.front();
	}
	else
	{
		SelectedRemote = "";
	}
}

bool CGitIntegration::BuildRepositoryKey(const xr_path& Path, xr_string& Out) const
{
	if (!IsRepository)
	{
		return false;
	}

	xr_string RootKey;
	{
		xrSRWLockGuard Guard(Mutex, true);
		RootKey = NormalizeKey(RepositoryRoot);
	}

	xr_string Candidate = NormalizeKey(Path.xstring());
	if (Candidate.size() <= RootKey.size() || Candidate.compare(0, RootKey.size(), RootKey) != 0)
	{
		return false;
	}

	Out = Candidate.substr(RootKey.size() + 1);
	return !Out.empty();
}
