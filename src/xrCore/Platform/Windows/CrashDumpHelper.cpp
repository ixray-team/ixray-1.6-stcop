#include "stdafx.h"
#include "../../CrashDumpHelper.h"

#include <dbghelp.h>

namespace CrashDumpHelper
{
constexpr const char* HelperArgument = "-crash_dump_helper";

class ScopedHandle final
{
public:
	explicit ScopedHandle(HANDLE Value) : Value(Value) {}
	~ScopedHandle()
	{
		if (Value != nullptr && Value != INVALID_HANDLE_VALUE)
		{
			CloseHandle(Value);
		}
	}
	ScopedHandle(const ScopedHandle&) = delete;
	ScopedHandle& operator=(const ScopedHandle&) = delete;
	HANDLE Get() const { return Value; }
	HANDLE Release()
	{
		HANDLE Result = Value;
		Value = nullptr;
		return Result;
	}

private:
	HANDLE Value;
};

// Shared with the helper through a file mapping: the crashing process fills it without allocating
struct SharedState
{
	DWORD ThreadId;
	EXCEPTION_POINTERS* Exception;
	char Folder[MAX_PATH];
	char Prefix[128];
	bool IsWritten;
};

static SharedState* Shared = nullptr;
static HANDLE RequestEvent = nullptr;
static HANDLE DoneEvent = nullptr;
static HANDLE HelperProcess = nullptr;

static void MakeDumpPath(char* Path, size_t Size, const SharedState& State, const SYSTEMTIME& Time, const char* Suffix)
{
	sprintf_s(Path, Size, "%s%s_%02d-%02d-%02d_%02d-%02d-%02d%s.mdmp", State.Folder, State.Prefix, Time.wMonth, Time.wDay, Time.wYear % 100, Time.wHour, Time.wMinute, Time.wSecond, Suffix);
}

static bool WriteDump(HANDLE Process, const SharedState& State, const char* Path, MINIDUMP_TYPE Type, HANDLE Report)
{
	ScopedHandle File(CreateFileA(Path, GENERIC_WRITE, FILE_SHARE_READ | FILE_SHARE_DELETE, nullptr, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, nullptr));
	if (File.Get() == INVALID_HANDLE_VALUE)
	{
		const DWORD Error = GetLastError();
		char Message[MAX_PATH * 2 + 128];
		const int Size = sprintf_s(Message, "CreateFile failed: error=0x%08lx path=%s\r\n", Error, Path);
		DWORD Written;
		WriteFile(Report, Message, Size, &Written, nullptr);
		return false;
	}

	MINIDUMP_EXCEPTION_INFORMATION Info = {};
	Info.ThreadId = State.ThreadId;
	Info.ExceptionPointers = State.Exception;
	// The pointers are addresses in the crashed process
	Info.ClientPointers = TRUE;
	const BOOL IsWritten = MiniDumpWriteDump(Process, GetProcessId(Process), File.Get(), Type, &Info, nullptr, nullptr);
	// DbgHelp returns an HRESULT through GetLastError; preserve it before closing the file.
	const DWORD Error = IsWritten ? ERROR_SUCCESS : GetLastError();
	char Message[MAX_PATH * 2 + 128];
	const int Size = sprintf_s(Message, "MiniDumpWriteDump: flags=0x%08lx error=0x%08lx path=%s\r\n", DWORD(Type), Error, Path);
	DWORD Written;
	WriteFile(Report, Message, Size, &Written, nullptr);
	if (!IsWritten)
	{
		DeleteFileA(Path);
	}
	return IsWritten != FALSE;
}

static int RunHelper(HANDLE Mapping, HANDLE Request, HANDLE Done, HANDLE Process)
{
	SharedState* State = (SharedState*)MapViewOfFile(Mapping, FILE_MAP_READ | FILE_MAP_WRITE, 0, 0, sizeof(SharedState));
	if (State == nullptr)
	{
		return 1;
	}

	// The helper ends with its process; a crash wakes it up first
	HANDLE Waits[2] = {Request, Process};
	if (WaitForMultipleObjects(2, Waits, FALSE, INFINITE) != WAIT_OBJECT_0)
	{
		UnmapViewOfFile(State);
		return 0;
	}

	SYSTEMTIME Time;
	GetLocalTime(&Time);
	char Path[MAX_PATH * 2];
	MakeDumpPath(Path, sizeof(Path), *State, Time, "");
	strcat_s(Path, ".txt");
	ScopedHandle Report(CreateFileA(Path, GENERIC_WRITE, FILE_SHARE_READ, nullptr, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, nullptr));
	MakeDumpPath(Path, sizeof(Path), *State, Time, "_small");
	const bool IsSmallWritten = WriteDump(Process, *State, Path, MINIDUMP_TYPE(MiniDumpNormal | MiniDumpWithThreadInfo), Report.Get());
	MakeDumpPath(Path, sizeof(Path), *State, Time, "");
	// IgnoreInaccessibleMemory applies to FullMemory, not PrivateReadWriteMemory. Keep all readable memory
	// even if a driver mapping or a region being freed by another thread cannot be read.
	const MINIDUMP_TYPE Common = MINIDUMP_TYPE(MiniDumpNormal | MiniDumpWithDataSegs | MiniDumpWithThreadInfo | MiniDumpWithFullMemoryInfo | MiniDumpWithHandleData);
	bool IsMemoryWritten = WriteDump(Process, *State, Path, MINIDUMP_TYPE(Common | MiniDumpWithFullMemory | MiniDumpIgnoreInaccessibleMemory), Report.Get());
	if (!IsMemoryWritten)
	{
		MakeDumpPath(Path, sizeof(Path), *State, Time, "_medium");
		IsMemoryWritten = WriteDump(Process, *State, Path, MINIDUMP_TYPE(Common | MiniDumpWithIndirectlyReferencedMemory | MiniDumpScanMemory), Report.Get());
	}
	State->IsWritten = IsSmallWritten || IsMemoryWritten;
	SetEvent(Done);
	UnmapViewOfFile(State);
	return 0;
}
} // namespace CrashDumpHelper

bool CrashDumpHelper::RunIfRequested()
{
	const char* CommandLine = GetCommandLineA();
	const char* Argument = strstr(CommandLine, HelperArgument);
	if (Argument == nullptr)
	{
		return false;
	}

	unsigned long long Values[4] = {};
	if (sscanf_s(Argument + strlen(HelperArgument), "%llu %llu %llu %llu", &Values[0], &Values[1], &Values[2], &Values[3]) != 4)
	{
		return true;
	}

	RunHelper(HANDLE(Values[0]), HANDLE(Values[1]), HANDLE(Values[2]), HANDLE(Values[3]));
	return true;
}

void CrashDumpHelper::Start()
{
	if (Shared != nullptr)
	{
		return;
	}

	SECURITY_ATTRIBUTES Inherit = {sizeof(SECURITY_ATTRIBUTES), nullptr, TRUE};
	ScopedHandle Mapping(CreateFileMappingA(INVALID_HANDLE_VALUE, &Inherit, PAGE_READWRITE, 0, sizeof(SharedState), nullptr));
	if (Mapping.Get() == nullptr)
	{
		return;
	}
	SharedState* State = (SharedState*)MapViewOfFile(Mapping.Get(), FILE_MAP_WRITE, 0, 0, sizeof(SharedState));
	if (State == nullptr)
	{
		return;
	}
	ScopedHandle Request(CreateEventA(&Inherit, FALSE, FALSE, nullptr));
	ScopedHandle Done(CreateEventA(&Inherit, FALSE, FALSE, nullptr));
	HANDLE ProcessHandle = nullptr;
	DuplicateHandle(GetCurrentProcess(), GetCurrentProcess(), GetCurrentProcess(), &ProcessHandle, PROCESS_QUERY_INFORMATION | PROCESS_VM_READ | PROCESS_DUP_HANDLE | SYNCHRONIZE, TRUE, 0);
	ScopedHandle Process(ProcessHandle);
	if (Request.Get() == nullptr || Done.Get() == nullptr || Process.Get() == nullptr)
	{
		UnmapViewOfFile(State);
		return;
	}

	*State = {};
	string_path Folder;
	if (FS.path_exist("$logs$"))
	{
		FS.update_path(Folder, "$logs$", "");
	}
	else
	{
		xr_strcpy(Folder, "logs\\");
	}
	xr_strcpy(State->Folder, Folder);
	sprintf_s(State->Prefix, "%s_%s", Core.ApplicationName, Core.UserName);

	char Executable[MAX_PATH];
	GetModuleFileNameA(nullptr, Executable, MAX_PATH);
	char CommandLine[MAX_PATH * 2];
	sprintf_s(CommandLine, "\"%s\" %s %llu %llu %llu %llu", Executable, HelperArgument, (unsigned long long)Mapping.Get(), (unsigned long long)Request.Get(), (unsigned long long)Done.Get(), (unsigned long long)Process.Get());

	// Only these four handles reach the helper
	HANDLE Handles[4] = {Mapping.Get(), Request.Get(), Done.Get(), Process.Get()};
	SIZE_T AttributeSize = 0;
	InitializeProcThreadAttributeList(nullptr, 1, 0, &AttributeSize);
	xr_vector<u8> AttributeBuffer(AttributeSize);
	LPPROC_THREAD_ATTRIBUTE_LIST Attributes = (LPPROC_THREAD_ATTRIBUTE_LIST)AttributeBuffer.data();
	if (!InitializeProcThreadAttributeList(Attributes, 1, 0, &AttributeSize))
	{
		UnmapViewOfFile(State);
		return;
	}
	if (!UpdateProcThreadAttribute(Attributes, 0, PROC_THREAD_ATTRIBUTE_HANDLE_LIST, Handles, sizeof(Handles), nullptr, nullptr))
	{
		DeleteProcThreadAttributeList(Attributes);
		UnmapViewOfFile(State);
		return;
	}

	STARTUPINFOEXA Startup = {};
	Startup.StartupInfo.cb = sizeof(Startup);
	Startup.lpAttributeList = Attributes;
	PROCESS_INFORMATION Info = {};
	const BOOL IsStarted = CreateProcessA(Executable, CommandLine, nullptr, nullptr, TRUE, EXTENDED_STARTUPINFO_PRESENT | CREATE_NO_WINDOW | BELOW_NORMAL_PRIORITY_CLASS, nullptr, nullptr, &Startup.StartupInfo, &Info);
	const DWORD Error = IsStarted ? ERROR_SUCCESS : GetLastError();
	DeleteProcThreadAttributeList(Attributes);
	if (!IsStarted)
	{
		UnmapViewOfFile(State);
		Msg("! Crash dump helper failed to start (error %u)", Error);
		return;
	}
	CloseHandle(Info.hThread);

	Shared = State;
	RequestEvent = Request.Release();
	DoneEvent = Done.Release();
	HelperProcess = Info.hProcess;
}

bool CrashDumpHelper::Write(void* ExceptionPointers)
{
	if (Shared == nullptr)
	{
		return false;
	}

	Shared->ThreadId = GetCurrentThreadId();
	Shared->Exception = (EXCEPTION_POINTERS*)ExceptionPointers;
	if (!SetEvent(RequestEvent))
	{
		return false;
	}
	// A full dump of a large process takes a while
	HANDLE Waits[] = {DoneEvent, HelperProcess};
	return WaitForMultipleObjects(2, Waits, FALSE, 5 * 60 * 1000) == WAIT_OBJECT_0 && Shared->IsWritten;
}
