#include "stdafx.h"
#include "../../CrashDumpHelper.h"

#include <dbghelp.h>

namespace
{
	constexpr const char* HelperArgument = "-crash_dump_helper";

	// Shared with the helper through a file mapping: the crashing process fills it without allocating
	struct SharedState
	{
		DWORD ThreadId;
		EXCEPTION_POINTERS* Exception;
		char Folder[MAX_PATH];
		char Prefix[128];
	};

	SharedState* Shared = nullptr;
	HANDLE RequestEvent = nullptr;
	HANDLE DoneEvent = nullptr;

	void MakeDumpPath(char* Path, size_t Size, const SharedState& State, const SYSTEMTIME& Time, const char* Suffix)
	{
		sprintf_s(Path, Size, "%s%s_%02d-%02d-%02d_%02d-%02d-%02d%s.mdmp", State.Folder, State.Prefix,
			Time.wMonth, Time.wDay, Time.wYear % 100, Time.wHour, Time.wMinute, Time.wSecond, Suffix);
	}

	bool WriteDump(HANDLE Process, const SharedState& State, const char* Path, MINIDUMP_TYPE Type)
	{
		HANDLE File = CreateFileA(Path, GENERIC_WRITE, 0, nullptr, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, nullptr);
		if (File == INVALID_HANDLE_VALUE)
			return false;

		MINIDUMP_EXCEPTION_INFORMATION Info = {};
		Info.ThreadId = State.ThreadId;
		Info.ExceptionPointers = State.Exception;
		// The pointers are addresses in the crashed process
		Info.ClientPointers = TRUE;
		const BOOL IsWritten = MiniDumpWriteDump(Process, GetProcessId(Process), File, Type, &Info, nullptr, nullptr);
		CloseHandle(File);
		return IsWritten != FALSE;
	}

	int RunHelper(HANDLE Mapping, HANDLE Request, HANDLE Done, HANDLE Process)
	{
		const SharedState* State = (const SharedState*)MapViewOfFile(Mapping, FILE_MAP_READ, 0, 0, sizeof(SharedState));
		if (State == nullptr)
			return 1;

		// The helper ends with its process; a crash wakes it up first
		HANDLE Waits[2] = { Request, Process };
		if (WaitForMultipleObjects(2, Waits, FALSE, INFINITE) != WAIT_OBJECT_0)
			return 0;

		SYSTEMTIME Time;
		GetLocalTime(&Time);
		char Path[MAX_PATH * 2];
		MakeDumpPath(Path, sizeof(Path), *State, Time, "_small");
		WriteDump(Process, *State, Path, MINIDUMP_TYPE(MiniDumpNormal | MiniDumpWithThreadInfo));
		MakeDumpPath(Path, sizeof(Path), *State, Time, "");
		// Regions of a running process may be unreadable (ERROR_PARTIAL_COPY): skip them, and fall back to the
		// referenced memory only if the full dump still fails
		const MINIDUMP_TYPE Common = MINIDUMP_TYPE(MiniDumpNormal | MiniDumpWithDataSegs | MiniDumpWithThreadInfo
			| MiniDumpWithFullMemoryInfo | MiniDumpWithHandleData | MiniDumpIgnoreInaccessibleMemory);
		if (!WriteDump(Process, *State, Path, MINIDUMP_TYPE(Common | MiniDumpWithPrivateReadWriteMemory)))
		{
			WriteDump(Process, *State, Path, MINIDUMP_TYPE(Common | MiniDumpWithIndirectlyReferencedMemory | MiniDumpScanMemory));
		}
		SetEvent(Done);
		return 0;
	}
}

bool CrashDumpHelper::RunIfRequested()
{
	const char* CommandLine = GetCommandLineA();
	const char* Argument = strstr(CommandLine, HelperArgument);
	if (Argument == nullptr)
		return false;

	unsigned long long Values[4] = {};
	if (sscanf_s(Argument + strlen(HelperArgument), "%llu %llu %llu %llu", &Values[0], &Values[1], &Values[2], &Values[3]) != 4)
		return true;

	RunHelper(HANDLE(Values[0]), HANDLE(Values[1]), HANDLE(Values[2]), HANDLE(Values[3]));
	return true;
}

void CrashDumpHelper::Start()
{
	if (Shared != nullptr)
		return;

	SECURITY_ATTRIBUTES Inherit = { sizeof(SECURITY_ATTRIBUTES), nullptr, TRUE };
	HANDLE Mapping = CreateFileMappingA(INVALID_HANDLE_VALUE, &Inherit, PAGE_READWRITE, 0, sizeof(SharedState), nullptr);
	if (Mapping == nullptr)
		return;
	SharedState* State = (SharedState*)MapViewOfFile(Mapping, FILE_MAP_WRITE, 0, 0, sizeof(SharedState));
	HANDLE Request = CreateEventA(&Inherit, FALSE, FALSE, nullptr);
	HANDLE Done = CreateEventA(&Inherit, FALSE, FALSE, nullptr);
	HANDLE Process = nullptr;
	DuplicateHandle(GetCurrentProcess(), GetCurrentProcess(), GetCurrentProcess(), &Process,
		PROCESS_QUERY_INFORMATION | PROCESS_VM_READ | PROCESS_DUP_HANDLE | SYNCHRONIZE, TRUE, 0);
	if (State == nullptr || Request == nullptr || Done == nullptr || Process == nullptr)
		return;

	*State = {};
	string_path Folder;
	if (FS.path_exist("$logs$"))
		FS.update_path(Folder, "$logs$", "");
	else
		xr_strcpy(Folder, "logs\\");
	xr_strcpy(State->Folder, Folder);
	sprintf_s(State->Prefix, "%s_%s", Core.ApplicationName, Core.UserName);

	char Executable[MAX_PATH];
	GetModuleFileNameA(nullptr, Executable, MAX_PATH);
	char CommandLine[MAX_PATH * 2];
	sprintf_s(CommandLine, "\"%s\" %s %llu %llu %llu %llu", Executable, HelperArgument,
		(unsigned long long)Mapping, (unsigned long long)Request, (unsigned long long)Done, (unsigned long long)Process);

	// Only these four handles reach the helper
	HANDLE Handles[4] = { Mapping, Request, Done, Process };
	SIZE_T AttributeSize = 0;
	InitializeProcThreadAttributeList(nullptr, 1, 0, &AttributeSize);
	xr_vector<u8> AttributeBuffer(AttributeSize);
	LPPROC_THREAD_ATTRIBUTE_LIST Attributes = (LPPROC_THREAD_ATTRIBUTE_LIST)AttributeBuffer.data();
	if (!InitializeProcThreadAttributeList(Attributes, 1, 0, &AttributeSize))
		return;
	UpdateProcThreadAttribute(Attributes, 0, PROC_THREAD_ATTRIBUTE_HANDLE_LIST, Handles, sizeof(Handles), nullptr, nullptr);

	STARTUPINFOEXA Startup = {};
	Startup.StartupInfo.cb = sizeof(Startup);
	Startup.lpAttributeList = Attributes;
	PROCESS_INFORMATION Info = {};
	const BOOL IsStarted = CreateProcessA(Executable, CommandLine, nullptr, nullptr, TRUE,
		EXTENDED_STARTUPINFO_PRESENT | CREATE_NO_WINDOW | BELOW_NORMAL_PRIORITY_CLASS, nullptr, nullptr, &Startup.StartupInfo, &Info);
	DeleteProcThreadAttributeList(Attributes);
	if (!IsStarted)
	{
		Msg("! Crash dump helper failed to start (error %u)", GetLastError());
		return;
	}
	CloseHandle(Info.hThread);
	CloseHandle(Info.hProcess);

	Shared = State;
	RequestEvent = Request;
	DoneEvent = Done;
}

bool CrashDumpHelper::Write(void* ExceptionPointers)
{
	if (Shared == nullptr)
		return false;

	Shared->ThreadId = GetCurrentThreadId();
	Shared->Exception = (EXCEPTION_POINTERS*)ExceptionPointers;
	SetEvent(RequestEvent);
	// A full dump of a large process takes a while
	return WaitForSingleObject(DoneEvent, 5 * 60 * 1000) == WAIT_OBJECT_0;
}
