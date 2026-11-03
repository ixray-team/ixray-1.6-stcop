#include "stdafx.h"
#include "../../CrashDumpHelper.h"
#include "CrashDumpHelperProtocol.h"

namespace CrashDumpHelper
{
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

static SharedState* Shared = nullptr;
static HANDLE RequestEvent = nullptr;
static HANDLE DoneEvent = nullptr;
static HANDLE HelperProcess = nullptr;

static bool ResolveHelperPath(char* Path, size_t Size)
{
	char Executable[MAX_PATH];
	const DWORD Length = GetModuleFileNameA(nullptr, Executable, MAX_PATH);
	if (Length == 0 || Length == MAX_PATH)
	{
		return false;
	}

	char* Separator = strrchr(Executable, '\\');
	if (Separator == nullptr)
	{
		return false;
	}
	*(Separator + 1) = '\0';

	const size_t ExecutableLength = strlen(Executable);
	const size_t NameLength = strlen(HelperExecutableName);
	if (ExecutableLength + NameLength >= Size)
	{
		return false;
	}
	memcpy(Path, Executable, ExecutableLength);
	memcpy(Path + ExecutableLength, HelperExecutableName, NameLength + 1);
	return true;
}

void Start()
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
	State->Magic = SharedStateMagic;
	State->Size = DWORD(sizeof(SharedState));
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

	char HelperPath[MAX_PATH * 2];
	if (!ResolveHelperPath(HelperPath, sizeof(HelperPath)))
	{
		Msg("! Crash dump helper: cannot resolve the executable path");
		UnmapViewOfFile(State);
		return;
	}
	if (GetFileAttributesA(HelperPath) == INVALID_FILE_ATTRIBUTES)
	{
		Msg("! Crash dump helper not found: %s", HelperPath);
		UnmapViewOfFile(State);
		return;
	}

	char CommandLine[MAX_PATH * 4];
	sprintf_s(CommandLine, "\"%s\" %s %llu %llu %llu %llu", HelperPath, HelperArgument,
		(unsigned long long)Mapping.Get(), (unsigned long long)Request.Get(), (unsigned long long)Done.Get(), (unsigned long long)Process.Get());

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
	const BOOL IsStarted = CreateProcessA(HelperPath, CommandLine, nullptr, nullptr, TRUE, EXTENDED_STARTUPINFO_PRESENT | CREATE_NO_WINDOW | BELOW_NORMAL_PRIORITY_CLASS, nullptr, nullptr, &Startup.StartupInfo, &Info);
	const DWORD Error = IsStarted ? ERROR_SUCCESS : GetLastError();
	DeleteProcThreadAttributeList(Attributes);
	if (!IsStarted)
	{
		Msg("! Crash dump helper failed to start (error %u)", Error);
		UnmapViewOfFile(State);
		return;
	}
	CloseHandle(Info.hThread);

	Shared = State;
	RequestEvent = Request.Release();
	DoneEvent = Done.Release();
	HelperProcess = Info.hProcess;
}

bool Write(void* ExceptionPointers)
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
} // namespace CrashDumpHelper
