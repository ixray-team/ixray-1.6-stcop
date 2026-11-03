#include "../xrCore/Platform/Windows/CrashDumpHelperProtocol.h"

#include <cstdio>
#include <cstring>
#include <dbghelp.h>

namespace
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

private:
	HANDLE Value;
};

void WriteReport(HANDLE Report, const char* Message)
{
	if (Report == nullptr || Report == INVALID_HANDLE_VALUE)
	{
		return;
	}

	DWORD Written = 0;
	WriteFile(Report, Message, DWORD(std::strlen(Message)), &Written, nullptr);
}

void MakeDumpPath(char* Path, size_t Size, const CrashDumpHelper::SharedState& State, const SYSTEMTIME& Time, const char* Suffix)
{
	std::snprintf(Path, Size, "%s%s_%02d-%02d-%02d_%02d-%02d-%02d%s.mdmp", State.Folder, State.Prefix,
		Time.wMonth, Time.wDay, Time.wYear % 100, Time.wHour, Time.wMinute, Time.wSecond, Suffix);
}

BOOL WriteDump(HANDLE Process, const CrashDumpHelper::SharedState& State, const char* Path, MINIDUMP_TYPE Type, HANDLE Report)
{
	ScopedHandle File(CreateFileA(Path, GENERIC_WRITE, FILE_SHARE_READ | FILE_SHARE_DELETE, nullptr, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, nullptr));
	if (File.Get() == INVALID_HANDLE_VALUE)
	{
		const DWORD Error = GetLastError();
		char Message[MAX_PATH * 2 + 128];
		std::snprintf(Message, sizeof(Message), "CreateFile failed: error=0x%08lx path=%s\r\n", Error, Path);
		WriteReport(Report, Message);
		return FALSE;
	}

	MINIDUMP_EXCEPTION_INFORMATION Info = {};
	MINIDUMP_EXCEPTION_INFORMATION* InfoPtr = nullptr;
	if (State.Exception != nullptr)
	{
		Info.ThreadId = State.ThreadId;
		Info.ExceptionPointers = State.Exception;
		// The pointers are addresses in the crashed process
		Info.ClientPointers = TRUE;
		InfoPtr = &Info;
	}
	const BOOL IsWritten = MiniDumpWriteDump(Process, GetProcessId(Process), File.Get(), Type, InfoPtr, nullptr, nullptr);
	// DbgHelp returns an HRESULT through GetLastError; preserve it before closing the file.
	const DWORD Error = IsWritten ? ERROR_SUCCESS : GetLastError();
	char Message[MAX_PATH * 2 + 128];
	std::snprintf(Message, sizeof(Message), "MiniDumpWriteDump: flags=0x%08lx error=0x%08lx path=%s\r\n", DWORD(Type), Error, Path);
	WriteReport(Report, Message);
	if (!IsWritten)
	{
		DeleteFileA(Path);
	}
	return IsWritten;
}

int RunHelper(HANDLE Mapping, HANDLE Request, HANDLE Done, HANDLE Process)
{
	CrashDumpHelper::SharedState* State = (CrashDumpHelper::SharedState*)MapViewOfFile(Mapping, FILE_MAP_READ | FILE_MAP_WRITE, 0, 0, sizeof(CrashDumpHelper::SharedState));
	if (State == nullptr)
	{
		return 1;
	}
	if (State->Magic != CrashDumpHelper::SharedStateMagic || State->Size != sizeof(CrashDumpHelper::SharedState))
	{
		UnmapViewOfFile(State);
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
	char ReportPath[MAX_PATH * 2];
	MakeDumpPath(ReportPath, sizeof(ReportPath), *State, Time, "");
	std::strncat(ReportPath, ".txt", sizeof(ReportPath) - std::strlen(ReportPath) - 1);
	ScopedHandle Report(CreateFileA(ReportPath, GENERIC_WRITE, FILE_SHARE_READ, nullptr, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, nullptr));

	char Path[MAX_PATH * 2];
	MakeDumpPath(Path, sizeof(Path), *State, Time, "_small");
	const BOOL IsSmallWritten = WriteDump(Process, *State, Path, MINIDUMP_TYPE(MiniDumpNormal | MiniDumpWithThreadInfo), Report.Get());
	MakeDumpPath(Path, sizeof(Path), *State, Time, "");
	// IgnoreInaccessibleMemory applies to FullMemory, not PrivateReadWriteMemory. Keep all readable memory
	// even if a driver mapping or a region being freed by another thread cannot be read.
	const MINIDUMP_TYPE Common = MINIDUMP_TYPE(MiniDumpNormal | MiniDumpWithDataSegs | MiniDumpWithThreadInfo | MiniDumpWithFullMemoryInfo | MiniDumpWithHandleData);
	BOOL IsMemoryWritten = WriteDump(Process, *State, Path, MINIDUMP_TYPE(Common | MiniDumpWithFullMemory | MiniDumpIgnoreInaccessibleMemory), Report.Get());
	if (!IsMemoryWritten)
	{
		MakeDumpPath(Path, sizeof(Path), *State, Time, "_medium");
		IsMemoryWritten = WriteDump(Process, *State, Path, MINIDUMP_TYPE(Common | MiniDumpWithIndirectlyReferencedMemory | MiniDumpScanMemory), Report.Get());
	}
	State->IsWritten = (IsSmallWritten || IsMemoryWritten) ? TRUE : FALSE;
	SetEvent(Done);
	UnmapViewOfFile(State);
	return 0;
}
} // namespace

int APIENTRY WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
	// The parent quotes its own path, which may contain the argument text as well
	const char* Arguments = GetCommandLineA();
	if (*Arguments == '"')
	{
		const char* ClosingQuote = std::strchr(Arguments + 1, '"');
		if (ClosingQuote != nullptr)
		{
			Arguments = ClosingQuote + 1;
		}
	}
	const char* Argument = std::strstr(Arguments, CrashDumpHelper::HelperArgument);
	if (Argument == nullptr)
	{
		return 1;
	}

	unsigned long long Values[4] = {};
	if (std::sscanf(Argument + std::strlen(CrashDumpHelper::HelperArgument), "%llu %llu %llu %llu", &Values[0], &Values[1], &Values[2], &Values[3]) != 4)
	{
		return 1;
	}

	return RunHelper(HANDLE(Values[0]), HANDLE(Values[1]), HANDLE(Values[2]), HANDLE(Values[3]));
}
