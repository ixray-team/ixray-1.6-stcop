#include "stdafx.h"
#include "../../ThreadStackDump.h"
#include <dbghelp.h>
#include <tlhelp32.h>
#include <cstdio>
#include <cstring>
#include <thread>

xrCriticalSection g_ThreadStackSymbolLock;

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

bool WriteReport(HANDLE Report, const char* Message)
{
	if (Report == nullptr || Report == INVALID_HANDLE_VALUE)
	{
		return false;
	}

	DWORD Written = 0;
	const DWORD Length = DWORD(std::strlen(Message));
	return WriteFile(Report, Message, Length, &Written, nullptr) && Written == Length;
}

class SuspendedThread final
{
public:
	explicit SuspendedThread(HANDLE Thread, bool IsCurrent) : Thread(Thread), IsSuspended(!IsCurrent && SuspendThread(Thread) != DWORD(-1)), IsCurrent(IsCurrent) {}
	~SuspendedThread()
	{
		if (IsSuspended)
		{
			ResumeThread(Thread);
		}
	}
	SuspendedThread(const SuspendedThread&) = delete;
	SuspendedThread& operator=(const SuspendedThread&) = delete;
	bool IsValid() const { return IsCurrent || IsSuspended; }

private:
	HANDLE Thread;
	bool IsSuspended;
	bool IsCurrent;
};

std::jthread StackListener;
} // namespace

bool DumpThreadStacks(const char* Path)
{
	xrCriticalSectionGuard Lock(g_ThreadStackSymbolLock);
	ScopedHandle Report(CreateFileA(Path, GENERIC_WRITE, FILE_SHARE_READ, nullptr, CREATE_NEW, FILE_ATTRIBUTE_NORMAL, nullptr));
	if (Report.Get() == INVALID_HANDLE_VALUE)
	{
		return false;
	}
	bool IsWritten = WriteReport(Report.Get(), "Thread stack capture\r\n");
	FlushFileBuffers(Report.Get());
	HANDLE ProcessHandle = nullptr;
	if (!DuplicateHandle(GetCurrentProcess(), GetCurrentProcess(), GetCurrentProcess(), &ProcessHandle, 0, FALSE, DUPLICATE_SAME_ACCESS))
	{
		char Message[128];
		std::snprintf(Message, sizeof(Message), "Cannot open process: error %lu\r\n", GetLastError());
		WriteReport(Report.Get(), Message);
		FlushFileBuffers(Report.Get());
		return false;
	}
	ScopedHandle ProcessOwner(ProcessHandle);
	HANDLE Process = ProcessOwner.Get();
	const auto ProcessId = GetProcessId(Process);
	ScopedHandle Snapshot(CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD, 0));
	if (Snapshot.Get() == INVALID_HANDLE_VALUE)
	{
		IsWritten &= WriteReport(Report.Get(), "Cannot enumerate threads\r\n");
		FlushFileBuffers(Report.Get());
		return false;
	}

	SymSetOptions(SymGetOptions() | SYMOPT_DEFERRED_LOADS | SYMOPT_LOAD_LINES | SYMOPT_UNDNAME);
	const bool HasSymbols = SymInitialize(Process, nullptr, TRUE) != FALSE;
	if (!HasSymbols)
	{
		char Message[128];
		std::snprintf(Message, sizeof(Message), "Symbol initialization failed: error %lu\r\n", GetLastError());
		WriteReport(Report.Get(), Message);
		FlushFileBuffers(Report.Get());
		return false;
	}
	THREADENTRY32 Entry = {};
	Entry.dwSize = sizeof(Entry);
	bool HasThreads = false;
	for (bool HasEntry = Thread32First(Snapshot.Get(), &Entry) != FALSE; HasEntry; HasEntry = Thread32Next(Snapshot.Get(), &Entry) != FALSE)
	{
		if (Entry.th32OwnerProcessID != ProcessId)
		{
			continue;
		}
		HasThreads = true;
		char Message[2048];
		std::snprintf(Message, sizeof(Message), "\r\nThread %lu:\r\n", Entry.th32ThreadID);
		IsWritten &= WriteReport(Report.Get(), Message);
		ScopedHandle Thread(OpenThread(THREAD_SUSPEND_RESUME | THREAD_GET_CONTEXT | THREAD_QUERY_INFORMATION, FALSE, Entry.th32ThreadID));
		if (Thread.Get() == nullptr)
		{
			IsWritten &= WriteReport(Report.Get(), "Cannot open thread (it may have exited)\r\n");
			continue;
		}
		{
			// The calling thread cannot suspend itself.
			const bool IsCurrent = Entry.th32ThreadID == GetCurrentThreadId();
			SuspendedThread Suspension(Thread.Get(), IsCurrent);
			CONTEXT Context = {};
			Context.ContextFlags = CONTEXT_FULL;
			if (IsCurrent)
			{
				RtlCaptureContext(&Context);
			}
			if (!Suspension.IsValid() || (!IsCurrent && !GetThreadContext(Thread.Get(), &Context)))
			{
				IsWritten &= WriteReport(Report.Get(), "Cannot capture thread context\r\n");
				continue;
			}
			STACKFRAME64 Frame = {};
#if defined(_M_X64) || defined(__x86_64__)
			constexpr auto Machine = IMAGE_FILE_MACHINE_AMD64;
			Frame.AddrPC.Offset = Context.Rip;
			Frame.AddrStack.Offset = Context.Rsp;
			Frame.AddrFrame.Offset = Context.Rbp;
#elif defined(_M_ARM64) || defined(__aarch64__)
			constexpr auto Machine = IMAGE_FILE_MACHINE_ARM64;
			Frame.AddrPC.Offset = Context.Pc;
			Frame.AddrStack.Offset = Context.Sp;
			Frame.AddrFrame.Offset = Context.Fp;
#else
			constexpr auto Machine = IMAGE_FILE_MACHINE_I386;
			Frame.AddrPC.Offset = Context.Eip;
			Frame.AddrStack.Offset = Context.Esp;
			Frame.AddrFrame.Offset = Context.Ebp;
#endif
			Frame.AddrPC.Mode = Frame.AddrStack.Mode = Frame.AddrFrame.Mode = AddrModeFlat;
			for (unsigned int Index = 0; Frame.AddrPC.Offset != 0; ++Index)
			{
				const auto Address = Frame.AddrPC.Offset;
				alignas(SYMBOL_INFO) char SymbolBuffer[sizeof(SYMBOL_INFO) + MAX_SYM_NAME] = {};
				auto* Symbol = reinterpret_cast<SYMBOL_INFO*>(SymbolBuffer);
				Symbol->SizeOfStruct = sizeof(SYMBOL_INFO);
				Symbol->MaxNameLen = MAX_SYM_NAME;
				DWORD64 Offset = 0;
				IMAGEHLP_MODULE64 Module = {};
				Module.SizeOfStruct = sizeof(Module);
				const bool HasModule = HasSymbols && SymGetModuleInfo64(Process, Address, &Module);
				const bool HasSymbol = HasSymbols && SymFromAddr(Process, Address, &Offset, Symbol);
				std::snprintf(Message, sizeof(Message), "  %03u 0x%016llx %s!%s + 0x%llx\r\n", Index, static_cast<unsigned long long>(Address), HasModule ? Module.ModuleName : "?", HasSymbol ? Symbol->Name : "?", static_cast<unsigned long long>(Offset));
				IsWritten &= WriteReport(Report.Get(), Message);
				IMAGEHLP_LINE64 Line = {};
				Line.SizeOfStruct = sizeof(Line);
				DWORD Displacement = 0;
				if (HasSymbols && SymGetLineFromAddr64(Process, Address, &Displacement, &Line))
				{
					std::snprintf(Message, sizeof(Message), "      %s:%lu\r\n", Line.FileName, Line.LineNumber);
					IsWritten &= WriteReport(Report.Get(), Message);
				}
				const auto Stack = Frame.AddrStack.Offset;
				if (!StackWalk64(Machine, Process, Thread.Get(), &Frame, &Context, nullptr, SymFunctionTableAccess64, SymGetModuleBase64, nullptr))
				{
					IsWritten &= WriteReport(Report.Get(), "  End of stack (or unwind unavailable)\r\n");
					break;
				}
				if (Index != 0 && Frame.AddrPC.Offset == Address && Frame.AddrStack.Offset == Stack)
				{
					break;
				}
			}
		}
		IsWritten &= FlushFileBuffers(Report.Get()) != FALSE;
	}
	if (HasSymbols)
	{
		SymCleanup(Process);
	}
	IsWritten &= WriteReport(Report.Get(), "\r\nEnd of thread snapshot\r\n");
	return FlushFileBuffers(Report.Get()) != FALSE && HasThreads && IsWritten;
}

void StartThreadStackListener(const char* Folder)
{
	if (StackListener.joinable())
	{
		return;
	}
	StackListener = std::jthread([Folder = xr_string(Folder)](std::stop_token Stop)
								 {
		if (!RegisterHotKey(nullptr, 1, MOD_CONTROL | MOD_SHIFT | MOD_NOREPEAT, 'J'))
		{
			Msg("! Cannot register Ctrl+Shift+J for thread stacks (error %lu)", GetLastError());
			return;
		}
		MSG Message;
		while (!Stop.stop_requested())
		{
			while (PeekMessage(&Message, nullptr, 0, 0, PM_REMOVE))
			{
				if (Message.message != WM_HOTKEY || Message.wParam != 1)
				{
					continue;
				}
				SYSTEMTIME Time;
				GetLocalTime(&Time);
				char Path[MAX_PATH * 2];
				const int Length = std::snprintf(Path, sizeof(Path), "%s%sthread_stacks_%04u-%02u-%02u_%02u-%02u-%02u-%03u_%llu.txt",
					Folder.c_str(), !Folder.empty() && (Folder.back() == '\\' || Folder.back() == '/') ? "" : "\\",
					Time.wYear, Time.wMonth, Time.wDay, Time.wHour, Time.wMinute, Time.wSecond, Time.wMilliseconds,
					static_cast<unsigned long long>(GetTickCount64()));
				const bool IsPathValid = Length >= 0 && size_t(Length) < sizeof(Path);
				const bool IsWritten = IsPathValid && DumpThreadStacks(Path);
				char Notice[MAX_PATH * 2 + 128];
				if (IsWritten)
				{
					std::snprintf(Notice, sizeof(Notice), "Thread stacks saved:\n%s", Path);
				}
				else
				{
					std::snprintf(Notice, sizeof(Notice), "Failed to capture thread stacks.%s%s", IsPathValid ? "\nReport path: " : "", IsPathValid ? Path : "");
				}
				MessageBoxA(nullptr, Notice, "Thread stack capture", MB_OK | MB_SETFOREGROUND | MB_TOPMOST | (IsWritten ? MB_ICONINFORMATION : MB_ICONERROR));
			}
			Sleep(25);
		}
		UnregisterHotKey(nullptr, 1); });
}

void StopThreadStackListener()
{
	if (StackListener.joinable())
	{
		StackListener.request_stop();
		StackListener.join();
	}
}
