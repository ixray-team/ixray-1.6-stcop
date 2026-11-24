#pragma once
#include "../Kernel/xrSyncronize.h"
#include <tlhelp32.h>
#include <cstdio>

extern xrCriticalSection g_ThreadStackSymbolLock;

namespace StackTrace
{
	using CrashHandler = void(*)();
	using DialogHandler = void(*)(bool);
	using OutOfMemoryCallbackFunc = void(*)();
	using FrameCallback = void(*)(const char* Line, void* UserData);

#if defined(IXR_ARM64)
#	define MACHINE_TYPE IMAGE_FILE_MACHINE_ARM64
#elif defined(IXR_WIN64)
#	define MACHINE_TYPE IMAGE_FILE_MACHINE_AMD64
#else
#	define MACHINE_TYPE IMAGE_FILE_MACHINE_I386
#endif

	inline bool TryLockSymbolEngine(u32 TimeoutMs)
	{
		for (u32 Elapsed = 0; Elapsed <= TimeoutMs; ++Elapsed)
		{
			if (g_ThreadStackSymbolLock.TryEnter())
			{
				return true;
			}
			if (Elapsed < TimeoutMs)
			{
				Sleep(1);
			}
		}
		return false;
	}

	inline void FormatStackFrame(HANDLE Process, DWORD64 Address, bool bHasSymbols, char* OutLine, size_t OutSize)
	{
		size_t Written = 0;

		char ModulePath[MAX_PATH] = {};
		const DWORD64 ModuleBase = bHasSymbols ? SymGetModuleBase64(Process, Address) : 0;
		if (ModuleBase != 0 && GetModuleFileNameA(reinterpret_cast<HMODULE>(ModuleBase), ModulePath, MAX_PATH) > 0)
		{
			int Added = std::snprintf(OutLine + Written, OutSize - Written, "%s", ModulePath);
			if (Added > 0)
			{
				Written += static_cast<size_t>(Added) < (OutSize - Written) ? static_cast<size_t>(Added) : (OutSize - Written - 1);
			}
		}
		else if (bHasSymbols)
		{
			IMAGEHLP_MODULE64 ModuleInfo = {};
			ModuleInfo.SizeOfStruct = sizeof(ModuleInfo);
			if (SymGetModuleInfo64(Process, Address, &ModuleInfo) && ModuleInfo.ModuleName[0] != '\0')
			{
				int Added = std::snprintf(OutLine + Written, OutSize - Written, "%s", ModuleInfo.ModuleName);
				if (Added > 0)
				{
					Written += static_cast<size_t>(Added) < (OutSize - Written) ? static_cast<size_t>(Added) : (OutSize - Written - 1);
				}
			}
		}

		{
			int Added = std::snprintf(OutLine + Written, OutSize - Written, "%sat 0x%016llx", Written > 0 ? " " : "", static_cast<unsigned long long>(Address));
			if (Added > 0)
			{
				Written += static_cast<size_t>(Added) < (OutSize - Written) ? static_cast<size_t>(Added) : (OutSize - Written - 1);
			}
		}

		if (bHasSymbols && Written + 1 < OutSize)
		{
			alignas(SYMBOL_INFO) char SymbolBuffer[sizeof(SYMBOL_INFO) + MAX_SYM_NAME] = {};
			auto* Symbol = reinterpret_cast<SYMBOL_INFO*>(SymbolBuffer);
			Symbol->SizeOfStruct = sizeof(SYMBOL_INFO);
			Symbol->MaxNameLen = MAX_SYM_NAME;
			DWORD64 Displacement = 0;

			if (SymFromAddr(Process, Address, &Displacement, Symbol))
			{
				int Added = Displacement != 0
					? std::snprintf(OutLine + Written, OutSize - Written, " %s() + %llu byte(s)", Symbol->Name, static_cast<unsigned long long>(Displacement))
					: std::snprintf(OutLine + Written, OutSize - Written, " %s()", Symbol->Name);
				if (Added > 0)
				{
					Written += static_cast<size_t>(Added) < (OutSize - Written) ? static_cast<size_t>(Added) : (OutSize - Written - 1);
				}
			}

			if (Written + 1 < OutSize)
			{
				DWORD LineDisplacement = 0;
				IMAGEHLP_LINE64 LineInfo = {};
				LineInfo.SizeOfStruct = sizeof(LineInfo);
				if (SymGetLineFromAddr64(Process, Address, &LineDisplacement, &LineInfo))
				{
					int Added = LineDisplacement != 0
						? std::snprintf(OutLine + Written, OutSize - Written, " in %s line %lu + %lu byte(s)", LineInfo.FileName, LineInfo.LineNumber, LineDisplacement)
						: std::snprintf(OutLine + Written, OutSize - Written, " in %s line %lu", LineInfo.FileName, LineInfo.LineNumber);
					if (Added > 0)
					{
						Written += static_cast<size_t>(Added) < (OutSize - Written) ? static_cast<size_t>(Added) : (OutSize - Written - 1);
					}
				}
			}
		}
	}

	inline void FormatThreadName(HANDLE ThreadHandle, char* OutName, size_t OutSize)
	{
		if (OutName == nullptr || OutSize == 0)
		{
			return;
		}
		OutName[0] = '\0';

		using GetThreadDescriptionFn = HRESULT(WINAPI*)(HANDLE, PWSTR*);
		static const auto GetThreadDesc = reinterpret_cast<GetThreadDescriptionFn>(
			GetProcAddress(GetModuleHandleA("kernel32.dll"), "GetThreadDescription")
		);
		if (GetThreadDesc == nullptr || ThreadHandle == nullptr)
		{
			return;
		}

		PWSTR WideDesc = nullptr;
		if (SUCCEEDED(GetThreadDesc(ThreadHandle, &WideDesc)) && WideDesc != nullptr)
		{
			if (WideDesc[0] != L'\0')
			{
				WideCharToMultiByte(CP_UTF8, 0, WideDesc, -1, OutName, static_cast<int>(OutSize), nullptr, nullptr);
			}
			LocalFree(WideDesc);
		}
	}

	inline void WalkContextFrames(HANDLE ProcessHandle, HANDLE ThreadHandle, const CONTEXT* ThreadCtx, u16 MaxFramesCount, bool bHasSymbols, FrameCallback OnFrame, void* UserData)
	{
		if (ThreadCtx == nullptr || OnFrame == nullptr || MaxFramesCount == 0)
		{
			return;
		}

		CONTEXT ContextCopy = *ThreadCtx;
		STACKFRAME64 Frame = {};
		Frame.AddrPC.Mode = AddrModeFlat;
		Frame.AddrStack.Mode = AddrModeFlat;
		Frame.AddrFrame.Mode = AddrModeFlat;

#if defined(IXR_ARM64)
		Frame.AddrPC.Offset = ContextCopy.Pc;
		Frame.AddrStack.Offset = ContextCopy.Sp;
		Frame.AddrFrame.Offset = ContextCopy.Fp;
#elif defined(IXR_WIN64)
		Frame.AddrPC.Offset = ContextCopy.Rip;
		Frame.AddrStack.Offset = ContextCopy.Rsp;
		Frame.AddrFrame.Offset = ContextCopy.Rbp;
#else
		Frame.AddrPC.Offset = ContextCopy.Eip;
		Frame.AddrStack.Offset = ContextCopy.Esp;
		Frame.AddrFrame.Offset = ContextCopy.Ebp;
#endif

		DWORD64 PrevPC = 0;
		DWORD64 PrevStack = 0;
		for (u16 FrameIndex = 0; FrameIndex < MaxFramesCount; ++FrameIndex)
		{
			if (!StackWalk64(MACHINE_TYPE, ProcessHandle, ThreadHandle, &Frame, &ContextCopy, nullptr,
				SymFunctionTableAccess64, SymGetModuleBase64, nullptr))
			{
				break;
			}

			if (Frame.AddrPC.Offset == 0)
			{
				break;
			}

			if (FrameIndex > 0 && Frame.AddrPC.Offset == PrevPC && Frame.AddrStack.Offset == PrevStack)
			{
				break;
			}
			PrevPC = Frame.AddrPC.Offset;
			PrevStack = Frame.AddrStack.Offset;

			char Line[1024] = {};
			FormatStackFrame(ProcessHandle, Frame.AddrPC.Offset, bHasSymbols, Line, sizeof(Line));
			OnFrame(Line, UserData);
		}
	}

	inline void WalkStackTrace(const CONTEXT* ThreadCtx, u16 MaxFramesCount, FrameCallback OnFrame, void* UserData)
	{
		if (ThreadCtx == nullptr || OnFrame == nullptr || MaxFramesCount == 0)
		{
			return;
		}

		const bool bLocked = TryLockSymbolEngine(200);

		// Use a duplicated process handle so our SymInitialize/SymCleanup session never collides with
		// another active DbgHelp user on GetCurrentProcess().
		HANDLE ProcessHandle = nullptr;
		bool bCloseProcessHandle = false;
		if (DuplicateHandle(GetCurrentProcess(), GetCurrentProcess(), GetCurrentProcess(), &ProcessHandle, 0, FALSE, DUPLICATE_SAME_ACCESS))
		{
			bCloseProcessHandle = true;
		}
		else
		{
			ProcessHandle = GetCurrentProcess();
		}

		SymSetOptions(SymGetOptions() | SYMOPT_DEFERRED_LOADS | SYMOPT_LOAD_LINES | SYMOPT_UNDNAME);
		const bool bHasSymbols = SymInitialize(ProcessHandle, nullptr, TRUE) != FALSE;

		WalkContextFrames(ProcessHandle, GetCurrentThread(), ThreadCtx, MaxFramesCount, bHasSymbols, OnFrame, UserData);

		if (bHasSymbols)
		{
			SymCleanup(ProcessHandle);
		}
		if (bCloseProcessHandle)
		{
			CloseHandle(ProcessHandle);
		}
		if (bLocked)
		{
			g_ThreadStackSymbolLock.Leave();
		}
	}

	// Captures raw instruction pointers from a CONTEXT without calling DbgHelp symbol lookup or any I/O.
	// Safe to invoke while the target thread is suspended; symbolize only after ResumeThread.
	inline u16 CaptureRawFrames(const CONTEXT* ThreadCtx, DWORD64* OutAddrs, u16 MaxFramesCount)
	{
		if (ThreadCtx == nullptr || OutAddrs == nullptr || MaxFramesCount == 0)
		{
			return 0;
		}

		CONTEXT ContextCopy = *ThreadCtx;
		u16 Count = 0;

#if defined(IXR_WIN64) && !defined(IXR_ARM64)
		// On x64, RtlVirtualUnwind walks the stack without touching DbgHelp or allocating memory.
		DWORD64 PrevPC = 0;
		DWORD64 PrevSP = 0;
		while (Count < MaxFramesCount && ContextCopy.Rip != 0)
		{
			if (Count > 0 && ContextCopy.Rip == PrevPC && ContextCopy.Rsp == PrevSP)
			{
				break;
			}
			PrevPC = ContextCopy.Rip;
			PrevSP = ContextCopy.Rsp;
			OutAddrs[Count++] = ContextCopy.Rip;

			DWORD64 ImageBase = 0;
			PRUNTIME_FUNCTION FunctionEntry = RtlLookupFunctionEntry(ContextCopy.Rip, &ImageBase, nullptr);
			if (FunctionEntry != nullptr)
			{
				PVOID HandlerData = nullptr;
				DWORD64 EstablisherFrame = 0;
				RtlVirtualUnwind(
					UNW_FLAG_NHANDLER,
					ImageBase,
					ContextCopy.Rip,
					FunctionEntry,
					&ContextCopy,
					&HandlerData,
					&EstablisherFrame,
					nullptr
				);
			}
			else
			{
				// Leaf function: return address is at [Rsp]
				if (ContextCopy.Rsp == 0 || (ContextCopy.Rsp & 7) != 0)
				{
					break;
				}
				DWORD64 ReturnAddr = 0;
				SIZE_T BytesRead = 0;
				if (!ReadProcessMemory(GetCurrentProcess(), reinterpret_cast<LPCVOID>(ContextCopy.Rsp), &ReturnAddr, sizeof(ReturnAddr), &BytesRead) || BytesRead != sizeof(ReturnAddr))
				{
					break;
				}
				ContextCopy.Rip = ReturnAddr;
				ContextCopy.Rsp += sizeof(DWORD64);
			}
		}
#else
		STACKFRAME64 Frame = {};
		Frame.AddrPC.Mode = AddrModeFlat;
		Frame.AddrStack.Mode = AddrModeFlat;
		Frame.AddrFrame.Mode = AddrModeFlat;
#	if defined(IXR_ARM64)
		Frame.AddrPC.Offset = ContextCopy.Pc;
		Frame.AddrStack.Offset = ContextCopy.Sp;
		Frame.AddrFrame.Offset = ContextCopy.Fp;
#	else
		Frame.AddrPC.Offset = ContextCopy.Eip;
		Frame.AddrStack.Offset = ContextCopy.Esp;
		Frame.AddrFrame.Offset = ContextCopy.Ebp;
#	endif
		DWORD64 PrevPC = 0;
		DWORD64 PrevStack = 0;
		while (Count < MaxFramesCount)
		{
			if (!StackWalk64(MACHINE_TYPE, GetCurrentProcess(), GetCurrentThread(), &Frame, &ContextCopy, nullptr,
				SymFunctionTableAccess64, SymGetModuleBase64, nullptr))
			{
				break;
			}
			if (Frame.AddrPC.Offset == 0)
			{
				break;
			}
			if (Count > 0 && Frame.AddrPC.Offset == PrevPC && Frame.AddrStack.Offset == PrevStack)
			{
				break;
			}
			PrevPC = Frame.AddrPC.Offset;
			PrevStack = Frame.AddrStack.Offset;
			OutAddrs[Count++] = Frame.AddrPC.Offset;
		}
#endif
		return Count;
	}

	inline void WalkAllThreads(const CONTEXT* CrashingCtx, u16 MaxFramesPerThread, FrameCallback OnLine, void* UserData)
	{
		if (OnLine == nullptr || MaxFramesPerThread == 0)
		{
			return;
		}

		const u16 ClampedMax = MaxFramesPerThread > 128 ? 128 : MaxFramesPerThread;
		const bool bLocked = TryLockSymbolEngine(200);

		HANDLE ProcessHandle = nullptr;
		bool bCloseProcessHandle = false;
		if (DuplicateHandle(GetCurrentProcess(), GetCurrentProcess(), GetCurrentProcess(), &ProcessHandle, 0, FALSE, DUPLICATE_SAME_ACCESS))
		{
			bCloseProcessHandle = true;
		}
		else
		{
			ProcessHandle = GetCurrentProcess();
		}

		SymSetOptions(SymGetOptions() | SYMOPT_DEFERRED_LOADS | SYMOPT_LOAD_LINES | SYMOPT_UNDNAME);
		const bool bHasSymbols = SymInitialize(ProcessHandle, nullptr, TRUE) != FALSE;

		const DWORD CurrentProcessId = GetCurrentProcessId();
		const DWORD CurrentThreadId = GetCurrentThreadId();

		// 1. Crashing thread first
		{
			char ThreadName[128] = {};
			FormatThreadName(GetCurrentThread(), ThreadName, sizeof(ThreadName));

			char Header[256] = {};
			if (ThreadName[0] != '\0')
			{
				std::snprintf(Header, sizeof(Header), "=== Crashing Thread %lu (%s) ===", CurrentThreadId, ThreadName);
			}
			else
			{
				std::snprintf(Header, sizeof(Header), "=== Crashing Thread %lu ===", CurrentThreadId);
			}
			OnLine(Header, UserData);

			CONTEXT FallbackCtx = {};
			const CONTEXT* ActiveCtx = CrashingCtx;
			if (ActiveCtx == nullptr)
			{
				FallbackCtx.ContextFlags = CONTEXT_FULL;
				RtlCaptureContext(&FallbackCtx);
				ActiveCtx = &FallbackCtx;
			}
			WalkContextFrames(ProcessHandle, GetCurrentThread(), ActiveCtx, ClampedMax, bHasSymbols, OnLine, UserData);
		}

		// 2. All other threads in this process
		// IMPORTANT: Never call DbgHelp symbol lookup or logging/IO while another thread is suspended
		// (the suspended thread may hold heap, loader, or CRT FILE locks and cause a deadlock).
		// Capture raw PCs while suspended, ResumeThread immediately, then symbolize.
		HANDLE Snapshot = CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD, 0);
		if (Snapshot != INVALID_HANDLE_VALUE)
		{
			THREADENTRY32 Entry = {};
			Entry.dwSize = sizeof(Entry);
			for (BOOL bHasEntry = Thread32First(Snapshot, &Entry); bHasEntry; bHasEntry = Thread32Next(Snapshot, &Entry))
			{
				if (Entry.th32OwnerProcessID != CurrentProcessId || Entry.th32ThreadID == CurrentThreadId)
				{
					continue;
				}

				HANDLE ThreadHandle = OpenThread(THREAD_SUSPEND_RESUME | THREAD_GET_CONTEXT | THREAD_QUERY_LIMITED_INFORMATION, FALSE, Entry.th32ThreadID);
				if (ThreadHandle == nullptr)
				{
					continue;
				}

				char ThreadName[128] = {};
				FormatThreadName(ThreadHandle, ThreadName, sizeof(ThreadName));

				DWORD64 CapturedAddrs[128] = {};
				u16 CapturedCount = 0;
				bool bSuspendedOk = false;
				bool bContextOk = false;

				if (SuspendThread(ThreadHandle) != static_cast<DWORD>(-1))
				{
					bSuspendedOk = true;
					CONTEXT OtherCtx = {};
					OtherCtx.ContextFlags = CONTEXT_FULL;
					if (GetThreadContext(ThreadHandle, &OtherCtx))
					{
						bContextOk = true;
						CapturedCount = CaptureRawFrames(&OtherCtx, CapturedAddrs, ClampedMax);
					}
					ResumeThread(ThreadHandle);
				}

				CloseHandle(ThreadHandle);

				OnLine("", UserData);
				char Header[256] = {};
				if (ThreadName[0] != '\0')
				{
					std::snprintf(Header, sizeof(Header), "=== Thread %lu (%s) ===", Entry.th32ThreadID, ThreadName);
				}
				else
				{
					std::snprintf(Header, sizeof(Header), "=== Thread %lu ===", Entry.th32ThreadID);
				}
				OnLine(Header, UserData);

				if (!bSuspendedOk)
				{
					OnLine("  <cannot suspend thread>", UserData);
				}
				else if (!bContextOk)
				{
					OnLine("  <cannot capture thread context>", UserData);
				}
				else
				{
					for (u16 i = 0; i < CapturedCount; ++i)
					{
						char Line[1024] = {};
						FormatStackFrame(ProcessHandle, CapturedAddrs[i], bHasSymbols, Line, sizeof(Line));
						OnLine(Line, UserData);
					}
				}
			}
			CloseHandle(Snapshot);
		}

		if (bHasSymbols)
		{
			SymCleanup(ProcessHandle);
		}
		if (bCloseProcessHandle)
		{
			CloseHandle(ProcessHandle);
		}
		if (bLocked)
		{
			g_ThreadStackSymbolLock.Leave();
		}
	}

	inline std::vector<std::string> BuildStackTrace(PCONTEXT threadCtx, u16 maxFramesCount)
	{
		std::vector<std::string> traceResult;
		traceResult.reserve(maxFramesCount);

		WalkStackTrace(
			threadCtx,
			maxFramesCount,
			[](const char* Line, void* UserData)
			{
				auto* Out = static_cast<std::vector<std::string>*>(UserData);
				Out->emplace_back(Line);
			},
			&traceResult
		);

		return traceResult;
	}
}
