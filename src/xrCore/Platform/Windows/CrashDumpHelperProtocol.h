#pragma once

// Shared between the engine and xrCrashDumpHelper.exe. Plain Win32 only: the helper must not link the
// engine, its profilers, or its logging.
#include <windows.h>

namespace CrashDumpHelper
{
	// Bumped only together with the layout below: the helper rejects a mapping with another value.
	inline constexpr DWORD SharedStateMagic = 0x49584448; // 'IXDH'
	inline constexpr const char* HelperArgument = "-crash_dump_helper";
	// Must match the OUTPUT_NAME of the xrCrashDumpHelper target.
	inline constexpr const char* HelperExecutableName = "xrCrashDumpHelper.exe";

	struct SharedState
	{
		DWORD Magic;
		DWORD Size;
		DWORD ThreadId;
		EXCEPTION_POINTERS* Exception;
		char Folder[MAX_PATH];
		char Prefix[128];
		BOOL IsWritten;
	};
}
