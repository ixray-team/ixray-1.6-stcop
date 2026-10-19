#pragma once

// Writes the crash dumps from a helper process. A crashing process often has a broken heap: MiniDumpWriteDump
// allocates from it, and Windows then ends the process before the dump is written. The helper is a copy of
// the executable started with the engine; the crashing process only signals it, without allocating.
namespace CrashDumpHelper
{
	// In the executable entry, before anything else: runs the helper and returns true if this process is one
	XRCORE_API bool RunIfRequested();
	// Starts the helper of this process once the logs folder is known (after CApplication::InitEngine).
	XRCORE_API void Start();
	// From the unhandled exception filter: the helper writes a small dump with the stacks, then a full one.
	// False if the helper is unavailable or could not write any dump: the caller writes the dump itself.
	bool Write(void* ExceptionPointers);
}

#ifndef IXR_WINDOWS
inline bool CrashDumpHelper::RunIfRequested() { return false; }
inline void CrashDumpHelper::Start() {}
inline bool CrashDumpHelper::Write(void*) { return false; }
#endif
