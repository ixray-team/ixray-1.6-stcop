#pragma once
#include "xr_cpuid.h"

namespace CPU
{
	XRCORE_API extern u64 qpc_freq;

	XRCORE_API const processor_info& ID();
	XRCORE_API extern u64 QPC();
	XRCORE_API extern size_t GetTickCount();

	IC u64 GetCLK(void) 
	{
		return SDL_GetPerformanceCounter();
	}
};

extern XRCORE_API	void	_initialize_cpu			();
extern XRCORE_API	void	_initialize_cpu_thread	();

// threading
// AI: "thread_t" is a Mach typedef (mach_port_t) on macOS, the engine alias must not take that name.
using xr_thread_entry_t = void(void*);
extern XRCORE_API void thread_name(const char* name);
extern XRCORE_API ThreadID thread_spawn(xr_thread_entry_t* entry, const char* name, unsigned	stack, void* arglist);