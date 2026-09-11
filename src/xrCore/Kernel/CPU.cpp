#include "stdafx.h"

#ifdef IXR_WINDOWS
#include <process.h>
#include <winver.h>
#include <VersionHelpers.h>

// mmsystem.h
#define MMNOSOUND
#define MMNOMIDI
#define MMNOAUX
#define MMNOMIXER
#define MMNOJOY
#include <mmsystem.h>
#endif

// Initialized on startup
XRCORE_API Fmatrix Fidentity;
XRCORE_API Dmatrix Didentity;
XRCORE_API CRandom Random;
XRCORE_API float pvUVAdjustment[0x2000];
namespace CPU
{
	XRCORE_API u64 qpc_freq = 0;
	
	XRCORE_API const processor_info& ID()
	{
		static processor_info SingletonID;
		return SingletonID;
	}
	
	XRCORE_API u64 QPC()
	{
		return SDL_GetPerformanceCounter();
	}
	
	XRCORE_API size_t GetTickCount()
	{
		return SDL_GetTicks();
	}
	
	void Detect()
	{
		// Detect QPC
		qpc_freq = SDL_GetPerformanceFrequency();
	}
};

bool g_initialize_cpu_called = false;

#ifdef IXR_WINDOWS
using SetThreadDescriptionDesc = HRESULT(WINAPI*)(HANDLE, PCWSTR);
static SetThreadDescriptionDesc SetThreadDescriptionProc;

bool IsRunningInWine()
{
	const char* WineVersion = std::getenv("WINELOADER");
	const char* WinePrefix = std::getenv("WINEPREFIX");
	return WineVersion != nullptr || WinePrefix != nullptr;
}

bool IsRunningInProton()
{
	if (!IsRunningInWine())
	{
		return false;
	}

	const char* SteamGameId = std::getenv("SteamGameId");
	const char* SteamAppId = std::getenv("SteamAppId");
	const char* SteamRuntime = std::getenv("STEAM_RUNTIME");

	return SteamGameId != nullptr || SteamAppId != nullptr || SteamRuntime != nullptr;
}
#endif

//------------------------------------------------------------------------------------
void _initialize_cpu(void)
{
	Msg(
		"* Detected CPU: %s [%s], F%d/M%d/S%d",
		CPU::ID().modelName,
		CPU::ID().vendor,
		CPU::ID().family,
		CPU::ID().model,
		CPU::ID().stepping
	);
#ifdef IXR_WINDOWS

	if (IsRunningInProton())
	{
		Msg("Running under Proton (Steam Play)!");
	}
	else if (IsRunningInWine())
	{
		Msg("Running under standard Wine!");
	}

	xr_string FeaturesString = "RDTSC";
	for (const CPUFeature CPUExt : magic_enum::enum_values<CPUFeature>())
	{
		if (CPU::ID().hasFeature(CPUExt))
		{
			FeaturesString += ", ";
			FeaturesString += magic_enum::enum_name(CPUExt);
		}
	}
	Msg("* CPU features: %s", FeaturesString.c_str());
#endif
	Msg("* CPU cores/threads: %d/%d\n", CPU::ID().n_cores, CPU::ID().n_threads);

	Fidentity.identity();  // Identity matrix
	Didentity.identity();  // Identity matrix
	pvInitializeStatics(); // Lookup table for compressed normals
	_initialize_cpu_thread();

	g_initialize_cpu_called = true;
}

// per-thread initialization
#define _MM_DENORMALS_ZERO_MASK 0x0040
#define _MM_DENORMALS_ZERO_ON 0x0040
#define _MM_FLUSH_ZERO_MASK 0x8000
#define _MM_FLUSH_ZERO_ON 0x8000

#ifndef IXR_ARM64
#	define _MM_SET_FLUSH_ZERO_MODE(mode) _mm_setcsr((_mm_getcsr() & ~_MM_FLUSH_ZERO_MASK) | (mode))
#	define _MM_SET_DENORMALS_ZERO_MODE(mode) _mm_setcsr((_mm_getcsr() & ~_MM_DENORMALS_ZERO_MASK) | (mode))
#endif

static bool DenormalsAreZeroSupported = true;
extern void __cdecl _terminate();
void debug_on_thread_spawn();

void _initialize_cpu_thread()
{
	debug_on_thread_spawn();

	if (CPU::ID().hasFeature(CPUFeature::SSE2))
	{
		_MM_SET_FLUSH_ZERO_MODE(_MM_FLUSH_ZERO_ON);
#ifdef IXR_WINDOWS
		if (DenormalsAreZeroSupported)
		{
			__try
			{
				_MM_SET_DENORMALS_ZERO_MODE(_MM_DENORMALS_ZERO_ON);
			}
			__except (EXCEPTION_EXECUTE_HANDLER)
			{
				DenormalsAreZeroSupported = false;
			}
		}
#endif
	}
}

// threading API
void thread_name(const char* name)
{
	Platform::SetThreadName(name);
}

struct THREAD_STARTUP
{
	thread_t* entry;
	char* name;
	void* args;
};

void __cdecl thread_entry(void* _params)
{
	// initialize
	THREAD_STARTUP* startup = (THREAD_STARTUP*)_params;
	thread_name(startup->name);
	thread_t* entry = startup->entry;
	void* arglist = startup->args;

	free(startup->name);
	xr_delete(startup);

	_initialize_cpu_thread();

	// call
	entry(arglist);
}
#ifndef IXR_WINDOWS
void* pthread_entry(void* params)
{
	thread_entry(params);
	return nullptr;
}
#endif

ThreadID thread_spawn(thread_t* entry, const char* name, unsigned stack, void* arglist)
{
	THREAD_STARTUP* startup = new THREAD_STARTUP();
	startup->entry = entry;
	startup->name = xr_strdup((char*)name);
	startup->args = arglist;

#ifdef IXR_WINDOWS
	return (ThreadID)_beginthread(thread_entry, stack, startup);
#else
	pthread_t handle;
	pthread_attr_t attr;
	pthread_attr_init(&attr);

	if (stack > 0)
	{
		pthread_attr_setstacksize(&attr, stack);
	}

	pthread_create(&handle, &attr, pthread_entry, startup);
	pthread_attr_destroy(&attr);

	return (ThreadID)handle;
#endif
}