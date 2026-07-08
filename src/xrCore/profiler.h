#pragma once

#ifdef IXRAY_PROFILER
#	include <optick.h>
#	define PROF_THREAD(Name) OPTICK_THREAD(Name)
#	define PROF_START_THREAD(Name) OPTICK_START_THREAD(Name) OPTICK_EVENT(Name)
#	define PROF_STOP_THREAD() OPTICK_STOP_THREAD()
#	define PROF_START_CAPTURE() OPTICK_START_CAPTURE()
#	define PROF_STOP_CAPTURE() OPTICK_STOP_CAPTURE()
#	define PROF_SAVE_CAPTURE(Name) OPTICK_SAVE_CAPTURE(Name)
#	define PROF_FRAME(Name) OPTICK_FRAME(Name)
#	define PROF_EVENT(Name) OPTICK_EVENT(Name)
#	define PROF_MESSAGE(Name)
#	define PROF_MEM_ALLOC_CAPTURE(Ptr, Size)
#	define PROF_MEM_FREE_CAPTURE(Ptr)
#elifdef IXRAY_PROFILER_TRACY
#	include <tracy/Tracy.hpp>
#	include <tracy/TracyC.h>
#	define TRACY_CALLSTACK 8
#   define PROF_THREAD(Name)
#   define PROF_START_THREAD(Name) TracyCZoneCtx zone; TracyFiberEnter(Name); TracyCZone(ctx, 1); zone = ctx;
#   define PROF_STOP_THREAD() TracyCZoneEnd(zone); TracyFiberLeave;
#   define PROF_START_CAPTURE()
#   define PROF_STOP_CAPTURE()
#   define PROF_SAVE_CAPTURE(Name)
#   define PROF_FRAME(Name) FrameMark;
#	define PROF_EVENT(Name) ZoneScopedN(Name);
#	define PROF_MESSAGE(Name) TracyMessageL(Name)
#	define PROF_MEM_ALLOC_CAPTURE(Ptr, Size) TracyAlloc(Ptr, Size);
#	define PROF_MEM_FREE_CAPTURE(Ptr) TracyFree(Ptr);
#else // no profiler
#	define PROF_THREAD(Name)
#	define PROF_START_THREAD(Name)
#	define PROF_STOP_THREAD()
#	define PROF_START_CAPTURE()
#	define PROF_STOP_CAPTURE()
#	define PROF_SAVE_CAPTURE(Name)
#	define PROF_FRAME(Name)
#	define PROF_EVENT(Name)
#	define PROF_MESSAGE(Name)
#	define PROF_EVENT_DYNAMIC(...) {};
#	define PROF_MEM_ALLOC_CAPTURE(Ptr, Size)
#	define PROF_MEM_FREE_CAPTURE(Ptr)
#endif