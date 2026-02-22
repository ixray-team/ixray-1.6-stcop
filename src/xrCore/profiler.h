#pragma once

#if defined(IXRAY_PROFILER)
#	include <optick.h>
#	define PROF_THREAD(Name) OPTICK_THREAD(Name)
#	define PROF_START_THREAD(Name) OPTICK_START_THREAD(Name)
#	define PROF_STOP_THREAD() OPTICK_STOP_THREAD()
#	define PROF_START_CAPTURE() OPTICK_START_CAPTURE()
#	define PROF_STOP_CAPTURE() OPTICK_STOP_CAPTURE()
#	define PROF_SAVE_CAPTURE(Name) OPTICK_SAVE_CAPTURE(Name)
#	define PROF_FRAME(Name) OPTICK_FRAME(Name)
#	define PROF_EVENT(Name) OPTICK_EVENT(Name)
#elif defined(IXRAY_PROFILER_TRACY)
#   define TRACY_ENABLE
#   include "tracy/Tracy.hpp"
#   define PROF_THREAD(Name)
#   define PROF_START_THREAD(Name) // Не нужно в Tracy - потоки определяются автоматически
#   define PROF_STOP_THREAD() // Не нужно в Tracy
#   define PROF_START_CAPTURE()
#   define PROF_STOP_CAPTURE()
#   define PROF_SAVE_CAPTURE(Name)
#   define PROF_FRAME(Name) FrameMarkNamed(Name)
#   define PROF_EVENT(Name) ZoneTransientNC(TracyConcat(__tracy_zone, __LINE__), Name, 0, true);
#else
#	define PROF_THREAD(Name)
#	define PROF_START_THREAD(Name)
#	define PROF_STOP_THREAD()
#	define PROF_START_CAPTURE()
#	define PROF_STOP_CAPTURE()
#	define PROF_SAVE_CAPTURE(Name)
#	define PROF_FRAME(Name)
#	define PROF_EVENT(Name)
#	define PROF_EVENT_DYNAMIC(...) {};
#endif