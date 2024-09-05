#pragma once

#if defined(IXRAY_PROFILER)
#	include <optick.h>
#	define PROF_THREAD(Name) OPTICK_THREAD(Name)
#	define PROF_FRAME(Name) OPTICK_FRAME(Name)
#	define PROF_EVENT(Name) OPTICK_EVENT(Name)

#	define START_PROFILE(a) { PROF_EVENT(a)
#	define STOP_PROFILE		}

#else // DEBUG
#	define START_PROFILE(a) {
#	define STOP_PROFILE		}

#	define PROF_THREAD(Name)
#	define PROF_FRAME(Name)
#	define PROF_EVENT(Name)
#endif // DEBUG