#pragma once

#if defined(IXRAY_PROFILER)
#	include <optick.h>
#	define PROF_THREAD(Name) OPTICK_THREAD(Name)
#	define PROF_START_CAPTURE() OPTICK_START_CAPTURE()
#	define PROF_STOP_CAPTURE() OPTICK_STOP_CAPTURE()
#	define PROF_SAVE_CAPTURE(Name) OPTICK_SAVE_CAPTURE(Name)
#	define PROF_FRAME(Name) OPTICK_FRAME(Name)
#	define PROF_EVENT(Name) OPTICK_EVENT(Name)
#	define PROF_EVENT_DYNAMIC(...)	 static ::Optick::EventDescription* OPTICK_CONCAT(autogen_description_, __LINE__) = nullptr; \
							 OPTICK_CONCAT(autogen_description_, __LINE__) = ::Optick::CreateDescription(OPTICK_FUNC, __FILE__, __LINE__, ##__VA_ARGS__); \
							 ::Optick::Event OPTICK_CONCAT(autogen_event_, __LINE__)( *(OPTICK_CONCAT(autogen_description_, __LINE__)) ); 
#	define START_PROFILE(a) { PROF_EVENT(a)
#	define STOP_PROFILE		}

#else // DEBUG
#	define START_PROFILE(a) {
#	define STOP_PROFILE		}

#	define PROF_THREAD(Name)
#	define PROF_START_CAPTURE()
#	define PROF_STOP_CAPTURE()
#	define PROF_SAVE_CAPTURE(Name)
#	define PROF_FRAME(Name)
#	define PROF_EVENT(Name)
#	define PROF_EVENT_DYNAMIC(...) {};
#endif // DEBUG