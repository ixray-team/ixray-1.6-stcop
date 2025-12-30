#pragma once

// you must define ENGINE_BUILD then building the engine itself
// and not define it if you are about to build DLL
#ifdef ENGINE_BUILD
    #define DLL_API			__declspec(dllimport)
    #define ENGINE_API		__declspec(dllexport)
#else
    #undef	DLL_API
    #define DLL_API			__declspec(dllexport)
    #define ENGINE_API		__declspec(dllimport)
#endif