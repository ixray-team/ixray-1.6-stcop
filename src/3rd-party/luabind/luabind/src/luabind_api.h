#pragma once

#ifdef _WIN32
#  ifdef LUABIND_BUILDING
#    define LUABIND_API 		__declspec(dllexport)
#  else 
#    define LUABIND_API		__declspec(dllimport)
#  endif
#else
#  define LUABIND_API
#endif
