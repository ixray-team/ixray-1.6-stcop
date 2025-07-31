#pragma once

#ifdef LUABIND_BUILDING
#	define LUABIND_API 		__declspec(dllexport)
#else 
#	define LUABIND_API		__declspec(dllimport)
#endif // #ifdef XR_SCRIPTS
