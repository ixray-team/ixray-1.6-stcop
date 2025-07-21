#pragma once
#include "../xrCore/xrCore.h"

#if !defined(XRSE_FACTORY_EXPORTS) && !defined(_EDITOR)
#	include "imgui.h"
#endif

#ifdef _DEBUG
#	define D3D_DEBUG_INFO
#endif


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

#include "../xrCore/API/xrAPI.h"

#ifndef ECORE_API
#	define ECORE_API
#endif

// Our headers
#include "EngineExternal.h"
#include "Engine.h"
#include "defines.h"
#ifndef NO_XRLOG
#include "../xrCore/log.h"
#endif
#include "device.h"
#include "../xrCore/FS.h"

#include "../xrCore/Collision/xrXRC.h"

#include "../xrSound/Sound.h"
#include "bone.h"

extern ENGINE_API CInifile *pGameIni;


#if !defined(DEBUG) && !defined(LUA_DEBUG)
#	define LUABIND_NO_ERROR_CHECKING
#endif

#define LUABIND_DONT_COPY_STRINGS

#define READ_IF_EXISTS(ltx,method,section,name,default_value)\
	(((ltx)->line_exist(section, name)) ? ((ltx)->method(section, name)) : (default_value))

#include "FontManager.h"

#ifndef _EDITOR
#include "ImGuiManager.h"
#endif


struct ISE_AbstractLEOwner{
	virtual void			get_bone_xform			(LPCSTR name, Fmatrix& xform) = 0;
};
