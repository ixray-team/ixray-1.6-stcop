#pragma once
#include "../xrCore/xrCore.h"

#if !defined(XRSE_FACTORY_EXPORTS) && !defined(_EDITOR)
#	include "imgui.h"
#endif

#ifdef _DEBUG
#	define D3D_DEBUG_INFO
#endif

#include "ExportDefines.h"

#include "../xrCore/API/xrAPI.h"

#ifndef ECORE_API
#	define ECORE_API
#endif

// Our headers
#include "../xrCore/EngineExternal.h"
#include "Engine.h"
#include "defines.h"
#ifndef NO_XRLOG
#include "../xrCore/log.h"
#endif
#include "device.h"
#include "../xrCore/FS.h"

#include "../xrCore/Collision/xrCDB.h"

#include "../xrSound/Sound.h"
#include "bone.h"

extern ENGINE_API CInifile *pGameIni;
extern ENGINE_API bool g_dedicated_server;

#if !defined(DEBUG) && !defined(LUA_DEBUG)
#	define LUABIND_NO_ERROR_CHECKING
#endif

#define LUABIND_DONT_COPY_STRINGS

#include "FontManager.h"

#ifndef _EDITOR
#include "ImGuiManager.h"
#endif


struct ISE_AbstractLEOwner{
	virtual void			get_bone_xform			(LPCSTR name, Fmatrix& xform) = 0;
};
