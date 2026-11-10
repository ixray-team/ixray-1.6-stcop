#pragma once
#include "../xrCore/xrCore.h"
#include "../xrCore/Collision/xrCDB.h"
#include "../xrCore/Kernel/xrAPI.h"
#include "../xrCore/Kernel/EngineExternal.h"

#if !defined(XRSE_FACTORY_EXPORTS) && !defined(_EDITOR)
#	include "imgui.h"
#endif

#include "ExportDefines.h"

#ifndef ECORE_API
#	define ECORE_API
#endif

// Our headers
#include "Engine.h"
#include "defines.h"
#include "device.h"

#include "../xrSound/Sound.h"
#include "bone.h"

extern ENGINE_API CInifile *pGameIni;
extern ENGINE_API bool g_dedicated_server;

#if !defined(DEBUG) && !defined(LUA_DEBUG)
#	define LUABIND_NO_ERROR_CHECKING
#endif

#include "FontManager.h"
#include "ImGuiManager.h"


struct ISE_AbstractLEOwner
{
	virtual void get_bone_xform(const char* name, Fmatrix& xform) = 0;
};
