#include "StdAfx.h"
#include "script_xr_effects.h"
#include "script_xr_effects_functions.cpp"

CScriptXREffectsStorage::CScriptXREffectsStorage() {}

CScriptXREffectsStorage::~CScriptXREffectsStorage() {}

void CScriptXREffectsStorage::initialize(CLevel* pLevelManager)
{
	R_ASSERT2(pLevelManager,
		"you have to have a valid pointer of LevelManager! early calling?");

	m_pLevel = pLevelManager;

#if defined(IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION) || \
	defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	IXRAY_LUA_TO_CPP_REGISTER_FUNCTION_TO_SCRIPT(update_npc_logic);
	IXRAY_LUA_TO_CPP_REGISTER_FUNCTION_TO_SCRIPT(update_obj_logic);
	IXRAY_LUA_TO_CPP_REGISTER_FUNCTION_TO_SCRIPT(disable_ui);
	IXRAY_LUA_TO_CPP_REGISTER_FUNCTION_TO_SCRIPT(disable_ui_only);
	IXRAY_LUA_TO_CPP_REGISTER_FUNCTION_TO_SCRIPT(enable_ui);
	IXRAY_LUA_TO_CPP_REGISTER_FUNCTION_TO_SCRIPT(run_cam_effector);
	IXRAY_LUA_TO_CPP_REGISTER_FUNCTION_TO_SCRIPT(stop_cam_effector);
	IXRAY_LUA_TO_CPP_REGISTER_FUNCTION_TO_SCRIPT(run_cam_effector_global);
	IXRAY_LUA_TO_CPP_REGISTER_FUNCTION_TO_SCRIPT(cam_effector_callback);
	IXRAY_LUA_TO_CPP_REGISTER_FUNCTION_TO_SCRIPT(run_postprocess);
	IXRAY_LUA_TO_CPP_REGISTER_FUNCTION_TO_SCRIPT(stop_postprocess);
	IXRAY_LUA_TO_CPP_REGISTER_FUNCTION_TO_SCRIPT(run_tutorial);
	IXRAY_LUA_TO_CPP_REGISTER_FUNCTION_TO_SCRIPT(jup_b32_place_scanner);
#endif
}

void CScriptXREffectsStorage::destroy() {}
