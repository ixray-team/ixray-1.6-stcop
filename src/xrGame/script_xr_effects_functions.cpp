// keep this file as .cpp format for users they don't need to use it as header
// file!!!!!!

#include "StdAfx.h"
#include "script_game_object.h"
#include "xrServer_Objects_ALife.h"
#include "script_utility.h"
#include "Level.h"
#include "xrServer_Objects_ALife_Monsters.h"

inline void update_npc_logic_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.update_npc_logic", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void update_npc_logic_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor("xr_effects.update_npc_logic", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void update_obj_logic_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.update_obj_logic", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void update_obj_logic_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.update_obj_logic", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void disable_ui_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.disable_ui", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void disable_ui_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.disable_ui", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void disable_ui_only_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.disable_ui_only", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void disable_ui_only_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.disable_ui_only", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}


inline void enable_ui_client(CScriptGameObject* pActor, CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.enable_ui", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void enable_ui_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.enable_ui", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void run_cam_effector_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.run_cam_effector", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void run_cam_effector_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.run_cam_effector", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void stop_cam_effector_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.stop_cam_effector", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void stop_cam_effector_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.stop_cam_effector", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void run_cam_effector_global_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.run_cam_effector_global", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void run_cam_effector_global_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.run_cam_effector_global", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void cam_effector_callback_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.cam_effector_callback", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	_impl();
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void cam_effector_callback_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.cam_effector_callback", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	_impl();
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void run_postprocess_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.run_postprocess", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void run_postprocess_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.run_postprocess", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void stop_postprocess_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.stop_postprocess", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void stop_postprocess_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.stop_postprocess", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void run_tutorial_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.run_tutorial", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void run_tutorial_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.run_tutorial", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void jup_b32_place_scanner_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.jup_b32_place_scanner", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	_impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void jup_b32_place_scanner_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.jup_b32_place_scanner", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	_impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void jup_b32_pda_check_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.jup_b32_pda_check", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	_impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void jup_b32_pda_check_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.jup_b32_pda_check", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	_impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void pri_b306_generator_start_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.pri_b306_generator_start", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	_impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void pri_b306_generator_start_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.pri_b306_generator_start", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	_impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void jup_b206_get_plant_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.jup_b206_get_plant", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	_impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void jup_b206_get_plant_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.jup_b206_get_plant", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	_impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void pas_b400_switcher_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.pas_b400_switcher", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	_impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void pas_b400_switcher_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.pas_b400_switcher", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	_impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void jup_b209_place_scanner_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.jup_b209_place_scanner", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	_impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void jup_b209_place_scanner_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.jup_b209_place_scanner", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	_impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void jup_b9_heli_1_searching_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.jup_b9_heli_1_searching", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	_impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void jup_b9_heli_1_searching_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.jup_b9_heli_1_searching", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	_impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void pri_a18_use_idol_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.pri_a18_use_idol", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	_impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void pri_a18_use_idol_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.pri_a18_use_idol", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	_impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void jup_b8_heli_4_searching_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.jup_b8_heli_4_searching", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	_impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void jup_b8_heli_4_searching_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.jup_b8_heli_4_searching", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	_impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void jup_b10_ufo_searching_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.jup_b10_ufo_searching", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	_impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void jup_b10_ufo_searching_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.jup_b10_ufo_searching", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	_impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void zat_b101_heli_5_searching_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.zat_b101_heli_5_searching", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	_impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void zat_b101_heli_5_searching_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.zat_b101_heli_5_searching", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	_impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void zat_b28_heli_3_searching_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.zat_b28_heli_3_searching", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	_impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void zat_b28_heli_3_searching_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.zat_b28_heli_3_searching", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	_impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}


inline void zat_b100_heli_2_searching_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.zat_b100_heli_2_searching", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	_impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void zat_b100_heli_2_searching_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.zat_b100_heli_2_searching", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	_impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void teleport_actor_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.teleport_actor", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void teleport_actor_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.teleport_actor", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}


inline void teleport_npc_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.teleport_npc", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void teleport_npc_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.teleport_npc", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void teleport_npc_by_story_id_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.teleport_npc_by_story_id", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void teleport_npc_by_story_id_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.teleport_npc_by_story_id", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}


inline void teleport_squad_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.teleport_squad", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void teleport_squad_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.teleport_squad", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}


inline void jup_teleport_actor_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.jup_teleport_actor", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	_impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void jup_teleport_actor_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.jup_teleport_actor", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	_impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void give_items_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.give_items", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void give_items_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.give_items", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void give_item_client(CScriptGameObject* pActor, CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.give_item", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void give_item_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.give_item", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}
inline void play_particle_on_path_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.play_particle_on_path", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void play_particle_on_path_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.play_particle_on_path", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}
inline void send_tip_client(CScriptGameObject* pActor, CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.send_tip", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void send_tip_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.send_tip", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}
inline void hit_npc_client(CScriptGameObject* pActor, CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.hit_npc", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void hit_npc_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.hit_npc", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}
inline void hit_obj_client(CScriptGameObject* pActor, CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.hit_obj", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void hit_obj_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.hit_obj", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}
inline void hit_by_killer_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.hit_by_killer", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void hit_by_killer_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.hit_by_killer", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}
inline void restore_health_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.restore_health", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void restore_health_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.restore_health", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}
inline void hit_npc_from_actor_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.hit_npc_from_actor", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void hit_npc_from_actor_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.hit_npc_from_actor", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}
inline void make_enemy_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.make_enemy", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void make_enemy_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.make_enemy", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}
inline void sniper_fire_mode_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.sniper_fire_mode", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void sniper_fire_mode_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.sniper_fire_mode", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}
inline void kill_npc_client(CScriptGameObject* pActor, CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.kill_npc", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void kill_npc_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.kill_npc", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}
inline void remove_npc_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.remove_npc", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void remove_npc_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.remove_npc", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}
inline void inc_counter_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.inc_counter", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void inc_counter_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.inc_counter", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}
inline void dec_counter_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.dec_counter", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void dec_counter_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.dec_counter", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}
inline void set_counter_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.set_counter", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void set_counter_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.set_counter", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}
inline void actor_punch_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.actor_punch", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	_impl(pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void actor_punch_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.actor_punch", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	_impl(pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}
inline void clearAbuse_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.clearAbuse", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	_impl(pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void clearAbuse_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.clearAbuse", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	_impl(pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}
inline void turn_off_underpass_lamps_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.turn_off_underpass_lamps", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	_impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void turn_off_underpass_lamps_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.turn_off_underpass_lamps", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	_impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}
inline void turn_off_client(CScriptGameObject* pActor, CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.turn_off", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void turn_off_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.turn_off", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}
inline void turn_off_object_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.turn_off_object", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	_impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void turn_off_object_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.turn_off_object", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	_impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}
inline void turn_on_client(CScriptGameObject* pActor, CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.turn_on", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void turn_on_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.turn_on", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}
inline void turn_on_and_force_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.turn_on_and_force", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void turn_on_and_force_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.turn_on_and_force", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}
inline void turn_off_and_force_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.turn_off_and_force", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void turn_off_and_force_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.turn_off_and_force", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}
inline void turn_on_object_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.turn_on_object", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	_impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void turn_on_object_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.turn_on_object", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	_impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}
inline void disable_combat_handler_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.disable_combat_handler", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	_impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void disable_combat_handler_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.disable_combat_handler", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	_impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}
inline void disable_combat_ignore_handler_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.disable_combat_ignore_handler", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	_impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void disable_combat_ignore_handler_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.disable_combat_ignore_handler", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	_impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void heli_start_flame_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.heli_start_flame", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	_impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void heli_start_flame_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.heli_start_flame", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	_impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}
inline void heli_die_client(CScriptGameObject* pActor, CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.heli_die", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	_impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void heli_die_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.heli_die", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	_impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}
inline void set_weather_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.set_weather", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void set_weather_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.set_weather", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void game_disconnect_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.game_disconnect", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	_impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void game_disconnect_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.game_disconnect", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	_impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void game_credits_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.game_credits", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	_impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void game_credits_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.game_credits", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	_impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void game_over_client(CScriptGameObject* pActor, CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.game_over", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	_impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void game_over_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.game_over", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	_impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void after_credits_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.after_credits", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	_impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void after_credits_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.after_credits", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	_impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void before_credits_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.before_credits", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	_impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void before_credits_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.before_credits", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	_impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}
inline void on_tutor_gameover_stop_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.on_tutor_gameover_stop", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	_impl();
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void on_tutor_gameover_stop_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.on_tutor_gameover_stop", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	_impl();
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}
inline void on_tutor_gameover_quickload_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.on_tutor_gameover_quickload", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	_impl();
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void on_tutor_gameover_quickload_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.on_tutor_gameover_quickload", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	_impl();
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}
inline void get_stalker_for_new_job_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.get_stalker_for_new_job", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}

inline void get_stalker_for_new_job_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return;

	luabind::functor<void> _impl;
	auto status = ai().script_engine().functor("xr_effects.get_stalker_for_new_job", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	_impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
#endif
}