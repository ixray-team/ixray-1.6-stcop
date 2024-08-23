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
