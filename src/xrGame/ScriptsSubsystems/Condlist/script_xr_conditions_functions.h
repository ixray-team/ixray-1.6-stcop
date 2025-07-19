// keep this file as .cpp format for users they don't need to use it as header
// file!!!!!!

#pragma once

#include "StdAfx.h"
#include "script_game_object.h"
#include "xrServer_Objects_ALife.h"
#include "script_utility.h"
#include "Level.h"
#include "xrServer_Objects_ALife_Monsters.h"

// specify your function implementation here as you do in xr_conditions.script
// on Lua programming language reminder for user, the game uses client server
// architecture thus they have client and server types that represent a NPC on
// level. CScriptGameObject - CLIENT representation of NPC (online) |/|\|
// CSE_ALifeXXX - SERVER representation of NPC (offline)

// always provide two implementations for your functions based on prefixes
// _client and _server respectively

// comment it if you can provide C++ implementation FOR ALL FUNCTIONS!!!
// because now some functions are provided in pure C++ implementation, but some
// of many functions can't be re-written so easily due to lack of manager
// implementations like surge manager, story manager, db.script (database) and
// etc, so if you can't provide implementation just use lua's implementation

inline bool fighting_dist_ge_client(CScriptGameObject* enemy,
	CScriptGameObject* npc, const xr_vector<xr_string>& buffer)
{
	if (!enemy)
		return false;

	if (!npc)
		return false;

	if (!buffer.size())
		return false;

	float distance = atof(buffer[0].c_str());

	return (enemy->Position().distance_to_sqr(npc->Position()) >=
		(distance * distance));
}

inline bool fighting_dist_ge_server(CScriptGameObject* enemy,
	CSE_ALifeDynamicObject* npc, const xr_vector<xr_string>& buffer)
{
	if (!enemy)
		return false;

	if (!npc)
		return false;

	if (!buffer.size())
		return false;

	float distance = atof(buffer[0].c_str());

	return (enemy->Position().distance_to_sqr(npc->Position()) >=
		(distance * distance));
}

inline bool fighting_dist_le_client(CScriptGameObject* enemy,
	CScriptGameObject* npc, const xr_vector<xr_string>& buffer)
{
	if (!enemy)
		return false;

	if (!npc)
		return false;

	if (!buffer.size())
		return false;

	float distance = atof(buffer[0].c_str());

	return (enemy->Position().distance_to_sqr(npc->Position()) <=
		(distance * distance));
}

inline bool fighting_dist_le_server(CScriptGameObject* enemy,
	CSE_ALifeDynamicObject* npc, const xr_vector<xr_string>& buffer)
{
	if (!enemy)
		return false;

	if (!npc)
		return false;

	if (!buffer.size())
		return false;

	float distance = atof(buffer[0].c_str());

	return (enemy->Position().distance_to_sqr(npc->Position()) <=
		(distance * distance));
}

inline bool enemy_in_zone_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.enemy_in_zone", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false,
		"implement signal rocket data storage, but only when you provided a "
		"manager for that!");
	return false;
#endif
}

inline bool enemy_in_zone_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.enemy_in_zone", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false,
		"implement signal rocket data storage, but only when you provided a "
		"manager for that!");
	return false;
#endif
}

inline bool black_screen_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
	return Device.dwPrecacheFrame > 1;
}

inline bool black_screen_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
	return Device.dwPrecacheFrame > 1;
}

inline bool check_npc_name_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
	bool result{};

	if (buffer.empty())
		return result;

	if (pBot)
	{
		for (const xr_string& npc_name : buffer)
		{
			if (npc_name.find(pBot->Name()) != xr_string::npos)
			{
				result = true;
			}
		}
	}

	return result;
}

inline bool check_npc_name_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
	bool result{};

	if (buffer.empty())
		return result;

	if (pBot)
	{
		for (const xr_string& npc_name : buffer)
		{
			if (npc_name.find(pBot->name_replace()) != xr_string::npos)
			{
				result = true;
			}
		}
	}

	return result;
}

inline bool check_enemy_name_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.check_enemy_name", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool check_enemy_name_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.check_enemy_name", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool is_playing_sound_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.is_playing_sound", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	return _impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool is_playing_sound_server(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.is_playing_sound", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	return _impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool actor_alive_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
	bool result{};
	if (pActor)
	{
		result = pActor->Alive();
	}
	return result;
}

inline bool actor_alive_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
	bool result{};

	if (pActor)
	{
		result = pActor->Alive();
	}

	return result;
}

inline bool see_npc_client(CScriptGameObject* pActor, CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor("xr_conditions.see_npc", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool see_npc_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor("xr_conditions.see_npc", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool actor_see_npc_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
	bool result{};

	if (pActor)
	{
		result = pActor->CheckObjectVisibility(pBot);
	}

	return result;
}

inline bool actor_see_npc_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
	R_ASSERT2(false,
		"implementation supposed to be only client! Something is wrong, don't "
		"call it on server side!");

	return false;
}

inline bool npc_in_actor_frustum_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor(
		"xr_conditions.npc_in_actor_frustum", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	return _impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool npc_in_actor_frustum_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor(
		"xr_conditions.npc_in_actor_frustum", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	return _impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool is_wounded_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.is_wounded", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	return _impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool is_wounded_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.is_wounded", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	return _impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool dist_to_actor_le_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
	bool result{};
	if (buffer.empty())
		return result;

	float fDistance = atof(buffer[0].c_str());

	if (pActor && pBot)
	{
		result = pBot->Position().distance_to_sqr(pActor->Position()) <=
			fDistance * fDistance;
	}

	return result;
}

inline bool dist_to_actor_ge_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
	bool result{};

	if (buffer.empty())
		return result;

	float fDistance = atof(buffer[0].c_str());

	if (pActor && pBot)
	{
		result = pBot->Position().distance_to_sqr(pActor->Position()) >=
			fDistance * fDistance;
	}

	return result;
}

inline bool dist_to_actor_ge_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
	bool result{};

	if (buffer.empty())
		return result;

	float fDistance = atof(buffer[0].c_str());

	if (pActor && pBot)
	{
		result = pBot->Position().distance_to_sqr(pActor->Position()) >=
			fDistance * fDistance;
	}

	return result;
}

inline bool is_obj_on_job_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)

{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.is_obj_on_job", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool is_obj_on_job_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)

{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.is_obj_on_job", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool distance_to_obj_on_job_le_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor(
		"xr_conditions.distance_to_obj_on_job_le", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool distance_to_obj_on_job_le_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor(
		"xr_conditions.distance_to_obj_on_job_le", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool obj_in_zone_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.obj_in_zone", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool obj_in_zone_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.obj_in_zone", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool one_obj_in_zone_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.one_obj_in_zone", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool one_obj_in_zone_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.one_obj_in_zone", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool story_obj_in_zone_by_name_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor(
		"xr_conditions.story_obj_in_zone_by_name", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool story_obj_in_zone_by_name_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor(
		"xr_conditions.story_obj_in_zone_by_name", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool actor_in_zone_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.actor_in_zone", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool actor_in_zone_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.actor_in_zone", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool npc_in_zone_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.npc_in_zone", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool npc_in_zone_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.npc_in_zone", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool health_le_client(CScriptGameObject* pActor, CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
	bool result{};

	if (buffer.empty())
		return result;

	if (pBot)
	{
		float fValue = atof(buffer[0].c_str());
		result = pBot->GetHealth() < fValue;
	}

	return result;
}

inline bool health_le_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
	bool result{};

	if (buffer.empty())
		return result;

	if (pBot)
	{
		CSE_ALifeCreatureAbstract* pCasted =
			dynamic_cast<CSE_ALifeCreatureAbstract*>(pBot);

		R_ASSERT2(pCasted, "failed to cast, did you pass a valid object?");

		if (pCasted)
		{
			float fValue = atof(buffer[0].c_str());

			result = pCasted->get_health() < fValue;
		}
	}

	return result;
}

inline bool actor_health_le_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
	bool result{};

	if (pActor)
	{
		float fValue = atof(buffer[0].c_str());
		result = pActor->GetHealth() < fValue;
	}

	return result;
}

inline bool actor_health_le_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
	bool result{};

	if (pActor)
	{
		float fValue = atof(buffer[0].c_str());
		result = pActor->GetHealth() < fValue;
	}

	return result;
}

inline bool npc_community_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.npc_community", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool npc_community_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.npc_community", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool hitted_by_client(CScriptGameObject* pActor, CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.hitted_by", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool hitted_by_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.hitted_by", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool hitted_on_bone_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.hitted_on_bone", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool hitted_on_bone_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.hitted_on_bone", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool best_pistol_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
	bool result{};

	if (pBot)
	{
		result = pBot->item_in_slot(1);
	}

	return result;
}

inline bool best_pistol_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
	R_ASSERT2(false,
		"supposed to be only client, because client has implementation "
		"only!!!!! Don't call this function");

	return false;
}

inline bool deadly_hit_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.deadly_hit", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	return _impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "implement animpoint for database");
	return false;
#endif
}

inline bool deadly_hit_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.deadly_hit", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	return _impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "implement animpoint for database");
	return false;
#endif
}

inline bool killed_by_client(CScriptGameObject* pActor, CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.killed_by", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool killed_by_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.killed_by", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool is_alive_all_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.is_alive_all", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool is_alive_all_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.is_alive_all", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool is_alive_one_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.is_alive_one", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool is_alive_one_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.is_alive_one", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool is_alive_client(CScriptGameObject* pActor, CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor("xr_conditions.is_alive", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool is_alive_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor("xr_conditions.is_alive", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool is_dead_all_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.is_dead_all", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool is_dead_all_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.is_dead_all", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool is_dead_one_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.is_dead_one", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool is_dead_one_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.is_dead_one", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool is_dead_client(CScriptGameObject* pActor, CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor("xr_conditions.is_dead", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool is_dead_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor("xr_conditions.is_dead", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool story_object_exist_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.story_object_exist", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool story_object_exist_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.story_object_exist", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool actor_has_item_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.actor_has_item", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool actor_has_item_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.actor_has_item", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool npc_has_item_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.npc_has_item", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool npc_has_item_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.npc_has_item", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool actor_has_item_count_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor(
		"xr_conditions.actor_has_item_count", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool actor_has_item_count_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor(
		"xr_conditions.actor_has_item_count", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool signal_client(CScriptGameObject* pActor, CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor("xr_conditions.signal", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool signal_server(CScriptGameObject* pActor, CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor("xr_conditions.signal", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool counter_greater_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.counter_greater", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool counter_greater_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.counter_greater", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool counter_equal_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.counter_equal", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool counter_equal_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.counter_equal", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool check_smart_alarm_status_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor(
		"xr_conditions.check_smart_alarm_status", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool check_smart_alarm_status_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor(
		"xr_conditions.check_smart_alarm_status", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool has_enemy_client(CScriptGameObject* pActor, CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
	bool result{};

	if (pBot)
	{
		result = pBot->GetBestEnemy() != nullptr;
	}

	return result;
}

inline bool has_enemy_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
	R_ASSERT2(false,
		"supposed to use only client implementation, because only client has "
	    "implementation!");
	return false;
}

inline bool has_actor_enemy_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
	bool result{};

	if (pBot)
	{
		CScriptGameObject* pEnemy = pBot->GetBestEnemy();

		if (pEnemy)
		{
			result = pEnemy->ID() == pActor->ID();
		}
	}

	return result;
}

inline bool has_actor_enemy_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
	R_ASSERT2(false,
		"supposed to use only client version, because only client has "
		"implementation!");
	return false;
}

inline bool see_enemy_client(CScriptGameObject* pActor, CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
	bool result{};

	if (pBot)
	{
		CScriptGameObject* pEnemy = pBot->GetBestEnemy();
		if (pEnemy)
		{
			result = pBot->CheckObjectVisibility(pEnemy);
		}
	}

	return result;
}

inline bool see_enemy_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
	R_ASSERT2(false,
		"supposed to use client version, because only client has "
	    "implementation!");

	return false;
}

inline bool has_enemy_in_current_loopholes_fov_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
	bool result{};

	if (pBot)
	{
		result = pBot->in_smart_cover() && pBot->GetBestEnemy() &&
			pBot->in_current_loophole_fov(pBot->GetBestEnemy()->Position());
	}

	return result;
}

inline bool has_enemy_in_current_loopholes_fov_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
	R_ASSERT2(false,
		"supposed to use client version, client has implementation only");
	return false;
}

inline bool talking_client(CScriptGameObject* pActor, CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
	bool result{};

	if (pActor)
	{
		result = pActor->IsTalking();
	}

	return result;
}

inline bool talking_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
	R_ASSERT2(false,
		"supposed to use client version, because client has implementation "
		"only!");
	return false;
}

inline bool npc_talking_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
	bool result{};

	if (pBot)
	{
		result = pBot->IsTalking();
	}

	return result;
}

inline bool npc_talking_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
	R_ASSERT2(false,
		"supposed to use client version, because client has implementation "
		"only!");
	return false;
}

inline bool see_actor_client(CScriptGameObject* pActor, CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
	bool result{};

	if (pBot)
	{
		result = pBot->Alive() && pBot->CheckObjectVisibility(pActor);
	}

	return result;
}

inline bool actor_enemy_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.actor_enemy", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	return _impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool actor_enemy_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.actor_enemy", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	return _impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool actor_friend_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.actor_friend", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	return _impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool actor_friend_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.actor_friend", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	return _impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool actor_neutral_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.actor_neutral", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	return _impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool actor_neutral_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.actor_neutral", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	return _impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool see_actor_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
	R_ASSERT2(false,
		"supposed to use client version, because client has implementation "
		"only!");
	return false;
}

inline bool is_factions_enemies_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor(
		"xr_conditions.is_factions_enemies", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool is_factions_enemies_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor(
		"xr_conditions.is_factions_enemies", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool is_factions_friends_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor(
		"xr_conditions.is_factions_friends", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool is_factions_friends_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor(
		"xr_conditions.is_factions_friends", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool is_faction_enemy_to_actor_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor(
		"xr_conditions.is_faction_enemy_to_actor", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool is_faction_enemy_to_actor_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor(
		"xr_conditions.is_faction_enemy_to_actor", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool is_faction_friend_to_actor_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor(
		"xr_conditions.is_faction_friend_to_actor", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool is_faction_friend_to_actor_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor(
		"xr_conditions.is_faction_friend_to_actor", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool is_faction_neutral_to_actor_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor(
		"xr_conditions.is_faction_neutral_to_actor", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool is_faction_neutral_to_actor_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor(
		"xr_conditions.is_faction_neutral_to_actor", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool is_squad_friend_to_actor_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor(
		"xr_conditions.is_squad_friend_to_actor", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool is_squad_friend_to_actor_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor(
		"xr_conditions.is_squad_friend_to_actor", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool is_squad_enemy_to_actor_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor(
		"xr_conditions.is_squad_enemy_to_actor", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool is_squad_enemy_to_actor_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor(
		"xr_conditions.is_squad_enemy_to_actor", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool is_squad_neutral_to_actor_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor(
		"xr_conditions.is_squad_neutral_to_actor", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool is_squad_neutral_to_actor_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor(
		"xr_conditions.is_squad_neutral_to_actor", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool fighting_actor_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.fighting_actor", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	return _impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool fighting_actor_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.fighting_actor", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	return _impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool hit_by_actor_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.hit_by_actor", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	return _impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool hit_by_actor_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.hit_by_actor", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	return _impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool killed_by_actor_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.killed_by_actor", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	return _impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool killed_by_actor_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.killed_by_actor", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	return _impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool actor_has_weapon_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.actor_has_weapon", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	return _impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool actor_has_weapon_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.actor_has_weapon", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	return _impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool actor_active_detector_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor(
		"xr_conditions.actor_active_detector", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool actor_active_detector_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor(
		"xr_conditions.actor_active_detector", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool heavy_wounded_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.heavy_wounded", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	return _impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool heavy_wounded_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.heavy_wounded", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	return _impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool time_period_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.time_period", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool time_period_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.time_period", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool is_rain_client(CScriptGameObject* pActor, CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor("xr_conditions.is_rain", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	return _impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool is_rain_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor("xr_conditions.is_rain", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	return _impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool is_heavy_rain_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.is_heavy_rain", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	return _impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool is_heavy_rain_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.is_heavy_rain", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	return _impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool is_day_client(CScriptGameObject* pActor, CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor("xr_conditions.is_day", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	return _impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool is_day_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor("xr_conditions.is_day", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	return _impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool is_dark_night_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.is_dark_night", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	return _impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool is_dark_night_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.is_dark_night", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	return _impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool is_jup_a12_mercs_time_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor(
		"xr_conditions.is_jup_a12_mercs_time", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	return _impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool is_jup_a12_mercs_time_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor(
		"xr_conditions.is_jup_a12_mercs_time", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	return _impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool zat_b7_is_night_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.zat_b7_is_night", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	return _impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool zat_b7_is_night_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.zat_b7_is_night", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	return _impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool zat_b7_is_late_attack_time_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor(
		"xr_conditions.zat_b7_is_late_attack_time", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	return _impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool zat_b7_is_late_attack_time_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor(
		"xr_conditions.zat_b7_is_late_attack_time", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	return _impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool mob_has_enemy_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.mob_has_enemy", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	return _impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool mob_has_enemy_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.mob_has_enemy", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	return _impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool mob_was_hit_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.mob_was_hit", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	return _impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool mob_was_hit_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.mob_was_hit", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	return _impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool actor_on_level_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.actor_on_level", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool actor_on_level_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.actor_on_level", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool squad_in_zone_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.squad_in_zone", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool squad_in_zone_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.squad_in_zone", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool squad_has_enemy_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.squad_has_enemy", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool squad_has_enemy_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.squad_has_enemy", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool squad_in_zone_all_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.squad_in_zone_all", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool squad_in_zone_all_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.squad_in_zone_all", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool squads_in_zone_b41_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.squads_in_zone_b41", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool squads_in_zone_b41_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.squads_in_zone_b41", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool target_squad_name_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.target_squad_name", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool target_squad_name_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.target_squad_name", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool target_smart_name_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.target_smart_name", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool target_smart_name_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.target_smart_name", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool squad_exist_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.squad_exist", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool squad_exist_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.squad_exist", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool is_squad_commander_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.is_squad_commander", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool is_squad_commander_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.is_squad_commander", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool squad_npc_count_ge_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.squad_npc_count_ge", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool squad_npc_count_ge_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.squad_npc_count_ge", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool dist_to_actor_le_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
	bool result{};
	if (buffer.empty())
		return result;

	float fDistance = atof(buffer[0].c_str());

	if (pActor && pBot)
	{
		result = pBot->Position().distance_to_sqr(pActor->Position()) <=
			fDistance * fDistance;
	}

	return result;
}

inline bool surge_started_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.surge_started", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	return _impl();
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "surge manager is not implemented yet!");
	return false;
#endif
}

inline bool surge_started_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.surge_started", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	return _impl();
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "surge manager is not implemented yet!");
	return false;
#endif
}

inline bool surge_complete_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.surge_complete", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	return _impl();
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "surge manager is not implemented yet!");
	return false;
#endif
}

inline bool surge_complete_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.surge_complete", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	return _impl();
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "surge manager is not implemented yet!");
	return false;
#endif
}

inline bool surge_kill_all_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.surge_kill_all", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	return _impl();
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "surge manager is not implemented yet!");
	return false;
#endif
}

inline bool surge_kill_all_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.surge_kill_all", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	return _impl();
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "surge manager is not implemented yet!");
	return false;
#endif
}

inline bool signal_rocket_flying_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor(
		"xr_conditions.signal_rocket_flying", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false,
		"implement signal rocket data storage, but only when you provided a "
		"manager for that!");
	return false;
#endif
}

inline bool signal_rocket_flying_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor(
		"xr_conditions.signal_rocket_flying", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;
	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false,
		"implement signal rocket data storage, but only when you provided a "
		"manager for that!");
	return false;
#endif
}

inline bool quest_npc_enemy_actor_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor(
		"xr_conditions.quest_npc_enemy_actor", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	bool result{};
	if (buffer.empty())
		return result;

	const xr_string& story_id_name = buffer[0];

	R_ASSERT2(false,
		"implement story registry, provide one class for these database "
		"classes into one!");

	return result;
#endif
}

inline bool quest_npc_enemy_actor_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	if (buffer.empty())
		return false;

	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor(
		"xr_conditions.quest_npc_enemy_actor", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;

	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	bool result{};
	if (buffer.empty())
		return result;

	const xr_string& story_id_name = buffer[0];

	R_ASSERT2(false,
		"implement story registry, provide one class for these database "
		"classes into one!");

	return result;
#endif
}

inline bool animpoint_reached_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.animpoint_reached", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	return _impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "implement animpoint for database");
	return false;
#endif
}

inline bool animpoint_reached_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.animpoint_reached", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	return _impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "implement animpoint for database");
	return false;
#endif
}

inline bool distance_to_obj_ge_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.distance_to_obj_ge", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;
	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "implement story registry!");
	return false;
#endif
}

inline bool distance_to_obj_ge_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.distance_to_obj_ge", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;
	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "implement story registry");
	return false;
#endif
}

inline bool distance_to_obj_le_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.distance_to_obj_le", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;
	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "implement story registry!");
	return false;
#endif
}

inline bool distance_to_obj_le_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.distance_to_obj_le", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;
	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "implement story registry!");
	return false;
#endif
}

inline bool in_dest_smart_cover_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
	bool result{};

	if (pBot)
	{
		result = pBot->in_smart_cover();
	}

	return result;
}

inline bool in_dest_smart_cover_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
	R_ASSERT2(false,
		"must not be called, because it is expected to call for client object. "
		"Otherwise something is wrong");
	return false;
}

inline bool active_item_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
	bool result{};

	if (pActor)
	{
		for (const xr_string& item_section_name : buffer)
		{
			if (pActor->item_in_slot(3))
			{
				if (pActor->item_in_slot(3)->Section() == item_section_name)
				{
					result = true;
					break;
				}
			}
		}
	}

	return result;
}

inline bool active_item_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
	return active_item_client(pActor, nullptr, buffer);
}

inline bool actor_nomove_nowpn_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
	bool result{};

	if (pActor)
	{
		if (!ixray::is_weapon(pActor->GetActiveItem()) || pActor->IsTalking())
		{
			result = true;
		}
	}

	return result;
}

inline bool actor_nomove_nowpn_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
	bool result{};

	if (pActor)
	{
		if (!ixray::is_weapon(pActor->GetActiveItem()) || pActor->IsTalking())
		{
			result = true;
		}
	}

	return result;
}

inline bool jup_b16_is_zone_active_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
	bool result{};

	if (pBot)
	{
		result = ixray::has_alife_info(pBot->Name());
	}

	return result;
}

inline bool jup_b16_is_zone_active_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
	bool result{};

	if (pBot)
	{
		result = ixray::has_alife_info(pBot->name_replace());
	}

	return result;
}

inline bool check_bloodsucker_state_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor(
		"xr_conditions.check_bloodsucker_state", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;
	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "not implemented!");
	return false;
#endif
}

inline bool check_bloodsucker_state_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor(
		"xr_conditions.check_bloodsucker_state", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;
	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "not implemented!");
	return false;
#endif
}

inline bool dist_to_story_obj_ge_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor(
		"xr_conditions.dist_to_story_obj_ge", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;
	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "not implemented!");
	return false;
#endif
}

inline bool dist_to_story_obj_ge_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor(
		"xr_conditions.dist_to_story_obj_ge", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;
	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "not implemented!");
	return false;
#endif
}

inline bool actor_has_nimble_weapon_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
	bool result{};

	if (pActor)
	{
		constexpr const char* needed_items[] = {"wpn_groza_nimble",
			"wpn_desert_eagle_nimble", "wpn_fn2000_nimble", "wpn_g36_nimble",
			"wpn_protecta_nimble", "wpn_mp5_nimble", "wpn_sig220_nimble",
			"wpn_spas12_nimble", "wpn_usp_nimble", "wpn_vintorez_nimble",
			"wpn_svu_nimble", "wpn_svd_nimble"};

		constexpr auto length_of_array =
			sizeof(needed_items) / sizeof(needed_items[0]);

		for (int i = 0; i < length_of_array; ++i)
		{
			if (pActor->GetObjectByName(needed_items[i]))
			{
				result = true;
				break;
			}
		}
	}

	return result;
}

inline bool actor_has_nimble_weapon_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
	bool result{};

	if (pActor)
	{
		constexpr const char* needed_items[] = {"wpn_groza_nimble",
			"wpn_desert_eagle_nimble", "wpn_fn2000_nimble", "wpn_g36_nimble",
			"wpn_protecta_nimble", "wpn_mp5_nimble", "wpn_sig220_nimble",
			"wpn_spas12_nimble", "wpn_usp_nimble", "wpn_vintorez_nimble",
			"wpn_svu_nimble", "wpn_svd_nimble"};

		constexpr auto length_of_array =
			sizeof(needed_items) / sizeof(needed_items[0]);

		for (int i = 0; i < length_of_array; ++i)
		{
			if (pActor->GetObjectByName(needed_items[i]))
			{
				result = true;
				break;
			}
		}
	}

	return result;
}

inline bool actor_has_active_nimble_weapon_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
	bool result{};

	if (pActor)
	{
		constexpr const char* needed_items[] = {"wpn_groza_nimble",
			"wpn_desert_eagle_nimble", "wpn_fn2000_nimble", "wpn_g36_nimble",
			"wpn_protecta_nimble", "wpn_mp5_nimble", "wpn_sig220_nimble",
			"wpn_spas12_nimble", "wpn_usp_nimble", "wpn_vintorez_nimble",
			"wpn_svu_nimble", "wpn_svd_nimble"};

		constexpr auto length_of_array =
			sizeof(needed_items) / sizeof(needed_items[0]);

		CScriptGameObject* slot2 = pActor->item_in_slot(2);
		if (slot2)
		{
			std::string_view view = slot2->Section();

			if (view.empty() == false)
			{
				for (int i = 0; i < length_of_array; ++i)
				{
					if (view == needed_items[i])
					{
						result = true;
						break;
					}
				}
			}
		}

		if (!result)
		{
			CScriptGameObject* slot3 = pActor->item_in_slot(3);

			if (slot3)
			{
				std::string_view view = slot3->Section();

				if (view.empty() == false)
				{
					for (int i = 0; i < length_of_array; ++i)
					{
						if (view == needed_items[i])
						{
							result = true;
							break;
						}
					}
				}
			}
		}
	}

	return result;
}

inline bool actor_has_active_nimble_weapon_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
	return actor_has_active_nimble_weapon_client(pActor, nullptr, buffer);
}

inline bool jup_b202_inventory_box_empty_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor(
		"xr_conditions.jup_b202_inventory_box_empty", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	return _impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "implement this and story registry manager");
	return false;
#endif
}

inline bool jup_b202_inventory_box_empty_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor(
		"xr_conditions.jup_b202_inventory_box_empty", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	return _impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "implement this and story registry manager");
	return false;
#endif
}

inline bool is_in_danger_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.is_in_danger", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;
	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "not implemented!");
	return false;
#endif
}

inline bool is_in_danger_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.is_in_danger", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;
	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "not implemented!");
	return false;
#endif
}

inline bool object_exist_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.object_exist", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;
	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "not implemented!");
	return false;
#endif
}

inline bool object_exist_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.object_exist", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;
	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "not implemented!");
	return false;
#endif
}

inline bool squad_curr_action_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.squad_curr_action", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;
	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false,
		"provide se_squad_group.script implementation on C++ side, not "
		"implemented!");
	return false;
#endif
}

inline bool squad_curr_action_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.squad_curr_action", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;
	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false,
		"provide se_squad_group.script implementation on C++ side, not "
		"implemented!");
	return false;
#endif
}

inline bool is_monster_snork_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
	bool result{};

	if (pBot)
	{
		result = pBot->clsid() == ixray::get_script_clsid("SM_SNORK");
	}

	return result;
}

inline bool is_monster_snork_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
	bool result{};

	if (pBot)
	{
		result = pBot->script_clsid() == ixray::get_script_clsid("SM_SNORK");
	}

	return result;
}

inline bool is_monster_dog_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
	bool result{};

	if (pBot)
	{
		result = pBot->clsid() == ixray::get_script_clsid("SM_DOG_S");
	}

	return result;
}

inline bool is_monster_dog_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
	bool result{};

	if (pBot)
	{
		result = pBot->script_clsid() == ixray::get_script_clsid("SM_DOG_S");
	}

	return result;
}

inline bool is_monster_psy_dog_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
	bool result{};

	if (pBot)
	{
		result = pBot->clsid() == ixray::get_script_clsid("SM_DOG_P");
	}

	return result;
}

inline bool is_monster_psy_dog_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
	bool result{};

	if (pBot)
	{
		result = pBot->script_clsid() == ixray::get_script_clsid("SM_DOG_P");
	}

	return result;
}

inline bool is_monster_polter_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
	bool result{};

	if (pBot)
	{
		result = pBot->clsid() == ixray::get_script_clsid("SM_POLTR");
	}

	return result;
}

inline bool is_monster_polter_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
	bool result{};

	if (pBot)
	{
		result = pBot->script_clsid() == ixray::get_script_clsid("SM_POLTR");
	}

	return result;
}

inline bool is_monster_tushkano_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
	bool result{};

	if (pBot)
	{
		result = pBot->clsid() == ixray::get_script_clsid("SM_TUSHK");
	}

	return result;
}

inline bool is_monster_tushkano_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
	bool result{};

	if (pBot)
	{
		result = pBot->script_clsid() == ixray::get_script_clsid("SM_TUSHK");
	}

	return result;
}

inline bool is_monster_burer_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
	bool result{};

	if (pBot)
	{
		result = pBot->clsid() == ixray::get_script_clsid("SM_BURER");
	}

	return result;
}

inline bool is_monster_burer_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
	bool result{};

	if (pBot)
	{
		result = pBot->script_clsid() == ixray::get_script_clsid("SM_BURER");
	}

	return result;
}

inline bool is_monster_controller_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
	bool result{};

	if (pBot)
	{
		result = pBot->clsid() == ixray::get_script_clsid("SM_CONTR");
	}

	return result;
}

inline bool is_monster_controller_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
	bool result{};

	if (pBot)
	{
		result = pBot->script_clsid() == ixray::get_script_clsid("SM_CONTR");
	}

	return result;
}

inline bool is_monster_flesh_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
	bool result{};

	if (pBot)
	{
		result = pBot->clsid() == ixray::get_script_clsid("SM_FLESH");
	}

	return result;
}

inline bool is_monster_flesh_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
	bool result{};

	if (pBot)
	{
		result = pBot->script_clsid() == ixray::get_script_clsid("SM_FLESH");
	}

	return result;
}

inline bool is_monster_boar_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
	bool result{};

	if (pBot)
	{
		result = pBot->clsid() == ixray::get_script_clsid("SM_BOARW");
	}

	return result;
}

inline bool is_monster_boar_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
	bool result{};

	if (pBot)
	{
		result = pBot->script_clsid() == ixray::get_script_clsid("SM_BOARW");
	}

	return result;
}

inline bool dead_body_searching_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor(
		"xr_conditions.dead_body_searching", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	return _impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "actor_menu provide implementation");
	return false;
#endif
}

inline bool dead_body_searching_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor(
		"xr_conditions.dead_body_searching", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	return _impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "actor_menu provide implementation");
	return false;
#endif
}

inline bool jup_b47_npc_online_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.jup_b47_npc_online", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;
	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool jup_b47_npc_online_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.jup_b47_npc_online", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;
	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool anomaly_has_artefact_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor(
		"xr_conditions.anomaly_has_artefact", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;
	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide storage of anomaly_by_name");
	return false;
#endif
}

inline bool anomaly_has_artefact_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor(
		"xr_conditions.anomaly_has_artefact", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;
	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide storage of anomaly_by_name");
	return false;
#endif
}

inline bool zat_b29_anomaly_has_af_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor(
		"xr_conditions.zat_b29_anomaly_has_af", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;
	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide storage of anomaly_by_name");
	return false;
#endif
}

inline bool zat_b29_anomaly_has_af_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor(
		"xr_conditions.zat_b29_anomaly_has_af", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;
	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide storage of anomaly_by_name");
	return false;
#endif
}

inline bool jup_b221_who_will_start_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
	bool bResult{};
	if (buffer.empty())
		return bResult;

	constexpr const char* infos[] = {"jup_b25_freedom_flint_gone",
		"jup_b25_flint_blame_done_to_duty", "jup_b4_monolith_squad_in_duty",
		"jup_a6_duty_leader_bunker_guards_work",
		"jup_a6_duty_leader_employ_work", "jup_b207_duty_wins",
		"jup_b207_freedom_know_about_depot",
		"jup_b46_duty_founder_pda_to_freedom",
		"jup_b4_monolith_squad_in_freedom",
		"jup_a6_freedom_leader_bunker_guards_work",
		"jup_a6_freedom_leader_employ_work", "jup_b207_freedom_wins"};

	constexpr auto nLengthOfArray = sizeof(infos) / sizeof(infos[0]);

	int nFactionFreedomNumber = 5;
	int nFactionDutyNumber = 0;
	const char* sDutyName = "duty";
	const char* sFreedomName = "freedom";
	int aIndexes[nLengthOfArray]{};
	memset(aIndexes, -1, sizeof(aIndexes));
	int nCurrentLengthOfIndexes{};

	for (int i = 0; i < nLengthOfArray; ++i)
	{
		char aInfoBuffer[32]{};
		const char* pFactionName{};
		int nFactionNumber{};

		if (i <= 5)
		{
			pFactionName = sDutyName;
			nFactionNumber = nFactionDutyNumber;
		}
		else
		{
			pFactionName = sFreedomName;
			nFactionNumber = nFactionFreedomNumber;
		}

		memcpy(&aInfoBuffer[0] + strlen(aInfoBuffer), "jup_b221_",
			sizeof("jup_b221_"));
		memcpy(&aInfoBuffer[0] + strlen(aInfoBuffer), pFactionName,
			strlen(pFactionName) * sizeof(char));
		memcpy(
			&aInfoBuffer[0] + strlen(aInfoBuffer), "_main_", sizeof("_main_"));

		int nNumberToConvert = i - nFactionNumber;
		char number[2]{};
		sprintf(number, "%d", nNumberToConvert);

		memcpy(&aInfoBuffer[0] + strlen(aInfoBuffer), number, sizeof(number));
		memcpy(&aInfoBuffer[0] + strlen(aInfoBuffer), "_played",
			sizeof("_played"));

		if (ixray::has_alife_info(infos[i]) &&
			!(ixray::has_alife_info(aInfoBuffer)))
		{
			aIndexes[nCurrentLengthOfIndexes] = i;
			++nCurrentLengthOfIndexes;
		}
	}

	const xr_string& sFlag = buffer[0];

	if (sFlag.empty() == false)
	{
		if (sFlag == "ability")
		{
			bResult = nCurrentLengthOfIndexes != 0;
		}
		else if (sFlag == "choose")
		{
			int nGeneratedIndex = Random.randI(0, nCurrentLengthOfIndexes - 1);
			bResult = aIndexes[nGeneratedIndex] <= 5;
		}
	}

	return bResult;
}

inline bool jup_b221_who_will_start_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
	return jup_b221_who_will_start_client(pActor, nullptr, buffer);
}

inline bool pas_b400_actor_far_forward_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor(
		"xr_conditions.pas_b400_actor_far_forward", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	return _impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool pas_b400_actor_far_forward_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor(
		"xr_conditions.pas_b400_actor_far_forward", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	return _impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool pas_b400_actor_far_backward_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor(
		"xr_conditions.pas_b400_actor_far_backward", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	return _impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool pas_b400_actor_far_backward_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor(
		"xr_conditions.pas_b400_actor_far_backward", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	return _impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool pri_a28_actor_is_far_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor(
		"xr_conditions.pri_a28_actor_is_far", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	return _impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool pri_a28_actor_is_far_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor(
		"xr_conditions.pri_a28_actor_is_far", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	return _impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool check_enemy_smart_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.check_enemy_smart", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;
	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool check_enemy_smart_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.check_enemy_smart", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;
	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool zat_b103_actor_has_needed_food_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor(
		"xr_conditions.zat_b103_actor_has_needed_food", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	return _impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool zat_b103_actor_has_needed_food_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor(
		"xr_conditions.zat_b103_actor_has_needed_food", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	return _impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool zat_b29_rivals_dialog_precond_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor(
		"xr_conditions.zat_b29_rivals_dialog_precond", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	return _impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool zat_b29_rivals_dialog_precond_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor(
		"xr_conditions.zat_b29_rivals_dialog_precond", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	return _impl(pActor, pBot);
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool poltergeist_get_actor_ignore_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
	bool result{};

	if (pBot)
	{
		result = pBot->poltergeist_get_actor_ignore();
	}

	return result;
}

inline bool poltergeist_get_actor_ignore_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
	R_ASSERT2(false,
		"must be not accessible for calling! something is wrong! use client "
		"version because only client has a such 'checking'");
	return false;
}

inline bool burer_gravi_attack_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
	bool result{};

	if (pBot)
	{
		result = pBot->burer_get_force_gravi_attack();
	}

	return result;
}

inline bool burer_gravi_attack_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
	R_ASSERT2(false,
		"must be not accessible for calling! something is wrong! use client "
		"version because only client has a such 'checking'");
	return false;
}

inline bool jup_b202_actor_treasure_not_in_steal_client(
	CScriptGameObject* pActor, CScriptGameObject* pBot,
	const xr_vector<xr_string>& buffer)
{
	bool before =
		((!ixray::has_alife_info("jup_b52_actor_items_can_be_stolen")) &&
			(!ixray::has_alife_info("jup_b202_actor_items_returned")));
	bool after = (ixray::has_alife_info("jup_b52_actor_items_can_be_stolen") &&
		ixray::has_alife_info("jup_b202_actor_items_returned"));
	return (before || after);
}

inline bool jup_b202_actor_treasure_not_in_steal_server(
	CScriptGameObject* pActor, CSE_ALifeDynamicObject* pBot,
	const xr_vector<xr_string>& buffer)
{
	return jup_b202_actor_treasure_not_in_steal_client(pActor, nullptr, buffer);
}

inline bool jup_b25_senya_spawn_condition_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
	return (ixray::has_alife_info("jup_b16_oasis_found") ||
			   ixray::has_alife_info("zat_b57_bloodsucker_lair_clear") ||
			   ixray::has_alife_info("jup_b6_complete_end") ||
			   ixray::has_alife_info("zat_b215_gave_maps")) &&
		ixray::has_alife_info("zat_b106_search_soroka");
}

inline bool jup_b25_senya_spawn_condition_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
	return jup_b25_senya_spawn_condition_client(pActor, nullptr, buffer);
}

inline bool jup_b25_flint_gone_condition_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
	return ixray::has_alife_info("jup_b25_flint_blame_done_to_duty") ||
		ixray::has_alife_info("jup_b25_flint_blame_done_to_freedom") ||
		ixray::has_alife_info("zat_b106_found_soroka_done");
}

inline bool jup_b25_flint_gone_condition_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
	return jup_b25_flint_gone_condition_client(pActor, nullptr, buffer);
}

inline bool check_deimos_phase_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.check_deimos_phase", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;
	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool check_deimos_phase_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status =
		ai().script_engine().functor("xr_conditions.check_deimos_phase", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;
	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool actor_in_surge_cover_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor(
		"xr_conditions.actor_in_surge_cover", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;
	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool actor_in_surge_cover_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor(
		"xr_conditions.actor_in_surge_cover", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;
	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool is_door_blocked_by_npc_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
	bool result{};

	if (pBot)
	{
		result = pBot->is_door_blocked_by_npc();
	}

	return result;
}

inline bool is_door_blocked_by_npc_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
	R_ASSERT2(false,
		"must be not accessible, because implementation only for client "
		"object!");
	return false;
}

inline bool has_active_tutorial_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor(
		"xr_conditions.has_active_tutorial", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	return _impl();
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool has_active_tutorial_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor(
		"xr_conditions.has_active_tutorial", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	return _impl();
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool upgrade_hint_kardan_client(CScriptGameObject* pActor,
	CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor(
		"xr_conditions.upgrade_hint_kardan", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;
	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}

inline bool upgrade_hint_kardan_server(CScriptGameObject* pActor,
	CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION
	luabind::functor<bool> _impl;
	auto status = ai().script_engine().functor(
		"xr_conditions.upgrade_hint_kardan", _impl);
	R_ASSERT2(status,
		"failed to obtain original function implementation in lua file!!!");

	xr_vector<const char*> temp;
	for (const xr_string& str : buffer)
	{
		temp.push_back(str.c_str());
	}

	return _impl(pActor, pBot, temp.data());
#elif defined(IXRAY_USE_CPP_ONLY_IMPLEMENTATION)
	R_ASSERT2(false, "provide implementation");
	return false;
#endif
}