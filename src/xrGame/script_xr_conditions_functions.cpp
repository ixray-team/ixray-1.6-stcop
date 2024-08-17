// keep this file as .cpp format for users they don't need to use it as header file!!!!!!

#include "StdAfx.h"
#include "script_game_object.h"
#include "xrServer_Objects_ALife.h"
#include "script_utility.h"

// specify your function implementation here as you do in xr_conditions.script on Lua programming language
// reminder for user, the game uses client server architecture thus they have client and server types that represent a NPC on level. 
// CScriptGameObject - client 
// CSE_ALifeXXX - server

// always provide two implementations for your functions based on prefixes _client and _server respectively

// comment it if you can provide C++ implementation
// because now some functions are provided in pure C++ implementation, but some of many functions can't be re-written so easily due to lack of manager implementations like surge manager, story manager, db.script (database) and etc, so if you can't provide implementation just use lua's implementation
#define IXRAY_USE_LUA_IMPLEMENTATION

inline bool fighting_dist_ge_client(
    CScriptGameObject* enemy, CScriptGameObject* npc, const xr_vector<xr_string>& buffer)
{
    if (!enemy)
        return false;

    if (!npc)
        return false;

    if (!buffer.size())
        return false;

    float distance = atof(buffer[0].c_str());

    return (enemy->Position().distance_to_sqr(npc->Position()) >= (distance * distance));
}

inline bool fighting_dist_ge_server(
    CScriptGameObject* enemy, CSE_ALifeDynamicObject* npc, const xr_vector<xr_string>& buffer)
{
    if (!enemy)
        return false;

    if (!npc)
        return false;

    if (!buffer.size())
        return false;

    float distance = atof(buffer[0].c_str());

    return (enemy->Position().distance_to_sqr(npc->Position()) >= (distance * distance));
}

inline bool surge_started_client(CScriptGameObject* pActor, CScriptGameObject* pBot, const xr_vector<xr_string>& buffer) 
{
#ifdef IXRAY_USE_LUA_IMPLEMENTATION
    luabind::functor<bool> _impl;
    auto status = ai().script_engine().functor("xr_conditions.surge_started", _impl);
    R_ASSERT2(status, "failed to obtain original function implementation in lua file!!!");
    
    return _impl();
#else
    R_ASSERT2(false, "surge manager is not implemented yet!");
    return false;
#endif
}

inline bool surge_started_server(CScriptGameObject* pActor, CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_IMPLEMENTATION
    luabind::functor<bool> _impl;
    auto status = ai().script_engine().functor("xr_conditions.surge_started", _impl);
    R_ASSERT2(status, "failed to obtain original function implementation in lua file!!!");

    return _impl();
#else
    R_ASSERT2(false, "surge manager is not implemented yet!");
    return false;
#endif
}

inline bool surge_complete_client(CScriptGameObject* pActor, CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_IMPLEMENTATION
    luabind::functor<bool> _impl;
    auto status = ai().script_engine().functor("xr_conditions.surge_complete", _impl);
    R_ASSERT2(status, "failed to obtain original function implementation in lua file!!!");

    return _impl();
#else
    R_ASSERT2(false, "surge manager is not implemented yet!");
    return false;
#endif
}

inline bool surge_complete_server(CScriptGameObject* pActor, CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_IMPLEMENTATION
    luabind::functor<bool> _impl;
    auto status = ai().script_engine().functor("xr_conditions.surge_complete", _impl);
    R_ASSERT2(status, "failed to obtain original function implementation in lua file!!!");

    return _impl();
#else
    R_ASSERT2(false, "surge manager is not implemented yet!");
    return false;
#endif
}

inline bool surge_kill_all_client(CScriptGameObject* pActor, CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_IMPLEMENTATION
    luabind::functor<bool> _impl;
    auto status = ai().script_engine().functor("xr_conditions.surge_kill_all", _impl);
    R_ASSERT2(status, "failed to obtain original function implementation in lua file!!!");

    return _impl();
#else
    R_ASSERT2(false, "surge manager is not implemented yet!");
    return false;
#endif
}

inline bool surge_kill_all_server(CScriptGameObject* pActor, CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_IMPLEMENTATION
    luabind::functor<bool> _impl;
    auto status = ai().script_engine().functor("xr_conditions.surge_kill_all", _impl);
    R_ASSERT2(status, "failed to obtain original function implementation in lua file!!!");

    return _impl();
#else
    R_ASSERT2(false, "surge manager is not implemented yet!");
    return false;
#endif
}

inline bool signal_rocket_flying_client(CScriptGameObject* pActor, CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_IMPLEMENTATION
    if (buffer.empty())
        return false;

    luabind::functor<bool> _impl;
    auto status = ai().script_engine().functor("xr_conditions.signal_rocket_flying", _impl);
    R_ASSERT2(status, "failed to obtain original function implementation in lua file!!!");

    xr_vector<const char*> temp;

    for (const xr_string& str : buffer)
    {
        temp.push_back(str.c_str());
    }

    return _impl(pActor, pBot, temp.data());
#else
    R_ASSERT2(false, "implement signal rocket data storage, but only when you provided a manager for that!");
    return false;
#endif
}

inline bool signal_rocket_flying_server(CScriptGameObject* pActor, CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_IMPLEMENTATION
    if (buffer.empty())
        return false;

    luabind::functor<bool> _impl;
    auto status = ai().script_engine().functor("xr_conditions.signal_rocket_flying", _impl);
    R_ASSERT2(status, "failed to obtain original function implementation in lua file!!!");

    xr_vector<const char*> temp;
    for (const xr_string& str : buffer)
    {
        temp.push_back(str.c_str());
    }

    return _impl(pActor, pBot, temp.data());
#else
    R_ASSERT2(false, "implement signal rocket data storage, but only when you provided a manager for that!");
    return false;
#endif
}

inline bool quest_npc_enemy_actor_client(CScriptGameObject* pActor, CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_IMPLEMENTATION
    if (buffer.empty())
        return false;

    luabind::functor<bool> _impl;
    auto status = ai().script_engine().functor("xr_conditions.quest_npc_enemy_actor", _impl);
    R_ASSERT2(status, "failed to obtain original function implementation in lua file!!!");

    xr_vector<const char*> temp;

    for (const xr_string& str : buffer)
    {
        temp.push_back(str.c_str());
    }

    return _impl(pActor, pBot, temp.data());
#else
    bool result{};
    if (buffer.empty())
        return result;

    const xr_string& story_id_name = buffer[0];

    R_ASSERT2(false, "implement story registry, provide one class for these database classes into one!");

    return result;
#endif
}

inline bool quest_npc_enemy_actor_server(CScriptGameObject* pActor, CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_IMPLEMENTATION
    if (buffer.empty())
        return false;

    luabind::functor<bool> _impl;
    auto status = ai().script_engine().functor("xr_conditions.quest_npc_enemy_actor", _impl);
    R_ASSERT2(status, "failed to obtain original function implementation in lua file!!!");

    xr_vector<const char*> temp;

    for (const xr_string& str : buffer)
    {
        temp.push_back(str.c_str());
    }

    return _impl(pActor, pBot, temp.data());
#else
    bool result{};
    if (buffer.empty())
        return result;

    const xr_string& story_id_name = buffer[0];

    R_ASSERT2(false, "implement story registry, provide one class for these database classes into one!");

    return result;
#endif
}

inline bool animpoint_reached_client(CScriptGameObject* pActor, CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_IMPLEMENTATION
    luabind::functor<bool> _impl;
    auto status = ai().script_engine().functor("xr_conditions.animpoint_reached", _impl);
    R_ASSERT2(status, "failed to obtain original function implementation in lua file!!!");

    return _impl(pActor, pBot);
#else
    R_ASSERT2(false, "implement animpoint for database");
    return false;
#endif
}

inline bool animpoint_reached_server(CScriptGameObject* pActor, CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_IMPLEMENTATION
    luabind::functor<bool> _impl;
    auto status = ai().script_engine().functor("xr_conditions.animpoint_reached", _impl);
    R_ASSERT2(status, "failed to obtain original function implementation in lua file!!!");

    return _impl(pActor, pBot);
#else
    R_ASSERT2(false, "implement animpoint for database");
    return false;
#endif
}

inline bool distance_to_obj_ge_client(CScriptGameObject* pActor, CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_IMPLEMENTATION
    luabind::functor<bool> _impl;
    auto status = ai().script_engine().functor("xr_conditions.distance_to_obj_ge", _impl);
    R_ASSERT2(status, "failed to obtain original function implementation in lua file!!!");

    xr_vector<const char*> temp;
    for (const xr_string& str : buffer)
    {
        temp.push_back(str.c_str());
    }

    return _impl(pActor, pBot, temp.data());
#else
    R_ASSERT2(false, "implement story registry!");
    return false;
#endif
}

inline bool distance_to_obj_ge_server(CScriptGameObject* pActor, CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_IMPLEMENTATION
    luabind::functor<bool> _impl;
    auto status = ai().script_engine().functor("xr_conditions.distance_to_obj_ge", _impl);
    R_ASSERT2(status, "failed to obtain original function implementation in lua file!!!");

    xr_vector<const char*> temp;
    for (const xr_string& str : buffer)
    {
        temp.push_back(str.c_str());
    }

    return _impl(pActor, pBot, temp.data());
#else
    R_ASSERT2(false, "implement story registry");
    return false;
#endif
}

inline bool distance_to_obj_le_client(CScriptGameObject* pActor, CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_IMPLEMENTATION
    luabind::functor<bool> _impl;
    auto status = ai().script_engine().functor("xr_conditions.distance_to_obj_le", _impl);
    R_ASSERT2(status, "failed to obtain original function implementation in lua file!!!");

    xr_vector<const char*> temp;
    for (const xr_string& str : buffer)
    {
        temp.push_back(str.c_str());
    }

    return _impl(pActor, pBot, temp.data());
#else
    R_ASSERT2(false, "implement story registry!");
    return false;
#endif
}

inline bool distance_to_obj_le_server(CScriptGameObject* pActor, CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_IMPLEMENTATION
    luabind::functor<bool> _impl;
    auto status = ai().script_engine().functor("xr_conditions.distance_to_obj_le", _impl);
    R_ASSERT2(status, "failed to obtain original function implementation in lua file!!!");

    xr_vector<const char*> temp;
    for (const xr_string& str : buffer)
    {
        temp.push_back(str.c_str());
    }

    return _impl(pActor, pBot, temp.data());
#else
    R_ASSERT2(false, "implement story registry!");
    return false;
#endif
}

inline bool in_dest_smart_cover_client(CScriptGameObject* pActor, CScriptGameObject* pBot, const xr_vector<xr_string>& buffer) 
{
    bool result{};

    if (pBot)
    {
         result = pBot->in_smart_cover();
    }
    
    return result;
}

inline bool in_dest_smart_cover_server(CScriptGameObject* pActor, CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
    R_ASSERT2(false, "must not be called, because it is expected to call for client object. Otherwise something is wrong");
    return false;
}

inline bool active_item_client(CScriptGameObject* pActor, CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
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

inline bool active_item_server(CScriptGameObject* pActor, CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
    return active_item_client(pActor, nullptr, buffer);
}

inline bool actor_nomove_nowpn_client(CScriptGameObject* pActor, CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
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

inline bool actor_nomove_nowpn_server(CScriptGameObject* pActor, CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
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

inline bool jup_b16_is_zone_active_client(CScriptGameObject* pActor, CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
    bool result{};
   
    if (pBot)
    {
        result = ixray::has_alife_info(pBot->Name());
    }

    return result;
}

inline bool jup_b16_is_zone_active_server(CScriptGameObject* pActor, CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
    bool result{};

    if (pBot)
    {
        result = ixray::has_alife_info(pBot->name_replace());
    }

    return result;
}

inline bool check_bloodsucker_state_client(CScriptGameObject* pActor, CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_IMPLEMENTATION
    luabind::functor<bool> _impl;
    auto status = ai().script_engine().functor("xr_conditions.check_bloodsucker_state", _impl);
    R_ASSERT2(status, "failed to obtain original function implementation in lua file!!!");

    xr_vector<const char*> temp;
    for (const xr_string& str : buffer)
    {
        temp.push_back(str.c_str());
    }

    return _impl(pActor, pBot, temp.data());
#else
    R_ASSERT2(false, "not implemented!");
    return false;
#endif
}

inline bool check_bloodsucker_state_server(CScriptGameObject* pActor, CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_IMPLEMENTATION
    luabind::functor<bool> _impl;
    auto status = ai().script_engine().functor("xr_conditions.check_bloodsucker_state", _impl);
    R_ASSERT2(status, "failed to obtain original function implementation in lua file!!!");

    xr_vector<const char*> temp;
    for (const xr_string& str : buffer)
    {
        temp.push_back(str.c_str());
    }

    return _impl(pActor, pBot, temp.data());
#else
    R_ASSERT2(false, "not implemented!");
    return false;
#endif
}

inline bool dist_to_story_obj_ge_client(CScriptGameObject* pActor, CScriptGameObject*
 pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_IMPLEMENTATION
    luabind::functor<bool> _impl;
    auto status = ai().script_engine().functor("xr_conditions.dist_to_story_obj_ge", _impl);
    R_ASSERT2(status, "failed to obtain original function implementation in lua file!!!");

    xr_vector<const char*> temp;
    for (const xr_string& str : buffer)
    {
        temp.push_back(str.c_str());
    }

    return _impl(pActor, pBot, temp.data());
#else
    R_ASSERT2(false, "not implemented!");
    return false;
#endif
}

inline bool dist_to_story_obj_ge_server(CScriptGameObject* pActor, CSE_ALifeDynamicObject*
    pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_IMPLEMENTATION
    luabind::functor<bool> _impl;
    auto status = ai().script_engine().functor("xr_conditions.dist_to_story_obj_ge", _impl);
    R_ASSERT2(status, "failed to obtain original function implementation in lua file!!!");

    xr_vector<const char*> temp;
    for (const xr_string& str : buffer)
    {
        temp.push_back(str.c_str());
    }

    return _impl(pActor, pBot, temp.data());
#else
    R_ASSERT2(false, "not implemented!");
    return false;
#endif
}

inline bool actor_has_nimble_weapon_client(CScriptGameObject* pActor, CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
    bool result{};

    if (pActor)
    {
        constexpr const char* needed_items[] = {
            "wpn_groza_nimble", 
            "wpn_desert_eagle_nimble",
            "wpn_fn2000_nimble",
            "wpn_g36_nimble",
            "wpn_protecta_nimble",
            "wpn_mp5_nimble",
            "wpn_sig220_nimble",
            "wpn_spas12_nimble",
            "wpn_usp_nimble",
            "wpn_vintorez_nimble",
            "wpn_svu_nimble",
            "wpn_svd_nimble"
        };

        constexpr auto length_of_array = sizeof(needed_items) / sizeof(needed_items[0]);


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

inline bool actor_has_nimble_weapon_server(CScriptGameObject* pActor, CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
    bool result{};

    if (pActor)
    {
        constexpr const char* needed_items[] = {
            "wpn_groza_nimble",
            "wpn_desert_eagle_nimble",
            "wpn_fn2000_nimble",
            "wpn_g36_nimble",
            "wpn_protecta_nimble",
            "wpn_mp5_nimble",
            "wpn_sig220_nimble",
            "wpn_spas12_nimble",
            "wpn_usp_nimble",
            "wpn_vintorez_nimble",
            "wpn_svu_nimble",
            "wpn_svd_nimble"
        };

        constexpr auto length_of_array = sizeof(needed_items) / sizeof(needed_items[0]);


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