// keep this file as .cpp format for users they don't need to use it as header file!!!!!!

#include "StdAfx.h"
#include "script_game_object.h"
#include "xrServer_Objects_ALife.h"
#include "script_utility.h"
#include "Level.h"
// specify your function implementation here as you do in xr_conditions.script on Lua programming language
// reminder for user, the game uses client server architecture thus they have client and server types that represent a NPC on level. 
// CScriptGameObject - client 
// CSE_ALifeXXX - server

// always provide two implementations for your functions based on prefixes _client and _server respectively

// comment it if you can provide C++ implementation FOR ALL FUNCTIONS!!!
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

inline bool actor_has_active_nimble_weapon_client(CScriptGameObject* 
pActor, CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
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

inline bool actor_has_active_nimble_weapon_server(CScriptGameObject*
    pActor, CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
    return actor_has_active_nimble_weapon_client(pActor, nullptr, buffer);
}

inline bool jup_b202_inventory_box_empty_client(CScriptGameObject* pActor, CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_IMPLEMENTATION
    luabind::functor<bool> _impl;
    auto status = ai().script_engine().functor("xr_conditions.jup_b202_inventory_box_empty", _impl);
    R_ASSERT2(status, "failed to obtain original function implementation in lua file!!!");

    return _impl(pActor, pBot);
#else
    R_ASSERT2(false, "implement this and story registry manager");
    return false;
#endif
}

inline bool jup_b202_inventory_box_empty_server(CScriptGameObject* pActor, CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_IMPLEMENTATION
    luabind::functor<bool> _impl;
    auto status = ai().script_engine().functor("xr_conditions.jup_b202_inventory_box_empty", _impl);
    R_ASSERT2(status, "failed to obtain original function implementation in lua file!!!");

    return _impl(pActor, pBot);
#else
    R_ASSERT2(false, "implement this and story registry manager");
    return false;
#endif
}

inline bool is_in_danger_client(CScriptGameObject* pActor, 
CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_IMPLEMENTATION
    luabind::functor<bool> _impl;
    auto status = ai().script_engine().functor("xr_conditions.is_in_danger", _impl);
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

inline bool is_in_danger_server(CScriptGameObject* pActor,
    CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_IMPLEMENTATION
    luabind::functor<bool> _impl;
    auto status = ai().script_engine().functor("xr_conditions.is_in_danger", _impl);
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

inline bool object_exist_client(CScriptGameObject* pActor, CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_IMPLEMENTATION
    luabind::functor<bool> _impl;
    auto status = ai().script_engine().functor("xr_conditions.object_exist", _impl);
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

inline bool object_exist_server(CScriptGameObject* pActor, CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_IMPLEMENTATION
    luabind::functor<bool> _impl;
    auto status = ai().script_engine().functor("xr_conditions.object_exist", _impl);
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

inline bool squad_curr_action_client(CScriptGameObject* pActor, CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_IMPLEMENTATION
    luabind::functor<bool> _impl;
    auto status = ai().script_engine().functor("xr_conditions.squad_curr_action", _impl);
    R_ASSERT2(status, "failed to obtain original function implementation in lua file!!!");

    xr_vector<const char*> temp;
    for (const xr_string& str : buffer)
    {
        temp.push_back(str.c_str());
    }

    return _impl(pActor, pBot, temp.data());
#else
    R_ASSERT2(false, "provide se_squad_group.script implementation on C++ side, not implemented!");
    return false;
#endif
}

inline bool squad_curr_action_server(CScriptGameObject* pActor, CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_IMPLEMENTATION
    luabind::functor<bool> _impl;
    auto status = ai().script_engine().functor("xr_conditions.squad_curr_action", _impl);
    R_ASSERT2(status, "failed to obtain original function implementation in lua file!!!");

    xr_vector<const char*> temp;
    for (const xr_string& str : buffer)
    {
        temp.push_back(str.c_str());
    }

    return _impl(pActor, pBot, temp.data());
#else
    R_ASSERT2(false, "provide se_squad_group.script implementation on C++ side, not implemented!");
    return false;
#endif
}

inline bool is_monster_snork_client(CScriptGameObject* pActor, CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
    bool result{};

    if (pBot)
    {
        result = pBot->clsid() == ixray::get_script_clsid("SM_SNORK");
    }

    return result;
}

inline bool is_monster_snork_server(CScriptGameObject* pActor, CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
    bool result{};

    if (pBot)
    {
        result = pBot->script_clsid() == ixray::get_script_clsid("SM_SNORK");
    }

    return result;
}

inline bool is_monster_dog_client(CScriptGameObject* pActor, CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
    bool result{};

    if (pBot)
    {
        result = pBot->clsid() == ixray::get_script_clsid("SM_DOG_S");
    }

    return result;
}

inline bool is_monster_dog_server(CScriptGameObject* pActor, CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
    bool result{};

    if (pBot)
    {
        result = pBot->script_clsid() == ixray::get_script_clsid("SM_DOG_S");
    }

    return result;
}

inline bool is_monster_psy_dog_client(CScriptGameObject* pActor, CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
    bool result{};

    if (pBot)
    {
        result = pBot->clsid() == ixray::get_script_clsid("SM_DOG_P");
    }

    return result;
}

inline bool is_monster_psy_dog_server(CScriptGameObject* pActor, CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
    bool result{};

    if (pBot)
    {
        result = pBot->script_clsid() == ixray::get_script_clsid("SM_DOG_P");
    }

    return result;
}

inline bool is_monster_polter_client(CScriptGameObject* pActor, CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
    bool result{};

    if (pBot)
    {
        result = pBot->clsid() == ixray::get_script_clsid("SM_POLTR");
    }

    return result;
}

inline bool is_monster_polter_server(CScriptGameObject* pActor, CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
    bool result{};

    if (pBot)
    {
        result = pBot->script_clsid() == ixray::get_script_clsid("SM_POLTR");
    }

    return result;
}

inline bool is_monster_tushkano_client(CScriptGameObject* pActor, CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
    bool result{};

    if (pBot)
    {
        result = pBot->clsid() == ixray::get_script_clsid("SM_TUSHK");
    }

    return result;
}

inline bool is_monster_tushkano_server(CScriptGameObject* pActor, CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
    bool result{};

    if (pBot)
    {
        result = pBot->script_clsid() == ixray::get_script_clsid("SM_TUSHK");
    }

    return result;
}

inline bool is_monster_burer_client(CScriptGameObject* pActor, CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
    bool result{};

    if (pBot)
    {
        result = pBot->clsid() == ixray::get_script_clsid("SM_BURER");
    }

    return result;
}

inline bool is_monster_burer_server(CScriptGameObject* pActor, CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
    bool result{};

    if (pBot)
    {
        result = pBot->script_clsid() == ixray::get_script_clsid("SM_BURER");
    }

    return result;
}

inline bool is_monster_controller_client(CScriptGameObject* pActor, CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
    bool result{};

    if (pBot)
    {
        result = pBot->clsid() == ixray::get_script_clsid("SM_CONTR");
    }

    return result;
}

inline bool is_monster_controller_server(CScriptGameObject* pActor, CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
    bool result{};

    if (pBot)
    {
        result = pBot->script_clsid() == ixray::get_script_clsid("SM_CONTR");
    }

    return result;
}

inline bool is_monster_flesh_client(CScriptGameObject* pActor, CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
    bool result{};

    if (pBot)
    {
        result = pBot->clsid() == ixray::get_script_clsid("SM_FLESH");
    }

    return result;
}

inline bool is_monster_flesh_server(CScriptGameObject* pActor, CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
    bool result{};

    if (pBot)
    {
        result = pBot->script_clsid() == ixray::get_script_clsid("SM_FLESH");
    }

    return result;
}

inline bool is_monster_boar_client(CScriptGameObject* pActor, CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
    bool result{};

    if (pBot)
    {
        result = pBot->clsid() == ixray::get_script_clsid("SM_BOARW");
    }

    return result;
}

inline bool is_monster_boar_server(CScriptGameObject* pActor, CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
    bool result{};

    if (pBot)
    {
        result = pBot->script_clsid() == ixray::get_script_clsid("SM_BOARW");
    }

    return result;
}

inline bool dead_body_searching_client(CScriptGameObject* pActor, CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_IMPLEMENTATION
    luabind::functor<bool> _impl;
    auto status = ai().script_engine().functor("xr_conditions.dead_body_searching", _impl);
    R_ASSERT2(status, "failed to obtain original function implementation in lua file!!!");

    return _impl(pActor, pBot);
#else
    R_ASSERT2(false, "actor_menu provide implementation");
    return false;
#endif
}

inline bool dead_body_searching_server(CScriptGameObject* pActor, CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_IMPLEMENTATION
    luabind::functor<bool> _impl;
    auto status = ai().script_engine().functor("xr_conditions.dead_body_searching", _impl);
    R_ASSERT2(status, "failed to obtain original function implementation in lua file!!!");

    return _impl(pActor, pBot);
#else
    R_ASSERT2(false, "actor_menu provide implementation");
    return false;
#endif
}

inline bool jup_b47_npc_online_client(CScriptGameObject* pActor, CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_IMPLEMENTATION
    luabind::functor<bool> _impl;
    auto status = ai().script_engine().functor("xr_conditions.jup_b47_npc_online", _impl);
    R_ASSERT2(status, "failed to obtain original function implementation in lua file!!!");

    xr_vector<const char*> temp;
    for (const xr_string& str : buffer)
    {
        temp.push_back(str.c_str());
    }

    return _impl(pActor, pBot, temp.data());
#else
    R_ASSERT2(false, "provide implementation");
    return false;
#endif
}

inline bool jup_b47_npc_online_server(CScriptGameObject* pActor, CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_IMPLEMENTATION
    luabind::functor<bool> _impl;
    auto status = ai().script_engine().functor("xr_conditions.jup_b47_npc_online", _impl);
    R_ASSERT2(status, "failed to obtain original function implementation in lua file!!!");

    xr_vector<const char*> temp;
    for (const xr_string& str : buffer)
    {
        temp.push_back(str.c_str());
    }

    return _impl(pActor, pBot, temp.data());
#else
    R_ASSERT2(false, "provide implementation");
    return false;
#endif
}

inline bool anomaly_has_artefact_client(CScriptGameObject* pActor, CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_IMPLEMENTATION
    luabind::functor<bool> _impl;
    auto status = ai().script_engine().functor("xr_conditions.anomaly_has_artefact", _impl);
    R_ASSERT2(status, "failed to obtain original function implementation in lua file!!!");

    xr_vector<const char*> temp;
    for (const xr_string& str : buffer)
    {
        temp.push_back(str.c_str());
    }

    return _impl(pActor, pBot, temp.data());
#else
    R_ASSERT2(false, "provide storage of anomaly_by_name");
    return false;
#endif
}


inline bool anomaly_has_artefact_server(CScriptGameObject* pActor, CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_IMPLEMENTATION
    luabind::functor<bool> _impl;
    auto status = ai().script_engine().functor("xr_conditions.anomaly_has_artefact", _impl);
    R_ASSERT2(status, "failed to obtain original function implementation in lua file!!!");

    xr_vector<const char*> temp;
    for (const xr_string& str : buffer)
    {
        temp.push_back(str.c_str());
    }

    return _impl(pActor, pBot, temp.data());
#else
    R_ASSERT2(false, "provide storage of anomaly_by_name");
    return false;
#endif
}

inline bool zat_b29_anomaly_has_af_client(CScriptGameObject* pActor, CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_IMPLEMENTATION
    luabind::functor<bool> _impl;
    auto status = ai().script_engine().functor("xr_conditions.zat_b29_anomaly_has_af", _impl);
    R_ASSERT2(status, "failed to obtain original function implementation in lua file!!!");

    xr_vector<const char*> temp;
    for (const xr_string& str : buffer)
    {
        temp.push_back(str.c_str());
    }

    return _impl(pActor, pBot, temp.data());
#else
    R_ASSERT2(false, "provide storage of anomaly_by_name");
    return false;
#endif
}

inline bool zat_b29_anomaly_has_af_server(CScriptGameObject* pActor, CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_IMPLEMENTATION
    luabind::functor<bool> _impl;
    auto status = ai().script_engine().functor("xr_conditions.zat_b29_anomaly_has_af", _impl);
    R_ASSERT2(status, "failed to obtain original function implementation in lua file!!!");

    xr_vector<const char*> temp;
    for (const xr_string& str : buffer)
    {
        temp.push_back(str.c_str());
    }

    return _impl(pActor, pBot, temp.data());
#else
    R_ASSERT2(false, "provide storage of anomaly_by_name");
    return false;
#endif
}

inline bool jup_b221_who_will_start_client(CScriptGameObject* pActor, CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
    bool bResult{};
    if (buffer.empty())
        return bResult;

    constexpr const char* infos[] = {
        "jup_b25_freedom_flint_gone",
        "jup_b25_flint_blame_done_to_duty",
        "jup_b4_monolith_squad_in_duty",
        "jup_a6_duty_leader_bunker_guards_work",
        "jup_a6_duty_leader_employ_work",
        "jup_b207_duty_wins",
        "jup_b207_freedom_know_about_depot",
        "jup_b46_duty_founder_pda_to_freedom",
        "jup_b4_monolith_squad_in_freedom",
        "jup_a6_freedom_leader_bunker_guards_work",
        "jup_a6_freedom_leader_employ_work",
        "jup_b207_freedom_wins"
    };

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

        memcpy(&aInfoBuffer[0] + strlen(aInfoBuffer), "jup_b221_", sizeof("jup_b221_"));
        memcpy(&aInfoBuffer[0] + strlen(aInfoBuffer), pFactionName, strlen(pFactionName) * sizeof(char));
        memcpy(&aInfoBuffer[0] + strlen(aInfoBuffer), "_main_", sizeof("_main_"));

        int nNumberToConvert = i - nFactionNumber;
        char number[2]{};
        sprintf(number, "%d", nNumberToConvert);

        memcpy(&aInfoBuffer[0] + strlen(aInfoBuffer), number, sizeof(number));
        memcpy(&aInfoBuffer[0] + strlen(aInfoBuffer), "_played", sizeof("_played"));

        if (ixray::has_alife_info(infos[i]) && !(ixray::has_alife_info(aInfoBuffer)))
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
           int nGeneratedIndex = Random.randI(0, nCurrentLengthOfIndexes-1);
           bResult = aIndexes[nGeneratedIndex] <= 5;
        }
    }

    return bResult;
}

inline bool jup_b221_who_will_start_server(CScriptGameObject* pActor, CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
    return jup_b221_who_will_start_client(pActor, nullptr, buffer);
}

inline bool pas_b400_actor_far_forward_client(CScriptGameObject* pActor, CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_IMPLEMENTATION
    luabind::functor<bool> _impl;
    auto status = ai().script_engine().functor("xr_conditions.pas_b400_actor_far_forward", _impl);
    R_ASSERT2(status, "failed to obtain original function implementation in lua file!!!");

    return _impl(pActor, pBot);
#else
    R_ASSERT2(false, "provide implementation");
    return false;
#endif
}

inline bool pas_b400_actor_far_forward_server(CScriptGameObject* pActor, CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_IMPLEMENTATION
    luabind::functor<bool> _impl;
    auto status = ai().script_engine().functor("xr_conditions.pas_b400_actor_far_forward", _impl);
    R_ASSERT2(status, "failed to obtain original function implementation in lua file!!!");

    return _impl(pActor, pBot);
#else
    R_ASSERT2(false, "provide implementation");
    return false;
#endif
}

inline bool pas_b400_actor_far_backward_client(CScriptGameObject* pActor, CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_IMPLEMENTATION
    luabind::functor<bool> _impl;
    auto status = ai().script_engine().functor("xr_conditions.pas_b400_actor_far_backward", _impl);
    R_ASSERT2(status, "failed to obtain original function implementation in lua file!!!");

    return _impl(pActor, pBot);
#else
    R_ASSERT2(false, "provide implementation");
    return false;
#endif
}

inline bool pas_b400_actor_far_backward_server(CScriptGameObject* pActor, CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
#ifdef IXRAY_USE_LUA_IMPLEMENTATION
    luabind::functor<bool> _impl;
    auto status = ai().script_engine().functor("xr_conditions.pas_b400_actor_far_backward", _impl);
    R_ASSERT2(status, "failed to obtain original function implementation in lua file!!!");

    return _impl(pActor, pBot);
#else
    R_ASSERT2(false, "provide implementation");
    return false;
#endif
}