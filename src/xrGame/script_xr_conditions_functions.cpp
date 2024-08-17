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
    Msg("! surge manager is not implemented yet!\n");
    return false;
}

inline bool surge_started_server(CScriptGameObject* pActor, CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
    Msg("! surge manager is not implemented yet!\n");
    return false;
}

inline bool surge_complete_client(CScriptGameObject* pActor, CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
    Msg("! surge manager is not implemented yet!\n");
    return false;
}

inline bool surge_complete_server(CScriptGameObject* pActor, CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
    Msg("! surge manager is not implemented yet!\n");
    return false;
}

inline bool surge_kill_all_client(CScriptGameObject* pActor, CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
    Msg("! surge manager is not implemented yet!\n");
    return false;
}

inline bool surge_kill_all_server(CScriptGameObject* pActor, CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
    Msg("! surge manager is not implemented yet!\n");
    return false;
}

inline bool signal_rocket_flying_client(CScriptGameObject* pActor, CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
    Msg("! implement signal rocket data storage, but only when you provided a manager for that!");
    return false;
}

inline bool signal_rocket_flying_server(CScriptGameObject* pActor, CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
    Msg("! implement signal rocket data storage, but only when you provided a manager for that!");
    return false;
}

inline bool quest_npc_enemy_actor_client(CScriptGameObject* pActor, CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
    bool result{};
    if (buffer.empty())
        return result;

    const xr_string& story_id_name = buffer[0];

    Msg("! implement story registry, provide one class for these database classes into one!");

    return result;
}

inline bool quest_npc_enemy_actor_server(CScriptGameObject* pActor, CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
    bool result{};
    if (buffer.empty())
        return result;

    const xr_string& story_id_name = buffer[0];

    Msg("! implement story registry, provide one class for these database classes into one!");

    return result;
}

inline bool animpoint_reached_client(CScriptGameObject* pActor, CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
    Msg("! implement animpoint for database");
    return false;
}

inline bool animpoint_reached_server(CScriptGameObject* pActor, CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
    Msg("! implement animpoint for database!");
    return false;
}

inline bool distance_to_obj_ge_client(CScriptGameObject* pActor, CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
    Msg("! implement story registry");
    return false;
}

inline bool distance_to_obj_ge_server(CScriptGameObject* pActor, CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
    Msg("! implement story registry");
    return false;
}

inline bool distance_to_obj_le_client(CScriptGameObject* pActor, CScriptGameObject* pBot, const xr_vector<xr_string>& buffer)
{
    Msg("! implement story registry");
    return false;
}

inline bool distance_to_obj_le_server(CScriptGameObject* pActor, CSE_ALifeDynamicObject* pBot, const xr_vector<xr_string>& buffer)
{
    Msg("! imlpement story registry");
    return false;
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

    }

    return result;
}