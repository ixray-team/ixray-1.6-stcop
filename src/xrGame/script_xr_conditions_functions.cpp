// keep this file as .cpp format for users they don't need to use it as header file!!!!!!

#include "StdAfx.h"
#include "script_game_object.h"
#include "xrServer_Objects_ALife.h"

// specify your function implementation here as you do in xr_conditions.script on Lua programming language

inline bool is_fighting_dist_ge_client(
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

inline bool is_fighting_dist_ge_server(
    CSE_ALifeDynamicObject* enemy, CSE_ALifeDynamicObject* npc, const xr_vector<xr_string>& buffer)
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