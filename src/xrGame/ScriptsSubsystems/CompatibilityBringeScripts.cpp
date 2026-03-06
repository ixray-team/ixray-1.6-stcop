#include "stdafx.h"
#include "GameObject.h"
#include "ai_object_location.h"
#include "../../xrServerEntities/xrMessages.h"

#include <luabind/luabind.hpp>

float SimDistTo(CGameObject* obj1, CGameObject* obj2)
{
	if (obj1 == nullptr || obj2 == nullptr)
	{
		return FLT_MAX;
	}

	const CGameGraph::CVertex* v1 = ai().game_graph().vertex(obj1->ai_location().game_vertex_id());
	const CGameGraph::CVertex* v2 = ai().game_graph().vertex(obj2->ai_location().game_vertex_id());

	if (v1 == nullptr || v2 == nullptr)
	{
		return FLT_MAX;
	}

	return v1->game_point().distance_to(v2->game_point());
}

void CompatibilityBringeExport(lua_State* L)
{
	luabind::module(L, "sim")
	[
		luabind::def("dist_to", SimDistTo)
	];

	luabind::object script_events = luabind::get_globals(L)["script_events"];
	script_events["M_SCRIPT_EVENT"] = M_SCRIPT_EVENT;
}