#include "stdafx.h"
#include "GameObject.h"
#include "ai_object_location.h"
#include "../../xrServerEntities/xrMessages.h"
#include "../../xrServerEntities/xrServer_Objects_ALife.h"

#include <luabind/luabind.hpp>

float SimDistTo(luabind::object Left, luabind::object Right)
{
	CGameObject* ClientObject1 = luabind::object_cast_nothrow<CGameObject*>(Left).value_or(nullptr);
	CGameObject* ClientObject2 = luabind::object_cast_nothrow<CGameObject*>(Right).value_or(nullptr);

	CSE_ALifeObject* ServerObject1 = ClientObject1 != nullptr ? nullptr : luabind::object_cast_nothrow<CSE_ALifeObject*>(Left).value_or(nullptr);
	CSE_ALifeObject* ServerObject2 = ClientObject2 != nullptr ? nullptr : luabind::object_cast_nothrow<CSE_ALifeObject*>(Right).value_or(nullptr);

	if ((ServerObject1 == nullptr && ClientObject1 == nullptr) || (ServerObject2 == nullptr && ClientObject2 == nullptr))
	{
		return FLT_MAX;
	}

	u32 VertexIDLeft = u32(-1);
	u32 VertexIDRight = u32(-1);
	
	if (ClientObject1 != nullptr)
	{
		VertexIDLeft = ClientObject1->ai_location().game_vertex_id();
	}
	else if (ServerObject1 != nullptr)
	{
		const auto& cross_table = ai().cross_table();
		const u32 level_vertex_id = ServerObject1->m_tNodeID;
		if (level_vertex_id >= cross_table.header().level_vertex_count())
		{
			return FLT_MAX;
		}

		VertexIDLeft = cross_table.vertex(level_vertex_id).game_vertex_id();
	}

	if (ClientObject2 != nullptr)
	{
		VertexIDRight = ClientObject2->ai_location().game_vertex_id();
	}
	else if (ServerObject2 != nullptr)
	{
		const auto& cross_table = ai().cross_table();
		const u32 level_vertex_id = ServerObject2->m_tNodeID;
		if (level_vertex_id >= cross_table.header().level_vertex_count())
		{
			return FLT_MAX;
		}

		VertexIDRight = cross_table.vertex(level_vertex_id).game_vertex_id();
	}

	if (!ai().game_graph().valid_vertex_id(VertexIDLeft) || !ai().game_graph().valid_vertex_id(VertexIDRight))
	{
		return FLT_MAX;
	}

	const CGameGraph::CVertex* v1 = ai().game_graph().vertex(VertexIDLeft);
	const CGameGraph::CVertex* v2 = ai().game_graph().vertex(VertexIDRight);

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