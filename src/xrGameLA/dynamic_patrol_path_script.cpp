#include "pch_script.h"
#include "dynamic_patrol_path.h"
#include "dynamic_patrol_point.h"

using namespace luabind;

#pragma optimize("s", on)
void CDynamicPatrolPath::script_register(lua_State *L)
{
	module(L)
	[
		class_<CDynamicPatrolPath>("DynamicPatrolPath")
			.def(						constructor<LPCSTR>())
			.def("add_point",			&CDynamicPatrolPath::AddPoint)
			.def("add_link",			&CDynamicPatrolPath::AddLink),
		class_<CDynamicPatrolPoint>("DynamicPatrolPoint")
			.def(						constructor<CPatrolPath*, CDynamicPatrolPoint*>())
			.def(						constructor<>())
			.def("set_name",			&CDynamicPatrolPoint::set_name)
			.def("set_position",		&CDynamicPatrolPoint::set_position)
			.def("set_level_vertex_id", &CDynamicPatrolPoint::set_level_vertex_id)
			.def("set_game_vertex_id",	&CDynamicPatrolPoint::set_game_vertex_id)
			.def("set_flags",			&CDynamicPatrolPoint::set_flags)
	];	
}