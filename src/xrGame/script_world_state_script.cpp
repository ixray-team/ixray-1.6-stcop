////////////////////////////////////////////////////////////////////////////
//	Module 		: script_world_state_script.cpp
//	Created 	: 19.03.2004
//  Modified 	: 19.03.2004
//	Author		: Dmitriy Iassenev
//	Description : Script world state script export
////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "pch_script.h"
#include "script_world_state.h"
#include "condition_state.h"

using namespace luabind;

#pragma optimize("s",on)
void CScriptWorldStateWrapper::script_register(lua_State *L)
{
	module(L)
	[
		class_<CWorldState>("world_state")
			.def(								constructor<>())
			.def(								constructor<CWorldState>())
			.def("add_property",				(void (CWorldState::*)(const CWorldProperty&))(&CWorldState::add_condition))
			.def("remove_property",				(void (CWorldState::*)(const u32&))(&CWorldState::remove_condition))
			.def("clear",						&CWorldState::clear)
			.def("includes",					&CWorldState::includes)
			.def("property",					&CWorldState::property)
			.def(const_self < CWorldState())
			.def(const_self == CWorldState())
	];
}
