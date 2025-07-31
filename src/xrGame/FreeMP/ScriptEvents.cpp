#include "stdafx.h"
#include "pch_script.h"
#include "ScriptEvents.h"

using namespace luabind;

#pragma optimize("s",on)
void ScriptEvent::script_register(lua_State* L)
{
	module(L)
	[
		class_<ScriptEvent>("script_event")
			.def(constructor<>())
			.def_readwrite("SenderID", &ScriptEvent::SenderID)
			.def_readwrite("Packet", &ScriptEvent::Packet)
	];
}