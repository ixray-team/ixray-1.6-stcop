#include "stdafx.h"
#include "pch_script.h"
#include "AnomalyZone.h"

using namespace luabind;

#pragma optimize("s",on)
void CAnomalyZone::script_register(lua_State *L)
{
	module(L)
		[
			class_<CAnomalyZone, CGameObject>("CAnomalyZone")
			.def(constructor<>())

		];
}