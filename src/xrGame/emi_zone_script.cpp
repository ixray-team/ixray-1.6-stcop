#include "stdafx.h"
#include "EmiZone.h"

using namespace luabind;

#pragma optimize("s",on)
void CEmiZone::script_register(lua_State* L)
{
	module(L)
		[
			class_<CEmiZone, CGameObject>("CEmiZone")
			.def(constructor<>())
		];
}
