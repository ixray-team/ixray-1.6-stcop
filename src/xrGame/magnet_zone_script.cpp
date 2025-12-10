#include "stdafx.h"
#include "MagnetZone.h"

using namespace luabind;

#pragma optimize("s",on)
void CMagnetZone::script_register(lua_State* L)
{
	module(L)
		[
			class_<CMagnetZone, CGameObject>("CMagnetZone")
			.def(constructor<>())
		];
}
