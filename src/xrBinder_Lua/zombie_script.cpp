#include "StdAfx.h"
#include "pch_script.h"
#include "../xrGame/ai/monsters/zombie/zombie.h"

using namespace luabind;

#pragma optimize("s",on)
void CZombie::script_register(lua_State *L)
{
	module(L)
	[
		class_<CZombie,CGameObject>("CZombie")
			.def(constructor<>())
	];
}
