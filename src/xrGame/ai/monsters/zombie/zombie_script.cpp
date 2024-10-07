#include "stdafx.h"
#include "pch_script.h"
#include "zombie.h"

using namespace luabind;

#pragma optimize("s",on)
void CZombieBase::script_register(lua_State *L)
{
	module(L)
	[
		class_<CZombieBase,CGameObject>("CZombieBase")
			.def(constructor<>())
	];
}
