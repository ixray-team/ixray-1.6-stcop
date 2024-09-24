#include "stdafx.h"
#include "pch_script.h"
#include "boar.h"

using namespace luabind;

#pragma optimize("s",on)
void CustomBoar::script_register(lua_State *L)
{
	module(L)
	[
		class_<CustomBoar,CGameObject>("CAI_Boar")
			.def(constructor<>())
	];
}
