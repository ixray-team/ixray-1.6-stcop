#include "stdafx.h"
#include "pch_script.h"
#include "cat.h"

using namespace luabind;

#pragma optimize("s",on)
void CustomCat::script_register(lua_State *L)
{
	module(L)
	[
		class_<CustomCat,CGameObject>("CCat")
			.def(constructor<>())
	];
}
