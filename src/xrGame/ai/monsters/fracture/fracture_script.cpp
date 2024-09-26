#include "stdafx.h"
#include "pch_script.h"
#include "fracture.h"

using namespace luabind;

#pragma optimize("s",on)
void CustomFracture::script_register(lua_State *L)
{
	module(L)
	[
		class_<CustomFracture,CGameObject>("CFracture")
			.def(constructor<>())
	];
}
