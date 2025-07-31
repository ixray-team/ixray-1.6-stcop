#include "stdafx.h"
#include "pch_script.h"
#include "medkit.h"

using namespace luabind;

#pragma optimize("s",on)
void CMedkit::script_register(lua_State *L)
{
	module(L)
		[
			class_<CMedkit,CGameObject>("CMedkit")
			.def(constructor<>())
		];
}