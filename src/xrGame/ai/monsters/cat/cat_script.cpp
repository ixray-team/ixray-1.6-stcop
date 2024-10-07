#include "stdafx.h"
#include "pch_script.h"
#include "cat.h"

using namespace luabind;

#pragma optimize("s",on)
void CCatBase::script_register(lua_State *L)
{
	module(L)
	[
		class_<CCatBase,CGameObject>("CCatBase")
			.def(constructor<>())
	];
}
