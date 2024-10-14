#include "stdafx.h"
#include "pch_script.h"
#include "tushkano.h"

using namespace luabind;

#pragma optimize("s",on)
void CTushkanoBase::script_register(lua_State *L)
{
	module(L)
	[
		class_<CTushkanoBase,CGameObject>("CTushkanoBase")
		.def(constructor<>())
	];
}
