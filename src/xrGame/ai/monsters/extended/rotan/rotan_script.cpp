#include "stdafx.h"
#include "pch_script.h"

#include "../../tushkano/tushkano.h"

#include "rotan.h"

using namespace luabind;

#pragma optimize("s",on)
void CRotan::script_register(lua_State* L) 
{
	module(L)
	[
		class_<CRotan, CGameObject>("CRotan")
		.def(constructor<>())
	];
}
