#include "stdafx.h"
#include "pch_script.h"

#include "../../tushkano/tushkano.h"

#include "frog.h"

using namespace luabind;

#pragma optimize("s",on)
void CFrog::script_register(lua_State* L) 
{
	module(L) 
	[
		class_<CFrog, CGameObject>("CFrog")
		.def(constructor<>())
	];
}
