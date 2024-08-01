#include "stdafx.h"
#include "pch_script.h"
#include "pseudogigant.h"

using namespace luabind;

#pragma optimize("s",on)
void CPseudogigant::script_register(lua_State *L)
{
	module(L)
	[
		class_<CPseudogigant,CGameObject>("CPseudogigant")
			.def(constructor<>())
	];
}
