#include "stdafx.h"
#include "pch_script.h"
#include "pseudodog.h"

using namespace luabind;

#pragma optimize("s",on)
void CPseudoDogBase::script_register(lua_State *L)
{
	module(L)
	[
		class_<CPseudoDogBase,CGameObject>("CPseudoDogBase")
			.def(constructor<>())
	];
}
