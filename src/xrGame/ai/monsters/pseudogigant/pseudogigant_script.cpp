#include "stdafx.h"
#include "pch_script.h"
#include "pseudogigant.h"

using namespace luabind;

#pragma optimize("s",on)
void CPseudoGiantBase::script_register(lua_State *L)
{
	module(L)
	[
		class_<CPseudoGiantBase,CGameObject>("CPseudoGiantBase")
			.def(constructor<>())
	];
}
