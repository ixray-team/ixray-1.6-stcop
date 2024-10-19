#include "stdafx.h"
#include "pch_script.h"
#include "../pseudodog/pseudodog.h"
#include "pseudodog_psy.h"

using namespace luabind;

void CPseudoPsyDogBase::script_register(lua_State *L)
{
	module(L)
	[
		class_<CPseudoPsyDogBase,CGameObject>("CPseudoPsyDogBase")
		.def(constructor<>())
	];
}
