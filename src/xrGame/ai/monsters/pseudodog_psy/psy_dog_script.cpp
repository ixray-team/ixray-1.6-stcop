#include "stdafx.h"
#include "pch_script.h"
#include "../pseudodog/pseudodog.h"
#include "psy_dog.h"

using namespace luabind;

void CPseudoPsyDogBase::script_register(lua_State *L)
{
	module(L)
		[
			class_<CPseudoPsyDogBase,CGameObject>("CPseudoPsyDogBase")
			.def(constructor<>())
		];
}

void CPsyDogPhantom::script_register(lua_State *L)
{
	module(L)
		[
			class_<CPsyDogPhantom,CGameObject>("CPsyDogPhantom")
			.def(constructor<>())
		];
}
