#include "stdafx.h"
#include "pch_script.h"
#include "../pseudodog/pseudodog.h"
#include "psy_dog.h"

using namespace luabind;

void CPsyDog::script_register(lua_State *L)
{
	module(L)
		[
			class_<CPsyDog,CGameObject>("CPsyDog")
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
