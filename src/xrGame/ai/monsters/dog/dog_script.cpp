#include "stdafx.h"
#include "pch_script.h"
#include "dog.h"

using namespace luabind;

#pragma optimize("s",on)
void CDogBase::script_register(lua_State *L)
{
	module(L)
	[
		class_<CDogBase,CGameObject>("CDogBase")
			.def(constructor<>())
	];
}
