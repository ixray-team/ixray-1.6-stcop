#include "stdafx.h"
#include "pch_script.h"
#include "poltergeist.h"

using namespace luabind;

#pragma optimize("s",on)
void CPoltergeistBase::script_register(lua_State *L)
{
	module(L)
	[
		class_<CPoltergeistBase,CGameObject>("CPoltergeistBase")
			.def(constructor<>())
	];
}
