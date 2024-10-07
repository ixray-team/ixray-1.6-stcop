#include "stdafx.h"
#include "pch_script.h"
#include "boar.h"

using namespace luabind;

#pragma optimize("s",on)
void CBoarBase::script_register(lua_State *L)
{
	module(L)
	[
		class_<CBoarBase,CGameObject>("CBoarBase")
			.def(constructor<>())
	];
}
