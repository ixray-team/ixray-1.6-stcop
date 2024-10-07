#include "stdafx.h"
#include "pch_script.h"
#include "snork.h"

using namespace luabind;

#pragma optimize("s",on)
void CSnorkBase::script_register(lua_State *L)
{
	module(L)
	[
		class_<CSnorkBase,CGameObject>("CSnorkBase")
			.def(constructor<>())
	];
}
