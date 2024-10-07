#include "stdafx.h"
#include "pch_script.h"
#include "burer.h"

using namespace luabind;

#pragma optimize("s",on)
void CBurerBase::script_register(lua_State *L)
{
	module(L)
	[
		class_<CBurerBase,CGameObject>("CBurerBase")
			.def(constructor<>())
	];
}
