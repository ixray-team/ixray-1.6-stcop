#include "stdafx.h"
#include "pch_script.h"
#include "PDA.h"

using namespace luabind;

void CPda::script_register	(lua_State *L)
{
	module(L)
	[
		class_<CPda,CGameObject>("CPda")
			.def(constructor<>())
	];
}
