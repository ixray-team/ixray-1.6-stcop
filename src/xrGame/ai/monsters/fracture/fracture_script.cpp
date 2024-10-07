#include "stdafx.h"
#include "pch_script.h"
#include "fracture.h"

using namespace luabind;

#pragma optimize("s",on)
void CFractureBase::script_register(lua_State *L)
{
	module(L)
	[
		class_<CFractureBase,CGameObject>("CFractureBase")
			.def(constructor<>())
	];
}
