#include "stdafx.h"
#include "pch_script.h"
#include "flesh.h"

using namespace luabind;

#pragma optimize("s",on)
void CFleshBase::script_register(lua_State *L)
{
	module(L)
	[
		class_<CFleshBase,CGameObject>("CFleshBase")
			.def(constructor<>())
	];
}
