#include "stdafx.h"
#include "pch_script.h"
#include "controller.h"

using namespace luabind;

#pragma optimize("s",on)
void CControllerBase::script_register(lua_State *L)
{
	module(L)
	[
		class_<CControllerBase,CGameObject>("CControllerBase")
			.def(constructor<>())
	];
}
