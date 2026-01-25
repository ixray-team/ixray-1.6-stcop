#include "StdAfx.h"
#include "pch_script.h"
#include "../xrGame/ai/monsters/controller/controller.h"

using namespace luabind;

#pragma optimize("s",on)
void CController::script_register(lua_State *L)
{
	module(L)
	[
		class_<CController,CGameObject>("CController")
			.def(constructor<>())
	];
}
