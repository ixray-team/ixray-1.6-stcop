#include "StdAfx.h"
#include "pch_script.h"
#include "../xrGame/ai/monsters/fracture/fracture.h"

using namespace luabind;

#pragma optimize("s",on)
void CFracture::script_register(lua_State *L)
{
	module(L)
	[
		class_<CFracture,CGameObject>("CFracture")
			.def(constructor<>())
	];
}
