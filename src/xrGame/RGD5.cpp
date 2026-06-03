#include "stdafx.h"
#include "pch_script.h"
#include "RGD5.h"

using namespace luabind;

#pragma optimize("s",on)
void CRGD5::script_register	(lua_State *L)
{
	module(L)
	[
		class_<CRGD5,CGameObject>("CRGD5")
			.def(constructor<>())
	];
}
