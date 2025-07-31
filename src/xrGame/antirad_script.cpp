#include "stdafx.h"
#include "pch_script.h"
#include "antirad.h"

using namespace luabind;

#pragma optimize("s",on)
void CAntirad::script_register(lua_State *L)
{
	module(L)
		[
			class_<CAntirad, CGameObject>("CAntirad")
			.def(constructor<>())
		];
}