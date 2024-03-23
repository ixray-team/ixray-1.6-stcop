#include "stdafx.h"
#include "pch_script.h"
#include "../../tushkano/tushkano.h" // base class
#include "beetle.h"

using namespace luabind;

#pragma optimize("s",on)
void CBeetle::script_register(lua_State *L) {
	module(L) [
			class_<CBeetle,CGameObject>("CBeetle")
			.def(constructor<>())
		];
}
