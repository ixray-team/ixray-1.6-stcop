#include "StdAfx.h"
#include "pch_script.h"
#include "StalkerOutfit.h"
#include "ActorHelmet.h"
#include "ActorBattery.h"

using namespace luabind;

#pragma optimize("s",on)
void CStalkerOutfit::script_register(lua_State* L)
{
	module(L)
		[
			class_<CStalkerOutfit, CGameObject>("CStalkerOutfit")
			.def(constructor<>()),

		class_<CBattery, CGameObject>("CBattery")
			.def(constructor<>())
		];
}
