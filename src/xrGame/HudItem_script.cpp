#include "stdafx.h"
#include "pch_script.h"
#include "HudItem.h"

using namespace luabind;

#pragma optimize("s",on)
void CHudItem::script_register(lua_State *L)
{
	module(L)
		[
			class_<CHudItem>("CHudItem")
			//.def(constructor<>())
			.def("HudAnimationExist", &CHudItem::HudAnimationExist)
			.def("animation_slot", &CHudItem::animation_slot)
		];
}