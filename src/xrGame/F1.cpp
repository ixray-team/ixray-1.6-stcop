#include "StdAfx.h"
#include "pch_script.h"
#include "F1.h"
#include "WeaponAmmo.h"
#include "medkit.h"
#include "antirad.h"
#include "FoodItem.h"
#include "BottleItem.h"
#include "ExplosiveItem.h"
#include "InventoryBox.h"

CF1::CF1(void) {
}

CF1::~CF1(void) {
}

using namespace luabind;

#pragma optimize("s",on)
void CF1::script_register	(lua_State *L)
{
	module(L)
	[
		class_<CF1,CGameObject>("CF1")
			.def(constructor<>()),
			//new 14.10.08 peacemaker
		class_<CExplosiveItem,CGameObject>("CExplosiveItem")
			.def(constructor<>())
	];
}
