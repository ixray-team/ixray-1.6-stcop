#include "pch_script.h"
#include "inventory_item_object.h"

using namespace luabind;

#pragma optimize("s",on)
void CInventoryItemObject::script_register	(lua_State *L)
{
	module(L)
	[
		class_<CInventoryItemObject,CGameObject>("CInventoryItemObject")
			.def(constructor<>())
	];
}
