#include "pch_script.h"
#include "Weaponzoomable.h"

using namespace luabind;

void CWeaponZoomable::script_register	(lua_State *L)
{
	module(L)
	[
		class_<CWeaponZoomable,CGameObject>("CWeaponZoomable")
			.def(constructor<>())
	];
}
