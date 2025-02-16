#include "pch_script.h"
#include "WeaponPM.h"

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////
CWeaponPM::CWeaponPM() : CWeaponPistol("PM")
{}

CWeaponPM::~CWeaponPM()
{}

using namespace luabind;

#pragma optimize("s",on)
void CWeaponPM::script_register	(lua_State *L)
{
	module(L)
	[
		class_<CWeaponPM,CGameObject>("CWeaponPM")
			.def(constructor<>())
	];
}
