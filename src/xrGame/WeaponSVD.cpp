#include "StdAfx.h"
#include "pch_script.h"
#include "WeaponSVD.h"

#include "../xrEngine/GamepadService.h"

void CWeaponSVD::switch2_Fire	()
{
	m_bFireSingleShot			= true;
	bWorking					= false;
	SetPending					(true);
	m_iShotNum					= 0;
	m_bStopedAfterQueueFired	= false;

}

void CWeaponSVD::OnAnimationEnd(u8 state) 
{
	switch(state) 
	{
	case eFire:	{
		SetPending			(false);
		}break;	// End of reload animation
	}
	inherited::OnAnimationEnd(state);
}

using namespace luabind;

#pragma optimize("s",on)
void CWeaponSVD::script_register	(lua_State *L)
{
	module(L)
	[
		class_<CWeaponSVD,CGameObject>("CWeaponSVD")
			.def(constructor<>())
	];
}

