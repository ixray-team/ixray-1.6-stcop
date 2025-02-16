#include "stdafx.h"

#include "Entity.h"
#include "WeaponCustomPistol.h"

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////
CWeaponCustomPistol::CWeaponCustomPistol(LPCSTR name) : CWeaponMagazined(name,SOUND_TYPE_WEAPON_PISTOL)
{
}

CWeaponCustomPistol::~CWeaponCustomPistol()
{
}


	