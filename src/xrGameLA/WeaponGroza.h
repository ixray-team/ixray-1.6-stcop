#pragma once

#include "weaponmagazinedwgrenade.h"
#include "../xrScripts/script_export_space.h"

class CWeaponGroza :
	public CWeaponMagazinedWGrenade
{
	typedef CWeaponMagazinedWGrenade inherited;
public:
				CWeaponGroza();
	virtual		~CWeaponGroza();

	DECLARE_SCRIPT_REGISTER_FUNCTION
};
