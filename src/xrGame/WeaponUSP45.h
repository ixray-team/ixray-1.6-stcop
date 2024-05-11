#pragma once

#include "weaponpistol.h"
#include "../xrScripts/script_export_space.h"

class CWeaponUSP45 :
	public CWeaponPistol
{
	typedef CWeaponPistol inherited;
public:
				CWeaponUSP45();
	virtual		~CWeaponUSP45();

	DECLARE_SCRIPT_REGISTER_FUNCTION
};