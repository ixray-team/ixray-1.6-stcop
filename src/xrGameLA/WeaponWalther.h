#pragma once

#include "weaponpistol.h"
#include "../xrScripts/script_export_space.h"

class CWeaponWalther :
	public CWeaponPistol
{
	typedef CWeaponPistol inherited;
public:
	CWeaponWalther(void);
	virtual ~CWeaponWalther(void);

	DECLARE_SCRIPT_REGISTER_FUNCTION
};
