#pragma once

#include "weaponmagazined.h"
#include "../xrScripts/script_export_space.h"

class CWeaponVintorez :
	public CWeaponMagazined
{
	typedef CWeaponMagazined inherited;
public:
	CWeaponVintorez(void);
	virtual ~CWeaponVintorez(void);

	DECLARE_SCRIPT_REGISTER_FUNCTION
};
	