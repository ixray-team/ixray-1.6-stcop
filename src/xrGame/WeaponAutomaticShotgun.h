#pragma once
#include "WeaponMagazined.h"
#include "../xrScripts/script_export_space.h"

class CWeaponAutomaticShotgun :	public CWeaponMagazined
{
	typedef CWeaponMagazined inherited;
public:
					CWeaponAutomaticShotgun	() {};
	virtual			~CWeaponAutomaticShotgun() {};

	DECLARE_SCRIPT_REGISTER_FUNCTION
};
