#pragma once

#include "WeaponMagazined.h"
#include "../xrScripts/script_export_space.h"

class CWeaponFN2000: public CWeaponMagazined
{
private:
	typedef CWeaponMagazined inherited;
public:
					CWeaponFN2000	();
	virtual			~CWeaponFN2000	();
	DECLARE_SCRIPT_REGISTER_FUNCTION
};