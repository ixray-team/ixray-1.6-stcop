#pragma once

#include "WeaponMagazined.h"
#include "../xrScripts/script_export_space.h"

class CWeaponLR300: public CWeaponMagazined
{
private:
	typedef CWeaponMagazined inherited;

public:
					CWeaponLR300		();
	virtual			~CWeaponLR300		();

	DECLARE_SCRIPT_REGISTER_FUNCTION
};