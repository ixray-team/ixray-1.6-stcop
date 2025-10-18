#pragma once

#include "WeaponMagazined.h"
#include "../xrScripts/script_export_space.h"

class CWeaponFN2000 final : public CWeaponMagazined
{
	using inherited = CWeaponMagazined;
public:
	CWeaponFN2000() = default;
	virtual	~CWeaponFN2000() = default;

	DECLARE_SCRIPT_REGISTER_FUNCTION
};