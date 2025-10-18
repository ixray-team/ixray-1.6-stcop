#pragma once

#include "WeaponMagazined.h"
#include "../xrScripts/script_export_space.h"

class CWeaponLR300 final : public CWeaponMagazined
{
private:
	using inherited = CWeaponMagazined;

public:
	CWeaponLR300() = default;
	virtual ~CWeaponLR300() = default;

	DECLARE_SCRIPT_REGISTER_FUNCTION
};