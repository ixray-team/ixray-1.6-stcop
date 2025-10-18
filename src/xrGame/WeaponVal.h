#pragma once

#include "WeaponMagazined.h"
#include "../xrScripts/script_export_space.h"

class CWeaponVal final : public CWeaponMagazined
{
	using inherited = CWeaponMagazined;
public:
	CWeaponVal() = default;
	virtual ~CWeaponVal() = default;

	DECLARE_SCRIPT_REGISTER_FUNCTION
};