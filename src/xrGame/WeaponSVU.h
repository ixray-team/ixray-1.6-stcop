#pragma once

#include "WeaponCustomPistol.h"
#include "../xrScripts/script_export_space.h"

class CWeaponSVU final : public CWeaponCustomPistol
{
	using inherited = CWeaponCustomPistol;
public:
	CWeaponSVU() = default;
	virtual ~CWeaponSVU() = default;

	DECLARE_SCRIPT_REGISTER_FUNCTION
};