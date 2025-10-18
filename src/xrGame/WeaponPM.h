#pragma once

#include "WeaponPistol.h"
#include "../xrScripts/script_export_space.h"

class CWeaponPM final : public CWeaponPistol
{
	using inherited = CWeaponPistol;
protected:
public:
	CWeaponPM() = default;
	virtual ~CWeaponPM() = default;

	DECLARE_SCRIPT_REGISTER_FUNCTION
};