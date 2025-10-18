#pragma once

#include "WeaponPistol.h"
#include "../xrScripts/script_export_space.h"

class CWeaponUSP45 final : public CWeaponPistol
{
	using inherited = CWeaponPistol;
public:
	CWeaponUSP45() = default;
	virtual	~CWeaponUSP45() = default;

	DECLARE_SCRIPT_REGISTER_FUNCTION
};