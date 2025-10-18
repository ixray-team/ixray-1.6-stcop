#pragma once

#include "WeaponPistol.h"
#include "../xrScripts/script_export_space.h"

class CWeaponWalther final : public CWeaponPistol
{
	using inherited = CWeaponPistol;
public:
	CWeaponWalther() = default;
	virtual ~CWeaponWalther() = default;

	DECLARE_SCRIPT_REGISTER_FUNCTION
};