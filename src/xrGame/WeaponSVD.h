#pragma once

#include "WeaponMagazined.h"
#include "../xrScripts/script_export_space.h"

class CWeaponSVD final : 
	public CWeaponMagazined
{
	using inherited = CWeaponMagazined;

protected:
	virtual void switch2_Fire() override;
	virtual void OnAnimationEnd(u8 state) override;


public:
	CWeaponSVD() = default;
	virtual ~CWeaponSVD() override = default;

	DECLARE_SCRIPT_REGISTER_FUNCTION
};