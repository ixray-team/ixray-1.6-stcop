#pragma once

#include "WeaponCustomPistol.h"
#include "../xrScripts/script_export_space.h"

class CWeaponSVD final : public CWeaponCustomPistol
{
	using inherited = CWeaponCustomPistol;
protected:
	virtual void switch2_Fire();
	virtual void OnAnimationEnd(u8 state);
public:
	CWeaponSVD() = default;
	virtual ~CWeaponSVD() = default;

	DECLARE_SCRIPT_REGISTER_FUNCTION
};