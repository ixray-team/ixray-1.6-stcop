#pragma once

#include "WeaponPistol.h"
#include "../xrScripts/script_export_space.h"

class CWeaponHPSA final : public CWeaponPistol
{
private:
	using inherited = CWeaponPistol;
protected:
public:
	CWeaponHPSA() = default;
	virtual ~CWeaponHPSA() = default;

	DECLARE_SCRIPT_REGISTER_FUNCTION
};