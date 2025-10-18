#pragma once

#include "WeaponPistol.h"
#include "../xrScripts/script_export_space.h"

class CWeaponFORT final : public CWeaponPistol
{
	typedef CWeaponPistol inherited;
public:
	CWeaponFORT() = default;
	virtual	~CWeaponFORT() = default;

	DECLARE_SCRIPT_REGISTER_FUNCTION
};