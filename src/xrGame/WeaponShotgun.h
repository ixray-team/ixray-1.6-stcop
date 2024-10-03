#pragma once

#include "weaponcustompistol.h"
#include "../xrScripts/script_export_space.h"

class CWeaponShotgun :	public CWeaponCustomPistol
{
	typedef CWeaponCustomPistol inherited;
public:
					CWeaponShotgun		() {};
	virtual			~CWeaponShotgun		() {};

	DECLARE_SCRIPT_REGISTER_FUNCTION
};