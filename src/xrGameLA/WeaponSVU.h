#pragma once

#include "weaponcustompistol.h"
#include "../xrScripts/script_export_space.h"

class CWeaponSVU :
	public CWeaponCustomPistol
{
	typedef CWeaponCustomPistol inherited;
public:
	CWeaponSVU(void);
	virtual ~CWeaponSVU(void);

	DECLARE_SCRIPT_REGISTER_FUNCTION
};
