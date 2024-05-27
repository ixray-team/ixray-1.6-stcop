#pragma once

#include "WeaponPistol.h"
#include "../xrScripts/script_export_space.h"

class CWeaponPM: public CWeaponPistol
{
private:
	typedef CWeaponPistol inherited;
protected:
public:
					CWeaponPM			();
	virtual			~CWeaponPM		();

	DECLARE_SCRIPT_REGISTER_FUNCTION
};