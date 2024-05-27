#pragma once

#include "WeaponPistol.h"
#include "../xrScripts/script_export_space.h"
 
class CWeaponHPSA: public CWeaponPistol
{
private:
	typedef CWeaponPistol inherited;
protected:
public:
					CWeaponHPSA			();
	virtual			~CWeaponHPSA		();

	DECLARE_SCRIPT_REGISTER_FUNCTION
};