#ifndef __XR_WEAPON_PM_H__
#define __XR_WEAPON_PM_H__

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

#endif //__XR_WEAPON_PM_H__
