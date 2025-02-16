#ifndef __XR_WEAPON_FORT_H__
#define __XR_WEAPON_FORT_H__

#pragma once

#include "WeaponPistol.h"
#include "../xrScripts/script_export_space.h"

#define SND_RIC_COUNT 5
 
class CWeaponFORT: public CWeaponPistol
{
private:
	typedef CWeaponPistol inherited;
protected:
public:
					CWeaponFORT			();
	virtual			~CWeaponFORT		();

	DECLARE_SCRIPT_REGISTER_FUNCTION
};

#endif //__XR_WEAPON_FORT_H__