#pragma once

#include "WeaponPistol.h"
#include "../xrScripts/script_export_space.h"

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