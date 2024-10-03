#pragma once

#include "WeaponCustomPistol.h"
#include "../xrScripts/script_export_space.h"

class CWeaponSVD :
	public CWeaponCustomPistol
{
	typedef CWeaponCustomPistol inherited;
protected:
	void switch2_Fire() override;
public:
	CWeaponSVD(void);
	virtual ~CWeaponSVD(void);

	DECLARE_SCRIPT_REGISTER_FUNCTION
};