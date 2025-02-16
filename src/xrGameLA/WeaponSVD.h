#pragma once

#include "weaponcustompistol.h"
#include "../xrScripts/script_export_space.h"

class CWeaponSVD :
	public CWeaponCustomPistol
{
	typedef CWeaponCustomPistol inherited;
protected:
	virtual void switch2_Fire	();
	virtual void OnAnimationEnd (u32 state);
public:
	CWeaponSVD(void);
	virtual ~CWeaponSVD(void);

	DECLARE_SCRIPT_REGISTER_FUNCTION
};
