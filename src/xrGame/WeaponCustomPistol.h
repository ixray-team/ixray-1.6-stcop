#pragma once

#include "WeaponMagazined.h"

class CWeaponCustomPistol : public CWeaponMagazined
{
	using inherited = CWeaponMagazined;
public:
	CWeaponCustomPistol() = default;
	virtual ~CWeaponCustomPistol() = default;

	virtual	int		GetCurrentFireMode	() { return 1; };
protected:
	virtual void	FireEnd				();
	virtual void	switch2_Fire		();
};
