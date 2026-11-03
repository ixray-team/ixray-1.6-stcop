#pragma once
#include "WeaponMagazined.h"

class CWeaponPistol : public CWeaponMagazined
{
	using inherited = CWeaponMagazined;
public:
	CWeaponPistol() = default;
	virtual ~CWeaponPistol() override = default;

	virtual void	Load			(const char* section) override;
	virtual void	PlayAnimHide	() override;

	virtual void	UpdateSounds	() override;
protected:	
	//virtual bool	AllowFireWhileWorking() {return true;}

	ESoundTypes	m_eSoundClose = SOUND_TYPE_WEAPON_RECHARGING;
};
