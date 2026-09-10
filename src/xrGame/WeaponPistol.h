#pragma once
#include "WeaponMagazined.h"

class CWeaponPistol : public CWeaponMagazined
{
	using inherited = CWeaponMagazined;
public:
	CWeaponPistol() = default;
	virtual	~CWeaponPistol() = default;

	virtual void	Load			(const char* section);
	virtual void	PlayAnimHide	();

	virtual void	UpdateSounds	();
protected:	
	//virtual bool	AllowFireWhileWorking() {return true;}

	ESoundTypes	m_eSoundClose = SOUND_TYPE_WEAPON_RECHARGING;
};
