#pragma once
#include "WeaponCustomPistol.h"

class CWeaponPistol : public CWeaponCustomPistol
{
	using inherited = CWeaponCustomPistol;
public:
	CWeaponPistol() = default;
	virtual	~CWeaponPistol() = default;

	virtual void	Load			(LPCSTR section);
	virtual void	PlayAnimHide	();

	virtual void	UpdateSounds	();
protected:	
	virtual bool	AllowFireWhileWorking() {return true;}

	ESoundTypes	m_eSoundClose = SOUND_TYPE_WEAPON_RECHARGING;
};
