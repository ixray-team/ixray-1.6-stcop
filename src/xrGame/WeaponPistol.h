#pragma once
#include "weaponcustompistol.h"

class CWeaponPistol :
	public CWeaponCustomPistol
{
	typedef CWeaponCustomPistol inherited;
public:
					CWeaponPistol	();
	virtual			~CWeaponPistol	();

	virtual void	Load			(LPCSTR section);

	virtual void	UpdateSounds	();
protected:	
	virtual bool	AllowFireWhileWorking() {return true;}

	ESoundTypes			m_eSoundClose;
};
