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

	virtual void	OnShot			();
	virtual void	net_Destroy		();
	virtual void	OnH_B_Chield	();

	virtual void	UpdateSounds	();
protected:	
	virtual bool	AllowFireWhileWorking() {return true;}

	ESoundTypes			m_eSoundClose;
};
