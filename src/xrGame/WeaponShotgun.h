#pragma once

#include "weaponcustompistol.h"
#include "../xrScripts/script_export_space.h"

class CWeaponShotgun :	public CWeaponCustomPistol
{
	typedef CWeaponCustomPistol inherited;
public:
					CWeaponShotgun		();
	virtual			~CWeaponShotgun		();

	virtual void	Load				(LPCSTR section);
	
	virtual void	net_Destroy			();

	virtual bool	TryReload			();
	virtual void	switch2_Fire		();
	void			switch2_StartReload ();
	void			switch2_AddCartgidge();
	void			switch2_EndReload	();

	virtual void	PlayAnimOpenWeapon	();
	virtual void	PlayAnimAddOneCartridgeWeapon();
	void			PlayAnimCloseWeapon	();

	bool			m_bAddCartridgeOpen;
	bool			m_bEmptyPreloadMode;
	bool			bPreloadAnimAdapter;

protected:
	virtual void	OnAnimationEnd		(u32 state);
	bool			TriStateReload		();
	virtual void	OnStateSwitch		(u32 S);

	ESoundTypes		m_eSoundOpen;
	ESoundTypes		m_eSoundAddCartridge;
	ESoundTypes		m_eSoundClose;

	DECLARE_SCRIPT_REGISTER_FUNCTION
};