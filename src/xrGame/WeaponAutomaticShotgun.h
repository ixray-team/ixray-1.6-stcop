#pragma once
#include "WeaponMagazined.h"
#include "WeaponShotgun.h"
#include "../xrScripts/script_export_space.h"

class CWeaponAutomaticShotgun :	public CWeaponMagazined
{
	typedef CWeaponMagazined inherited;
public:
					CWeaponAutomaticShotgun	();
	virtual			~CWeaponAutomaticShotgun();

	virtual void	Load					(LPCSTR section);
	
	virtual void	net_Export				(NET_Packet& P);
	virtual void	net_Import				(NET_Packet& P);

	virtual void	Reload					();
	void			switch2_StartReload		();
	void			switch2_AddCartgidge	();
	void			switch2_EndReload		();

	virtual void	PlayAnimOpenWeapon		();
	virtual void	PlayAnimAddOneCartridgeWeapon();
	void			PlayAnimCloseWeapon		();

	virtual	int		GetCurrentFireMode	() { return m_aFireModes[m_iCurFireMode]; };

	bool			m_bAddCartridgeOpen;
	bool			m_bEmptyPreloadMode;
	bool			bPreloadAnimAdapter;

protected:
	virtual void	OnAnimationEnd			(u32 state);
	void			TriStateReload			();
	virtual void	OnStateSwitch			(u32 S);

	ESoundTypes		m_eSoundOpen;
	ESoundTypes		m_eSoundAddCartridge;
	ESoundTypes		m_eSoundClose;

	DECLARE_SCRIPT_REGISTER_FUNCTION
};
