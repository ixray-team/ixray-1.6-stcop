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
	virtual void	LoadSounds				(LPCSTR section);
	
	virtual BOOL	net_Spawn				(CSE_Abstract* DC);
	virtual void	net_Export				(NET_Packet& P);
	virtual void	net_Import				(NET_Packet& P);

	virtual void	Reload					();
	void			switch2_StartReload		();
	void			switch2_AddCartgidge	();
	void			switch2_EndReload		();

	shared_str		SelectOpenWeaponAnimation();
	shared_str		SelectAddCartridgeWeaponAnimation();
	shared_str		SelectCloseWeaponAnimation();

	virtual void	PlayAnimOpenWeapon		();
	virtual void	PlayAnimAddOneCartridgeWeapon();
	void			PlayAnimCloseWeapon		();

	virtual	int		GetCurrentFireMode	() { return m_aFireModes[m_iCurFireMode]; };
	virtual void	OnMotionMark(u32 state, const motion_marks&);


protected:
	virtual void	OnAnimationEnd			(u32 state);
	void			TriStateReload			();
	virtual void	OnStateSwitch			(u32 S);

	ESoundTypes		m_eSoundOpen;
	ESoundTypes		m_eSoundAddCartridge;
	ESoundTypes		m_eSoundClose;

	DECLARE_SCRIPT_REGISTER_FUNCTION
};
