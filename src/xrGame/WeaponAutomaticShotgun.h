#pragma once
#include "WeaponMagazined.h"
#include "WeaponShotgun.h"
#include "../xrScripts/script_export_space.h"

XREFLECT(LuaExport, Bases="CGameObject", Constructible)
class CWeaponAutomaticShotgun :	public CWeaponMagazined
{
	typedef CWeaponMagazined inherited;
public:
					CWeaponAutomaticShotgun	();
	virtual ~CWeaponAutomaticShotgun() override;

	virtual void	Load					(const char* section) override;
	virtual void	LoadSounds				(const char* section) override;

	virtual bool	net_Spawn				(CSE_Abstract* DC) override;
	virtual void	net_Export				(NET_Packet& P) override;
	virtual void	net_Import				(NET_Packet& P) override;

	virtual void	Reload					() override;
	void			switch2_StartReload		();
	void			switch2_AddCartgidge	();
	void			switch2_EndReload		();

	shared_str		SelectOpenWeaponAnimation();
	shared_str		SelectAddCartridgeWeaponAnimation();
	shared_str		SelectCloseWeaponAnimation();

	virtual void	PlayAnimOpenWeapon		();
	virtual void	PlayAnimAddOneCartridgeWeapon();
	void			PlayAnimCloseWeapon		();

	virtual int		GetCurrentFireMode	() override { return m_aFireModes[m_iCurFireMode]; };
	virtual void	OnMotionMark(u8 state, const motion_marks&) override;

	virtual CWeaponAutomaticShotgun* cast_weapon_auto_shotgun() override { return this; }

protected:
	virtual void	OnAnimationEnd			(u8 state) override;
	void			TriStateReload			();
	virtual void	OnStateSwitch			(u8 S) override;

	ESoundTypes		m_eSoundOpen;
	ESoundTypes		m_eSoundAddCartridge;
	ESoundTypes		m_eSoundClose;

	bool bReloadEmptyByScheme = false;

};
