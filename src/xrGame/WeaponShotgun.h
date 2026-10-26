#pragma once

#include "WeaponMagazined.h"
#include "../xrScripts/script_export_space.h"

class CWeaponShotgun : public CWeaponMagazined
{
	using inherited = CWeaponMagazined;
public:
	CWeaponShotgun();
	virtual ~CWeaponShotgun() override = default;

	virtual void	Load				(const char* section) override;
	virtual void	LoadSounds			(const char* section) override;

	virtual bool	net_Spawn			(CSE_Abstract* DC) override;
	virtual void	net_Destroy			() override;
	virtual void	net_Export			(NET_Packet& P) override;
	virtual void	net_Import			(NET_Packet& P) override;

	virtual void	Reload				() override;
	virtual void	switch2_Fire		() override;
	void			switch2_StartReload ();
	void			switch2_AddCartgidge();
	void			switch2_EndReload	();

	shared_str		SelectOpenWeaponAnimation();
	shared_str		SelectAddCartridgeWeaponAnimation();
	shared_str		SelectCloseWeaponAnimation();

	virtual void	PlayAnimOpenWeapon	();
	virtual void	PlayAnimAddOneCartridgeWeapon();
	void			PlayAnimCloseWeapon	();

	virtual void	OnMotionMark(u8 state, const motion_marks&) override;

	virtual CWeaponShotgun* cast_weapon_shotgun() override { return this; }

protected:
	virtual void	OnAnimationEnd		(u8 state) override;
	void			TriStateReload		();
	virtual void	OnStateSwitch		(u8 S) override;

	ESoundTypes		m_eSoundOpen;
	ESoundTypes		m_eSoundAddCartridge;
	ESoundTypes		m_eSoundClose;

	bool bReloadEmptyByScheme = false;

	DECLARE_SCRIPT_REGISTER_FUNCTION
};