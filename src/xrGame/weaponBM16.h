#pragma once

#include "WeaponShotgun.h"
#include "../xrScripts/script_export_space.h"

class CWeaponBM16 final : public CWeaponShotgun
{
	using inherited = CWeaponShotgun;

public:
	CWeaponBM16() = default;
	virtual	~CWeaponBM16() = default;

	virtual void	Load							(LPCSTR section);
	virtual void	LoadSounds						(LPCSTR section);

	virtual bool	HudAnimationExist				(const shared_str& anim_name);

protected:
	virtual void	PlayAnimReload					();
	virtual void	PlayReloadSound					();
	virtual shared_str SetCurrentStateAnimation(const shared_str& first_name);
	virtual shared_str SetCurrentShootAnimation();
	virtual shared_str SetCurrentReloadAnimation();

	virtual CWeaponBM16* cast_weapon_bm16() { return this; }

	virtual int GetMagCapacity() override { int size = m_iAmmoCountToReload; m_iAmmoCountToReload = iMagazineSize; return size; }

	bool m_bUseAltReloadSystem = false;
	int m_iAmmoCountToReload = 2;

	DECLARE_SCRIPT_REGISTER_FUNCTION
};