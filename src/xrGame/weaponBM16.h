#pragma once

#include "WeaponAutomaticShotgun.h"
#include "../xrScripts/script_export_space.h"

XREFLECT(LuaExport, Constructible)
class CWeaponBM16 final : public CWeaponAutomaticShotgun
{
	using inherited = CWeaponAutomaticShotgun;

public:
	CWeaponBM16() = default;
	virtual ~CWeaponBM16() override = default;

	virtual void	Load							(const char* section) override;
	virtual void	LoadSounds						(const char* section) override;

	virtual bool	HudAnimationExist				(const shared_str& anim_name, bool only_for_actor = true) override;

protected:
	virtual void	PlayAnimReload					() override;
	virtual void	PlayReloadSound					() override;
	virtual shared_str SetCurrentStateAnimation(const shared_str& first_name) override;
	virtual shared_str SetCurrentShootAnimation() override;
	virtual shared_str SetCurrentReloadAnimation() override;
	virtual shared_str SetCurrentAimAnimation() override;

	virtual CWeaponBM16* cast_weapon_bm16() override { return this; }
	virtual CWeaponAutomaticShotgun* cast_weapon_auto_shotgun() override { return this; }

	virtual int GetMagCapacity() override { int size = m_iAmmoCountToReload; m_iAmmoCountToReload = iMagazineSize; return size; }

	bool m_bUseAltReloadSystem = false;
	int m_iAmmoCountToReload = 2;

};