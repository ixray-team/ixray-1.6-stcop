#pragma once

#include "weaponShotgun.h"
#include "../xrScripts/script_export_space.h"

class CWeaponBM16 : public CWeaponShotgun
{
	typedef CWeaponShotgun inherited;

public:
	virtual ~CWeaponBM16();
	void Load(LPCSTR section) override;

protected:
	xr_string NeedAddSuffix(const xr_string& M) override;
	void PlayAnimShoot() override;
	void PlayReloadSound() override;
	void PlayAnimReload() override;
	DECLARE_SCRIPT_REGISTER_FUNCTION
};