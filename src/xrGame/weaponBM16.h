#pragma once

#include "WeaponShotgun.h"
#include "../xrScripts/script_export_space.h"

class CWeaponBM16 :public CWeaponShotgun
{
	typedef CWeaponShotgun inherited;

public:
	virtual			~CWeaponBM16					();
	virtual void	Load							(LPCSTR section);

	virtual bool	HudAnimationExist				(const shared_str& anim_name);

protected:
	virtual void	PlayAnimReload					();
	virtual void	PlayReloadSound					();
	virtual shared_str SetCurrentStateAnimation(const shared_str& first_name);
	virtual shared_str SetCurrentShootAnimation();
	DECLARE_SCRIPT_REGISTER_FUNCTION
};