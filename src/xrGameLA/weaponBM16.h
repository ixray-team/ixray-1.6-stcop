#pragma once

#include "weaponShotgun.h"
#include "../xrScripts/script_export_space.h"

class CWeaponBM16 :public CWeaponShotgun
{
	typedef CWeaponShotgun inherited;

public:
	virtual			~CWeaponBM16					();
	virtual void	Load							(LPCSTR section);

protected:
	virtual void	PlayAnimShoot					();
	virtual void	PlayAnimReload					();
	virtual void	PlayReloadSound					();
	virtual void	PlayAnimIdle					();
	virtual void	PlayAnimIdleMoving				();
	virtual void	PlayAnimIdleSprint				();
	virtual void	PlayAnimShow					();
	virtual void	PlayAnimHide					();
	virtual void	PlayAnimBore					();
	DECLARE_SCRIPT_REGISTER_FUNCTION
};
