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
	virtual std::string	NeedAddSuffix(std::string M);
	virtual void	PlayAnimShoot					();
	virtual void	PlayReloadSound					();
	virtual void	PlayAnimReload					();
	DECLARE_SCRIPT_REGISTER_FUNCTION
};
add_to_type_list(CWeaponBM16)
#undef script_type_list
#define script_type_list save_type_list(CWeaponBM16)
