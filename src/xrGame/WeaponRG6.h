#pragma once

#include "RocketLauncher.h"
#include "WeaponShotgun.h"
#include "../xrScripts/script_export_space.h"

class CWeaponRG6 :  public CRocketLauncher,
					public CWeaponShotgun
{
	typedef CRocketLauncher		inheritedRL;
	typedef CWeaponShotgun		inheritedSG;
	
public:
	virtual			~CWeaponRG6				();
	virtual BOOL	net_Spawn				(CSE_Abstract* DC) override;
	virtual void	Load					(LPCSTR section) override;
	virtual void	OnEvent					(NET_Packet& P, u16 type) override;
protected:
	virtual u8		AddCartridge			(u8 cnt) override;
	virtual void	UnloadMagazine			(bool spawn_ammo = true) override;
	virtual void	ReloadMagazine			() override;
	virtual void	FireTrace				(const Fvector& P, const Fvector& D) override;

	DECLARE_SCRIPT_REGISTER_FUNCTION
};