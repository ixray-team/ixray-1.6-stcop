#pragma once

#include "rocketlauncher.h"
#include "weaponShotgun.h"
#include "../xrScripts/script_export_space.h"

class CWeaponRG6 :  public CRocketLauncher,
					public CWeaponShotgun
{
	typedef CRocketLauncher		inheritedRL;
	typedef CWeaponShotgun		inheritedSG;
	
public:
	virtual			~CWeaponRG6				();
	virtual BOOL	net_Spawn				(CSE_Abstract* DC);
	virtual void	Load					(LPCSTR section);
	virtual void	OnEvent					(NET_Packet& P, u16 type);
protected:
	virtual void	FireStart				();
	virtual u8		AddCartridge			(u8 cnt);
	virtual void	UnloadMagazine			(bool spawn_ammo = true);
	virtual void	ReloadMagazine			();
	virtual void	FireTrace				(const Fvector& P, const Fvector& D);

	DECLARE_SCRIPT_REGISTER_FUNCTION
};