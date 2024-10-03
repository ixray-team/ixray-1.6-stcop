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
	BOOL			net_Spawn				(CSE_Abstract* DC) override;
	void			Load					(LPCSTR section) override;
	void			OnEvent					(NET_Packet& P, u16 type) override;
protected:
	void			FireStart				() override;
	u8				AddCartridge			(u8 cnt) override;
	void			UnloadMagazine			(bool spawn_ammo = true) override;
	void			ReloadMagazine			() override;
	void			FireTrace				(const Fvector& P, const Fvector& D) override;

	DECLARE_SCRIPT_REGISTER_FUNCTION
};