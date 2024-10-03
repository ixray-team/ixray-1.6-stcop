#pragma once

#include "WeaponPistol.h"
#include "RocketLauncher.h"
#include "../xrScripts/script_export_space.h"

class CWeaponRPG7 :	public CWeaponCustomPistol,
					public CRocketLauncher
{
private:
	typedef CWeaponCustomPistol inherited;
public:
			 CWeaponRPG7		();
	virtual	 ~CWeaponRPG7		();

	BOOL net_Spawn				(CSE_Abstract* DC) override;
	void OnStateSwitch			(u32 S) override;
	void OnEvent				(NET_Packet& P, u16 type) override;
	void ReloadMagazine			() override;
	void Load					(LPCSTR section) override;
	void FireTrace				(const Fvector& P, const Fvector& D) override;
	void on_a_hud_attach		() override;
	
	void FireStart				() override;
	void SwitchState			(u32 S) override;
	
	void UpdateMissileVisibility	();
	void UnloadMagazine				(bool spawn_ammo = true) override;
	void net_Import					(NET_Packet& P) override;				// import from server
protected:
	const bool AllowBore() const override;

	shared_str	m_sRocketSection;

	DECLARE_SCRIPT_REGISTER_FUNCTION
};