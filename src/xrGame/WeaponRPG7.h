#pragma once

#include "WeaponPistol.h"
#include "RocketLauncher.h"
#include "../xrScripts/script_export_space.h"

class CWeaponRPG7 final : public CWeaponCustomPistol,
	public CRocketLauncher
{
	using inherited = CWeaponCustomPistol;
public:
	CWeaponRPG7();
	virtual	~CWeaponRPG7() = default;

	virtual BOOL net_Spawn		(CSE_Abstract* DC);
	virtual void OnStateSwitch	(u32 S);
	virtual void OnEvent		(NET_Packet& P, u16 type);
	virtual void ReloadMagazine	();
	virtual void Load			(LPCSTR section);
	virtual	void FireTrace		(const Fvector& P, const Fvector& D);
	virtual void on_a_hud_attach();

	virtual void FireStart		();
	virtual void SwitchState	(u32 S);

			void UpdateMissileVisibility	();
	virtual void UnloadMagazine				(bool spawn_ammo = true);

	virtual void net_Import			( NET_Packet& P);				// import from server

	virtual CWeaponRPG7* cast_weapon_rpg7() { return this; }
	virtual CRocketLauncher* cast_rocket_launcher() override { return this; }

	bool CheckRLMisfireRocket();

protected:
	virtual bool	AllowBore		();

	struct
	{
		float start_tr = 0.0f;
		float end_tr = 0.0f;
		float start_prob = 0.0f;
		float end_prob = 0.0f;
	} m_rocket_explode_params;

	shared_str	m_sRocketSection;

	DECLARE_SCRIPT_REGISTER_FUNCTION
};