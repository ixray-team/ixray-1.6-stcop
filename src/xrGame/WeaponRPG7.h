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

	virtual bool net_Spawn		(CSE_Abstract* DC);
	virtual void OnStateSwitch	(u8 S);
	virtual void OnEvent		(NET_Packet& P, u16 type);
	virtual void ReloadMagazine	();
	virtual void Load			(const char* section);
	virtual	void FireTrace		(const Fvector& P, const Fvector& D);
	virtual void on_a_hud_attach();

	virtual void FireStart		();

			void UpdateMissileVisibility	();
	virtual void UnloadMagazine				(bool spawn_ammo = true);

	virtual void net_Import			( NET_Packet& P);				// import from server

	virtual CWeaponRPG7* cast_weapon_rpg7() { return this; }
	virtual CRocketLauncher* cast_rocket_launcher() override { return this; }

	bool CheckRLMisfireRocket();
	void ReactiveHit();

protected:
	shared_str	m_sGrenadeBoneName;
	shared_str	m_sHudGrenadeBoneName;
	virtual bool	AllowBore		();

	struct
	{
		float start_tr = 0.0f;
		float end_tr = 0.0f;
		float start_prob = 0.0f;
		float end_prob = 0.0f;
	} m_rocket_explode_params;

	struct
	{
		float dist = 0.0f;
		float power = 0.0f;
		float impulse = 0.0f;
		int buck = 1;
		int reverse_buck = 1;
		float buck_disp = 1.0f;
		float reverse_disp = 0.1f;
		float reverse_disp2 = 0.1f;
		float reverse_power = 0.0f;
		ALife::EHitType type = ALife::eHitTypeExplosion;
		float reverse_k = 1.0f;
		const char* bullet_material = "default";
	} m_reactive_hit_params;

	shared_str	m_sRocketSection;

	DECLARE_SCRIPT_REGISTER_FUNCTION
};