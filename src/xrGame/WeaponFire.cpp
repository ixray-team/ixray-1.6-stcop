// WeaponFire.cpp: implementation of the CWeapon class.
// function responsible for firing with CWeapon
//////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "Weapon.h"
#include "Entity.h"
#include "Actor.h"

#include "ActorEffector.h"
#include "EffectorShot.h"

#include "Level_Bullet_Manager.h"

#include "game_cl_mp.h"
#include "WeaponRPG7.h"
#include "ParticlesObject.h"
#include "Weapons/Components/WeaponAmmoBones.h"

#define FLAME_TIME 0.05f


float _nrand(float sigma)
{
#define ONE_OVER_SIGMA_EXP (1.0f / 0.7975f)

	if(sigma == 0) return 0;

	float y;
	do{
		y = -logf(Random.randF());
	}while(Random.randF() > expf(-_sqr(y - 1.0f)*0.5f));
	if(rand() & 0x1)	return y * sigma * ONE_OVER_SIGMA_EXP;
	else				return -y * sigma * ONE_OVER_SIGMA_EXP;
}

void random_dir(Fvector& tgt_dir, const Fvector& src_dir, float dispersion)
{
	float sigma			= dispersion/3.f;
	float alpha			= clampr		(_nrand(sigma),-dispersion,dispersion);
	float theta			= Random.randF	(0,PI);
	float r 			= tan			(alpha);
	Fvector 			U,V,T;
	Fvector::generate_orthonormal_basis	(src_dir,U,V);
	U.mul				(r*std::sin(theta));
	V.mul				(r*std::cos(theta));
	T.add				(U,V);
	tgt_dir.add			(src_dir,T).normalize();
}

float CWeapon::GetWeaponDeterioration	()
{
	return conditionDecreasePerShot;
};

void CWeapon::FireTrace		(const Fvector& P, const Fvector& D)
{
	VERIFY		(m_magazine.size());

	CCartridge &l_cartridge = m_magazine.back();
//	Msg("ammo - %s", l_cartridge.m_ammoSect.c_str());
	VERIFY		(u16(-1) != l_cartridge.bullet_material_idx);
	//-------------------------------------------------------------	
	bool is_tracer	= m_bHasTracers && !!l_cartridge.m_flags.test(CCartridge::cfTracer);
	if ( is_tracer && !IsGameTypeSingle() )
		is_tracer	= is_tracer	/*&& (m_magazine.size() % 3 == 0)*/ && !IsSilencerAttached();

	l_cartridge.m_flags.set	(CCartridge::cfTracer, is_tracer );
	if (m_u8TracerColorID != u8(-1))
		l_cartridge.param_s.u8ColorID	= m_u8TracerColorID;
	//-------------------------------------------------------------
	//повысить изношенность оружия с учетом влияния конкретного патрона
//	float Deterioration = GetWeaponDeterioration();
//	Msg("Deterioration = %f", Deterioration);

	if (!psActorFlags.test(AF_INFINITE_DURABILITY))
	{
		ChangeCondition(-GetWeaponDeterioration() * l_cartridge.param_s.impair);
	}

	float fire_disp = 0.f;
	CActor* tmp_actor = nullptr;
	if (!IsGameTypeSingle())
	{
		CObject* obj = Level().CurrentControlEntity();

		tmp_actor = obj != nullptr ? obj->cast_actor() : nullptr;
		if (tmp_actor)
		{
			CEntity::SEntityState state;
			tmp_actor->g_State(state);
			if (m_first_bullet_controller.is_bullet_first(state.fVelocity))
			{
				fire_disp = m_first_bullet_controller.get_fire_dispertion();
				m_first_bullet_controller.make_shot();
			}
		}
	}

	if (fsimilar(fire_disp, 0.f))
	{
		if (H_Parent() && (H_Parent() == tmp_actor))
		{
			fire_disp = tmp_actor->GetFireDispertion();
		}
		else
		{
			fire_disp = GetFireDispersion(true);
		}
	}
	
	bool SendHit = SendHitAllowed(H_Parent());
	//выстерлить пулю (с учетом возможной стрельбы дробью)
	for(int i = 0; i < l_cartridge.param_s.buckShot; ++i) 
	{
		FireBullet(P, D, fire_disp, l_cartridge, H_Parent()->ID(), ID(), SendHit);
	}
	
	if(m_bLightShotEnabled) 
		Light_Start			();

	
	// Ammo
	if (!infinite_fire() || m_bIAmWeaponRPG7)
	{
		m_bJustAfterReload = false;
		m_LastShotAmmoType = m_magazine.back().m_LocalAmmoType;
		m_magazine.pop_back();
		--iAmmoElapsed;

		if (m_bIsPumpEnabled)
		{
			m_bNeedPumpState = true;
			m_bHaveShell = true;
		}

		if (TLiteAmmoBones* LiteAmmoBones = GetComponent<TLiteAmmoBones>())
		{
			LiteAmmoBones->UpdateLiteAmmoBones(this, GetCurrentElapsed() + iAmmoChamberElapsed);
		}

		if (!m_bBlockUpdateAmmoBonesShooting)
		{
			u8 type_to_update = m_bUseLastAmmoType && m_LastShotAmmoType != undefined_ammo_type ? m_LastShotAmmoType : GetTargetAmmoType();

			if (TAmmoBones* AmmoBones = GetComponent<TAmmoBones>())
			{
				AmmoBones->UpdateAmmoBones(this, iAmmoElapsed, type_to_update);
			}
		}
	}

	VERIFY((u32)iAmmoElapsed == m_magazine.size());
}

void CWeapon::FireTraceChamber(const Fvector& P, const Fvector& D)
{
	VERIFY(m_chamber.size());

	CCartridge& l_cartridge = m_chamber.back();
	VERIFY(u16(-1) != l_cartridge.bullet_material_idx);
	//-------------------------------------------------------------	
	bool is_tracer = m_bHasTracers && !!l_cartridge.m_flags.test(CCartridge::cfTracer);
	if (is_tracer && !IsGameTypeSingle())
		is_tracer = is_tracer && !IsSilencerAttached();

	l_cartridge.m_flags.set(CCartridge::cfTracer, is_tracer);
	if (m_u8TracerColorID != u8(-1))
		l_cartridge.param_s.u8ColorID = m_u8TracerColorID;
	//-------------------------------------------------------------

	if (!psActorFlags.test(AF_INFINITE_DURABILITY))
	{
		ChangeCondition(-GetWeaponDeterioration() * l_cartridge.param_s.impair);
	}

	float fire_disp = 0.f;
	CActor* tmp_actor = nullptr;
	if (!IsGameTypeSingle())
	{
		CObject* obj = Level().CurrentControlEntity();

		tmp_actor = obj != nullptr ? obj->cast_actor() : nullptr;
		if (tmp_actor)
		{
			CEntity::SEntityState state;
			tmp_actor->g_State(state);
			if (m_first_bullet_controller.is_bullet_first(state.fVelocity))
			{
				fire_disp = m_first_bullet_controller.get_fire_dispertion();
				m_first_bullet_controller.make_shot();
			}
		}
	}

	if (fsimilar(fire_disp, 0.f))
	{
		if (H_Parent() && (H_Parent() == tmp_actor))
		{
			fire_disp = tmp_actor->GetFireDispertion();
		}
		else
		{
			fire_disp = GetFireDispersion(true);
		}
	}


	bool SendHit = SendHitAllowed(H_Parent());

	for (int i = 0; i < l_cartridge.param_s.buckShot; ++i)
	{
		FireBullet(P, D, fire_disp, l_cartridge, H_Parent()->ID(), ID(), SendHit);
	}

	if (m_bLightShotEnabled)
		Light_Start();

	if (!infinite_fire() || m_bIAmWeaponRPG7)
	{
		m_bJustAfterReload = false;
		m_LastShotAmmoType = m_chamber.back().m_LocalAmmoType;
		DeleteAmmoInChamber();

		if (!m_bIsPumpEnabled)
		{
			GiveAmmoFromMagToChamber();
		}
		else
		{
			m_bNeedPumpState = true;
			m_bHaveShell = true;
		}

		if (TLiteAmmoBones* LiteAmmoBones = GetComponent<TLiteAmmoBones>())
		{
			LiteAmmoBones->UpdateLiteAmmoBones(this, GetCurrentElapsed() + iAmmoChamberElapsed);
		}

		if (!m_bBlockUpdateAmmoBonesShooting)
		{
			u8 type_to_update = m_bUseLastAmmoType && m_LastShotAmmoType != undefined_ammo_type ? m_LastShotAmmoType : GetTargetAmmoType();

			if (TAmmoBones* AmmoBones = GetComponent<TAmmoBones>())
			{
				AmmoBones->UpdateAmmoBones(this, iAmmoElapsed, type_to_update);
			}
		}
	}

	VERIFY((u32)iAmmoChamberElapsed == m_chamber.size());
}

void CWeapon::StopShooting()
{
	if (!ParentIsActor() && GetState() == eFire)
	{
		SwitchState(eIdle);
	}

	u8 type_to_update = m_bUseLastAmmoType && m_LastShotAmmoType != undefined_ammo_type ? m_LastShotAmmoType : GetTargetAmmoType();

	if (TAmmoBones* AmmoBones = GetComponent<TAmmoBones>())
	{
		AmmoBones->UpdateAmmoBones(this, iAmmoElapsed, type_to_update);
	}

	StopShotEffector();
	StopPattern();

	bWorking = false;
}

void CWeapon::FireEnd() 
{
	CShootingObject::FireEnd();

}
