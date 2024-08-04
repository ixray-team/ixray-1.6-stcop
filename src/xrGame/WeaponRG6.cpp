#include "stdafx.h"
#include "WeaponRG6.h"
#include "entity.h"
#include "explosiveRocket.h"
#include "Level.h"
#include "../xrPhysics/MathUtils.h"
#include "Actor.h"
#include "UIGameCustom.h"
#include "inventory.h"
#include "inventoryOwner.h"

#ifdef DEBUG
#	include "phdebug.h"
#endif


CWeaponRG6::~CWeaponRG6()
{
}

BOOL	CWeaponRG6::net_Spawn				(CSE_Abstract* DC)
{
	BOOL l_res = inheritedSG::net_Spawn(DC);
	if (!l_res) return l_res;

	if (iAmmoElapsed && !getCurrentRocket())
	{
		shared_str grenade_name = m_ammoTypes[0];
		shared_str fake_grenade_name = pSettings->r_string(grenade_name, "fake_grenade_name");

		if (fake_grenade_name.size())
		{
			int k=iAmmoElapsed;
			while (k)
			{
				k--;
				inheritedRL::SpawnRocket(*fake_grenade_name, this);
			}
		}
//			inheritedRL::SpawnRocket(*fake_grenade_name, this);
	}
	

	
	return l_res;
};

void CWeaponRG6::Load(LPCSTR section)
{
	inheritedRL::Load(section);
	inheritedSG::Load(section);
}

void CWeaponRG6::FireStart()
{
	if (!IsMisfire())
	{
		if (GetState() != eIdle)
			return;

		if (iAmmoElapsed)
		{
			if (!IsWorking() || AllowFireWhileWorking())
			{
				CWeapon::FireStart();

				if (iAmmoElapsed == 0)
					switch2_Empty();
				else
				{
					R_ASSERT(H_Parent());
					SwitchState(eFire);
				}
			}
		}
		else
		{
			if (GetState() == eIdle)
				switch2_Empty();
		}
	}
	else
	{
		//misfire

		CGameObject* object = smart_cast<CGameObject*>(H_Parent());
		if (object)
			object->callback(GameObject::eOnWeaponJammed)(object->lua_game_object(), this->lua_game_object());

		if (smart_cast<CActor*>(this->H_Parent()) && (Level().CurrentViewEntity() == H_Parent()))
			CurrentGameUI()->AddCustomStatic("gun_jammed", true);

		OnEmptyClick();
	}
}

void CWeaponRG6::FireTrace(const Fvector& P, const Fvector& D)
{
	inheritedSG::FireTrace(P, D);
	
	Fvector p1, d; 
	p1.set(P); 
	d.set(D);

	CEntity* E = smart_cast<CEntity*>(H_Parent());
	if (E){
		CInventoryOwner* io		= smart_cast<CInventoryOwner*>(H_Parent());
		if(nullptr == io->inventory().ActiveItem())
		{
			Msg("current_state %d", GetState());
			Msg("next_state %d", GetNextState());
			Msg("item_sect %s", cNameSect().c_str());
			Msg("H_Parent %s", H_Parent()->cNameSect().c_str());
		}
		E->g_fireParams (this, p1,d);
	}

	Fmatrix launch_matrix;
	launch_matrix.identity();
	launch_matrix.k.set(d);
	Fvector::generate_orthonormal_basis(launch_matrix.k,
										launch_matrix.j, launch_matrix.i);
	launch_matrix.c.set(p1);

	if (IsGameTypeSingle() && IsZoomed() && smart_cast<CActor*>(H_Parent()))
	{
		H_Parent()->setEnabled(FALSE);
		setEnabled(FALSE);
	
		collide::rq_result RQ;
		BOOL HasPick = Level().ObjectSpace.RayPick(p1, d, 300.0f, collide::rqtStatic, RQ, this);

		setEnabled(TRUE);
		H_Parent()->setEnabled(TRUE);

		if (HasPick)
		{
			Fvector Transference;		
			Transference.mul(d, RQ.range);
			Fvector res[2];
			u8 canfire0 = TransferenceAndThrowVelToThrowDir(Transference, CRocketLauncher::m_fLaunchSpeed, EffectiveGravity(), res);
			if (canfire0 != 0)
			{
				d = res[0];
			};
		}
	};

	d.normalize();
	d.mul(m_fLaunchSpeed);
	VERIFY2(_valid(launch_matrix),"CWeaponRG6::FireStart. Invalid launch_matrix");
	CRocketLauncher::LaunchRocket(launch_matrix, d, zero_vel);

	CExplosiveRocket* pGrenade = smart_cast<CExplosiveRocket*>(getCurrentRocket());
	VERIFY(pGrenade);
	pGrenade->SetInitiator(H_Parent()->ID());

	if (OnServer())
	{
		NET_Packet P;
		u_EventGen(P,GE_LAUNCH_ROCKET,ID());
		P.w_u16(u16(getCurrentRocket()->ID()));
		u_EventSend(P);
	}
	dropCurrentRocket();
}

void CWeaponRG6::UnloadMagazine(bool spawn_ammo)
{
	inheritedSG::UnloadMagazine(spawn_ammo);

	while (getRocketCount())
		dropCurrentRocket();
}

u8 CWeaponRG6::AddCartridge		(u8 cnt)
{
	u8 t = inheritedSG::AddCartridge(cnt);
	u8 k = cnt-t;
	shared_str fake_grenade_name = pSettings->r_string(m_ammoTypes[m_ammoType].c_str(), "fake_grenade_name");
	while(k){
		--k;
		inheritedRL::SpawnRocket(*fake_grenade_name, this);
	}
	return k;
}

void CWeaponRG6::OnEvent(NET_Packet& P, u16 type) 
{
	inheritedSG::OnEvent(P,type);

	u16 id;
	switch (type) {
		case GE_OWNERSHIP_TAKE : {
			P.r_u16(id);
			inheritedRL::AttachRocket(id, this);
		} break;
		case GE_OWNERSHIP_REJECT : 
		case GE_LAUNCH_ROCKET : 
			{
			bool bLaunch = (type==GE_LAUNCH_ROCKET);
			P.r_u16						(id);
			inheritedRL::DetachRocket	(id, bLaunch);
		} break;
	}
}
