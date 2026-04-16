// Actor_Weapon.cpp:	 для работы с оружием
//////////////////////////////////////////////////////////////////////

#include "StdAfx.h"

#include "Actor.h"
#include "ActorEffector.h"
#include "Missile.h"
#include "Inventory.h"
#include "InventoryWeaponSlotLayout.h"
#include "Weapon.h"
#include "map_manager.h"
#include "Level.h"
#include "CharacterPhysicsSupport.h"
#include "EffectorShot.h"
#include "WeaponMagazined.h"
#include "Grenade.h"
#include "game_base_space.h"
#include "Artefact.h"
#include "HUDManager.h"

static const float VEL_MAX		= 10.f;
static const float VEL_A_MAX	= 10.f;

#define GetWeaponParam(pWeapon, func_name, def_value)	((pWeapon) ? (pWeapon->func_name) : def_value)

//возвращает текуший разброс стрельбы (в радианах)с учетом движения
float CActor::GetWeaponAccuracy() const
{
	PIItem active_item = inventory().ActiveItem();
	CWeapon* W = active_item != nullptr ? active_item->cast_weapon() : nullptr;
	
	if ( IsZoomAimingMode() && W && !GetWeaponParam(W, IsRotatingToZoom(), false) )
	{
		return m_fDispAim;
	}
	float dispersion = m_fDispBase*GetWeaponParam(W, Get_PDM_Base(), 1.0f);

	CEntity::SEntityState state;
	if ( g_State(state) )
	{
		//fAVelocity = angle velocity
		dispersion *= ( 1.0f + (state.fAVelocity/VEL_A_MAX) * m_fDispVelFactor * GetWeaponParam(W, Get_PDM_Vel_F(), 1.0f) );
		//fVelocity = linear velocity
		dispersion *= ( 1.0f + (state.fVelocity/VEL_MAX) * m_fDispVelFactor * GetWeaponParam(W, Get_PDM_Vel_F(), 1.0f) );

		bool bAccelerated = isActorAccelerated( mstate_real, IsZoomAimingMode() );
		if ( bAccelerated || !state.bCrouch )
		{
			dispersion *= ( 1.0f + m_fDispAccelFactor * GetWeaponParam(W, Get_PDM_Accel_F(), 1.0f) );
		}

		if ( state.bCrouch )
		{	
			dispersion *= ( 1.0f + m_fDispCrouchFactor * GetWeaponParam(W, Get_PDM_Crouch(), 1.0f) );
			if ( !bAccelerated )
			{
				dispersion *= ( 1.0f + m_fDispCrouchNoAccelFactor * GetWeaponParam(W, Get_PDM_Crouch_NA(), 1.0f) );
			}
		}
	}
	return dispersion;
}

//возвращает учет движения актора с оружем
float CActor::GetAgility() const
{
	PIItem active_item = inventory().ActiveItem();
	CWeapon* W = active_item != nullptr ? active_item->cast_weapon() : nullptr;

	if (!W) return 0.0f;

	CameraRecoil current_recoil = W->IsZoomed() ? W->zoom_cam_recoil : W->cam_recoil;

	float Agility = current_recoil.Pattern.FactorAgility;

	CEntity::SEntityState state;
	if (g_State(state))
	{
		bool bAccelerated = isActorAccelerated(mstate_real, IsZoomAimingMode());

		// Скоростной модификатор
		float vel_combined = 1.0f + (m_fAgilityVelFactor * current_recoil.Pattern.FactorAgilityVel - 1.0f);
		float speed_factor = 1.0f;
		speed_factor *= (1.0f + (state.fAVelocity / VEL_A_MAX) * (vel_combined - 1.0f));
		speed_factor *= (1.0f + (state.fVelocity / VEL_MAX) * (vel_combined - 1.0f));

		// Модификатор ускорения
		float accel_modifier = 1.0f;
		if (bAccelerated || !state.bCrouch)
		{
			float accel_combined = 1.0f + (m_fAgilityAccelFactor * current_recoil.Pattern.FactorAgilityAccel - 1.0f);
			accel_modifier = accel_combined;
		}

		// Модификатор полуприседа (crouch)
		float crouch_modifier = 1.0f;
		if (state.bCrouch && bAccelerated)
		{
			float crouch_combined = 1.0f + (m_fAgilityCrouchFactor * current_recoil.Pattern.FactorAgilityCrouch - 1.0f);
			crouch_modifier = crouch_combined;
		}

		// Модификатор полного приседа (crouch no accel) 
		float crouch_no_accel_modifier = 1.0f;
		if (state.bCrouch && !bAccelerated)
		{
			float crouch_no_accel_combined = 1.0f + (m_fAgilityCrouchNoAccelFactor * current_recoil.Pattern.FactorAgilityCrouchNoAcc - 1.0f);
			crouch_no_accel_modifier = crouch_no_accel_combined;
		}


		Agility = Agility * speed_factor * accel_modifier * crouch_modifier * crouch_no_accel_modifier;
	//	Msg("Agility=%.3f, speed_factor=%.3f, accel_modifier=%.3f, crouch_modifier=%.3f, crouch_no_accel_modifier=%.3f",
	//		Agility, speed_factor, accel_modifier, crouch_modifier, crouch_no_accel_modifier);
	}

	return Agility;
}


void CActor::g_fireParams(const CHudItem* pHudItem, Fvector& fire_pos, Fvector& fire_dir)
{
	CHudItem* casted_hud_item = const_cast<CHudItem*>(pHudItem);
	CWeapon* pWeap = casted_hud_item != nullptr ? casted_hud_item->cast_weapon() : nullptr;
	if (!IsGameTypeSingle() || HUDview() || (pWeap && pWeap->render_item_ui_query()))
	{
		fire_pos = Cameras().Position();
		fire_dir = Cameras().Direction();

		const CMissile* pMissile = casted_hud_item != nullptr ? casted_hud_item->cast_missile() : nullptr;
		if (pMissile)
		{
			Fvector offset;
			XFORM().transform_dir(offset, pMissile->throw_point_offset());
			fire_pos.add(offset);
		}
		else if (pWeap != nullptr && pWeap->cast_weapon_knife() == nullptr)
		{
			fire_pos = pWeap->get_LastFP();
			fire_dir = Cameras().Direction();

			fire_pos.lerp(fire_pos, Cameras().Position(), pWeap->GetAimFactor());

			if (IsFocused())
			{
				Device.transform_hud2world(fire_pos, fire_dir);
			}

			if (!pWeap->IsZoomed())
			{
				collide::rq_result RQ;
				Fvector ray_trace_dir = Fvector().set(fire_pos).sub(Cameras().Position());
				float ray_trace_len = ray_trace_dir.magnitude();
				ray_trace_dir.normalize();

				if (g_pGameLevel->ObjectSpace.RayPick(Cameras().Position(), ray_trace_dir, ray_trace_len, collide::rqtBoth, RQ, this))
				{
					fire_pos.sub(ray_trace_dir.mul(ray_trace_len - RQ.range + 0.01f));
				}
			}
		}
	}
	else
	{
		const CMissile* pMissile = casted_hud_item != nullptr ? casted_hud_item->cast_missile() : nullptr;
		if (pMissile)
		{
			fire_pos = pMissile->Position();
			fire_dir = Cameras().Direction();
		}
		if (pWeap)
		{
			fire_pos = pWeap->get_LastFP();
			fire_dir = Cameras().Direction();

			if (pWeap->get_LastFD().dotproduct(Cameras().Direction()) >= 0.7f)
			{
				float pick_dist = HUD().GetCurrentRayQuery().range;
				clamp(pick_dist, 10.f, 1000.f);
				Fvector picked_pos = Fvector(Cameras().Position()).mad(Cameras().Direction(), pick_dist);
				fire_dir = Fvector().sub(picked_pos, fire_pos).normalize_safe();
			}
		}
	}
}

void CActor::g_WeaponBones	(int &L, int &R1, int &R2)
{
	R1				= m_r_hand;
	R2				= m_r_finger2;
	L				= m_l_finger1;
}

BOOL CActor::g_State (SEntityState& state) const
{
	state.bJump			= !!(mstate_real&mcJump);
	state.bCrouch		= !!(mstate_real&mcCrouch);
	state.bFall			= !!(mstate_real&mcFall);
	state.bSprint		= !!(mstate_real&mcSprint);
	state.fVelocity		= character_physics_support()->movement()->GetVelocityActual();
	state.fAVelocity	= fCurAVelocity;
	return TRUE;
}

void CActor::SetCantRunState(bool bDisable)
{
	if (g_Alive() && this == Level().CurrentControlEntity())
	{
		NET_Packet	P;
		u_EventGen	(P, GEG_PLAYER_DISABLE_SPRINT, ID());
		P.w_s8		(bDisable?1:-1);
		u_EventSend	(P);
	};
}
void CActor::SetWeaponHideState (u16 State, bool bSet)
{
	if (g_Alive() && this == Level().CurrentControlEntity())
	{
		NET_Packet	P;
		u_EventGen	(P, GEG_PLAYER_WEAPON_HIDE_STATE, ID());
		P.w_u16		(State);
		P.w_u8		(u8(bSet));
		u_EventSend	(P);
	};
}

static	u16 BestWeaponSlots [] = {
	INV_SLOT_3		,		// 2
	INV_SLOT_2		,		// 1
	PISTOL_SLOT_NEW	,		// 1
	GRENADE_SLOT	,		// 3
	KNIFE_SLOT		,		// 0
};

void CActor::SelectBestWeapon(CObject* O)
{
	if (O == nullptr)
	{
		return;
	}

	if (IsGameTypeSingle())
	{
		return;
	}

	//-------------------------------------------------
	CWeapon* pWeapon = O->cast_weapon();
	CGrenade* pGrenade = O->cast_grenade();
	CArtefact* pArtefact = O->cast_artefact();
	CInventoryItem* pIItem = O->cast_inventory_item();
	bool NeedToSelectBestWeapon = false;

	if (pArtefact != nullptr && pArtefact->H_Parent()) //just take an artefact
	{
		return;
	}

	if ((pWeapon != nullptr || pGrenade != nullptr || pArtefact != nullptr) && pIItem != nullptr)
	{
		NeedToSelectBestWeapon = true;
		if ((GameID() & eGameIDArtefactHunt) || (GameID() & eGameIDCaptureTheArtefact)) //only for test...
		{
			if (IsSidearmOrPrimaryPhysicalSlot(pIItem->BaseSlot()))
			{
				CInventoryItem* pIItemInSlot = inventory().ItemFromSlot(pIItem->BaseSlot());
				if (pIItemInSlot != nullptr && pIItemInSlot != pIItem)
				{
					NeedToSelectBestWeapon = false;
				}
			}
		}
	}

	if (!NeedToSelectBestWeapon)
	{
		return;
	}

	//-------------------------------------------------
	for (int i = 0; i < 4; i++)
	{
		if (inventory().ItemFromSlot(BestWeaponSlots[i]))
		{
			if (inventory().GetActiveSlot() != BestWeaponSlots[i])
			{
				PIItem best_item = inventory().ItemFromSlot(BestWeaponSlots[i]);
				if (best_item && best_item->can_kill())
				{
#ifdef DEBUG
					Msg("--- Selecting best weapon [%d], Frame[%d]", BestWeaponSlots[i], Device.dwFrame);
#endif // #ifdef DEBUG
					inventory().Activate(BestWeaponSlots[i]);
				}
				else
				{
#ifdef DEBUG
					Msg("--- Weapon is not best...");
#endif // #ifdef DEBUG
				}
			}
			return;
		};
	};
}

#define ENEMY_HIT_SPOT "mp_hit_sector_location"
BOOL g_bShowHitSectors = TRUE;

void	CActor::HitSector(CObject* who, CObject* weapon)
{
	if (!g_bShowHitSectors) return;
	if (!g_Alive()) return;

	bool bShowHitSector = true;
	
	CEntityAlive* pEntityAlive = who != nullptr ? who->cast_entity_alive() : nullptr;

	if (!pEntityAlive || this == who) bShowHitSector = false;

	if (weapon)
	{
		CWeapon* pWeapon = weapon->cast_weapon();
		if (pWeapon)
		{
			if (pWeapon->IsSilencerAttached())
			{
				bShowHitSector = false;

			}
		}
	}

	if (!bShowHitSector) return;	
		Level().MapManager().AddMapLocation(ENEMY_HIT_SPOT, who->ID());
}

void CActor::on_weapon_shot_start(CWeapon* weapon)
{
	CCameraShotEffector* effector = smart_cast<CCameraShotEffector*>(Cameras().GetCamEffector(eCEShot));

	if (!effector)
	{
		effector = (CCameraShotEffector*)Cameras().AddCamEffector(new CCameraShotEffector());
	}
	else
	{
		if (effector->m_WeaponID != weapon->ID())
		{
			effector->Reset();
		}
	}

	effector->m_WeaponID = weapon->ID();
	R_ASSERT(effector);

	effector->SetRndSeed(GetShotRndSeed());
	effector->SetActor(this);
	effector->Shot(weapon); 
}

void CActor::on_weapon_shot_update		()
{
	CCameraShotEffector* effector = smart_cast<CCameraShotEffector*>( Cameras().GetCamEffector(eCEShot) );
	if ( effector )
	{
		update_camera( effector );
	}
}

void CActor::on_weapon_shot_remove		(CWeapon *weapon)
{
	Cameras().RemoveCamEffector(eCEShot);
}

void CActor::on_weapon_shot_stop		()
{
	CCameraShotEffector				*effector = smart_cast<CCameraShotEffector*>(Cameras().GetCamEffector(eCEShot)); 
	if (effector && effector->IsActive())
	{
		effector->StopShoting();
	}
}

void CActor::on_weapon_hide				(CWeapon *weapon)
{
	CCameraShotEffector				*effector = smart_cast<CCameraShotEffector*>(Cameras().GetCamEffector(eCEShot)); 
	if (effector && effector->IsActive())
		effector->Reset				();
}

Fvector CActor::weapon_recoil_delta_angle	()
{
	CCameraShotEffector				*effector = smart_cast<CCameraShotEffector*>(Cameras().GetCamEffector(eCEShot));
	Fvector							result = {0.f,0.f,0.f};

	if (effector)
		effector->GetDeltaAngle		(result);

	return							(result);
}

Fvector CActor::weapon_recoil_last_delta()
{
	CCameraShotEffector				*effector = smart_cast<CCameraShotEffector*>(Cameras().GetCamEffector(eCEShot));
	Fvector							result = {0.f,0.f,0.f};

	if (effector)
		effector->GetLastDelta		(result);

	return							(result);
}
//////////////////////////////////////////////////////////////////////////

void	CActor::SpawnAmmoForWeapon	(CInventoryItem *pIItem)
{
	if (OnClient()) return;
	if (!pIItem) return;

	CWeaponMagazined* pWM = pIItem->cast_weapon_magazined();
	if (!pWM || !pWM->AutoSpawnAmmo()) return;

	///	CWeaponAmmo* pAmmo = smart_cast<CWeaponAmmo*>(inventory().GetAny( (pWM->m_ammoTypes[0].c_str()) ));
	//	if (!pAmmo) 
	pWM->SpawnAmmo(0xffffffff, nullptr, ID());
};

void	CActor::RemoveAmmoForWeapon	(CInventoryItem *pIItem)
{
	if (OnClient()) return;
	if (!pIItem) return;

	CWeaponMagazined* pWM = pIItem->cast_weapon_magazined();
	if (!pWM || !pWM->AutoSpawnAmmo()) return;

	PIItem get_any = inventory().GetAny(pWM->m_ammoTypes[0].c_str());
	CWeaponAmmo* pAmmo = get_any != nullptr ? get_any->cast_weapon_ammo() : nullptr;
	if (!pAmmo) return;
	//--- мы нашли патроны к текущему оружию	
	/*
	//--- проверяем не подходят ли они к чему-то еще
	bool CanRemove = true;
	TIItemContainer::const_iterator I = inventory().m_all.begin();//, B = I;
	TIItemContainer::const_iterator E = inventory().m_all.end();
	for ( ; I != E; ++I)
	{
	CInventoryItem* pItem = (*I);//->m_pIItem;
	CWeaponMagazined* pWM = smart_cast<CWeaponMagazined*> (pItem);
	if (!pWM || !pWM->AutoSpawnAmmo()) continue;
	if (pWM == pIItem) continue;
	if (pWM->m_ammoTypes[0] != pAmmo->CInventoryItem::object().cNameSect()) continue;
	CanRemove = false;
	break;
	};

	if (!CanRemove) return;
	*/
	pAmmo->DestroyObject();
	//	NET_Packet			P;
	//	u_EventGen			(P,GE_DESTROY,pAmmo->ID());
	//	u_EventSend			(P);
};
