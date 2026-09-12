#include "StdAfx.h"
#include "controller_psy_hit.h"
#include "../basemonster/base_monster.h"
#include "controller.h"
#include "../control_animation_base.h"
#include "../control_direction_base.h"
#include "../control_movement_base.h"
#include "../../../Level.h"
#include "../../../Actor.h"
#include "../../../ActorEffector.h"
#include "../../../../xrEngine/CameraBase.h"
#include "../../../CharacterPhysicsSupport.h"
#include "../../../level_debug.h"
#include "../../../ActorCondition.h"
#include "../../../HUDManager.h"
#include "../../../Inventory.h"
#include "../../../Weapon.h"
#include "../../../WeaponKnife.h"
#include "../../../ui/UIGameCustom.h"
#include "../../../visual_memory_manager.h"
#include "../../../memory_manager.h"

bool CControllerPsyHit::EnableSuicide = false;

void CControllerPsyHit::load(const char* section)
{
	m_min_tube_dist = READ_IF_EXISTS(pSettings, r_float, section, "tube_condition_min_distance", 10.0f);

	static bool SuicideEnabledStatus = false;

	if (!SuicideEnabledStatus)
	{
		EnableSuicide = EngineExternal()[EEngineExternalGame::EnableSuicideByController];
		SuicideEnabledStatus = true;
	}

	if (EnableSuicide)
	{
		FeelParams.MinDist = READ_IF_EXISTS(pSettings, r_float, "gunslinger_base", "controller_min_feel_dist", 10.0f);
		FeelParams.MaxDist = READ_IF_EXISTS(pSettings, r_float, "gunslinger_base", "controller_max_feel_dist", 30.0f);
		ControllerPsyBlockedTime = READ_IF_EXISTS(pSettings, r_float, "gunslinger_base", "controller_psyblocked_time", 5.0f);
		ControllerTime = READ_IF_EXISTS(pSettings, r_float, "gunslinger_base", "controller_time", 3.0f);
		ControllerQueueStopProb = READ_IF_EXISTS(pSettings, r_float, "gunslinger_base", "controller_queue_stop_prob", 0.95f);

		PsiUnBlockParams.MinDist = READ_IF_EXISTS(pSettings, r_float, "gunslinger_base", "controller_psi_unblock_mindist", 7.0f);
		PsiUnBlockParams.MaxDist = READ_IF_EXISTS(pSettings, r_float, "gunslinger_base", "controller_psi_unblock_maxdist", 60.0f);
		PsiUnBlockParams.MinDistProb = READ_IF_EXISTS(pSettings, r_float, "gunslinger_base", "controller_psi_unblock_mindist_prob", 0.95f);
		PsiUnBlockParams.MaxDistProb = READ_IF_EXISTS(pSettings, r_float, "gunslinger_base", "controller_psi_unblock_maxdist_prob", 0.1f);
	}
}

void CControllerPsyHit::reinit()
{
	inherited::reinit();

	IKinematicsAnimated* skel = m_object->Visual()->dcast_PKinematicsAnimated();
	m_stage[0] = skel->ID_Cycle_Safe("psy_attack_0");
	VERIFY(m_stage[0]);
	m_stage[1] = skel->ID_Cycle_Safe("psy_attack_1");
	VERIFY(m_stage[1]);
	m_stage[2] = skel->ID_Cycle_Safe("psy_attack_2");
	VERIFY(m_stage[2]);
	m_stage[3] = skel->ID_Cycle_Safe("psy_attack_3");
	VERIFY(m_stage[3]);
	m_current_index = 0;

	m_time_last_tube = 0;
	m_sound_state = eNone;
}


bool CControllerPsyHit::tube_ready() const
{
	u32 tube_condition_min_delay = 5000;
	if (CController* controller = smart_cast<CController*>(m_object))
	{
		tube_condition_min_delay = controller->m_tube_condition_min_delay;
	}

	return m_time_last_tube + tube_condition_min_delay < time();
}

bool CControllerPsyHit::IsNeedPsiHitOverride()
{
	CActor* pActor = Level().CurrentControlEntity() ? Level().CurrentControlEntity()->cast_actor() : nullptr;
	return pActor && pActor->inventory().ActiveItem();
}

void CControllerPsyHit::UpdatePsiBlockFailedState(CController* monster_controller, CActor* Actor)
{
	float dist = Actor->DistToSelectedContr(monster_controller);

	float prob = 0.0f;

	if (dist <= PsiUnBlockParams.MinDist)
	{
		prob = PsiUnBlockParams.MinDistProb;
	}
	else if (dist >= PsiUnBlockParams.MaxDist)
	{
		prob = PsiUnBlockParams.MaxDistProb;
	}
	else
	{
		prob = 1.0f - (dist - PsiUnBlockParams.MinDist) / (PsiUnBlockParams.MaxDist - PsiUnBlockParams.MinDist);
		prob = prob * (PsiUnBlockParams.MaxDistProb - PsiUnBlockParams.MaxDistProb) + PsiUnBlockParams.MaxDistProb;
	}

	Actor->PsiBlockFailed = Random.randF(0.0f, 1.0f) < prob;
}

void CControllerPsyHit::OnPsyHitActivate(CController* monster_controller)
{
	CActor* Actor = Level().CurrentControlEntity() ? Level().CurrentControlEntity()->cast_actor() : nullptr;
	if (!Actor)
	{
		return;
	}

	if (Actor->ControlledTimeRemains == 0)
	{
		UpdatePsiBlockFailedState(monster_controller, Actor);
		Actor->ChangeInputRotateAngle();
	}

	Actor->ControllerPreparingStartTime = Device.dwTimeGlobal;

	luabind::functor<void> funct;
	if (ai().script_engine().functor("gunsl_controller.on_psi_attack_prepare", funct))
	{
		funct("", monster_controller->ID());
	}

	if ((!Actor->IsPsiBlocked() || Actor->PsiBlockFailed) && Actor->ControlledTimeRemains > 0)
	{
		Actor->ControlledTimeRemains = Actor->ControllerPrepareTime;
	}
}

bool CControllerPsyHit::TryFeelActor(CActor* Actor)
{
	if (!Actor || !Actor->g_Alive())
	{
		return false;
	}

	float Dist = m_object->Position().distance_to(Actor->Position());

	if (Dist <= FeelParams.MinDist)
	{
		return true;
	}
	else if (Dist >= FeelParams.MaxDist)
	{
		return false;
	}
	
	return ((Dist - FeelParams.MinDist) / (FeelParams.MaxDist - FeelParams.MinDist)) > Random.randF(0.0f, 1.0f);
}

bool CControllerPsyHit::check_start_conditions()
{
	CActor* pActor = Actor();

	if (!IsGameTypeSingle())
	{
		CEntityAlive* cast_entity_alive = const_cast<CEntityAlive*>(m_object->EnemyMan.get_enemy());
		pActor = cast_entity_alive != nullptr ? cast_entity_alive->cast_actor() : nullptr;

		if (!pActor)
		{
			m_curent_actor_id = u16(-1);
			return false;
		}
	}

	if (is_active())
	{
		return false;
	}

	if (m_man->is_captured_pure())
	{
		return false;
	}

	if (pActor->Cameras().GetCamEffector(eCEControllerPsyHit))
	{
		return false;
	}

	if (!see_enemy(pActor))
	{
		return false;
	}

	if (!tube_ready())
	{
		return false;
	}

	if (m_object->Position().distance_to(pActor->Position()) < m_min_tube_dist)
	{
		if (!EnableSuicide || !IsNeedPsiHitOverride())
		{
			return false;
		}
	}

	if (!IsGameTypeSingle())
	{
		m_curent_actor_id = pActor->ID();
	}

	return true;
}

void CControllerPsyHit::activate()
{
	CActor* pActor = Actor();
	if (!IsGameTypeSingle())
	{
		CObject* object = Level().Objects.net_Find(m_curent_actor_id);
		pActor = object != nullptr ? object->cast_actor() : nullptr;
		if (!pActor)
		{
			return;
		}
	}

	m_man->capture_pure(this);
	m_man->subscribe(this, ControlCom::eventAnimationEnd);

	m_man->path_stop(this);
	m_man->move_stop(this);

	//////////////////////////////////////////////////////////////////////////
	// set direction
	SControlDirectionData* ctrl_dir = (SControlDirectionData*)m_man->data(this, ControlCom::eControlDir);
	VERIFY(ctrl_dir);
	if (ctrl_dir == nullptr)
	{
		return;
	}

	ctrl_dir->heading.target_speed = 3.f;
	ctrl_dir->heading.target_angle = m_man->direction().angle_to_target(pActor->Position());

	//////////////////////////////////////////////////////////////////////////
	m_current_index = 0;
	play_anim();

	m_blocked = false;

	if (!IsGameTypeSingle())
	{
		NET_Packet tmp_packet;
		CGameObject::u_EventGen(tmp_packet, GE_CONTROLLER_PSY_FIRE, m_object->ID());
		tmp_packet.w_u16(pActor->ID());
		tmp_packet.w_u8(0);
		Level().Server->SendBroadcast(BroadcastCID, tmp_packet, net_flags(true, true));
	}

	if (EnableSuicide)
	{
		OnPsyHitActivate(static_cast<CController*>(m_object));
	}

	set_sound_state(ePrepare);
}

void CControllerPsyHit::deactivate()
{
	CActor* pActor = Actor();
	m_man->release_pure(this);
	m_man->unsubscribe(this, ControlCom::eventAnimationEnd);

	if (m_blocked)
	{
		NET_Packet P;

		if (IsGameTypeSingle())
		{
			pActor->u_EventGen(P, GEG_PLAYER_WEAPON_HIDE_STATE, pActor->ID());
			P.w_u16(INV_STATE_BLOCK_ALL);
			P.w_u8(u8(false));
			pActor->u_EventSend(P);
		}
		else
		{
			CObject* object = Level().Objects.net_Find(m_curent_actor_id);
			pActor = object != nullptr ? object->cast_actor() : nullptr;
			if (pActor)
			{
				pActor->u_EventGen(P, GEG_PLAYER_WEAPON_HIDE_STATE, pActor->ID());
				P.w_u16(INV_STATE_BLOCK_ALL);
				P.w_u8(u8(false));
				pActor->u_EventSend(P);
			}
		}
	}

	set_sound_state(eNone);
}

void CControllerPsyHit::on_event(ControlCom::EEventType type, ControlCom::IEventData* data)
{
	if (type == ControlCom::eventAnimationEnd)
	{
		if (m_current_index < 3)
		{
			m_current_index++;
			play_anim();

			switch (m_current_index)
			{
				case 1:
					death_glide_start();
					break;
				case 2:
					hit();
					break;
				case 3:
					death_glide_end();
					break;
			}
		}
		else
		{
			m_man->deactivate(this);
			return;
		}
	}
}

void CControllerPsyHit::play_anim()
{
	SControlAnimationData* ctrl_anim = (SControlAnimationData*)m_man->data(this, ControlCom::eControlAnimation);
	VERIFY(ctrl_anim);
	if (ctrl_anim == nullptr)
	{
		return;
	}
	ctrl_anim->global.set_motion(m_stage[m_current_index]);
	ctrl_anim->global.actual = false;
}

namespace detail
{

bool check_actor_visibility(const Fvector trace_from, const Fvector trace_to, CObject* object)
{
	CActor* pActor = Actor();
	const float dist = trace_from.distance_to(trace_to);
	Fvector trace_dir;
	trace_dir.sub(trace_to, trace_from);


	collide::rq_result l_rq;
	l_rq.O = nullptr;
	Level().ObjectSpace.RayPick(trace_from, trace_dir, dist, collide::rqtBoth, l_rq, object);

	return l_rq.O == pActor || (l_rq.range >= dist - 0.1f);
}

} // namespace detail

extern CActor* g_actor;

bool CControllerPsyHit::see_enemy(CActor* pA)
{
	return m_object->memory().visual().visible_now(pA);
}

bool CControllerPsyHit::check_conditions_final()
{
	if (!m_object->g_Alive())
	{
		return false;
	}

	CActor* pActor = Actor();

	if (!IsGameTypeSingle())
	{
		CObject* object = Level().Objects.net_Find(m_curent_actor_id);
		pActor = object != nullptr ? object->cast_actor() : nullptr;

		if (!pActor)
		{
			return false;
		}
	}

	if (!m_object->EnemyMan.is_enemy(pActor))
	{
		return false;
	}
	
	if (!pActor->g_Alive())
	{
		return false;
	}
	
	if (m_object->Position().distance_to_xz(pActor->Position()) < m_min_tube_dist - 2)
	{
		if (!EnableSuicide || !IsNeedPsiHitOverride())
		{
			return false;
		}
	}

	return see_enemy(pActor);
}

bool CControllerPsyHit::PsiEffects(CController* monster_controller, CActor* Actor)
{
	bool PsiBlocked = Actor->IsPsiBlocked() && !Actor->PsiBlockFailed;
	bool NotSeen = !see_enemy(Actor) && !TryFeelActor(Actor);

	if ((PsiBlocked || NotSeen) && !Actor->SuicideNow && !Actor->IsSuicideInreversible())
	{
		Actor->PlanningSuicide = false;
		Actor->SuicideNow = false;
		Actor->SetHandsJitterTime(floor(ControllerPsyBlockedTime * 1000.0f));
		return NotSeen;
	}

	Actor->ControlledTimeRemains = floor(ControllerTime * 1000.0f);

	CHudItem* SuicideItem = Actor->inventory().ActiveItem() ? Actor->inventory().ActiveItem()->cast_hud_item() : nullptr;

	if (Actor->GetDevice() || SuicideItem && !Actor->CanUseItemForSuicide(SuicideItem))
	{
		Actor->PlanningSuicide = Actor->CanUseItemForSuicide(SuicideItem);
		Actor->SuicideNow = false;
		return !SuicideItem || (!(SuicideItem->GetState() == CHUDState::eHidden || SuicideItem->GetState() == CHUDState::eHiding));
	}

	if (!SuicideItem)
	{
		if (Actor->CanUseItemForSuicide(Actor->inventory().ItemFromSlot(KNIFE_SLOT) ? Actor->inventory().ItemFromSlot(KNIFE_SLOT)->cast_hud_item() : nullptr))
		{
			Actor->PlanningSuicide = true;
			return true;
		}
		else
		{
			Actor->PlanningSuicide = false;
			return false;
		}
	}

	CWeaponKnife* SuicideKnife = SuicideItem ? SuicideItem->cast_weapon_knife() : nullptr;

	if (SuicideKnife)
	{
		Actor->ControlledTimeRemains = floor((SuicideKnife->ControllerTime > 0.0f ? SuicideKnife->ControllerTime : (Actor->ControlledTimeRemains / 1000.0f)) * 1000.0f);
		Actor->PlanningSuicide = true;

		if (!Actor->SuicideNow && SuicideKnife->GetNextState() != CWeapon::eSuicide && SuicideKnife->GetNextState() != CWeapon::eFire && SuicideKnife->GetNextState() != CWeapon::eFire2)
		{
			SuicideKnife->SwitchState(CWeapon::eSuicide);
			Actor->SuicideNow = true;
		}

		return true;
	}

	CWeapon* SuicideWeapon = SuicideItem->cast_weapon();

	if (SuicideWeapon && SuicideWeapon->IsGrenadeMode())
	{
		bool CanSwitchGL = SuicideWeapon->ControllerCanSwitchGL;
		bool CanShootGL = SuicideWeapon->ControllerCanShootGL;
		if (CanShootGL && SuicideWeapon->GetCurrentElapsed(true) > 0 && SuicideWeapon->ControllerShootGLMinDist < m_object->Position().distance_to(Actor->Position()))
		{
		}
		else if (CanSwitchGL && SuicideWeapon->GetAmmoElapsed() + SuicideWeapon->GetAmmoChamberElapsed() > 0 && !SuicideWeapon->IsMisfire())
		{
			SuicideWeapon->SwitchState(CWeapon::eSwitch);
			Actor->PlanningSuicide = true;
			Actor->SuicideNow = false;
			return true;
		}
		else
		{
			Actor->g_PerformDrop();
			return true;
		}
	}
	else if (SuicideWeapon && (SuicideWeapon->cast_weapon_rg6() || SuicideWeapon->cast_weapon_rpg7()))
	{
		if (SuicideWeapon->ControllerShootExplMinDist > m_object->Position().distance_to(Actor->Position()))
		{
			Actor->g_PerformDrop();
			return true;
		}
	}

	Actor->PlanningSuicide = false;
	Actor->SuicideNow = false;

	if (SuicideWeapon)
	{
		if (SuicideWeapon->SuicideByAnimation)
		{
			Actor->SuicideNow = SuicideWeapon->GetNextState() == CWeapon::eSuicide || (Actor->LastShotDoneTime > 0);
			if (!Actor->SuicideNow)
			{
				SuicideWeapon->SwitchState(CWeapon::eSuicide);
				Actor->SuicideNow = true;
			}

			if (Actor->SuicideNow)
			{
				Actor->ControlledTimeRemains = floor((SuicideWeapon->ControllerTime > 0.0f ? SuicideWeapon->ControllerTime : (Actor->ControlledTimeRemains / 1000.0f)) * 1000.0f);
			}

			Actor->PlanningSuicide = true;
		}
		else
		{
			if (SuicideWeapon->CanStartAction(Actor))
			{
				Actor->SuicideNow = true;
				Actor->ControlledTimeRemains = floor((SuicideWeapon->ControllerTime > 0.0f ? SuicideWeapon->ControllerTime : (Actor->ControlledTimeRemains / 1000.0f)) * 1000.0f);
			}
			Actor->PlanningSuicide = true;
		}

		if (SuicideWeapon->IsWorking())
		{
			if (ControllerQueueStopProb >= ::Random.randF(0.0f, 1.0f))
			{
				SuicideWeapon->StopShooting();
			}
		}
	}

	return true;
}

bool CControllerPsyHit::PsiStart(CController* monster_controller)
{
	CActor* Actor = Level().CurrentControlEntity() ? Level().CurrentControlEntity()->cast_actor() : nullptr;
	if (!Actor)
	{
		return false;
	}

	bool result = PsiEffects(monster_controller, Actor);

	if (!Actor->inventory_disabled())
	{
		CurrentGameUI()->HideShownDialogs();
		Actor->set_inventory_disabled(true);
		Actor->set_pda_disabled(true);
		Actor->set_use_disabled(true);

		if (!Level().is_block_action(kWPN_1))
		{
			Level().block_action(kWPN_1);
		}

		if (!Level().is_block_action(kWPN_2))
		{
			Level().block_action(kWPN_2);
		}

		if (!Level().is_block_action(kWPN_3))
		{
			Level().block_action(kWPN_3);
		}

		if (!Level().is_block_action(kWPN_4))
		{
			Level().block_action(kWPN_4);
		}

		if (!Level().is_block_action(kWPN_5))
		{
			Level().block_action(kWPN_5);
		}

		if (!Level().is_block_action(kWPN_6))
		{
			Level().block_action(kWPN_6);
		}

		if (!Level().is_block_action(kWPN_7))
		{
			Level().block_action(kWPN_7);
		}

		if (!Level().is_block_action(kDROP))
		{
			Level().block_action(kDROP);
		}

		if (!Level().is_block_action(kUSE))
		{
			Level().block_action(kUSE);
		}

		if (!Level().is_block_action(kQUICK_USE_1))
		{
			Level().block_action(kQUICK_USE_1);
		}

		if (!Level().is_block_action(kQUICK_USE_2))
		{
			Level().block_action(kQUICK_USE_2);
		}

		if (!Level().is_block_action(kQUICK_USE_3))
		{
			Level().block_action(kQUICK_USE_3);
		}

		if (!Level().is_block_action(kQUICK_USE_4))
		{
			Level().block_action(kQUICK_USE_4);
		}

		if (!Level().is_block_action(kUSE_BANDAGE))
		{
			Level().block_action(kUSE_BANDAGE);
		}

		if (!Level().is_block_action(kUSE_MEDKIT))
		{
			Level().block_action(kUSE_MEDKIT);
		}

		if (!Level().is_block_action(kINVENTORY))
		{
			Level().block_action(kINVENTORY);
		}

		if (!Level().is_block_action(kACTIVE_JOBS))
		{
			Level().block_action(kACTIVE_JOBS);
		}

		if (!Level().is_block_action(kMAP))
		{
			Level().block_action(kMAP);
		}

		if (!Level().is_block_action(kCONTACTS))
		{
			Level().block_action(kCONTACTS);
		}

		if (!Level().is_block_action(kQUICK_BOLT))
		{
			Level().block_action(kQUICK_BOLT);
		}

		if (!Level().is_block_action(kQUICK_GRENADE))
		{
			Level().block_action(kQUICK_GRENADE);
		}
	}

	if (result)
	{
		bool Found = false;
		for (const auto& Contr : Actor->ActiveControllers)
		{
			if (Contr == monster_controller)
			{
				Found = true;
				break;
			}
		}

		if (!Found)
		{
			Actor->AddActiveController(monster_controller);
		}

		if (Actor->PlanningSuicide || Actor->SuicideNow)
		{
			luabind::functor<void> funct;
			if (ai().script_engine().functor("gunsl_controller.on_suicide_attack", funct))
			{
				funct("", monster_controller->ID());
			}
		}
	}
	else
	{
		luabind::functor<void> funct;
		if (ai().script_engine().functor("gunsl_controller.on_std_attack", funct))
		{
			funct("", monster_controller->ID());
		}
	}

	return result;
}

void CControllerPsyHit::death_glide_start()
{
	CActor* pActor = Actor();
	if (!check_conditions_final() || EnableSuicide && PsiStart(static_cast<CController*>(m_object)))
	{
		m_man->deactivate(this);
		return;
	}

	if (!IsGameTypeSingle())
	{
		CObject* object = Level().Objects.net_Find(m_curent_actor_id);
		pActor = object != nullptr ? object->cast_actor() : nullptr;
		if (!pActor)
		{
			return;
		}
	}

	if (IsGameTypeSingle())
	{
		HUD().SetRenderable(false);

		if (CController* controller = smart_cast<CController*>(m_object))
		{
			controller->CControlledActor::install();
			controller->CControlledActor::dont_need_turn();
		}

		// Start effector
		CEffectorCam* ce = pActor->Cameras().GetCamEffector(eCEControllerPsyHit);
		VERIFY(!ce);

		Fvector src_pos = pActor->cam_Active()->vPosition;
		Fvector target_pos = m_object->Position();
		target_pos.y += 1.2f;

		Fvector dir;
		dir.sub(target_pos, src_pos);

		float dist = dir.magnitude();
		dir.normalize();


		float const actor_psy_immunity = pActor->conditions().GetHitImmunity(ALife::eHitTypeTelepatic);

		target_pos.mad(src_pos, dir, 0.01f + actor_psy_immunity * (dist - 4.8f));


		float const base_fov = g_fov;
		float const dest_fov = g_fov - (g_fov - 10.f) * actor_psy_immunity;

		pActor->Cameras().AddCamEffector(new CControllerPsyHitCamEffector(eCEControllerPsyHit, src_pos, target_pos, m_man->animation().motion_time(m_stage[1], m_object->Visual()), base_fov, dest_fov));

		smart_cast<CController*>(m_object)->draw_fire_particles();

		dir.sub(src_pos, target_pos);
		dir.normalize();
		float h, p;
		dir.getHP(h, p);
		dir.setHP(h, p + PI_DIV_3);
		pActor->character_physics_support()->movement()->ApplyImpulse(dir, pActor->GetMass() * 530.f);

		set_sound_state(eStart);
	}
	else
	{
		NET_Packet tmp_packet;
		CGameObject::u_EventGen(tmp_packet, GE_CONTROLLER_PSY_FIRE, m_object->ID());
		tmp_packet.w_u16(pActor->ID());
		tmp_packet.w_u8(1);
		Level().Server->SendBroadcast(BroadcastCID, tmp_packet, net_flags(true, true));
	}

	if (IsGameTypeSingle())
	{
		NET_Packet P;
		pActor->u_EventGen(P, GEG_PLAYER_WEAPON_HIDE_STATE, pActor->ID());
		P.w_u16(INV_STATE_BLOCK_ALL);
		P.w_u8(u8(true));
		pActor->u_EventSend(P);
	}
	else
	{
		NET_Packet P;
		pActor->u_EventGen(P, GEG_PLAYER_WEAPON_HIDE_STATE, pActor->ID());
		P.w_u16(INV_STATE_BLOCK_ALL);
		P.w_u8(u8(true));
		pActor->u_EventSend(P);
	}
	m_blocked = true;

	//////////////////////////////////////////////////////////////////////////
	// set direction
	SControlDirectionData* ctrl_dir = (SControlDirectionData*)m_man->data(this, ControlCom::eControlDir);
	VERIFY(ctrl_dir);
	if (ctrl_dir == nullptr)
	{
		return;
	}
	ctrl_dir->heading.target_speed = 3.f;
	ctrl_dir->heading.target_angle = m_man->direction().angle_to_target(pActor->Position());

	//////////////////////////////////////////////////////////////////////////
}

void CControllerPsyHit::death_glide_end()
{
	CActor* pActor = Actor();
	if (IsGameTypeSingle())
	{
		CController* monster = smart_cast<CController*>(m_object);
		monster->draw_fire_particles();

		monster->m_sound_tube_hit_left.play_at_pos(pActor, Fvector().set(-1.f, 0.f, 1.f), sm_2D);
		monster->m_sound_tube_hit_right.play_at_pos(pActor, Fvector().set(1.f, 0.f, 1.f), sm_2D);

		m_object->Hit_Psy(pActor, monster->m_tube_damage);
	}
	else
	{
		CController* monster = smart_cast<CController*>(m_object);
		CObject* object = Level().Objects.net_Find(m_curent_actor_id);
		pActor = object != nullptr ? object->cast_actor() : nullptr;
		if (pActor)
		{
			m_object->Hit_Psy(pActor, monster->m_tube_damage);
		}
	}

	m_time_last_tube = Device.dwTimeGlobal;
	stop();
}

void CControllerPsyHit::update_frame()
{
}

void CControllerPsyHit::set_sound_state(ESoundState state)
{
	CActor* pActor = Actor();
	CController* monster = smart_cast<CController*>(m_object);
	if (state == ePrepare)
	{
		monster->m_sound_tube_prepare.play_at_pos(pActor, Fvector().set(0.f, 0.f, 0.f), sm_2D);
	}
	else if (state == eStart)
	{
		if (monster->m_sound_tube_prepare.is_playing())
		{
			monster->m_sound_tube_prepare.stop();
		}

		monster->m_sound_tube_start.play_at_pos(pActor, Fvector().set(0.f, 0.f, 0.f), sm_2D);
		monster->m_sound_tube_pull.play_at_pos(pActor, Fvector().set(0.f, 0.f, 0.f), sm_2D);
	}
	else if (state == eHit)
	{
		if (monster->m_sound_tube_start.is_playing())
		{
			monster->m_sound_tube_start.stop();
		}
		if (monster->m_sound_tube_pull.is_playing())
		{
			monster->m_sound_tube_pull.stop();
		}
	}
	else if (state == eNone)
	{
		if (monster->m_sound_tube_start.is_playing())
		{
			monster->m_sound_tube_start.stop();
		}
		if (monster->m_sound_tube_pull.is_playing())
		{
			monster->m_sound_tube_pull.stop();
		}
		if (monster->m_sound_tube_prepare.is_playing())
		{
			monster->m_sound_tube_prepare.stop();
		}
	}

	m_sound_state = state;
}

void CControllerPsyHit::hit()
{
	if (!IsGameTypeSingle())
	{
		CObject* object = Level().Objects.net_Find(m_curent_actor_id);
		CActor* pActor = object != nullptr ? object->cast_actor() : nullptr;
		if (pActor)
		{
			NET_Packet tmp_packet;
			CGameObject::u_EventGen(tmp_packet, GE_CONTROLLER_PSY_FIRE, m_object->ID());
			tmp_packet.w_u16(pActor->ID());
			tmp_packet.w_u8(3);
			Level().Server->SendBroadcast(BroadcastCID, tmp_packet, net_flags(true, true));
		}
	}

	set_sound_state(eHit);
}

void CControllerPsyHit::stop()
{
	CActor* pActor = Actor();
	if (IsGameTypeSingle())
	{
		HUD().SetRenderable(true);

		if (CController* controller = smart_cast<CController*>(m_object))
		{
			if (controller->CControlledActor::is_controlling())
			{
				controller->CControlledActor::release();
			}
		}

		// Stop camera effector
		CEffectorCam* ce = pActor->Cameras().GetCamEffector(eCEControllerPsyHit);
		if (ce)
		{
			pActor->Cameras().RemoveCamEffector(eCEControllerPsyHit);
		}
	}
	else
	{
		CObject* object = Level().Objects.net_Find(m_curent_actor_id);
		pActor = object != nullptr ? object->cast_actor() : nullptr;
		if (pActor)
		{
			NET_Packet tmp_packet;
			CGameObject::u_EventGen(tmp_packet, GE_CONTROLLER_PSY_FIRE, m_object->ID());
			tmp_packet.w_u16(pActor->ID());
			tmp_packet.w_u8(2);
			Level().Server->SendBroadcast(BroadcastCID, tmp_packet, net_flags(true, true));
		}
	}
}

void CControllerPsyHit::on_death()
{
	if (!is_active())
	{
		return;
	}

	stop();

	m_man->deactivate(this);
}
