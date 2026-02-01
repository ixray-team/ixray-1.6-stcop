#include "StdAfx.h"
#include "burer.h"
#include "../../../../xrPhysics/PhysicsShell.h"
#include "../../../CharacterPhysicsSupport.h"
#include "../../../Actor.h"
#include "burer_state_manager.h"
#include "../../../../Include/xrRender/KinematicsAnimated.h"
#include "../../../sound_player.h"
#include "../../../Level.h"
#include "../../../ai_monster_space.h"
#include "../../../level_debug.h"
#include "../monster_velocity_space.h"
#include "../../../GamePersistent.h"
#include "../control_animation_base.h"
#include "../control_movement_base.h"
#include "burer_fast_gravi.h"
#include "ParticlesObject.h"
#include "../anti_aim_ability.h"
#include "../anti_aim_ability.h"

#include "../../../Inventory.h"
#include "../../../ActorCondition.h"
#include "../../../../xrEngine/xr_level_controller.h"
#include "../../../Weapon.h"

#include "../../../../xrCore/_vector3d_ext.h"
#include "../control_direction_base.h"

bool CBurer::can_scan = true;

CBurer::CBurer()
{
	StateMan				=	new CStateManagerBurer(this);

 	m_fast_gravi			=	new CBurerFastGravi();

 	control().add				(m_fast_gravi,  ControlCom::eComCustom1);
	m_shield_expire_time	= 0;
}

CBurer::~CBurer()
{
	xr_delete					(StateMan);
	xr_delete					(m_fast_gravi);
}


void CBurer::reinit()
{
	inherited::reinit			();

	DeactivateShield			();

	time_last_scan			=	0;
}

void CBurer::net_Destroy()
{
	inherited::net_Destroy();
}

void CBurer::reload(LPCSTR section)
{
	inherited::reload		(section);

	// add specific sounds
	sound().add				(pSettings->r_string(section,"sound_gravi_attack"),	DEFAULT_SAMPLE_COUNT,	
							SOUND_TYPE_MONSTER_ATTACKING,	MonsterSound::eHighPriority + 2,	
							u32(MonsterSound::eBaseChannel),	eMonsterSoundGraviAttack, "head");

	sound().add				(pSettings->r_string(section,"sound_tele_attack"),	DEFAULT_SAMPLE_COUNT,	
							SOUND_TYPE_MONSTER_ATTACKING,	MonsterSound::eHighPriority + 3,	
							u32(MonsterSound::eBaseChannel),	eMonsterSoundTeleAttack, "head");
}

void CBurer::ActivateShield()
{
	m_shield_active = true;
	m_shield_expire_time = Device.dwTimeGlobal + m_shield_time;
}

void CBurer::DeactivateShield()
{
	m_shield_active = false;
	m_shield_expire_time = 0;
}

void CBurer::Load(LPCSTR section)
{
	inherited::Load							(section);

	anim().accel_load						(section);
	anim().accel_chain_add					(eAnimWalkFwd,		eAnimRun);

	particle_gravi_wave					=	pSettings->r_string(section,"Particle_Gravi_Wave");
	particle_gravi_prepare				=	pSettings->r_string(section,"Particle_Gravi_Prepare");
	particle_tele_object				=	pSettings->r_string(section,"Particle_Tele_Object");
	
	::Sound->create(sound_gravi_wave,	pSettings->r_string(section,"sound_gravi_wave"),st_Effect,SOUND_TYPE_WORLD);
	::Sound->create(sound_tele_hold,	pSettings->r_string(section,"sound_tele_hold"),	st_Effect,SOUND_TYPE_WORLD);
	::Sound->create(sound_tele_throw,	pSettings->r_string(section,"sound_tele_throw"),st_Effect,SOUND_TYPE_WORLD);

	m_gravi.cooldown					= 	READ_IF_EXISTS(pSettings, r_u32, section,"Gravi_Cooldown", 0.f);
	m_gravi.min_dist					= 	READ_IF_EXISTS(pSettings, r_float, section,"Gravi_MinDist", 6.f);
	m_gravi.max_dist					= 	READ_IF_EXISTS(pSettings, r_float, section,"Gravi_MaxDist", 0);
	m_gravi.speed						= 	pSettings->r_float(section,"Gravi_Speed");
	m_gravi.step						= 	pSettings->r_float(section,"Gravi_Step");
	m_gravi.time_to_hold				= 	pSettings->r_u32(section,"Gravi_Time_To_Hold");
	m_gravi.radius						= 	pSettings->r_float(section,"Gravi_Radius");
	m_gravi.impulse_to_objects			= 	pSettings->r_float(section,"Gravi_Impulse_To_Objects");
	m_gravi.impulse_to_enemy			= 	pSettings->r_float(section,"Gravi_Impulse_To_Enemy");
	m_gravi.hit_power					= 	pSettings->r_float(section,"Gravi_Hit_Power");

	m_weight_to_stamina_hit				= 	READ_IF_EXISTS(pSettings, r_float, section, "weight_to_stamina_hit", 0.02f);
	m_weapon_drop_stamina_k				= 	READ_IF_EXISTS(pSettings, r_float, section, "weapon_drop_stamina_k", 3.f);

	m_runaway_distance					= 	READ_IF_EXISTS(pSettings, r_float, section, "runaway_distance", 6.f);
	m_normal_distance					= 	READ_IF_EXISTS(pSettings, r_float, section, "normal_distance", 12.f);
	m_max_runaway_time					= 	READ_IF_EXISTS(pSettings, r_u32, section, "max_runaway_time", 5000);

	m_weapon_drop_velocity				= 	READ_IF_EXISTS(pSettings, r_float, section, "weapon_drop_velocity", 8);

	m_shield_cooldown					= 	READ_IF_EXISTS(pSettings, r_u32, section, "shield_cooldown", 4000);
	m_shield_time						= 	READ_IF_EXISTS(pSettings, r_u32, section, "shield_time", 3000);	
	m_shield_keep_particle				= 	READ_IF_EXISTS(pSettings, r_string, section, "shield_keep_particle", 0);	
	m_shield_keep_particle_period		= 	READ_IF_EXISTS(pSettings, r_u32, section, "shield_keep_particle_period", 1000);
		
	m_tele_max_handled_objects			= 	pSettings->r_u32(section,"Tele_Max_Handled_Objects");
	m_tele_max_time						= 	READ_IF_EXISTS(pSettings, r_u32, section, "Tele_Max_Time", 10000);
	m_tele_time_to_hold					= 	pSettings->r_u32(section,"Tele_Time_To_Hold");
	m_tele_object_min_mass				= 	pSettings->r_float(section,"Tele_Object_Min_Mass");
	m_tele_object_max_mass				= 	pSettings->r_float(section,"Tele_Object_Max_Mass");
	m_tele_find_radius					= 	pSettings->r_float(section,"Tele_Find_Radius");
	m_tele_min_distance					= 	READ_IF_EXISTS(pSettings, r_float, section, "tele_min_distance", 8);
	m_tele_max_distance					= 	READ_IF_EXISTS(pSettings, r_float, section, "tele_max_distance", 30);
	m_tele_raise_speed					= 	READ_IF_EXISTS(pSettings, r_float, section, "tele_raise_speed", 5.f);
	m_tele_fly_velocity					=	READ_IF_EXISTS(pSettings, r_float, section, "tele_fly_velocity", 30.f);
	m_tele_object_height				=	READ_IF_EXISTS(pSettings, r_float, section, "tele_object_height", 2.f);
	
	particle_fire_shield				= 	pSettings->r_string(section,"Particle_Shield");
	
	SVelocityParam &velocity_none		= 	move().get_velocity(MonsterMovement::eVelocityParameterIdle);	
	SVelocityParam &velocity_turn		= 	move().get_velocity(MonsterMovement::eVelocityParameterStand);
	SVelocityParam &velocity_walk		= 	move().get_velocity(MonsterMovement::eVelocityParameterWalkNormal);
	SVelocityParam &velocity_run		= 	move().get_velocity(MonsterMovement::eVelocityParameterRunNormal);

    SVelocityParam& velocity_walk_dmg = move().get_velocity(MonsterMovement::eVelocityParameterWalkDamaged);
    SVelocityParam& velocity_run_dmg  = move().get_velocity(MonsterMovement::eVelocityParameterRunDamaged);
    SVelocityParam& velocity_steal    = move().get_velocity(MonsterMovement::eVelocityParameterSteal);

    // Stand idle + damaged + turn + look around
    anim().AddAnim(eAnimStandIdle, "stand_idle_", -1, &velocity_none, PS_STAND);
    anim().LinkAction(ACT_STAND_IDLE, eAnimStandIdle);

	if (Visual()->dcast_PKinematicsAnimated()->ID_Cycle_Safe("stand_idle_dmg_0"))
	{
		anim().AddAnim(eAnimStandDamaged, "stand_idle_dmg_", -1, &velocity_none, PS_STAND);
		anim().AddReplacedAnim(&m_bDamaged, eAnimStandIdle, eAnimStandDamaged);
	}
    anim().AddAnim(eAnimStandTurnLeft, "stand_turn_ls_", -1, &velocity_turn, PS_STAND);
    anim().AddAnim(eAnimStandTurnRight, "stand_turn_rs_", -1, &velocity_turn, PS_STAND);

    // Stand walk + damaged
    anim().AddAnim(eAnimWalkFwd, "stand_walk_fwd_", -1, &velocity_walk, PS_STAND);
    anim().LinkAction(ACT_WALK_FWD, eAnimWalkFwd);
    anim().LinkAction(ACT_WALK_BKWD, eAnimWalkFwd);
    anim().LinkAction(ACT_DRAG, eAnimWalkFwd);

	if (Visual()->dcast_PKinematicsAnimated()->ID_Cycle_Safe("stand_walk_fwd_dmg_0"))
	{
		anim().AddAnim(eAnimWalkDamaged, "stand_walk_fwd_dmg_", -1, &velocity_walk_dmg, PS_STAND);
        anim().AddReplacedAnim(&m_bDamaged, eAnimWalkFwd, eAnimWalkDamaged);
	}

    // Stand run + damaged + rurn
    anim().AddAnim(eAnimRun, "stand_run_fwd_", -1, &velocity_run, PS_STAND);
    anim().LinkAction(ACT_RUN, eAnimRun);

	if (Visual()->dcast_PKinematicsAnimated()->ID_Cycle_Safe("stand_run_dmg_0"))
	{
		anim().AddAnim(eAnimRunDamaged, "stand_run_dmg_", -1, &velocity_run_dmg, PS_STAND);
		anim().AddReplacedAnim(&m_bDamaged, eAnimRun, eAnimRunDamaged);
	}

	if (Visual()->dcast_PKinematicsAnimated()->ID_Cycle_Safe("stand_run_fwd_turn_left_0"))
	{
		anim().AddAnim(eAnimRunTurnLeft, "stand_run_fwd_turn_left_", -1, &velocity_run, PS_STAND);
		anim().AddReplacedAnim(&m_bRunTurnLeft, eAnimRun, eAnimRunTurnLeft);
	}
	if (Visual()->dcast_PKinematicsAnimated()->ID_Cycle_Safe("stand_run_fwd_turn_right_0"))
	{
		anim().AddAnim(eAnimRunTurnRight, "stand_run_fwd_turn_right_", -1, &velocity_run, PS_STAND);
		anim().AddReplacedAnim(&m_bRunTurnRight, eAnimRun, eAnimRunTurnRight);
	}

    // Stand steal
	if (Visual()->dcast_PKinematicsAnimated()->ID_Cycle_Safe("stand_steal_0"))
	{
		anim().AddAnim(eAnimSteal, "stand_steal_", -1, &velocity_steal, PS_STAND);
		anim().LinkAction(ACT_STEAL, eAnimSteal);
	}
    // Attack
    anim().AddAnim(eAnimAttack, "stand_attack_", -1, &velocity_turn, PS_STAND);
    anim().LinkAction(ACT_ATTACK, eAnimAttack);

    // Die
    anim().AddAnim(eAnimDie, "stand_die_", -1, &velocity_none, PS_STAND);

    // Shield
	if (Visual()->dcast_PKinematicsAnimated()->ID_Cycle_Safe("stand_shield_0"))
	{
		anim().AddAnim(eAnimShieldStart, "stand_shield_", -1, &velocity_turn, PS_STAND);
	}
	else
	{
		anim().AddAnim(eAnimShieldStart, "stand_gravi_", 0, &velocity_turn, PS_STAND);
	}

	// Shield
	if (Visual()->dcast_PKinematicsAnimated()->ID_Cycle_Safe("stand_shield_idle_0"))
	{
		anim().AddAnim(eAnimShieldContinue, "stand_shield_idle_", -1, &velocity_turn, PS_STAND);
	}
	else
	{
		anim().AddAnim(eAnimShieldContinue, "stand_gravi_", 0, &velocity_turn, PS_STAND);
	}

    // Telekinesis
	if (Visual()->dcast_PKinematicsAnimated()->ID_Cycle_Safe("telekinesis_0"))
	{
		anim().AddAnim(eAnimTelekinesis, "telekinesis_", -1, &velocity_turn, PS_STAND);
	}
	else
	{
		anim().AddAnim(eAnimTelekinesis, "stand_tele_", 0, &velocity_turn, PS_STAND);
	}

	if (Visual()->dcast_PKinematicsAnimated()->ID_Cycle_Safe("stand_power_attack_0"))
	{
		anim().AddAnim(eAnimTeleFire, "stand_power_attack_", -1, &velocity_turn, PS_STAND);
	}
	else
	{
		anim().AddAnim(eAnimTeleFire, "stand_tele_", 0, &velocity_turn, PS_STAND);
	}

    // Gravi attack
    m_use_three_gravi_anims = false;
	if (Visual()->dcast_PKinematicsAnimated()->ID_Cycle_Safe("stand_power_attack_0"))
	{
		anim().AddAnim(eAnimGraviFire, "stand_power_attack_", -1, &velocity_turn, PS_STAND);
	}
	else
	{
		anim().AddAnim(eAnimGraviFire, "stand_gravi_", -1, &velocity_turn, PS_STAND);
		m_use_three_gravi_anims = true;
	}
    // Sit
	if (Visual()->dcast_PKinematicsAnimated()->ID_Cycle_Safe("sit_stand_up_0"))
	{
		anim().AddAnim(eAnimSitStandUp, "sit_stand_up_", -1, &velocity_none, PS_SIT);
		anim().AddTransition(PS_SIT, PS_STAND, eAnimSitStandUp, false);
	}

	if (Visual()->dcast_PKinematicsAnimated()->ID_Cycle_Safe("stand_sit_down_0"))
	{
		anim().AddAnim(eAnimStandSitDown, "stand_sit_down_", -1, &velocity_none, PS_STAND);
		anim().AddTransition(PS_STAND, PS_SIT, eAnimStandSitDown, false);
	}

	if (Visual()->dcast_PKinematicsAnimated()->ID_Cycle_Safe("sit_idle_0"))
    {
		anim().AddAnim(eAnimSitIdle, "sit_idle_", -1, &velocity_none, PS_SIT);
        anim().LinkAction(ACT_SIT_IDLE, eAnimSitIdle);
        anim().LinkAction(ACT_LIE_IDLE, eAnimSitIdle);
        anim().LinkAction(ACT_SLEEP, eAnimSitIdle);
        anim().LinkAction(ACT_REST, eAnimSitIdle);
    }
    else
    {
        anim().LinkAction(ACT_SLEEP, eAnimStandIdle);
        anim().LinkAction(ACT_REST, eAnimStandIdle);
    }

	if (Visual()->dcast_PKinematicsAnimated()->ID_Cycle_Safe("sit_eat_0"))
	{
		anim().AddAnim(eAnimEat, "sit_eat_", -1, &velocity_none, PS_SIT);
		anim().LinkAction(ACT_EAT, eAnimEat);
	}

#ifdef DEBUG
    anim().accel_chain_test();
#endif

    m_force_gravi_attack = false;

    PostLoad(section);
}
void CBurer::PostLoad (LPCSTR section)
{
	inherited::PostLoad						(section);
	if (m_anti_aim)
		m_anti_aim->set_callback				(anti_aim_ability::hit_callback(this, &CBurer::StaminaHit));
}

void CBurer::shedule_Update(u32 dt)
{
	inherited::shedule_Update		(dt);

	CTelekinesis::schedule_update	();
}

void CBurer::CheckSpecParams(u32 spec_params)
{
}

void  CBurer::StaminaHit()
{
	if (GodMode())
	{
		return;
	}

	CObject* current_entity = Level().CurrentControlEntity();
	CActor* pActor = current_entity != nullptr ? current_entity->cast_actor() : nullptr;

	if (pActor == nullptr)
	{
		return;
	}

	PIItem active_item = pActor->inventory().ActiveItem();
	CWeapon* const active_weapon = active_item != nullptr ? active_item->cast_weapon() : nullptr;

	if (active_weapon == nullptr)
	{
		return;
	}

	float const weight = active_weapon->Weight();
	float const stamina_hit = weight * m_weight_to_stamina_hit;

	bool const do_weapon_drop = pActor->conditions().GetPower() < stamina_hit * m_weapon_drop_stamina_k && !pActor->inventory().SlotIsPersistent(active_weapon->BaseSlot());

	pActor->conditions().PowerHit(stamina_hit, false);

	if (!do_weapon_drop)
	{
		return;
	}

	Fvector dir = pActor->Direction();
	if (dir.y < 0.0f)
	{
		dir.y = -dir.y;
	}
	
	active_weapon->SetActivationSpeedOverride(normalize(dir) * m_weapon_drop_velocity);
	
	if (!pActor->inventory().Action((u16)kDROP, CMD_STOP))
	{
		pActor->g_PerformDrop();
	}
	
	if (CCustomDevice* pDevice = pActor->GetDevice())
	{
		pDevice->SetActivationSpeedOverride(normalize(dir) * m_weapon_drop_velocity);
		pDevice->SetDropManual(TRUE);
	}

	const shared_str& kick_animator = pActor->m_sBurerKickAnimator;
	
	if (kick_animator.size() > 0)
	{
		pActor->inventory().SetActiveSlot(NO_ACTIVE_SLOT);
		pActor->HudAnimator()->ItemAnimator()->StartAnimator(kick_animator);
	}
}

void CBurer::UpdateGraviObject()
{
	if ( !m_gravi_object.active ) 
	{
		return;
	}
	
	if ( !m_gravi_object.enemy || (m_gravi_object.enemy && m_gravi_object.enemy->getDestroy()) ) 
	{
		m_gravi_object.deactivate();
		return;
	}

	if ( m_gravi_object.from_pos.distance_to(m_gravi_object.cur_pos) 
				> 
		 m_gravi_object.from_pos.distance_to(m_gravi_object.target_pos) ) 
	{
		m_gravi_object.deactivate();
		return;
	}

	float dt = float(Device.dwTimeGlobal - m_gravi_object.time_last_update);
	float dist = dt * float(m_gravi.speed)/1000.f;
		
	if (dist < m_gravi.step) return;
	
	Fvector new_pos;
	Fvector dir;
	dir.sub(m_gravi_object.target_pos,m_gravi_object.cur_pos);
	dir.normalize();
	
	new_pos.mad(m_gravi_object.cur_pos,dir,dist);

	// Trace to enemy 
	Fvector enemy_center;
	m_gravi_object.enemy->Center(enemy_center);
	dir.sub(enemy_center, new_pos);
	dir.normalize();

	float trace_dist = float(m_gravi.step);

	collide::rq_result	l_rq;
	if (Level().ObjectSpace.RayPick(new_pos, dir, trace_dist, collide::rqtBoth, l_rq, nullptr))
	{
		CEntityAlive* casted_enemy = const_cast<CEntityAlive*>(m_gravi_object.enemy);
		const CObject *enemy = casted_enemy->dcast_CObject();
		if ((l_rq.O == enemy) && (l_rq.range < trace_dist)) {
			
			// check for visibility
			bool b_enemy_visible = false;
			xr_vector<CObject *> visible_objects;
			feel_vision_get(visible_objects);

			// find object
			for (u32 i = 0; i<visible_objects.size(); i++) {
				if (visible_objects[i] == enemy) {
					b_enemy_visible = true;
					break;
				}
			}
			
			if (b_enemy_visible) {
				Fvector impulse_dir;

				impulse_dir.set(0.0f,0.0f,1.0f);
				impulse_dir.normalize();

				HitEntity(m_gravi_object.enemy, m_gravi.hit_power, m_gravi.impulse_to_enemy, impulse_dir, ALife::eHitTypeStrike, false);
				m_gravi_object.deactivate();
				return;
			}
		}
	}
																								
	m_gravi_object.cur_pos				= new_pos;
	m_gravi_object.time_last_update		= Device.dwTimeGlobal;

	// ---------------------------------------------------------------------
	// draw particle
	CParticlesObject* ps = Particles::Details::Create(particle_gravi_wave,TRUE).get();

	// вычислить позицию и направленность партикла
	Fmatrix pos; 
	pos.identity();
	pos.k.set(dir);
	Fvector::generate_orthonormal_basis_normalized(pos.k,pos.j,pos.i);
	// установить позицию
	pos.translate_over(m_gravi_object.cur_pos);

	ps->UpdateParent(pos, zero_vel);
	ps->Play(false);
	
	// hit objects
	m_nearest.clear();
	Level().ObjectSpace.GetNearest	(m_nearest,m_gravi_object.cur_pos, m_gravi.radius, nullptr); 
	//xr_vector<CObject*> &m_nearest = Level().ObjectSpace.q_nearest;

	for (u32 i=0;i<m_nearest.size();i++)
	{
		CPhysicsShellHolder  *obj = m_nearest[i]->cast_physics_shell_holder();
		if (!obj || !obj->m_pPhysicsShell) continue;
		
		Fvector dir_;
		dir_.sub(obj->Position(), m_gravi_object.cur_pos);
		dir_.normalize();
		obj->m_pPhysicsShell->applyImpulse(dir_,m_gravi.impulse_to_objects * obj->m_pPhysicsShell->getMass());
	}

	// играть звук
	Fvector snd_pos = m_gravi_object.cur_pos;
	snd_pos.y += 0.5f;
	if (sound_gravi_wave.is_playing())		{
		sound_gravi_wave.set_position	(snd_pos);
	} else ::Sound->play_at_pos			(sound_gravi_wave,0,snd_pos);
}

void CBurer::UpdateGraviObjectCL()
{
	if (!m_gravi_object.active)
	{
		return;
	}

	if (!m_gravi_object.enemy || (m_gravi_object.enemy && m_gravi_object.enemy->getDestroy()))
	{
		m_gravi_object.deactivate();
		return;
	}

	if (m_gravi_object.from_pos.distance_to(m_gravi_object.cur_pos)
	>
		m_gravi_object.from_pos.distance_to(m_gravi_object.target_pos))
	{
		m_gravi_object.deactivate();
		return;
	}

	float dt = float(Device.dwTimeGlobal - m_gravi_object.time_last_update);
	float dist = dt * float(m_gravi.speed) / 1000.f;

	if (dist < m_gravi.step) return;

	Fvector new_pos;
	Fvector dir;
	dir.sub(m_gravi_object.target_pos, m_gravi_object.cur_pos);
	dir.normalize();

	new_pos.mad(m_gravi_object.cur_pos, dir, dist);

	// Trace to enemy 
	Fvector enemy_center;
	m_gravi_object.enemy->Center(enemy_center);
	dir.sub(enemy_center, new_pos);
	dir.normalize();

	float trace_dist = float(m_gravi.step);

	m_gravi_object.cur_pos = new_pos;
	m_gravi_object.time_last_update = Device.dwTimeGlobal;

	// ---------------------------------------------------------------------
	// draw particle
	CParticlesObject* ps = Particles::Details::Create(particle_gravi_wave, TRUE).get();

	// вычислить позицию и направленность партикла
	Fmatrix pos;
	pos.identity();
	pos.k.set(dir);
	Fvector::generate_orthonormal_basis_normalized(pos.k, pos.j, pos.i);
	// установить позицию
	pos.translate_over(m_gravi_object.cur_pos);

	ps->UpdateParent(pos, zero_vel);
	ps->Play(false);


	// играть звук
	Fvector snd_pos = m_gravi_object.cur_pos;
	snd_pos.y += 0.5f;
	if (sound_gravi_wave.is_playing())
	{
		sound_gravi_wave.set_position(snd_pos);
	}
	else ::Sound->play_at_pos(sound_gravi_wave, 0, snd_pos);
}


void CBurer::UpdateCL()
{
	inherited::UpdateCL();

	if (OnServer() && m_shield_active && m_shield_expire_time && Device.dwTimeGlobal >= m_shield_expire_time)
	{
		DeactivateShield();
	}

	const bool process_server_logic = IsGameTypeSingle() || OnServer();
	if (process_server_logic)
		UpdateGraviObject();
	else
		UpdateGraviObjectCL();
	//if (m_fast_gravi->check_start_conditions()) 
	//	control().activate(ControlCom::eComCustom1);
}

void CBurer::StartGraviPrepare()
{
	const CEntityAlive* enemy = EnemyMan.get_enemy();
	if (!enemy)
		return;

	CEntityAlive* cast_enemy = const_cast<CEntityAlive*>(enemy);

	CActor* pA = cast_enemy->cast_actor();
	if (!pA)
		return;

	if (IsGameTypeSingle())
	{
		TParticlesPlayer* PPlayer = pA->GetOrCreateComponent<TParticlesPlayer>();
		PPlayer->StartParticles(particle_gravi_prepare, Fvector().set(0.0f, 0.1f, 0.0f), pA->ID());
	}
	else
	{
		NET_Packet	tmp_packet;
		CGameObject::u_EventGen(tmp_packet, GE_BURER_GRAVI_PARTICLES, ID());
		tmp_packet.w_u8(1);
		tmp_packet.w_u16(pA->ID());
		Level().Server->SendBroadcast(BroadcastCID, tmp_packet, net_flags(TRUE, TRUE));
	}
}

void CBurer::StopGraviPrepare()
{
	if (IsGameTypeSingle())
	{
		CActor* pA = Actor();
		if (!pA)
			return;

		TParticlesPlayer* PPlayer = pA->GetOrCreateComponent<TParticlesPlayer>();
		PPlayer->StopParticles(particle_gravi_prepare, BI_NONE, true);
	}
	else
	{
		const CEntityAlive* enemy = m_gravi_object.enemy;
		if (!enemy)
			return;

		CEntityAlive* cast_enemy = const_cast<CEntityAlive*>(enemy);

		CActor* pA = cast_enemy->cast_actor();
		if (!pA)
			return;

		NET_Packet	tmp_packet;
		CGameObject::u_EventGen(tmp_packet, GE_BURER_GRAVI_PARTICLES, ID());
		tmp_packet.w_u8(0);
		tmp_packet.w_u16(pA->ID());
		Level().Server->SendBroadcast(BroadcastCID, tmp_packet, net_flags(TRUE, TRUE));
	}
}

void CBurer::StartTeleObjectParticle(CGameObject *pO) 
{
	TParticlesPlayer* PPlayer = pO->GetOrCreateComponent<TParticlesPlayer>();
	PPlayer->StartParticles(particle_tele_object, Fvector().set(0.0f, 0.1f, 0.0f), pO->ID());
}

void CBurer::StopTeleObjectParticle(CGameObject *pO) 
{
	TParticlesPlayer* PPlayer = pO->GetOrCreateComponent<TParticlesPlayer>();
	PPlayer->StopParticles(particle_tele_object, BI_NONE, true);
}

void CBurer::Hit(SHit* pHDS)
{
	if (m_shield_active && pHDS->hit_type == ALife::eHitTypeFireWound && Device.dwFrame != last_hit_frame) 
	{
		if (IsGameTypeSingle())
		{
			// вычислить позицию и направленность партикла
			Fmatrix pos;
			TParticlesPlayer* PPlayer = GetOrCreateComponent<TParticlesPlayer>();
			PPlayer->MakeXFORM(this, pHDS->bone(), pHDS->dir, pHDS->p_in_bone_space, pos);
			// установить particles
			xr_shared_ptr<CParticlesObject> ps = Particles::Details::Create(particle_fire_shield, TRUE);
			ps->UpdateParent(pos, Fvector().set(0.f, 0.f, 0.f));
			GamePersistent().ps_needtoplay.push_back(ps);
		}
		else if (IsGameTypeSingleCompatible()) {

			NET_Packet	tmp_packet;
			CGameObject::u_EventGen(tmp_packet, GE_BURER_SHIELD_HIT, ID());
			//tmp_packet.w_u8(0);
			tmp_packet.w_u16(pHDS->bone());
			tmp_packet.w_vec3(pHDS->dir);
			tmp_packet.w_vec3(pHDS->p_in_bone_space);
			Level().Server->SendBroadcast(BroadcastCID, tmp_packet, net_flags(TRUE, TRUE));

		}
	} 
	else if ( !m_shield_active )
	{
		inherited::Hit								(pHDS);
	}

	last_hit_frame								=	Device.dwFrame;
}

void CBurer::Die(CObject* who)
{
	inherited::Die(who);

	if ( com_man().ta_is_active() )
	{
		com_man().ta_deactivate();
	}

	CTelekinesis::Deactivate();
}

void CBurer::net_Relcase(CObject *O)
{
	inherited::net_Relcase		(O);

	TTelekinesis::remove_links	(O);
}

void CBurer::OnEvent(NET_Packet& P, u16 type)
{
	inherited::OnEvent(P, type);

	switch (type)
	{
	case GE_BURER_GRAVI_PARTICLES:
	{
		u16 target;
		u8 start_particles;
		P.r_u8(start_particles);
		P.r_u16(target);

		CObject* obj = Level().Objects.net_Find(target);
		if (!obj)
			break;

		CActor* pA = obj->cast_actor();
		if (!pA)
			break;

		if (start_particles == 1)
		{
			TParticlesPlayer* PPlayer = GetOrCreateComponent<TParticlesPlayer>();
			PPlayer->StartParticles(particle_gravi_prepare, Fvector().set(0.0f, 0.1f, 0.0f), pA->ID());
		}
		else
		{
			if (Actor())
			{
				TParticlesPlayer* PPlayer = GetOrCreateComponent<TParticlesPlayer>();
				PPlayer->StopParticles(particle_gravi_prepare, BI_NONE, true);
			}
		}

		break;
	}
	case GE_BURER_GRAVI_WAVE:
	{
		u16 target;
		P.r_u16(target);

		if (OnServer())
			break;

		CObject* obj = Level().Objects.net_Find(target);
		if (!obj)
			break;

		CEntityAlive* entity = obj->cast_entity_alive();
		if (!entity)
			break;

		Fvector from_pos;
		Fvector target_pos;
		from_pos = Position();
		from_pos.y += 0.5f;
		target_pos = entity->Position();
		target_pos.y += 0.5f;
		m_gravi_object.activate(entity, from_pos, target_pos);
		break;
	}
	case GE_BURER_SHIELD:
	{
		TParticlesPlayer* PPlayer = GetOrCreateComponent<TParticlesPlayer>();
		PPlayer->StartParticles(m_shield_keep_particle, Fvector().set(0, 1, 0), ID(), -1, true);
		break;
	}
	case GE_BURER_SHIELD_HIT:
	{
		u16 bone;
		Fvector dir;
		Fvector p_in_bone_space;
		P.r_u16(bone);
		P.r_vec3(dir);
		P.r_vec3(p_in_bone_space);

		// вычислить позицию и направленность партикла
		if (bone)
		{
			Fmatrix pos;
			TParticlesPlayer* PPlayer = GetOrCreateComponent<TParticlesPlayer>();
			PPlayer->MakeXFORM(this, bone, dir, p_in_bone_space, pos);
			// установить particles
			xr_shared_ptr<CParticlesObject> ps = Particles::Details::Create(particle_fire_shield, TRUE);
			ps->UpdateParent(pos, Fvector().set(0.f, 0.f, 0.f));
			GamePersistent().ps_needtoplay.push_back(ps);
		}
	}
	}
}

#ifdef DEBUG
CBaseMonster::SDebugInfo CBurer::show_debug_info()
{
	CBaseMonster::SDebugInfo info = inherited::show_debug_info();
	if (!info.active) return CBaseMonster::SDebugInfo();

	string128 text;
	DBG().text(this).add_item(text, info.x, info.y+=info.delta_y, info.color);
	DBG().text(this).add_item("---------------------------------------", info.x, info.y+=info.delta_y, info.delimiter_color);

	return CBaseMonster::SDebugInfo();
}
#endif

void CBurer::StartGraviMP()
{

	Fvector from_pos;
	Fvector target_pos;
	from_pos = Position();
	from_pos.y += 0.5f;
	target_pos = EnemyMan.get_enemy()->Position();
	target_pos.y += 0.5f;
	m_gravi_object.activate(EnemyMan.get_enemy(), from_pos, target_pos);

	NET_Packet	tmp_packet;
	CGameObject::u_EventGen(tmp_packet, GE_BURER_GRAVI_WAVE, ID());
	tmp_packet.w_u16(EnemyMan.get_enemy()->ID());
	Level().Server->SendBroadcast(BroadcastCID, tmp_packet, net_flags(TRUE, TRUE));
}

void CBurer::shieldParticlesMP()
{
	NET_Packet	tmp_packet;
	CGameObject::u_EventGen(tmp_packet, GE_BURER_SHIELD, ID());
	Level().Server->SendBroadcast(BroadcastCID, tmp_packet, net_flags(TRUE, TRUE));
}

void   CBurer::face_enemy ()
{
	if ( !EnemyMan.get_enemy() )
	{
		return;
	}
	Fvector	const	enemy_pos		=	EnemyMan.get_enemy()->Position();
	Fvector	const	self_pos		=	Position();
	Fvector	const	self2enemy		=	enemy_pos - self_pos;
	bool  	const 	good_aiming		=	angle_between_vectors(self2enemy, Direction()) 
										< deg2rad(20.f);

	if ( !good_aiming )
	{
		dir().face_target				(enemy_pos);
	}

	set_action							(ACT_STAND_IDLE);
}

bool actor_is_reloading_weapon() {
	CActor* pActor = Actor();

	if(pActor == nullptr) {
		return false;
	}

	PIItem active_item = pActor->inventory().ActiveItem();

	CWeapon* const active_weapon = active_item != nullptr ? active_item->cast_weapon() : nullptr;
	if (active_weapon && active_weapon->GetState() == CWeapon::eReload)
	{
		return true;
	}

	return false;
}

