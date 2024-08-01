///////////////////////////////////////////////////////////////////////////
//  Базовый класс: Кровосос ТЧ
//	Мутант: Кровосос (Матёрый)
//  Заметка: Использует логику всех трех классов: ЗП/ТЧ/ЧН
////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "bloodsucker_ix.h"
#include "bloodsucker_ix_state_manager.h"
#include "../../../../actor.h"
#include "../../../../ActorEffector.h"
#include "../../../../../Include/xrRender/KinematicsAnimated.h"
#include "../../../../level.h"
#include "../../../../material_manager.h"
#include "bloodsucker_ix_vampire_effector.h"
#include "../../../../detail_path_manager.h"
#include "../../monster_velocity_space.h"
#include "../../../../gamepersistent.h"
#include "../../../../game_object_space.h"

#include "../../control_animation_base.h"
#include "../../control_movement_base.h"
#include "../../control_rotation_jump.h"

#include "../../../../sound_player.h"
#include "../../../../../xrEngine/camerabase.h"
//#include "../../../../xr_level_controller.h"
#include "../../../../ActorCondition.h"

#include "../../../../PHDestroyable.h"
#include "../../../../CharacterPhysicsSupport.h"

u32 CAI_BloodsuckerIX::m_time_last_vampire = 0;

CAI_BloodsuckerIX::CAI_BloodsuckerIX()
{
	StateMan = new CStateManagerBloodsuckerIX(this);
	m_alien_control.init_external(this);

	com_man().add_ability(ControlCom::eControlRunAttack);
	com_man().add_ability(ControlCom::eControlRotationJump);
	com_man().add_ability(ControlCom::eControlThreaten);

	invisible_vel.set(0.1f, 0.1f);

	EnemyMemory.init_external(this, 40000);
}

CAI_BloodsuckerIX::~CAI_BloodsuckerIX()
{
	xr_delete(StateMan);
}

void CAI_BloodsuckerIX::Load(LPCSTR section)
{
	inherited::Load(section);

	anim().AddReplacedAnim(&m_bDamaged, eAnimRun, eAnimRunDamaged);
	anim().AddReplacedAnim(&m_bDamaged, eAnimWalkFwd, eAnimWalkDamaged);
	anim().AddReplacedAnim(&m_bDamaged, eAnimStandIdle, eAnimStandDamaged);
	anim().AddReplacedAnim(&m_bRunTurnLeft, eAnimRun, eAnimRunTurnLeft);
	anim().AddReplacedAnim(&m_bRunTurnRight, eAnimRun, eAnimRunTurnRight);


	anim().accel_load(section);
	anim().accel_chain_add(eAnimWalkFwd, eAnimRun);
	anim().accel_chain_add(eAnimWalkFwd, eAnimRunTurnLeft);
	anim().accel_chain_add(eAnimWalkFwd, eAnimRunTurnRight);
	anim().accel_chain_add(eAnimWalkDamaged, eAnimRunDamaged);


	SVelocityParam& velocity_none = move().get_velocity(MonsterMovement::eVelocityParameterIdle);
	SVelocityParam& velocity_turn = move().get_velocity(MonsterMovement::eVelocityParameterStand);
	SVelocityParam& velocity_walk = move().get_velocity(MonsterMovement::eVelocityParameterWalkNormal);
	SVelocityParam& velocity_run = move().get_velocity(MonsterMovement::eVelocityParameterRunNormal);
	SVelocityParam& velocity_walk_dmg = move().get_velocity(MonsterMovement::eVelocityParameterWalkDamaged);
	SVelocityParam& velocity_run_dmg = move().get_velocity(MonsterMovement::eVelocityParameterRunDamaged);
	SVelocityParam& velocity_steal = move().get_velocity(MonsterMovement::eVelocityParameterSteal);

	anim().AddAnim(eAnimStandIdle, "stand_idle_", -1, &velocity_none, PS_STAND, "fx_run_f", "fx_stand_b", "fx_stand_l", "fx_stand_r");
	anim().AddAnim(eAnimStandDamaged, "stand_damaged_", -1, &velocity_none, PS_STAND, "fx_run_f", "fx_stand_b", "fx_stand_l", "fx_stand_r");
	anim().AddAnim(eAnimStandTurnLeft, "stand_turn_ls_", -1, &velocity_turn, PS_STAND, "fx_run_f", "fx_stand_b", "fx_stand_l", "fx_stand_r");
	anim().AddAnim(eAnimStandTurnRight, "stand_turn_rs_", -1, &velocity_turn, PS_STAND, "fx_run_f", "fx_stand_b", "fx_stand_l", "fx_stand_r");
	anim().AddAnim(eAnimSleep, "lie_sleep_", -1, &velocity_none, PS_LIE, "fx_run_f", "fx_stand_b", "fx_stand_l", "fx_stand_r");
	anim().AddAnim(eAnimWalkFwd, "stand_walk_fwd_", -1, &velocity_walk, PS_STAND, "fx_run_f", "fx_stand_b", "fx_stand_l", "fx_stand_r");
	anim().AddAnim(eAnimWalkDamaged, "stand_walk_fwd_dmg_", -1, &velocity_walk_dmg, PS_STAND, "fx_run_f", "fx_stand_b", "fx_stand_l", "fx_stand_r");
	anim().AddAnim(eAnimRun, "stand_run_", -1, &velocity_run, PS_STAND, "fx_run_f", "fx_stand_b", "fx_stand_l", "fx_stand_r");
	anim().AddAnim(eAnimRunDamaged, "stand_run_dmg_", -1, &velocity_run_dmg, PS_STAND, "fx_run_f", "fx_stand_b", "fx_stand_l", "fx_stand_r");

	anim().AddAnim(eAnimRunTurnLeft, "stand_run_turn_left_", -1, &velocity_run, PS_STAND, "fx_run_f", "fx_stand_b", "fx_stand_l", "fx_stand_r");
	anim().AddAnim(eAnimRunTurnRight, "stand_run_turn_right_", -1, &velocity_run, PS_STAND, "fx_run_f", "fx_stand_b", "fx_stand_l", "fx_stand_r");
	anim().AddAnim(eAnimScared, "stand_scared_", -1, &velocity_none, PS_STAND, "fx_run_f", "fx_stand_b", "fx_stand_l", "fx_stand_r");

	anim().AddAnim(eAnimCheckCorpse, "stand_check_corpse_", -1, &velocity_none, PS_STAND, "fx_run_f", "fx_stand_b", "fx_stand_l", "fx_stand_r");
	anim().AddAnim(eAnimEat, "sit_eat_", -1, &velocity_none, PS_SIT, "fx_run_f", "fx_stand_b", "fx_stand_l", "fx_stand_r");

	anim().AddAnim(eAnimDie, "stand_idle_", -1, &velocity_none, PS_STAND, "fx_run_f", "fx_stand_b", "fx_stand_l", "fx_stand_r");

	anim().AddAnim(eAnimAttack, "stand_attack_", -1, &velocity_turn, PS_STAND, "fx_run_f", "fx_stand_b", "fx_stand_l", "fx_stand_r");
	anim().AddAnim(eAnimAttackRun, "stand_attack_run_", -1, &velocity_run, PS_STAND, "fx_run_f", "fx_stand_b", "fx_stand_l", "fx_stand_r");

	anim().AddAnim(eAnimLookAround, "stand_look_around_", -1, &velocity_none, PS_STAND, "fx_run_f", "fx_stand_b", "fx_stand_l", "fx_stand_r");
	anim().AddAnim(eAnimSitIdle, "sit_idle_", -1, &velocity_none, PS_SIT, "fx_run_f", "fx_stand_b", "fx_stand_l", "fx_stand_r");
	anim().AddAnim(eAnimSitStandUp, "sit_stand_up_", -1, &velocity_none, PS_SIT, "fx_run_f", "fx_stand_b", "fx_stand_l", "fx_stand_r");
	anim().AddAnim(eAnimSitToSleep, "sit_sleep_down_", -1, &velocity_none, PS_SIT, "fx_run_f", "fx_stand_b", "fx_stand_l", "fx_stand_r");
	anim().AddAnim(eAnimStandSitDown, "stand_sit_down_", -1, &velocity_none, PS_STAND, "fx_run_f", "fx_stand_b", "fx_stand_l", "fx_stand_r");

	anim().AddAnim(eAnimSteal, "stand_steal_", -1, &velocity_steal, PS_STAND, "fx_run_f", "fx_stand_b", "fx_stand_l", "fx_stand_r");

	anim().AddAnim(eAnimThreaten, "stand_threaten_", -1, &velocity_none, PS_STAND, "fx_run_f", "fx_stand_b", "fx_stand_l", "fx_stand_r");
	anim().AddAnim(eAnimMiscAction_00, "stand_to_aggressive_", -1, &velocity_none, PS_STAND, "fx_run_f", "fx_stand_b", "fx_stand_l", "fx_stand_r");

	// define transitions
	//	anim().AddTransition(PS_STAND,			eAnimThreaten,	eAnimMiscAction_00,	false);
	anim().AddTransition(eAnimStandSitDown, eAnimSleep, eAnimSitToSleep, false);
	anim().AddTransition(PS_STAND, eAnimSleep, eAnimStandSitDown, true);
	anim().AddTransition(PS_STAND, PS_SIT, eAnimStandSitDown, false);
	anim().AddTransition(PS_STAND, PS_LIE, eAnimStandSitDown, false);
	anim().AddTransition(PS_SIT, PS_STAND, eAnimSitStandUp, false);
	anim().AddTransition(PS_LIE, PS_STAND, eAnimSitStandUp, false);

	// define links from Action to animations
	anim().LinkAction(ACT_STAND_IDLE, eAnimStandIdle);
	anim().LinkAction(ACT_SIT_IDLE, eAnimSitIdle);
	anim().LinkAction(ACT_LIE_IDLE, eAnimSitIdle);
	anim().LinkAction(ACT_WALK_FWD, eAnimWalkFwd);
	anim().LinkAction(ACT_WALK_BKWD, eAnimWalkBkwd);
	anim().LinkAction(ACT_RUN, eAnimRun);
	anim().LinkAction(ACT_EAT, eAnimEat);
	anim().LinkAction(ACT_SLEEP, eAnimSleep);
	anim().LinkAction(ACT_REST, eAnimSitIdle);
	anim().LinkAction(ACT_DRAG, eAnimWalkBkwd);
	anim().LinkAction(ACT_ATTACK, eAnimAttack);
	anim().LinkAction(ACT_STEAL, eAnimSteal);
	anim().LinkAction(ACT_LOOK_AROUND, eAnimLookAround);

	m_hits_before_vampire = 0;

	// load other misc stuff
	invisible_vel.set(pSettings->r_float(section, "Velocity_Invisible_Linear"), pSettings->r_float(section, "Velocity_Invisible_Angular"));
	movement().detail().add_velocity(MonsterMovement::eVelocityParameterInvisible, CDetailPathManager::STravelParams(invisible_vel.linear, invisible_vel.angular));

	LoadVampirePPEffector(pSettings->r_string(section, "vampire_effector"));
	m_vampire_min_delay = pSettings->r_u32(section, "Vampire_Delay");

	m_visual_predator = pSettings->r_string(section, "Predator_Visual");

	m_vampire_want_speed = pSettings->r_float(section, "Vampire_Want_Speed");
	m_vampire_wound = pSettings->r_float(section, "Vampire_Wound");
	m_vampire_gain_health = pSettings->r_float(section, "Vampire_GainHealth");
	m_vampire_distance = pSettings->r_float(section, "Vampire_Distance");
	m_sufficient_hits_before_vampire = pSettings->r_u32(section, "Vampire_Sufficient_Hits");
	m_sufficient_hits_before_vampire_random = -1 + (rand() % 3);

	invisible_particle_name = pSettings->r_string(section, "Particle_Invisible");
}

void CAI_BloodsuckerIX::reinit()
{
	inherited::reinit();
	CControlledActor::reinit();

	if (CCustomMonster::use_simplified_visual())	return;

	Bones.Reset();

	com_man().ta_fill_data(anim_triple_vampire, "vampire_0", "vampire_1", "vampire_2", TA_EXECUTE_LOOPED, TA_DONT_SKIP_PREPARE, 0);

	start_threaten = false;
	com_man().set_threaten_data("stand_threaten_0", 0.63f);

	m_alien_control.reinit();

	state_invisible = false;

	com_man().add_rotation_jump_data("run_turn_l_0", "run_turn_l_1", "run_turn_r_0", "run_turn_r_1", PI_DIV_2);

	// save visual	
	m_visual_default = cNameVisual();

	m_vampire_want_value = 0.f;
	m_threaten_time = 0;
	m_predator = false;
}

void CAI_BloodsuckerIX::reload(LPCSTR section)
{
	inherited::reload(section);

	sound().add(pSettings->r_string(section, "Sound_Vampire_Grasp"), DEFAULT_SAMPLE_COUNT, SOUND_TYPE_MONSTER_ATTACKING,
		MonsterSound::eHighPriority + 4, MonsterSound::eBaseChannel, eVampireGrasp, "bip01_head");
	sound().add(pSettings->r_string(section, "Sound_Vampire_Sucking"), DEFAULT_SAMPLE_COUNT,
		SOUND_TYPE_MONSTER_ATTACKING, MonsterSound::eHighPriority + 3, MonsterSound::eBaseChannel, eVampireSucking,
		"bip01_head");
	sound().add(pSettings->r_string(section, "Sound_Vampire_Hit"), DEFAULT_SAMPLE_COUNT, SOUND_TYPE_MONSTER_ATTACKING,
		MonsterSound::eHighPriority + 2, MonsterSound::eBaseChannel, eVampireHit, "bip01_head");
	sound().add(pSettings->r_string(section, "Sound_Vampire_StartHunt"), DEFAULT_SAMPLE_COUNT,
		SOUND_TYPE_MONSTER_ATTACKING, MonsterSound::eHighPriority + 5, MonsterSound::eBaseChannel, eVampireStartHunt,
		"bip01_head");

	sound().add(pSettings->r_string(section, "Sound_Invisibility_Change_State"), DEFAULT_SAMPLE_COUNT, SOUND_TYPE_MONSTER_ATTACKING, MonsterSound::eNormalPriority, MonsterSound::eChannelIndependent << 1, eChangeVisibility, "bip01_head");
	sound().add(pSettings->r_string(section, "Sound_Growl"), DEFAULT_SAMPLE_COUNT, SOUND_TYPE_MONSTER_ATTACKING, MonsterSound::eHighPriority + 6, MonsterSound::eBaseChannel, eGrowl, "bip01_head");
	sound().add(pSettings->r_string(section, "Sound_Alien"), DEFAULT_SAMPLE_COUNT, SOUND_TYPE_MONSTER_ATTACKING, MonsterSound::eCriticalPriority, u32(MonsterSound::eCaptureAllChannels), eAlien, "bip01_head");
}

void CAI_BloodsuckerIX::LoadVampirePPEffector(LPCSTR section)
{
	pp_vampire_effector.duality.h = pSettings->r_float(section, "duality_h");
	pp_vampire_effector.duality.v = pSettings->r_float(section, "duality_v");
	pp_vampire_effector.gray = pSettings->r_float(section, "gray");
	pp_vampire_effector.blur = pSettings->r_float(section, "blur");
	pp_vampire_effector.noise.intensity = pSettings->r_float(section, "noise_intensity");
	pp_vampire_effector.noise.grain = pSettings->r_float(section, "noise_grain");
	pp_vampire_effector.noise.fps = pSettings->r_float(section, "noise_fps");
	VERIFY(!fis_zero(pp_vampire_effector.noise.fps));

	sscanf(pSettings->r_string(section, "color_base"), "%f,%f,%f", &pp_vampire_effector.color_base.r,
		&pp_vampire_effector.color_base.g, &pp_vampire_effector.color_base.b);
	sscanf(pSettings->r_string(section, "color_gray"), "%f,%f,%f", &pp_vampire_effector.color_gray.r,
		&pp_vampire_effector.color_gray.g, &pp_vampire_effector.color_gray.b);
	sscanf(pSettings->r_string(section, "color_add"), "%f,%f,%f", &pp_vampire_effector.color_add.r,
		&pp_vampire_effector.color_add.g, &pp_vampire_effector.color_add.b);
}

void CAI_BloodsuckerIX::ActivateVampireEffector()
{
	Actor()->Cameras().AddCamEffector(
		new CVampireCameraEffectorsoc(6.0f, get_head_position(this), get_head_position(Actor())));
	Actor()->Cameras().AddPPEffector(new CVampirePPEffectsocor(pp_vampire_effector, 6.0f));
}

bool CAI_BloodsuckerIX::WantVampire() { return !!fsimilar(m_vampire_want_value, 1.f); }

void CAI_BloodsuckerIX::SatisfyVampire()
{
	m_vampire_want_value = 0.f;

	float health = conditions().GetHealth();
	health += m_vampire_gain_health;

	health = std::min(health, conditions().GetMaxHealth());
	conditions().SetHealth(health);
}

void  CAI_BloodsuckerIX::BoneCallback(CBoneInstance* B)
{
	CAI_BloodsuckerIX* this_class = static_cast<CAI_BloodsuckerIX*> (B->callback_param());

	this_class->Bones.Update(B, Device.dwTimeGlobal);
}

void CAI_BloodsuckerIX::vfAssignBones()
{
	// Установка callback на кости

	bone_spine = &smart_cast<IKinematics*>(Visual())->LL_GetBoneInstance(smart_cast<IKinematics*>(Visual())->LL_BoneID("bip01_spine"));
	bone_head = &smart_cast<IKinematics*>(Visual())->LL_GetBoneInstance(smart_cast<IKinematics*>(Visual())->LL_BoneID("bip01_head"));
	if (!PPhysicsShell())//нельзя ставить колбеки, если создан физ шел - у него стоят свои колбеки!!!
	{
		bone_spine->set_callback(bctCustom, BoneCallback, this);
		bone_head->set_callback(bctCustom, BoneCallback, this);
	}

	// Bones settings
	Bones.Reset();
	Bones.AddBone(bone_spine, AXIS_X);	Bones.AddBone(bone_spine, AXIS_Y);
	Bones.AddBone(bone_head, AXIS_X);	Bones.AddBone(bone_head, AXIS_Y);
}

void CAI_BloodsuckerIX::CheckSpecParams(u32 spec_params)
{
	if ((spec_params & ASP_CHECK_CORPSE) == ASP_CHECK_CORPSE) {
		com_man().seq_run(anim().get_motion_id(eAnimCheckCorpse));
	}

	if ((spec_params & ASP_THREATEN) == ASP_THREATEN) {
		anim().SetCurAnim(eAnimThreaten);
		return;
	}

	if ((spec_params & ASP_STAND_SCARED) == ASP_STAND_SCARED) {
		if (Random.randI(100) < 60)
			anim().SetCurAnim(eAnimLookAround);
		else
			anim().SetCurAnim(eAnimScared);
		return;
	}
}

BOOL CAI_BloodsuckerIX::net_Spawn(CSE_Abstract* DC)
{
	if (!inherited::net_Spawn(DC))
		return(FALSE);

	vfAssignBones();

	return(TRUE);
}

void CAI_BloodsuckerIX::UpdateCL()
{
	inherited::UpdateCL();
	CControlledActor::frame_update();

	if (g_Alive())
	{
		// update vampire need
		m_vampire_want_value += m_vampire_want_speed * client_update_fdelta();
		clamp(m_vampire_want_value, 0.f, 1.f);
	}

	if (m_threaten_time + 1670 < Device.dwTimeGlobal)
		m_threaten_time = 0;
}

bool CAI_BloodsuckerIX::done_enough_hits_before_vampire()
{
	return (int)m_hits_before_vampire >=
		(int)m_sufficient_hits_before_vampire + m_sufficient_hits_before_vampire_random;
}

void CAI_BloodsuckerIX::on_attack_on_run_hit() { ++m_hits_before_vampire; }

void CAI_BloodsuckerIX::shedule_Update(u32 dt)
{
	inherited::shedule_Update(dt);

	if (!g_Alive())	setVisible(TRUE);

	if (m_alien_control.active())	sound().play(eAlien);
}

void CAI_BloodsuckerIX::Die(CObject* who)
{
	predator_stop				();
	inherited::Die				(who);
}

void CAI_BloodsuckerIX::post_fsm_update()
{
	inherited::post_fsm_update();
}

bool CAI_BloodsuckerIX::check_start_conditions(ControlCom::EControlType type)
{
	if (!inherited::check_start_conditions(type))	return false;

	if (type == ControlCom::eControlRunAttack)
		return (!state_invisible);

	if (type == ControlCom::eControlThreaten) {
		if (!start_threaten) return false;

		start_threaten = false;

		m_threaten_time = Device.dwTimeGlobal;

		if (Random.randI(100) < 70) return false;

	}

	return true;
}

void CAI_BloodsuckerIX::set_alien_control(bool val)
{
	val ? m_alien_control.activate() : m_alien_control.deactivate();
}

void CAI_BloodsuckerIX::predator_start()
{
	if (m_predator)					return;
	cNameVisual_set(m_visual_predator);
	CDamageManager::reload(*cNameSect(), "damage", pSettings);

	control().animation().restart();

	CParticlesPlayer::StartParticles(invisible_particle_name, Fvector().set(0.0f, 0.1f, 0.0f), ID());
	sound().play(CAI_BloodsuckerIX::eChangeVisibility);

	m_predator = true;
	state_invisible = false;
}

void CAI_BloodsuckerIX::predator_stop()
{
	if (!m_predator)				return;

	cNameVisual_set(*m_visual_default);
	character_physics_support()->in_ChangeVisual();

	CDamageManager::reload(*cNameSect(), "damage", pSettings);

	control().animation().restart();

	CParticlesPlayer::StartParticles(invisible_particle_name, Fvector().set(0.0f, 0.1f, 0.0f), ID());
	sound().play(CAI_BloodsuckerIX::eChangeVisibility);
	m_predator = false;
}

void CAI_BloodsuckerIX::predator_freeze()
{
	control().animation().freeze();
}

void CAI_BloodsuckerIX::predator_unfreeze()
{
	control().animation().unfreeze();
}

void CAI_BloodsuckerIX::move_actor_cam()
{
	float turn_angle = PI_DIV_3;
	if (Actor()->cam_Active()) {
		Actor()->cam_Active()->Move(Random.randI(2) ? kRIGHT : kLEFT, turn_angle);	//Random.randF(turn_angle)); 
		Actor()->cam_Active()->Move(Random.randI(2) ? kUP : kDOWN, turn_angle);	//Random.randF(turn_angle)); 
	}
}

void CAI_BloodsuckerIX::HitEntity(const CEntity* pEntity, float fDamage, float impulse, Fvector& dir)
{
	inherited::HitEntity(pEntity, fDamage, impulse, dir);

	EMonsterState state = StateMan->get_state_type();
}

void CAI_BloodsuckerIX::start_invisible_predator()
{
	state_invisible = true;
	predator_start();
}

void CAI_BloodsuckerIX::stop_invisible_predator()
{
	state_invisible = false;
	predator_stop();
}

void CAI_BloodsuckerIX::manual_activate()
{
	state_invisible = true;
	setVisible(FALSE);
}

void CAI_BloodsuckerIX::manual_deactivate()
{
	state_invisible = false;
	setVisible(TRUE);
}

void CAI_BloodsuckerIX::on_activate_control(ControlCom::EControlType type)
{
	if (type == ControlCom::eControlThreaten) {
		sound().play(MonsterSound::eMonsterSoundThreaten);
	}
}
