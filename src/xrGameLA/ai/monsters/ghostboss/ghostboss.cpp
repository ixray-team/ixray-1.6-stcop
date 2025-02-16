#include "stdafx.h"
#include "ghostboss.h"
#include "../../../PhysicsShell.h"
#include "../../../characterphysicssupport.h"
#include "../../../actor.h"
#include "ghostboss_state_manager.h"
#include "../../../../Include/xrRender/KinematicsAnimated.h"
#include "../../../sound_player.h"
#include "../../../level.h"
#include "../../../ai_monster_space.h"
#include "../../../level_debug.h"
#include "../monster_velocity_space.h"
#include "../../../GamePersistent.h"
#include "../control_animation_base.h"
#include "../control_movement_base.h"
#include "ghostboss_fast_gravi.h"
#include "../control_path_builder_base.h"
#include "../../../restricted_object.h"
#include "../../../detail_path_manager.h"

bool CGhostBoss::can_scan = true;

CGhostBoss::CGhostBoss()
{
	StateMan = new CStateManagerGhostBoss(this);
	TScanner::init_external(this);

	m_fast_gravi		= new CGhostBossFastGravi();
	control().add		(m_fast_gravi,  ControlCom::eComCustom1);

}

CGhostBoss::~CGhostBoss()
{
	xr_delete(StateMan);
	xr_delete(m_fast_gravi);
}


void CGhostBoss::reinit()
{
	inherited::reinit			();
	TScanner::reinit			();

	DeactivateShield();

	time_last_scan				= 0;
}

void CGhostBoss::net_Destroy()
{
	inherited::net_Destroy();
	TScanner::on_destroy();
}

void CGhostBoss::reload(LPCSTR section)
{
	inherited::reload	(section);

	// add specific sounds
	sound().add			(pSettings->r_string(section,"sound_gravi_attack"),	DEFAULT_SAMPLE_COUNT,	SOUND_TYPE_MONSTER_ATTACKING,	MonsterSound::eHighPriority + 2,	u32(MonsterSound::eBaseChannel),	eMonsterSoundGraviAttack, "bip01_head");
	sound().add			(pSettings->r_string(section,"sound_tele_attack"),	DEFAULT_SAMPLE_COUNT,	SOUND_TYPE_MONSTER_ATTACKING,	MonsterSound::eHighPriority + 3,	u32(MonsterSound::eBaseChannel),	eMonsterSoundTeleAttack, "bip01_head");

	// add triple animations
	com_man().ta_fill_data(anim_triple_gravi,	"stand_gravi_0",	"stand_gravi_1",	"stand_gravi_2",	TA_EXECUTE_ONCE, TA_DONT_SKIP_PREPARE, ControlCom::eCapturePath | ControlCom::eCaptureMovement);
	com_man().ta_fill_data(anim_triple_tele,	"stand_tele_0",		"stand_tele_1",		"stand_tele_2",		TA_EXECUTE_ONCE, TA_DONT_SKIP_PREPARE, ControlCom::eCapturePath | ControlCom::eCaptureMovement);
	com_man().ta_fill_data(anim_triple_shield,	"idle_to_shield_0",		"shield_0",		"shield_0_idle_0",		TA_EXECUTE_LOOPED, TA_DONT_SKIP_PREPARE, ControlCom::eCapturePath | ControlCom::eCaptureMovement);
}

void CGhostBoss::Load(LPCSTR section)
{
	inherited::Load				(section);
	TScanner::load				(section);

	anim().AddReplacedAnim(&m_bDamaged, eAnimStandIdle,		eAnimStandDamaged);	
	anim().AddReplacedAnim(&m_bDamaged, eAnimRun,			eAnimRunDamaged);
	anim().AddReplacedAnim(&m_bDamaged, eAnimWalkFwd,		eAnimWalkDamaged);
	
	anim().accel_load			(section);
	anim().accel_chain_add		(eAnimWalkFwd,		eAnimRun);

	particle_gravi_wave		= pSettings->r_string(section,"Particle_Gravi_Wave");
	particle_gravi_prepare	= pSettings->r_string(section,"Particle_Gravi_Prepare");
	particle_tele_object	= pSettings->r_string(section,"Particle_Tele_Object");
	
	::Sound->create(sound_gravi_wave,	pSettings->r_string(section,"sound_gravi_wave"),st_Effect,SOUND_TYPE_WORLD);
	::Sound->create(sound_tele_hold,	pSettings->r_string(section,"sound_tele_hold"),	st_Effect,SOUND_TYPE_WORLD);
	::Sound->create(sound_tele_throw,	pSettings->r_string(section,"sound_tele_throw"),st_Effect,SOUND_TYPE_WORLD);

	m_gravi_speed					= pSettings->r_u32(section,"Gravi_Speed");
	m_gravi_step					= pSettings->r_u32(section,"Gravi_Step");
	m_gravi_time_to_hold			= pSettings->r_u32(section,"Gravi_Time_To_Hold");
	m_gravi_radius					= pSettings->r_float(section,"Gravi_Radius");
	m_gravi_impulse_to_objects		= pSettings->r_float(section,"Gravi_Impulse_To_Objects");
	m_gravi_impulse_to_enemy		= pSettings->r_float(section,"Gravi_Impulse_To_Enemy");
	m_gravi_hit_power				= pSettings->r_float(section,"Gravi_Hit_Power");
	
	m_tele_max_handled_objects		= pSettings->r_u32(section,"Tele_Max_Handled_Objects");
	m_tele_time_to_hold				= pSettings->r_u32(section,"Tele_Time_To_Hold");
	m_tele_object_min_mass			= pSettings->r_float(section,"Tele_Object_Min_Mass");
	m_tele_object_max_mass			= pSettings->r_float(section,"Tele_Object_Max_Mass");
	m_tele_find_radius				= pSettings->r_float(section,"Tele_Find_Radius");

	m_shield_cooldown				= READ_IF_EXISTS(pSettings,r_u32,section,"shield_cooldown",4000);
	m_shield_time					= READ_IF_EXISTS(pSettings,r_u32,section,"shield_time",3000);
	m_shield_keep_particle			= READ_IF_EXISTS(pSettings,r_string,section,"shield_keep_particle",0);
	m_shield_keep_particle_period	= READ_IF_EXISTS(pSettings,r_u32,section,"shield_keep_particle_period",1000);
	particle_fire_shield			= pSettings->r_string(section,"Particle_Shield");

	m_teleport_step					= pSettings->r_u32(section,"Teleport_Step");
	m_teleport_length				= pSettings->r_u32(section,"Teleport_Length");
	m_teleport_particle				= pSettings->r_string(section,"Teleport_Particle");
	
	//m_teleport_step					+= Random.randI(m_teleport_step/5) - m_teleport_step/10
	
	SVelocityParam &velocity_none		= move().get_velocity(MonsterMovement::eVelocityParameterIdle);	
	SVelocityParam &velocity_turn		= move().get_velocity(MonsterMovement::eVelocityParameterStand);
	SVelocityParam &velocity_walk		= move().get_velocity(MonsterMovement::eVelocityParameterWalkNormal);
	SVelocityParam &velocity_run		= move().get_velocity(MonsterMovement::eVelocityParameterRunNormal);
	SVelocityParam &velocity_walk_dmg	= move().get_velocity(MonsterMovement::eVelocityParameterWalkDamaged);
	SVelocityParam &velocity_run_dmg	= move().get_velocity(MonsterMovement::eVelocityParameterRunDamaged);
	SVelocityParam &velocity_steal		= move().get_velocity(MonsterMovement::eVelocityParameterSteal);
//		SVelocityParam &velocity_drag		= move().get_velocity(MonsterMovement::eVelocityParameterDrag);

	anim().AddAnim(eAnimStandIdle,		"stand_idle_",			-1, &velocity_none,				PS_STAND, 	"fx_stand_f", "fx_stand_b", "fx_stand_l", "fx_stand_r");
	anim().AddAnim(eAnimStandTurnLeft,	"stand_turn_ls_",		-1, &velocity_turn,			PS_STAND, 	"fx_stand_f", "fx_stand_b", "fx_stand_l", "fx_stand_r");
	anim().AddAnim(eAnimStandTurnRight,	"stand_turn_rs_",		-1, &velocity_turn,			PS_STAND, 	"fx_stand_f", "fx_stand_b", "fx_stand_l", "fx_stand_r");
	anim().AddAnim(eAnimStandDamaged,	"stand_idle_dmg_",		-1, &velocity_none,				PS_STAND, 	"fx_stand_f", "fx_stand_b", "fx_stand_l", "fx_stand_r");

	anim().AddAnim(eAnimWalkFwd,		"stand_walk_fwd_",		-1, &velocity_walk,		PS_STAND, 	"fx_stand_f", "fx_stand_b", "fx_stand_l", "fx_stand_r");
	anim().AddAnim(eAnimWalkDamaged,	"stand_walk_fwd_dmg_",	-1, &velocity_walk_dmg,	PS_STAND, 	"fx_stand_f", "fx_stand_b", "fx_stand_l", "fx_stand_r");
	anim().AddAnim(eAnimRun,			"stand_run_fwd_",		-1,	&velocity_run,		PS_STAND, 	"fx_stand_f", "fx_stand_b", "fx_stand_l", "fx_stand_r");
	anim().AddAnim(eAnimRunDamaged,		"stand_run_dmg_",		-1,	&velocity_run_dmg,		PS_STAND, 	"fx_stand_f", "fx_stand_b", "fx_stand_l", "fx_stand_r");

	anim().AddAnim(eAnimAttack,			"stand_attack_",		-1, &velocity_turn,			PS_STAND, 	"fx_stand_f", "fx_stand_b", "fx_stand_l", "fx_stand_r");

	anim().AddAnim(eAnimDie,			"stand_die_",			-1, &velocity_none,				PS_STAND, 	"fx_stand_f", "fx_stand_b", "fx_stand_l", "fx_stand_r");

	anim().AddAnim(eAnimScared,			"stand_scared_",		-1, &velocity_none,				PS_STAND, 	"fx_stand_f", "fx_stand_b", "fx_stand_l", "fx_stand_r");
	anim().AddAnim(eAnimSteal,			"stand_steal_",			-1, &velocity_steal,				PS_STAND, 	"fx_stand_f", "fx_stand_b", "fx_stand_l", "fx_stand_r");
	anim().AddAnim(eAnimEat,			"sit_eat_",				-1, &velocity_none,				PS_SIT, 	"fx_stand_f", "fx_stand_b", "fx_stand_l", "fx_stand_r");	

	anim().AddAnim(eAnimSitIdle,		"sit_idle_",			-1, &velocity_none,				PS_SIT, 	"fx_stand_f", "fx_stand_b", "fx_stand_l", "fx_stand_r");
	anim().AddAnim(eAnimCheckCorpse,	"sit_check_corpse_",	-1, &velocity_none,				PS_SIT, 	"fx_stand_f", "fx_stand_b", "fx_stand_l", "fx_stand_r");
	anim().AddAnim(eAnimSitStandUp,		"sit_stand_up_",		-1, &velocity_none,				PS_SIT, 	"fx_stand_f", "fx_stand_b", "fx_stand_l", "fx_stand_r");	
	anim().AddAnim(eAnimStandSitDown,	"stand_sit_down_",		-1, &velocity_none,				PS_STAND, 	"fx_stand_f", "fx_stand_b", "fx_stand_l", "fx_stand_r");

	anim().AddTransition(PS_SIT,		PS_STAND,		eAnimSitStandUp,	false);
	anim().AddTransition(PS_STAND,		PS_SIT,			eAnimStandSitDown,	false);

	anim().LinkAction(ACT_STAND_IDLE,	eAnimStandIdle);
	anim().LinkAction(ACT_SIT_IDLE,		eAnimSitIdle);
	anim().LinkAction(ACT_LIE_IDLE,		eAnimSitIdle);
	anim().LinkAction(ACT_WALK_FWD,		eAnimWalkFwd);
	anim().LinkAction(ACT_WALK_BKWD,	eAnimWalkFwd);
	anim().LinkAction(ACT_RUN,			eAnimRun);
	anim().LinkAction(ACT_EAT,			eAnimEat);
	anim().LinkAction(ACT_SLEEP,		eAnimSitIdle);
	anim().LinkAction(ACT_REST,			eAnimSitIdle);
	anim().LinkAction(ACT_DRAG,			eAnimWalkFwd);
	anim().LinkAction(ACT_ATTACK,		eAnimAttack);
	anim().LinkAction(ACT_STEAL,		eAnimSteal);
	anim().LinkAction(ACT_LOOK_AROUND,	eAnimScared);

#ifdef DEBUG	
	anim().accel_chain_test		();
#endif

}

void CGhostBoss::shedule_Update(u32 dt)
{
	inherited::shedule_Update		(dt);

	TTelekinesis::schedule_update	();
	TScanner::schedule_update		();

	(!EnemyMan.get_enemy()) ? TScanner::enable() : TScanner::disable();
}

void CGhostBoss::CheckSpecParams(u32 spec_params)
{
}

void CGhostBoss::UpdateGraviObject()
{
	if (!m_gravi_object.active) return;
	
	if (!m_gravi_object.enemy || (m_gravi_object.enemy && m_gravi_object.enemy->getDestroy())) {
		m_gravi_object.deactivate();
		return;
	}

	if (m_gravi_object.from_pos.distance_to(m_gravi_object.cur_pos) > (m_gravi_object.from_pos.distance_to(m_gravi_object.target_pos))) {
		m_gravi_object.deactivate();
		return;
	}

	float dt = float(Device.dwTimeGlobal - m_gravi_object.time_last_update);
	float dist = dt * float(m_gravi_speed)/1000.f;
		
	if (dist < m_gravi_step) return;
	
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

	float trace_dist = float(m_gravi_step);

	collide::rq_result	l_rq;
	if (Level().ObjectSpace.RayPick(new_pos, dir, trace_dist, collide::rqtBoth, l_rq, nullptr)) {
		const CObject *enemy = smart_cast<const CObject *>(m_gravi_object.enemy);
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

				HitEntity(m_gravi_object.enemy, m_gravi_hit_power, m_gravi_impulse_to_enemy, impulse_dir);
				m_gravi_object.deactivate();
				return;
			}
		}
	}
																								
	m_gravi_object.cur_pos				= new_pos;
	m_gravi_object.time_last_update		= Device.dwTimeGlobal;

	// ---------------------------------------------------------------------
	// draw particle
	CParticlesObject* ps = CParticlesObject::Create(particle_gravi_wave,TRUE);

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
	m_nearest.clear		();
	Level().ObjectSpace.GetNearest	(m_nearest,m_gravi_object.cur_pos, m_gravi_radius, nullptr); 
	//xr_vector<CObject*> &m_nearest = Level().ObjectSpace.q_nearest;

	for (u32 i=0;i<m_nearest.size();i++) {
		CPhysicsShellHolder  *obj = smart_cast<CPhysicsShellHolder *>(m_nearest[i]);
		if (!obj || !obj->m_pPhysicsShell) continue;
		
		Fvector dir;
		dir.sub(obj->Position(), m_gravi_object.cur_pos);
		dir.normalize();
		obj->m_pPhysicsShell->applyImpulse(dir,m_gravi_impulse_to_objects * obj->m_pPhysicsShell->getMass());
	}

	// играть звук
	Fvector snd_pos = m_gravi_object.cur_pos;
	snd_pos.y += 0.5f;
	if (sound_gravi_wave._feedback())		{
		sound_gravi_wave.set_position	(snd_pos);
	} else ::Sound->play_at_pos			(sound_gravi_wave,0,snd_pos);
}

void CGhostBoss::UpdateCL()
{
	inherited::UpdateCL();
	TScanner::frame_update(Device.dwTimeDelta);

	UpdateGraviObject();

	
	//if (m_fast_gravi->check_start_conditions()) 
	//	control().activate(ControlCom::eComCustom1);

}


bool CGhostBoss::CanTeleport()
{
	Fvector position;
	return GetTeleportPosition(position);
}

bool CGhostBoss::GetTeleportPosition(Fvector &position)
{
	Fvector	dir;
	position = Position();
	u32 node = u32(-1);

	dir.sub						(Actor()->Position(), position);
	dir.normalize_safe			();
	float distance = Actor()->Position().distance_to_xz(Position());
	distance += std::max(2.0f, distance/2);

	position.mad				(Actor()->Position(), dir, distance);

	// проверить позицию на accessible
	if(!movement().restrictions().accessible(position)) {
		//node = movement().restrictions().accessible_nearest(Fvector().set(position), position);
	} else node = 1;
	
	if (node==u32(-1)) return false;
	return true;
}

void CGhostBoss::ActivateTeleport()
{
	setVisible(false);
	CParticlesPlayer::StartParticles(m_teleport_particle,Fvector().set(0.0f,0.1f,0.0f),ID());
//	CSE_ALifeDynamicObject* O = ai().alife().objects().object(ID(),true);
//	O->m_bDirectControl = false;

	Fvector pos;
	GetTeleportPosition(pos);


	Fmatrix F = XFORM();
	F.c = pos;
	movement().detail().make_inactual();
	if (animation_movement_controlled())
		destroy_anim_mov_ctrl();

	if(!g_Alive())				return;
	XFORM().set					(F);
	if(character_physics_support()->movement()->CharacterExist()) character_physics_support()->movement()->EnableCharacter	();
	character_physics_support()->set_movement_position( F.c );
	character_physics_support()->movement()->SetVelocity		(0,0,0);
}

void CGhostBoss::DeactiveTeleport()
{
	setVisible(true);

	//CParticlesPlayer::StartParticles(m_teleport_particle,Fvector().set(0.0f,0.1f,0.0f),ID());
//	CSE_ALifeDynamicObject* O = ai().alife().objects().object(ID(),true);
//	O->m_bDirectControl = false;
}

void CGhostBoss::StartGraviPrepare() 
{
	const CEntityAlive *enemy = EnemyMan.get_enemy();
	if (!enemy) return;
	
	CActor *pA = const_cast<CActor *>(smart_cast<const CActor*>(enemy));
	if (!pA) return;

	pA->CParticlesPlayer::StartParticles(particle_gravi_prepare,Fvector().set(0.0f,0.1f,0.0f),pA->ID());
}
void CGhostBoss::StopGraviPrepare() 
{
	CActor *pA = smart_cast<CActor*>(Level().CurrentEntity());
	if (!pA) return;

	pA->CParticlesPlayer::StopParticles(particle_gravi_prepare, BI_NONE, true);
}

void CGhostBoss::StartTeleObjectParticle(CGameObject *pO) 
{
	CParticlesPlayer* PP = smart_cast<CParticlesPlayer*>(pO);
	if(!PP) return;
	PP->StartParticles(particle_tele_object,Fvector().set(0.0f,0.1f,0.0f),pO->ID());
}
void CGhostBoss::StopTeleObjectParticle(CGameObject *pO) 
{
	CParticlesPlayer* PP = smart_cast<CParticlesPlayer*>(pO);
	if(!PP) return;
	PP->StopParticles(particle_tele_object, BI_NONE, true);
}

//void CGhostBoss::Hit(float P,Fvector &dir,CObject*who,s16 element,Fvector p_in_object_space,float impulse, ALife::EHitType hit_type)
void	CGhostBoss::Hit								(SHit* pHDS)
{
	if (m_shield_active && (pHDS->hit_type == ALife::eHitTypeFireWound) && (Device.dwFrame != last_hit_frame)) {

		// вычислить позицию и направленность партикла
		Fmatrix pos; 
		//CParticlesPlayer::MakeXFORM(this,element,Fvector().set(0.f,0.f,1.f),p_in_object_space,pos);
		CParticlesPlayer::MakeXFORM(this,pHDS->bone(),pHDS->dir,pHDS->p_in_bone_space,pos);

		// установить particles
		CParticlesObject* ps = CParticlesObject::Create(particle_fire_shield,TRUE);
		
		ps->UpdateParent(pos,Fvector().set(0.f,0.f,0.f));
		GamePersistent().ps_needtoplay.push_back(ps);

	} else if (!m_shield_active)
//				inherited::Hit(P,dir,who,element,p_in_object_space,impulse,hit_type);
				inherited::Hit(pHDS);

	last_hit_frame = Device.dwFrame;
}


void CGhostBoss::Die(CObject* who)
{
	inherited::Die(who);
	TScanner::on_destroy();

	if (com_man().ta_is_active()) com_man().ta_deactivate();
	CTelekinesis::Deactivate();
}

void CGhostBoss::on_scanning()
{
	time_last_scan = Device.dwTimeGlobal;
}

void CGhostBoss::on_scan_success()
{
	CActor *pA = smart_cast<CActor *>(Level().CurrentEntity());
	if (!pA) return;

	EnemyMan.add_enemy(pA);
}

void CGhostBoss::net_Relcase(CObject *O)
{
	inherited::net_Relcase		(O);

	TTelekinesis::remove_links	(O);
}


#ifdef DEBUG
CBaseMonster::SDebugInfo CGhostBoss::show_debug_info()
{
	CBaseMonster::SDebugInfo info = inherited::show_debug_info();
	if (!info.active) return CBaseMonster::SDebugInfo();

	string128 text;
	xr_sprintf(text, "Scan Value = [%f]", TScanner::get_scan_value());
	DBG().text(this).add_item(text, info.x, info.y+=info.delta_y, info.color);
	DBG().text(this).add_item("---------------------------------------", info.x, info.y+=info.delta_y, info.delimiter_color);

	return CBaseMonster::SDebugInfo();
}
#endif
