#include "stdafx.h"
#include "zombie.h"
#include "zombie_state_manager.h"
#include "../../../profiler.h"
#include "../../../../../Include/xrRender/KinematicsAnimated.h"
#include "../../../entitycondition.h"
#include "../monster_velocity_space.h"
#include "../../../characterphysicssupport.h"
#include "../../../PHMovementControl.h"
#include "../../../actor.h"
#include "../../../ActorEffector.h"
#include "zombie_choke_effector.h"
#include "../../../../camerabase.h"

#include "../control_animation_base.h"
#include "../control_movement_base.h"
#ifdef _DEBUG
#endif


CZombie::CZombie()
{
	StateMan = new CStateManagerZombie(this);
	
	CControlled::init_external(this);
}

CZombie::~CZombie()
{
	xr_delete		(StateMan);
}

void CZombie::Load(LPCSTR section)
{
	inherited::Load	(section);

	anim().accel_load			(section);
	anim().accel_chain_add		(eAnimWalkFwd,		eAnimRun);

	fake_death_count		= 1 + u8(Random.randI(pSettings->r_u8(section,"FakeDeathCount")));
	health_death_threshold	= pSettings->r_float(section,"StartFakeDeathHealthThreshold");

	SVelocityParam &velocity_none		= move().get_velocity(MonsterMovement::eVelocityParameterIdle);	
	SVelocityParam &velocity_turn		= move().get_velocity(MonsterMovement::eVelocityParameterStand);
	SVelocityParam &velocity_walk		= move().get_velocity(MonsterMovement::eVelocityParameterWalkNormal);
	SVelocityParam &velocity_run		= move().get_velocity(MonsterMovement::eVelocityParameterRunNormal);
	//SVelocityParam &velocity_walk_dmg	= move().get_velocity(MonsterMovement::eVelocityParameterWalkDamaged);
	//SVelocityParam &velocity_run_dmg	= move().get_velocity(MonsterMovement::eVelocityParameterRunDamaged);
	//SVelocityParam &velocity_steal		= move().get_velocity(MonsterMovement::eVelocityParameterSteal);
	//SVelocityParam &velocity_drag		= move().get_velocity(MonsterMovement::eVelocityParameterDrag);


	anim().AddAnim(eAnimStandIdle,		"stand_idle_",			-1, &velocity_none,		PS_STAND,	"fx_stand_f", "fx_stand_b", "fx_stand_l", "fx_stand_r");
	anim().AddAnim(eAnimStandTurnLeft,	"stand_turn_ls_",		-1, &velocity_turn,		PS_STAND,	"fx_stand_f", "fx_stand_b", "fx_stand_l", "fx_stand_r");
	anim().AddAnim(eAnimStandTurnRight,	"stand_turn_rs_",		-1, &velocity_turn,		PS_STAND,	"fx_stand_f", "fx_stand_b", "fx_stand_l", "fx_stand_r");
	anim().AddAnim(eAnimWalkFwd,			"stand_walk_fwd_",		-1, &velocity_walk,		PS_STAND,	"fx_stand_f", "fx_stand_b", "fx_stand_l", "fx_stand_r");
	anim().AddAnim(eAnimRun,				"stand_run_",			-1,	&velocity_run,		PS_STAND,	"fx_stand_f", "fx_stand_b", "fx_stand_l", "fx_stand_r");
	anim().AddAnim(eAnimAttack,			"stand_attack_",		-1, &velocity_turn,		PS_STAND,	"fx_stand_f", "fx_stand_b", "fx_stand_l", "fx_stand_r");
	anim().AddAnim(eAnimDie,				"stand_die_",			0, &velocity_none,		PS_STAND,	"fx_stand_f", "fx_stand_b", "fx_stand_l", "fx_stand_r");

	anim().LinkAction(ACT_STAND_IDLE,	eAnimStandIdle);
	anim().LinkAction(ACT_SIT_IDLE,		eAnimStandIdle);
	anim().LinkAction(ACT_LIE_IDLE,		eAnimStandIdle);
	anim().LinkAction(ACT_WALK_FWD,		eAnimWalkFwd);
	anim().LinkAction(ACT_WALK_BKWD,		eAnimWalkFwd);
	anim().LinkAction(ACT_RUN,			eAnimRun);
	anim().LinkAction(ACT_EAT,			eAnimStandIdle);
	anim().LinkAction(ACT_SLEEP,			eAnimStandIdle);
	anim().LinkAction(ACT_REST,			eAnimStandIdle);
	anim().LinkAction(ACT_DRAG,			eAnimStandIdle);
	anim().LinkAction(ACT_ATTACK,		eAnimAttack);
	anim().LinkAction(ACT_STEAL,			eAnimWalkFwd);
	anim().LinkAction(ACT_LOOK_AROUND,	eAnimStandIdle);

	LoadChokePPEffector			(pSettings->r_string(section,"choke_effector"));
	m_choke_want_speed			= pSettings->r_float(section,"Choke_Need_Speed");

#ifdef DEBUG	
	anim().accel_chain_test		();
#endif

}

void CZombie::reinit()
{
	inherited::reinit();

	CControlledActor::reinit	();

	Bones.Reset();

	com_man().ta_fill_data(anim_triple_choke, "stand_attack_4_", "stand_attack_5_", "stand_attack_6_", TA_EXECUTE_LOOPED, TA_DONT_SKIP_PREPARE, ControlCom::eCapturePath | ControlCom::eCaptureMovement);
	
	time_dead_start			= 0;
	last_hit_frame			= 0;
	time_resurrect			= 0;
	fakedeath_is_active		= false;
	fake_death_left			= fake_death_count;
	m_choke_want_value		= 0.f;

	active_triple_idx		= u8(-1);
}

void CZombie::reload(LPCSTR section)
{
	inherited::reload(section);

	com_man().ta_fill_data(anim_triple_death[0],	"fake_death_0_0",	"fake_death_0_1",	"fake_death_0_2",	true, false);
	com_man().ta_fill_data(anim_triple_death[1],	"fake_death_1_0",	"fake_death_1_1",	"fake_death_1_2",	true, false);
	com_man().ta_fill_data(anim_triple_death[2],	"fake_death_2_0",	"fake_death_2_1",	"fake_death_2_2",	true, false);
	com_man().ta_fill_data(anim_triple_death[3],	"fake_death_3_0",	"fake_death_3_1",	"fake_death_3_2",	true, false);
}


void CZombie::BoneCallback(CBoneInstance *B)
{
	CZombie*	this_class = static_cast<CZombie*>(B->callback_param());

	START_PROFILE("Zombie/Bones Update");
	this_class->Bones.Update(B, Device.dwTimeGlobal);
	STOP_PROFILE("AI/Zombie/Bones Update");
}


void CZombie::vfAssignBones()
{
	// Установка callback на кости
	bone_spine =	&smart_cast<IKinematics*>(Visual())->LL_GetBoneInstance(smart_cast<IKinematics*>(Visual())->LL_BoneID("bip01_spine"));
	bone_head =		&smart_cast<IKinematics*>(Visual())->LL_GetBoneInstance(smart_cast<IKinematics*>(Visual())->LL_BoneID("bip01_head"));
	//if(!PPhysicsShell())//нельзя ставить колбеки, если создан физ шел - у него стоят свои колбеки!!!
	//{
		//bone_spine->set_callback(BoneCallback,this);
		//bone_head->set_callback(BoneCallback,this);
	//}

	// Bones settings
	Bones.Reset();
	Bones.AddBone(bone_spine, AXIS_Z);	Bones.AddBone(bone_spine, AXIS_Y); Bones.AddBone(bone_spine, AXIS_X);
	Bones.AddBone(bone_head, AXIS_Z);	Bones.AddBone(bone_head, AXIS_Y);
}

BOOL CZombie::net_Spawn (CSE_Abstract* DC) 
{
	if (!inherited::net_Spawn(DC))
		return(FALSE);

	vfAssignBones	();

	return(TRUE);
}

#define TIME_FAKE_DEATH			5000
#define TIME_RESURRECT_RESTORE	2000

//void CZombie::Hit(float P,Fvector &dir,CObject*who,s16 element,Fvector p_in_object_space,float impulse, ALife::EHitType hit_type)
void	CZombie::Hit								(SHit* pHDS)
{
//	inherited::Hit(P,dir,who,element,p_in_object_space,impulse,hit_type);
	inherited::Hit(pHDS);

	if (!g_Alive()) return;
	
	if ((pHDS->hit_type == ALife::eHitTypeFireWound) && (Device.dwFrame != last_hit_frame)) {
		if (!com_man().ta_is_active() && (time_resurrect + TIME_RESURRECT_RESTORE < Device.dwTimeGlobal) && (conditions().GetHealth() < health_death_threshold)) {
			if (conditions().GetHealth() < (health_death_threshold - float(fake_death_count - fake_death_left) * health_death_threshold / fake_death_count)) {
				active_triple_idx			= u8(Random.randI(FAKE_DEATH_TYPES_COUNT));
				com_man().ta_activate		(anim_triple_death[active_triple_idx]);
				move().stop					();
				if (g_Alive())
					character_physics_support()->movement()->DestroyCharacter();
				time_dead_start				= Device.dwTimeGlobal;
				fakedeath_is_active			= true;
				
				if (fake_death_left == 0)	fake_death_left = 1;
				fake_death_left--;
			}
		} 
	}

	last_hit_frame = Device.dwFrame;
}


void CZombie::shedule_Update(u32 dt)
{
	inherited::shedule_Update(dt);

	if (time_dead_start != 0) {
		if (time_dead_start + TIME_FAKE_DEATH < Device.dwTimeGlobal) {
			time_dead_start  = 0;
			fakedeath_is_active = false;

			com_man().ta_pointbreak();
			if (g_Alive())	
				character_physics_support()->CreateCharacter();

			time_resurrect = Device.dwTimeGlobal;
		}
	}
}

void CZombie::UpdateCL()
{
	inherited::UpdateCL				();
	CControlledActor::frame_update	();
	
	// update choke need
	m_choke_want_value += m_choke_want_speed * client_update_fdelta();
	clamp(m_choke_want_value,0.f,1.f);
	//Msg("clamp=[%f]", m_choke_want_value);

}

void CZombie::LoadChokePPEffector(LPCSTR section)
{
	pp_choke_effector.duality.h			= pSettings->r_float(section,"duality_h");
	pp_choke_effector.duality.v			= pSettings->r_float(section,"duality_v");
	pp_choke_effector.gray				= pSettings->r_float(section,"gray");
	pp_choke_effector.blur				= pSettings->r_float(section,"blur");
	pp_choke_effector.noise.intensity		= pSettings->r_float(section,"noise_intensity");
	pp_choke_effector.noise.grain			= pSettings->r_float(section,"noise_grain");
	pp_choke_effector.noise.fps			= pSettings->r_float(section,"noise_fps");
	VERIFY(!fis_zero(pp_choke_effector.noise.fps));

	sscanf(pSettings->r_string(section,"color_base"),	"%f,%f,%f", &pp_choke_effector.color_base.r, &pp_choke_effector.color_base.g, &pp_choke_effector.color_base.b);
	sscanf(pSettings->r_string(section,"color_gray"),	"%f,%f,%f", &pp_choke_effector.color_gray.r, &pp_choke_effector.color_gray.g, &pp_choke_effector.color_gray.b);
	sscanf(pSettings->r_string(section,"color_add"),	"%f,%f,%f", &pp_choke_effector.color_add.r,  &pp_choke_effector.color_add.g,  &pp_choke_effector.color_add.b);
}

void CZombie::ActivateChokeEffector()
{
	Actor()->Cameras().AddPPEffector(new CChokePPEffector(pp_choke_effector, 6.0f));
}

bool CZombie::fake_death_fall_down()
{
	if (com_man().ta_is_active()) return false;

	com_man().ta_activate		(anim_triple_death[u8(Random.randI(FAKE_DEATH_TYPES_COUNT))]);
	move().stop					();
	fakedeath_is_active				= true;
	if (g_Alive())
		character_physics_support()->movement()->DestroyCharacter();

	return true;
}

void CZombie::fake_death_stand_up()
{
	// check if state active
	bool active = false;
	for (u32 i=0; i<FAKE_DEATH_TYPES_COUNT; i++) {
		if (com_man().ta_is_active(anim_triple_death[i])) {
			active = true;
			break;
		}
	}
	if (!active) return;

	fakedeath_is_active	= false;

	com_man().ta_pointbreak();

	if (g_Alive())
		character_physics_support()->CreateCharacter();
}


#ifdef _DEBUG
void CZombie::debug_on_key(int key)
{
	switch (key){
	case SDL_SCANCODE_MINUS:
		{
			fake_death_fall_down();
		}
		break;
	case SDL_SCANCODE_EQUALS:
		{
			fake_death_stand_up();
		}
		break;
	}
}
#endif
