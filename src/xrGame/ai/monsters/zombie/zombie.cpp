#include "StdAfx.h"
#include "zombie.h"
#include "zombie_state_manager.h"
#include "../../../../Include/xrRender/KinematicsAnimated.h"
#include "../../../EntityCondition.h"
#include "../monster_velocity_space.h"

#include "../control_animation_base.h"
#include "../control_movement_base.h"
#include "../../../sound_player.h"
#include "CharacterPhysicsSupport.h"

void CZombie::StartFakeDeathRagdoll()
{
	active_triple_idx = u8(Random.randI(FAKE_DEATH_TYPES_COUNT));
	com_man().ta_activate(anim_triple_death[active_triple_idx]);
	move().stop();
				
	IsSpatialReactSound = (SpatialComponent->type & ESPATIAL_TYPE::REACTTOSOUND) != ESPATIAL_TYPE::NONE;
	IsSpatialPhysMove = (SpatialComponent->type & ESPATIAL_TYPE::PHYSIC_MOVEMENT) != ESPATIAL_TYPE::NONE;
				
	SpatialComponent->type &= ~ESPATIAL_TYPE::REACTTOSOUND;
	SpatialComponent->type &= ~ESPATIAL_TYPE::PHYSIC_MOVEMENT;

	auto PhysicsSupport = character_physics_support();
	if(PhysicsSupport)
	{
		PhysicsSupport->ActivateRagdoll();
	}
					
	IsFakeDeathActive = true;
}

void CZombie::StartFakeDeathVanilla()
{
	active_triple_idx			= u8(Random.randI(FAKE_DEATH_TYPES_COUNT));
	com_man().ta_activate		(anim_triple_death[active_triple_idx]);
	move().stop					();

	IsFakeDeathActive = true;
}

void CZombie::StopFakeDeathRagdoll()
{
	if (IsSpatialReactSound)
	{
		SpatialComponent->type |= ESPATIAL_TYPE::REACTTOSOUND;
	}
	if (IsSpatialPhysMove)
	{
		SpatialComponent->type |= ESPATIAL_TYPE::PHYSIC_MOVEMENT;
	}

	auto PhysicsSupport = character_physics_support();
	if(PhysicsSupport)
	{
		PhysicsSupport->DeactivateRagdoll();
	}
}

CZombie::CZombie()
{
	StateMan = new CStateManagerZombie(this);
	
	CControlled::init_external(this);
}

CZombie::~CZombie()
{
	xr_delete		(StateMan);
}

constexpr u32 default_time_fake_death = 5000;
constexpr u32 default_time_resurrect_restore = 2000;
constexpr u32 default_time_out_frustum_timeout = 1000;

void CZombie::Load(LPCSTR section)
{
	inherited::Load	(section);

	anim().accel_load			(section);
	anim().accel_chain_add		(eAnimWalkFwd,		eAnimRun);

	fake_death_count = 1 + u8(Random.randI(pSettings->r_u8(section,"FakeDeathCount")));
	health_death_threshold	= pSettings->r_float(section,"StartFakeDeathHealthThreshold");

	time_dead_duration = pSettings->read_if_exists<float>(section, "time_dead_duration", default_time_fake_death);
	time_resurrect_duration = pSettings->read_if_exists<float>(section, "time_resurrect_duration", default_time_resurrect_restore);
	time_out_frustum_duration = pSettings->read_if_exists<float>(section, "time_out_frustum_duration", default_time_out_frustum_timeout);

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

#ifdef DEBUG	
	anim().accel_chain_test		();
#endif

	PostLoad					(section);
}

void CZombie::reinit()
{
	inherited::reinit();

	Bones.Reset();
	
	time_dead_start			= 0;
	last_hit_frame			= 0;
	time_resurrect			= 0;
	fake_death_left			= fake_death_count;

	active_triple_idx		= u8(-1);
}

void CZombie::reload(const char* section)
{
	inherited::reload(section);

	com_man().ta_fill_data(anim_triple_death[0],	"fake_death_0_0",	"fake_death_0_1",	"fake_death_0_2",	true, false);
	com_man().ta_fill_data(anim_triple_death[1],	"fake_death_1_0",	"fake_death_1_1",	"fake_death_1_2",	true, false);
	com_man().ta_fill_data(anim_triple_death[2],	"fake_death_2_0",	"fake_death_2_1",	"fake_death_2_2",	true, false);
	com_man().ta_fill_data(anim_triple_death[3],	"fake_death_3_0",	"fake_death_3_1",	"fake_death_3_2",	true, false);
}


void CZombie::BoneCallback(CBoneInstance* B)
{
	CZombie* this_class = static_cast<CZombie*>(B->callback_param());

	PROF_EVENT("Zombie/Bones Update");
	this_class->Bones.Update(B, Device.dwTimeGlobal);
}

void CZombie::vfAssignBones()
{
	// Установка callback на кости
	IKinematics* kin = PKinematics(Visual());
	bone_spine = &kin->LL_GetBoneInstance(kin->LL_BoneID("bip01_spine"));
	bone_head =	&kin->LL_GetBoneInstance(kin->LL_BoneID("bip01_head"));
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

void CZombie::Die(CObject* who)
{
	if (EngineExternal()[EEngineExternalGame::EnableRagdolledZombiePseudodeath] && IsFakeDeathActive)
	{
		
		StopFakeDeathRagdoll();
		//com_man().ta_pointbreak();
	
		IsFakeDeathActive = false;
	}
	inherited::Die(who);
}

void CZombie::save(NET_Packet& output_packet)
{
	inherited::save(output_packet);
	output_packet.w_u8(IsFakeDeathActive);
	output_packet.w_u32(time_dead_start);
}

void CZombie::load(IReader& input_packet)
{
	inherited::load(input_packet);
	IsFakeDeathActive = input_packet.r_u8();
	time_dead_start = input_packet.r_u32();
	if (IsFakeDeathActive && !com_man().ta_is_active())
	{
		if (EngineExternal()[EEngineExternalGame::EnableRagdolledZombiePseudodeath])
		{
			StartFakeDeathRagdoll();
		} else
		{
			StartFakeDeathVanilla();
		}
		if (time_dead_start)
		{
			time_dead_start = Device.dwTimeGlobal;
		}
	}
}

void CZombie::Serialize(ISaveObject& Object)
{
	BEGIN_CHUNK(Object,"CZombie")
	{
		inherited::Serialize(Object);
		Object << IsFakeDeathActive << time_dead_start;
		if (!Object.IsSave() && IsFakeDeathActive && !com_man().ta_is_active())
		{
			if (EngineExternal()[EEngineExternalGame::EnableRagdolledZombiePseudodeath])
			{
				StartFakeDeathRagdoll();
			} else
			{
				StartFakeDeathVanilla();
			}
			if (time_dead_start)
			{
				time_dead_start = Device.dwTimeGlobal;
			}
		}
	}
}

bool CZombie::net_Spawn (CSE_Abstract* DC) 
{
	if (!inherited::net_Spawn(DC))
		return(false);

	vfAssignBones	();

	return(true);
}

void CZombie::net_Destroy()
{
	IsFakeDeathActive = false;
	CBaseMonster::net_Destroy();
}

void CZombie::Hit(SHit* pHDS)
{
	inherited::Hit(pHDS);

	if (!g_Alive()) return;
	
	if ((pHDS->hit_type == ALife::eHitTypeFireWound) && (Device.dwFrame != last_hit_frame)) {
		if (EngineExternal()[EEngineExternalGame::EnableRagdolledZombiePseudodeath])
		{
			if (!IsFakeDeathActive && (time_resurrect + time_resurrect_duration < Device.dwTimeGlobal) && (conditions().GetHealth() < health_death_threshold))
			{
				time_dead_start = Device.dwTimeGlobal;
				
				StartFakeDeathRagdoll();
				
				sound().play(MonsterSound::eMonsterSoundDie);
			}
		} else
		{
			if (!IsFakeDeathActive && !com_man().ta_is_active() && (time_resurrect + time_resurrect_duration < Device.dwTimeGlobal) && (conditions().GetHealth() < health_death_threshold)) {
				if (conditions().GetHealth() < (health_death_threshold - float(fake_death_count - fake_death_left) * health_death_threshold / fake_death_count)) {
					time_dead_start				= Device.dwTimeGlobal;

					StartFakeDeathVanilla();
				
					if (fake_death_left == 0)	fake_death_left = 1;
					fake_death_left--;
				}
			}
		}
	}

	last_hit_frame = Device.dwFrame;
}


void CZombie::shedule_Update(u32 dt)
{
	inherited::shedule_Update(dt);

	if (g_Alive() && time_dead_start != 0) {
		bool CanWakeUp = time_dead_start + time_dead_duration < Device.dwTimeGlobal;
		if (EngineExternal()[EEngineExternalGame::EnableRagdolledZombiePseudodeath])
		{
			CFrustum CameraFrustum;  
			CameraFrustum.CreateFromMatrix(Device.mFullTransform, FRUSTUM_P_LRTB | FRUSTUM_P_FAR);  

			CanWakeUp = !CameraFrustum.testSphere_dirty(SpatialComponent->sphere.P, SpatialComponent->sphere.R);
			if (CanWakeUp)
			{
				if (!time_out_frustum)
				{
					time_out_frustum = Device.dwTimeGlobal;
				}
				if (time_out_frustum + time_out_frustum_duration >= Device.dwTimeGlobal)
				{
					CanWakeUp = false;
				}
			} else
			{
				time_out_frustum = 0;
			}
		}
		if (CanWakeUp) {
			time_dead_start = 0;
			time_out_frustum = 0;

			if (EngineExternal()[EEngineExternalGame::EnableRagdolledZombiePseudodeath])
			{
				StopFakeDeathRagdoll();
			}
			com_man().ta_pointbreak();

			time_resurrect = Device.dwTimeGlobal;

			IsFakeDeathActive = false;
		}
	}
}


bool CZombie::fake_death_fall_down()
{
	if (com_man().ta_is_active()) return false;

	if (EngineExternal()[EEngineExternalGame::EnableRagdolledZombiePseudodeath])
	{
		StartFakeDeathRagdoll();
	} else
	{
		StartFakeDeathVanilla();
	}

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

	if (EngineExternal()[EEngineExternalGame::EnableRagdolledZombiePseudodeath])
	{
		StopFakeDeathRagdoll();
	}
	com_man().ta_pointbreak();
	
	IsFakeDeathActive = false;
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
