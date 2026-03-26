#include "StdAfx.h"
#include "chimera.h"
#include "chimera_state_manager.h"
//#include "../../../../Include/xrRender/KinematicsAnimated.h"
#include "../../../detail_path_manager.h"
#include "../monster_velocity_space.h"
#include "../../../Level.h"
//#include "../../../PhysicsShell.h"
#include "../../../sound_player.h"
#include "../control_animation_base.h"
#include "../control_movement_base.h"
#include "../control_path_builder_base.h"


CChimera::CChimera ()
{
	StateMan							=	new CStateManagerChimera	(this);
	com_man().add_ability					(ControlCom::eControlJump);
}

CChimera::~CChimera ()
{
	xr_delete								(StateMan);
}

void   CChimera::Load (const char* section)
{
	inherited::Load							(section);

	anim().accel_load						(section);
	anim().accel_chain_add					(eAnimWalkFwd,		eAnimRun);
	anim().accel_chain_add					(eAnimWalkFwd,		eAnimRunTurnLeft);
	anim().accel_chain_add					(eAnimWalkFwd,		eAnimRunTurnRight);
	anim().accel_chain_add					(eAnimWalkDamaged,	eAnimRunDamaged);

	anim().AddReplacedAnim					(&m_bDamaged, eAnimRun,		eAnimRunDamaged);
	anim().AddReplacedAnim					(&m_bDamaged, eAnimWalkFwd,	eAnimWalkDamaged);
	anim().AddReplacedAnim					(&m_bRunTurnLeft,	eAnimRun,		eAnimRunTurnLeft);
	anim().AddReplacedAnim					(&m_bRunTurnRight,	eAnimRun,		eAnimRunTurnRight);


	SVelocityParam &velocity_none		= 	move().get_velocity(MonsterMovement::eVelocityParameterIdle);	
	SVelocityParam &velocity_turn		= 	move().get_velocity(MonsterMovement::eVelocityParameterStand);
	SVelocityParam &velocity_walk		= 	move().get_velocity(MonsterMovement::eVelocityParameterWalkNormal);
	SVelocityParam &velocity_run		= 	move().get_velocity(MonsterMovement::eVelocityParameterRunNormal);
	SVelocityParam &velocity_walk_dmg	= 	move().get_velocity(MonsterMovement::eVelocityParameterWalkDamaged);
	SVelocityParam &velocity_run_dmg	= 	move().get_velocity(MonsterMovement::eVelocityParameterRunDamaged);
	SVelocityParam &velocity_steal		= 	move().get_velocity(MonsterMovement::eVelocityParameterSteal);

	bool rotateVelocityExist = m_velocity_rotate.Load(section, "Velocity_Rotate");
	bool jumpVelocityExist = m_velocity_jump_start.Load				(section, "Velocity_JumpStart");
	
#define ANIM_NAME(Var, Def) pSettings->read_if_exists<LPCSTR>(section, Var, Def)
	
	anim().AddAnim(eAnimStandIdle,		ANIM_NAME("AnimStandIdle_prefix", "stand_idle_"),			-1, &velocity_none,		PS_STAND);
	anim().AddAnim(eAnimStandTurnLeft,	ANIM_NAME("AnimStandTurnLeft_prefix", "stand_turn_ls_"),		-1, &velocity_turn,		PS_STAND);
	anim().AddAnim(eAnimStandTurnRight,	ANIM_NAME("AnimStandTurnRight_prefix", "stand_turn_rs_"),		-1, &velocity_turn,		PS_STAND);
	
	anim().AddAnim(eAnimFastStandTurnLeft, ANIM_NAME("AnimFastStandTurnLeft_prefix", "stand_run_turn_90_ls_"), -1, rotateVelocityExist ? &m_velocity_rotate : &velocity_turn, PS_STAND);
	anim().AddAnim(eAnimFastStandTurnRight, ANIM_NAME("AnimFastStandTurnRight_prefix", "stand_run_turn_90_rs_"), -1, rotateVelocityExist ? &m_velocity_rotate : &velocity_turn, PS_STAND);
	
	anim().AddAnim(eAnimLieIdle,			ANIM_NAME("AnimLieIdle_prefix", "stand_idle_"),			-1, &velocity_none,		PS_LIE);
	anim().AddAnim(eAnimSleep,			ANIM_NAME("AnimSleep_prefix", "stand_idle_"),			-1, &velocity_none,		PS_LIE);

	anim().AddAnim(eAnimWalkFwd, ANIM_NAME("AnimWalkFwd_prefix", "stand_walk_"), -1, &velocity_walk, PS_STAND);

	anim().AddAnim(eAnimWalkDamaged, ANIM_NAME("AnimWalkDamaged_prefix", "stand_walk_dmg_"), -1, &velocity_walk_dmg, PS_STAND);

	anim().AddAnim(eAnimRun,				ANIM_NAME("AnimRun_prefix", "stand_run_fwd_"),		-1,	&velocity_run,		PS_STAND);
	anim().AddAnim(eAnimRunDamaged,		ANIM_NAME("AnimRunDamaged_prefix", "stand_run_dmg_"),		-1,	&velocity_run_dmg,	PS_STAND);
	anim().AddAnim(eAnimCheckCorpse,		ANIM_NAME("AnimCheckCorpse_prefix", "stand_check_corpse_"),	-1,	&velocity_none,		PS_STAND);
	anim().AddAnim(eAnimEat,				ANIM_NAME("AnimEat_prefix", "stand_eat_"),			-1, &velocity_none,		PS_STAND);
	anim().AddAnim(eAnimAttack,			ANIM_NAME("AnimAttack_prefix", "stand_idle_"),			-1, &velocity_turn,		PS_STAND);

	anim().AddAnim(eAnimLookAround,		ANIM_NAME("AnimLookAround_prefix", "stand_idle_"),			-1, &velocity_none,		PS_STAND);
	anim().AddAnim(eAnimSteal, ANIM_NAME("AnimSteal_prefix", "stand_walk_"), -1, &velocity_steal, PS_STAND);

	anim().AddAnim(eAnimPrepareAttack,	ANIM_NAME("AnimPrepareAttack_prefix", "stand_agressive_idle_"), -1, &velocity_none, PS_STAND);

	anim().AddAnim(eAnimDie,				ANIM_NAME("AnimDie_prefix", "stand_idle_"),			-1, &velocity_none,		PS_STAND);
	anim().AddAnim(eAnimThreaten,			ANIM_NAME("AnimThreaten_prefix", "stand_idle_"),			-1, &velocity_none,		PS_STAND);

	anim().AddAnim	(eAnimRunTurnLeft,		ANIM_NAME("AnimRunTurnLeft_prefix", "stand_run_turn_ls_"),	-1, &velocity_run,	PS_STAND);
	anim().AddAnim	(eAnimRunTurnRight,		ANIM_NAME("AnimRunTurnRight_prefix", "stand_run_turn_rs_"), -1, &velocity_run, PS_STAND);

	anim().AddAnim	(eAnimUpperAttack,		ANIM_NAME("AnimUpperAttack_prefix", "jump_attack_"), -1, jumpVelocityExist ? &m_velocity_jump_start : &velocity_turn, PS_STAND);

#undef ANIM_NAME
	
	// link action
	anim().LinkAction						(ACT_STAND_IDLE,	eAnimStandIdle);
	anim().LinkAction						(ACT_SIT_IDLE,		eAnimLieIdle);
	anim().LinkAction						(ACT_LIE_IDLE,		eAnimLieIdle);
	anim().LinkAction						(ACT_WALK_FWD,		eAnimWalkFwd);
	//anim().LinkAction						(ACT_WALK_BKWD,		eAnimDragCorpse);
	anim().LinkAction						(ACT_RUN,			eAnimRun);
	anim().LinkAction						(ACT_EAT,			eAnimEat);
	anim().LinkAction						(ACT_SLEEP,			eAnimSleep);
	anim().LinkAction						(ACT_REST,			eAnimLieIdle);
	//anim().LinkAction						(ACT_DRAG,			eAnimDragCorpse);
	anim().LinkAction						(ACT_ATTACK,		eAnimAttack);
	anim().LinkAction						(ACT_STEAL,			eAnimSteal);
	anim().LinkAction						(ACT_LOOK_AROUND,	eAnimLookAround);

	m_attack_params.attack_radius			=	pSettings->read_if_exists<float>(section,"attack_radius",10.f);
	m_attack_params.prepare_jump_timeout	=	pSettings->read_if_exists<u32>(section,"prepare_jump_timeout",2000);
	m_attack_params.attack_jump_timeout		=	pSettings->read_if_exists<u32>(section,"attack_jump_timeout",1000);
	m_attack_params.stealth_timeout			=	pSettings->read_if_exists<u32>(section,"stealth_timeout",2000);
	m_attack_params.force_attack_distance	=	pSettings->read_if_exists<float>(section,"force_attack_distance",8);
	m_attack_params.num_attack_jumps		=	pSettings->read_if_exists<u32>(section,"num_attack_jumps",4);
	m_attack_params.num_prepare_jumps		=	pSettings->read_if_exists<u32>(section,"num_prepare_jumps",2);
#ifdef DEBUG	
	anim().accel_chain_test					();
#endif

	PostLoad								(section);
}

EAction   CChimera::CustomVelocityIndex2Action (u32 velocity_index) 
{
	switch ( velocity_index ) 
	{
		case MonsterMovement::eChimeraVelocityParameterJumpGround:	return ACT_RUN;
		case MonsterMovement::eChimeraVelocityParameterPrepare:		return ACT_RUN;
	}

	return ACT_STAND_IDLE;
}

void   CChimera::reinit ()
{
	inherited::reinit						();

	move().load_velocity					(*cNameSect(), 
											 "Velocity_JumpGround",
											 MonsterMovement::eChimeraVelocityParameterJumpGround);
	
	static string16 def_s3 = "jump_attack_1";
	static string16 def_s4 = "jump_attack_2";

	const char* s3_anim = pSettings->read_if_exists<LPCSTR>(get_section(),"jump_data_s3",def_s3);
	const char* s4_anim = pSettings->read_if_exists<LPCSTR>(get_section(),"jump_data_s4",def_s4);

	com_man().load_jump_data				(nullptr,//"jump_attack_0",
											 nullptr,//"jump_attack_0",
											 s3_anim,
											 s4_anim,
											 u32(-1),//MonsterMovement::eVelocityParameterRunNormal,
											 MonsterMovement::eChimeraVelocityParameterJumpGround,
											 0);
}

void   CChimera::CheckSpecParams (u32 spec_params)
{
// 	if ( (spec_params & ASP_THREATEN) == ASP_THREATEN )
// 	{
// 		anim().SetCurAnim(eAnimThreaten);
// 	}
// 	if ( (spec_params & ASP_ATTACK_RUN) == ASP_ATTACK_RUN ) 
// 	{
// 		anim().SetCurAnim(eAnimAttackRun);
// 	}
}

void   CChimera::HitEntityInJump (const CEntity *pEntity)
{
	auto AttackParamAnim = pSettings->read_if_exists<LPCSTR>(get_section(),"AttackParamAnim","jump_attack_1");
	SAAParam &params = anim().AA_GetParams(AttackParamAnim);
	
	HitEntity(pEntity, params.hit_power, params.impulse, params.impulse_dir);
}

void   CChimera::jump (Fvector const &position, float const factor)
{
	com_man().script_jump					(position, factor);
	sound().play							(MonsterSound::eMonsterSoundAggressive);
}

void CChimera::UpdateCL()
{
	inherited::UpdateCL						();
}
