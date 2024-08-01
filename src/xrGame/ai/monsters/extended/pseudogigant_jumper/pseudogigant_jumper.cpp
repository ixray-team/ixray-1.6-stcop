#include "stdafx.h"
#include "../../pseudogigant/pseudogigant.h"

#include "pseudogigant_jumper.h"
#include "pseudogigant_jumper_step_effector.h"

#include "../../../../actor.h"
#include "../../../../ActorEffector.h"
#include "../../../../level.h"
#include "pseudogigant_jumper_state_manager.h"
#include "../../monster_velocity_space.h"
#include "../../control_animation_base.h"
#include "../../control_movement_base.h"
#include "../../ai_monster_effector.h"
#include "../../../../../xrEngine/CameraBase.h"
#include "../../../../detail_path_manager_space.h"
#include "../../../../detail_path_manager.h"
#include "../../../../CharacterPhysicsSupport.h"
#include "../../control_path_builder_base.h"

CPseudogigantJumper::CPseudogigantJumper() 
{
	StateMan = new CStateManagerPseudogigantJumper(this);

	m_damage_after_jump = 0.f;
	m_damage_after_jump_impulse = 0.f;

	com_man().add_ability(ControlCom::eControlJump);
	com_man().add_ability(ControlCom::eControlRotationJump);
}

CPseudogigantJumper::~CPseudogigantJumper() 
{
	xr_delete(StateMan);
}

void CPseudogigantJumper::Load(LPCSTR section) 
{
	inherited::Load(section);

	m_damage_after_jump = pSettings->r_float(section, "damage_after_jump");
	m_damage_after_jump_impulse = pSettings->r_float(section, "damage_after_jump_impulse");
}

void CPseudogigantJumper::reinit() 
{
	inherited::reinit();

	move().load_velocity(*cNameSect(), "Velocity_JumpPrepare",MonsterMovement::eGiantVelocityParameterJumpPrepare);
	move().load_velocity(*cNameSect(), "Velocity_JumpGround",MonsterMovement::eGiantVelocityParameterJumpGround);

	com_man().load_jump_data("jump_attack_0", 0, "jump_attack_1", "jump_attack_2", MonsterMovement::eGiantVelocityParameterJumpPrepare, MonsterMovement::eSnorkVelocityParameterJumpGround, 0);

	com_man().add_rotation_jump_data("1","2","3","4", PI_DIV_2);
}

void CPseudogigantJumper::HitEntityInJump(const CEntity *pEntity) 
{
	SAAParam &params	= anim().AA_GetParams("jump_attack_1");
	HitEntity			(pEntity, this->m_damage_after_jump, this->m_damage_after_jump_impulse, params.impulse_dir);
}

void CPseudogigantJumper::EndStateJump() 
{
	inherited::EndStateJump();

	inherited::on_threaten_execute();
}
