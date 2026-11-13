#include "StdAfx.h"
#include "xrGame/ai/monsters/monster_options.h"
#include "control_jump.h"
#include "basemonster/base_monster.h"
#include "control_manager.h"
#include "../../PHMovementControl.h"
#include "../../../Include/xrRender/KinematicsAnimated.h"
#include "../../detail_path_manager.h"
#include "../../Level.h"
#include "control_animation_base.h"
#include "control_direction_base.h"
#include "control_movement_base.h"
#include "control_path_builder_base.h"
#include "monster_velocity_space.h"
#include "../../ai_space.h"
#include "../../level_graph.h"
#include "../../ai_object_location.h"
#include "../../CharacterPhysicsSupport.h"
#ifdef DEBUG
#include "../../level_debug.h"
#endif

#include "../../trajectories.h"
#include "../../../xrPhysics/IPHWorld.h"
#include "../../../xrPhysics/IPHCharacter.h"
#include "../../../xrCore/math/_vector3d_ext.h"

void CControlJump::reinit()
{
	inherited::reinit		();

	m_time_started			= 0;
	m_time_next_allowed		= 0;
	EvasionRecoveryUntil = 0;
}


void CControlJump::load(const char* section)
{
	AttackJumpEnabled = ExtendedEntityesOptions::Read<bool>(pSettings, section, "monster_attack_jump_enabled", true);
	float Recovery = ExtendedEntityesOptions::Read<float>(pSettings, section, "monster_dodge_attack_jump_delay", .6f);
	clamp(Recovery, 0.f, 5.f);
	EvasionRecoveryTime = u32(Recovery * 1000.f);
	EvasiveAnimationSpeedMax = ExtendedEntityesOptions::Read<float>(pSettings, section, "monster_dodge_jump_animation_speed_max", 1.35f);
	clamp(EvasiveAnimationSpeedMax, 1.f, 2.f);
	EvasiveDistanceMax = ExtendedEntityesOptions::Read<float>(pSettings, section, "monster_dodge_jump_distance_max", 5.f);
	clamp(EvasiveDistanceMax, .5f, 12.f);
	EvasiveDistanceMin = ExtendedEntityesOptions::Read<float>(pSettings, section, "monster_dodge_jump_distance_min", .5f);
	clamp(EvasiveDistanceMin, .1f, EvasiveDistanceMax);
	m_delay_after_jump				= pSettings->r_u32	(section,"jump_delay");
	m_jump_factor					= pSettings->r_float(section,"jump_factor");
	m_trace_ground_range			= pSettings->r_float(section,"jump_ground_trace_range");
	m_hit_trace_range				= pSettings->r_float(section,"jump_hit_trace_range");
	m_build_line_distance			= pSettings->r_float(section,"jump_build_line_distance");
	m_min_distance					= pSettings->r_float(section,"jump_min_distance");
	m_max_distance					= pSettings->r_float(section,"jump_max_distance");
	m_max_angle						= pSettings->r_float(section,"jump_max_angle");
	m_max_height					= pSettings->r_float(section,"jump_max_height");
	
	m_auto_aim_factor				= 0.f;

	if ( pSettings->line_exist(section, "jump_auto_aim_factor") )
		m_auto_aim_factor			= pSettings->r_float(section,"jump_auto_aim_factor");
}

bool CControlJump::IsEvasiveDistanceValid(const Fvector& Target) const
{
	Fvector Offset = Fvector().sub(Target, m_object->Position());
	Offset.y = 0.f;
	const float Scale = m_object->GetVisualScale();
	return Offset.square_magnitude() + EPS_L >= _sqr(EvasiveDistanceMin * Scale) && Offset.square_magnitude() <= _sqr(EvasiveDistanceMax * Scale) + EPS_L;
}

float CControlJump::get_max_distance() const
{
	return m_max_distance;
}

float CControlJump::get_min_distance() const
{
	return m_min_distance;
}

bool CControlJump::check_start_conditions()
{
	if (s32(Device.dwTimeGlobal - EvasionRecoveryUntil) < 0)
	{
		return false;
	}
	if (is_active())				return false;	
	if (m_man->is_captured_pure())	return false;
	IPHCharacter* Character = m_object->character_physics_support()->movement()->PHCharacter();
	// Controller release is not proof of physical touchdown.
	if (Character->JumpState() || !Character->HasGroundSupport())
	{
		return false;
	}


	return true;
}

void CControlJump::remove_links	(CObject* object)
{
	if ( m_data.target_object == object )
		m_data.target_object	=	nullptr;
}

void CControlJump::activate()
{
	m_man->capture_pure	(this);
	m_man->subscribe	(this, ControlCom::eventAnimationEnd);
	m_man->subscribe	(this, ControlCom::eventAnimationStart);

	if ( !m_data.flags.test(SControlJumpData::eDontUseVelocityBounce) )
	{
		m_man->subscribe	(this, ControlCom::eventVelocityBounce);
		m_data.flags.set(SControlJumpData::eDontUseVelocityBounce, false);
	}

	if ( m_data.target_object && 
		 !m_data.flags.test(SControlJumpData::eUseTargetPosition) )
	{
		start_jump	(get_target(m_data.target_object));
	}
	else 
	{
		m_data.flags.set(SControlJumpData::eUseTargetPosition, false);
		start_jump	(m_data.target_position);
	}
}

void CControlJump::on_release()
{
	if (FlightStarted && is_flag(SControlJumpData::eEvasiveJump))
	{
		// Native attacking jumps share the post-evasion touchdown recovery window.
		EvasionRecoveryUntil = Device.dwTimeGlobal + EvasionRecoveryTime;
	}
	m_man->unlock						(this, ControlCom::eControlPath);

	SControlDirectionData				*ctrl_data_dir = (SControlDirectionData*)m_man->data(this, ControlCom::eControlDir); 
	VERIFY								(ctrl_data_dir);
	ctrl_data_dir->linear_dependency	= true;

	m_man->unsubscribe					(this, ControlCom::eventVelocityBounce);
	m_man->unsubscribe					(this, ControlCom::eventAnimationEnd);
	m_man->unsubscribe					(this, ControlCom::eventAnimationStart);
	m_man->release_pure(this);

	if ( m_data.target_object && m_data.flags.test(SControlJumpData::eUseAutoAim) )
	{
		m_object->character_physics_support()->movement()->PHCharacter()->SetAirControlFactor(0.f);
	}

	m_data.flags.set					(SControlJumpData::eUseAutoAim, 0);

	m_object->path().prepare_builder	();
	m_object->set_ignore_collision_hit	(false);
	m_data.flags.set(SControlJumpData::eEvasiveJump, false);
	FlightStarted = false;
	m_time_started = 0;
	m_jump_time = 0.f;
	m_anim_state_current = m_anim_state_prev = eStateNone;
}

//////////////////////////////////////////////////////////////////////////
// Start jump
//////////////////////////////////////////////////////////////////////////
void CControlJump::start_jump(const Fvector &point)
{
	// initialize internals
	m_last_time_added_impulse			= 0;
	m_velocity_bounced					= false;
	m_object_hitted						= false;

	m_target_position					= point;
	m_blend_speed						= -1.f;

	m_jump_start_pos					= m_object->Position();
	FlightStarted = false;
	FlightDuration = 0.f;
	FlightStartedAt = 0;
	m_time_started						= 0;
	m_jump_time							= 0;
	m_last_saved_pos_time				= 0;

	// ignore collision hit when object is landing
	m_object->set_ignore_collision_hit	(true);

	// select correct state 
	if (is_flag(SControlJumpData::ePrepareSkip)) {
		m_anim_state_current	= eStateGlide;
		m_anim_state_prev		= eStatePrepare;

		m_man->path_stop		(this);
		m_man->move_stop		(this);

	} else {
		// check if can prepare in move
		bool prepared = false;

		if (is_flag(SControlJumpData::ePrepareInMove)) {

			// get animation time
			float time			= m_man->animation().motion_time(m_data.state_prepare_in_move.motion, m_object->Visual());
			// set acceleration and velocity
			SVelocityParam &vel	= m_object->move().get_velocity(m_data.state_prepare_in_move.velocity_mask);
			float dist = time * vel.velocity.linear;

			// check nodes in direction
			Fvector target_point;
			target_point.mad(m_object->Position(), m_object->Direction(), dist);
			if (m_man->path_builder().accessible(target_point)) {
				// нода в пр€мой видимости?
				m_man->path_builder().restrictions().add_border(m_object->Position(), target_point);
				u32 node = ai().level_graph().check_position_in_direction(m_object->ai_location().level_vertex_id(),m_object->Position(),target_point);
				m_man->path_builder().restrictions().remove_border();

				if (ai().level_graph().valid_vertex_id(node) && m_man->path_builder().accessible(node)) 
					prepared = true;
			}

			// node is checked, so try to build path
			if (prepared) {

				if (m_man->build_path_line(this, target_point, u32(-1), m_data.state_prepare_in_move.velocity_mask | MonsterMovement::eVelocityParameterStand)) {
					//---------------------------------------------------------------------------------------------------
					// set path params
					SControlPathBuilderData		*ctrl_path = (SControlPathBuilderData*)m_man->data(this, ControlCom::eControlPath); 
					VERIFY						(ctrl_path);
					ctrl_path->enable			= true;

					m_man->lock					(this, ControlCom::eControlPath);
					//---------------------------------------------------------------------------------------------------

					m_anim_state_current		= eStatePrepareInMove;
					m_anim_state_prev			= eStateNone;

					m_man->dir_stop				(this);

				} else {
					prepared = false;
				}
			}
		} 

		// if cannot perform prepare in move
		if (!prepared) {
			VERIFY(m_data.state_prepare.motion.valid() || is_flag(SControlJumpData::eGlideOnPrepareFailed));

			if (m_data.state_prepare.motion.valid()) {
				m_anim_state_current	= eStatePrepare;
				m_anim_state_prev		= eStateNone;

				m_man->path_stop		(this);
				m_man->move_stop		(this);

			} else {
				m_anim_state_current	= eStateGlide;
				m_anim_state_prev		= eStatePrepare;
			}
		}
	}

	select_next_anim_state	();
}

//////////////////////////////////////////////////////////////////////////
// Animation startup
//////////////////////////////////////////////////////////////////////////
void CControlJump::select_next_anim_state()
{
	if (m_anim_state_current == eStateNone) {
		stop();
		return;
	}
	// check gliding state
	if ((m_anim_state_current == eStateGlide) && (m_anim_state_prev == eStateGlide)) 
		if (is_flag(SControlJumpData::eGlidePlayAnimOnce)) return;

	//---------------------------------------------------------------------------------------------------
	// start new animation
	SControlAnimationData		*ctrl_data = (SControlAnimationData*)m_man->data(this, ControlCom::eControlAnimation); 
	VERIFY						(ctrl_data);
	ctrl_data->global.actual	= false;
	ctrl_data->global.SynchronizePhase = false;

	switch (m_anim_state_current) {
	case eStatePrepare:			ctrl_data->global.set_motion (m_data.state_prepare.motion);			break;
	case eStatePrepareInMove:	ctrl_data->global.set_motion (m_data.state_prepare_in_move.motion);	break;
	case eStateGlide:			ctrl_data->global.set_motion (m_data.state_glide.motion);			break;
	case eStateGround:			ctrl_data->global.set_motion (m_data.state_ground.motion);			break;
	default:					NODEFAULT;
	}

	//---------------------------------------------------------------------------------------------------
	// switch state if needed
	m_anim_state_prev = m_anim_state_current;
	
	if (m_anim_state_current != eStateGlide) {
		if (m_anim_state_current != eStatePrepare)
			m_anim_state_current = EStateAnimJump(m_anim_state_current + 1);
		else 
			m_anim_state_current = eStateGlide;
	}

	if ( in_auto_aim() )
		m_object->character_physics_support()->movement()->PHCharacter()->SetAirControlFactor(100.f * m_auto_aim_factor);
}

float CControlJump::GetFlightElapsed() const
{
	return FlightStartedAt ? float(Device.dwTimeGlobal - FlightStartedAt) * .001f : 0.f;
}

const char* CControlJump::GetPhaseName()
{
	if (!is_active())
	{
		return "завершен";
	}
	if (!FlightStarted)
	{
		return "подготовка";
	}
	return m_time_started ? "полет" : "наземный этап";
}

float CControlJump::relative_time ()
{
	float time	=	((Device.dwTimeGlobal - m_time_started) / 1000.f) / m_jump_time;
	if ( time > 1.f )
		time	=	1.f;

	return			time;
}

bool CControlJump::in_auto_aim ()
{
	if ( !m_data.target_object )
		return		false;

	if ( !m_data.flags.test(SControlJumpData::eUseAutoAim) )
		return		false;

	if ( !m_auto_aim_factor )
		return		false;

	if ( m_anim_state_prev != eStateGlide )
		return		false;

	return			true;
}

//////////////////////////////////////////////////////////////////////////
// Frame update jump state
//////////////////////////////////////////////////////////////////////////
void CControlJump::update_frame()
{
	if (IsEvasiveFlight())
	{
		IPHCharacter* Character = m_object->character_physics_support()->movement()->PHCharacter();
		if (!Character->IsEnabled())
		{
			// Ballistic flight cannot remain asleep while its visual animation keeps running.
			Character->Enable();
		}
	}

	// check if all jump stages are ended
	if (!IsEvasiveFlight() && m_velocity_bounced && m_man->path_builder().is_path_end(0.1f))
	{
		stop			();
		return;
	}

	if ( m_anim_state_current == eStateGlide && in_auto_aim() )
	{
		// set angular speed in exclusive force mode
		SControlDirectionData * ctrl_data_dir = (SControlDirectionData*)m_man->data(this, ControlCom::eControlDir); 
		VERIFY					(ctrl_data_dir);

		ctrl_data_dir->heading.target_angle	= m_man->direction().angle_to_target(m_data.target_object->Position());

		float cur_yaw, target_yaw;
		m_man->direction().get_heading			(cur_yaw, target_yaw);
		ctrl_data_dir->heading.target_speed		= angle_difference(cur_yaw,target_yaw) / m_jump_time;
		ctrl_data_dir->linear_dependency		= false;

// 		ctrl_data->set_speed	(m_man->animation().current_blend()->timeTotal / m_man->animation().current_blend()->speed / m_jump_time);
	}

	// trace enemy for hit
	hit_test			();

	// set velocity from path if we are on it
	if (!IsEvasiveFlight() && m_man->path_builder().is_moving_on_path()) {
		//---------------------------------------------------------------------------------------------------------------------------------
		// Set Velocity from path
		//---------------------------------------------------------------------------------------------------------------------------------
		SControlMovementData		*ctrl_move = (SControlMovementData*)m_man->data(this, ControlCom::eControlMovement); 
		VERIFY						(ctrl_move);

		ctrl_move->velocity_target	= m_object->move().get_velocity_from_path();
		ctrl_move->acc				= flt_max;
		//---------------------------------------------------------------------------------------------------------------------------------
	}

	// check if we landed
	if (is_on_the_ground()) 
		grounding();
}

//////////////////////////////////////////////////////////////////////////
// Trace ground to check if we have already landed
//////////////////////////////////////////////////////////////////////////
bool CControlJump::is_on_the_ground()
{
	if (m_time_started == 0) return false;
	if (m_time_started + (m_jump_time*1000) > time()) return false;

	if (is_flag(SControlJumpData::eEvasiveJump))
	{
		// The animated center/ray proximity is not a physical landing contact.
		CPHMovementControl* Movement = m_object->character_physics_support()->movement();
		IPHCharacter* Character = Movement->PHCharacter();
		return !Character->JumpState() && Character->HasGroundSupport();
	}

	// Physical support remains reliable when the visual center is above the ground ray range.
	if (m_object->character_physics_support()->movement()->Environment() == CPHMovementControl::peOnGround)
	{
		return true;
	}

	Fvector direction;
	direction.set(0.f, -1.f, 0.f);
	Fvector trace_from;
	m_object->Center(trace_from);

	collide::rq_result		l_rq;

	bool on_the_ground = false;
	if (Level().ObjectSpace.RayPick(trace_from, direction, m_trace_ground_range * m_object->GetVisualScale(), collide::rqtStatic, l_rq, m_object)) {
		if (l_rq.range < m_trace_ground_range * m_object->GetVisualScale()) on_the_ground = true;
	}
	return (on_the_ground);
}

//////////////////////////////////////////////////////////////////////////
// 
//////////////////////////////////////////////////////////////////////////

void CControlJump::grounding()
{

	if (is_flag(SControlJumpData::eEvasiveJump))
	{
		// The native ground motion expects a running follow-through, not a stationary landing.
		// Hand locomotion back at physical touchdown instead of holding that motion in place.
		stop();
		return;
	}

	if ((m_data.state_ground.velocity_mask == u32(-1)) || is_flag(SControlJumpData::eGroundSkip) || !m_data.state_ground.motion.valid()) {
		stop();
		return;
	}

	Fvector target_position;
	target_position.mad(m_object->Position(), m_object->Direction(), m_build_line_distance);

	if (!m_man->build_path_line(this, target_position, u32(-1), m_data.state_ground.velocity_mask | MonsterMovement::eVelocityParameterStand)) 
	{
		stop						();
	}
	else 
	{ 
		SControlPathBuilderData		*ctrl_path = (SControlPathBuilderData*)m_man->data(this, ControlCom::eControlPath); 
		VERIFY						(ctrl_path);
		ctrl_path->enable			= true;
		m_man->lock					(this, ControlCom::eControlPath);

		// lock dir
		m_man->dir_stop				(this);

		m_time_started				= 0;
		m_jump_time					= 0;
		m_anim_state_current		= eStateGround;
		select_next_anim_state		();
	}
}

void CControlJump::stop()
{
	m_man->notify (ControlCom::eventJumpEnd, 0);
}

//////////////////////////////////////////////////////////////////////////
// Get target point in world space
Fvector CControlJump::get_target(CObject *obj)
{
	u16 bone_id			= PKinematics(obj->Visual())->LL_GetBoneRoot			();
	CBoneInstance &bone = PKinematics(obj->Visual())->LL_GetBoneInstance		(bone_id);

	Fmatrix	global_transform;
	global_transform.mul	(obj->XFORM(),bone.mTransform);

	if (m_object->m_monster_type == CBaseMonster::eMonsterTypeOutdoor)
		return	(predict_position(obj, global_transform.c));
	else
		return	(global_transform.c);
}

float CControlJump::calculate_jump_time (Fvector const & target, bool const check_force_factor, bool Evasive)
{			
	float ph_time = m_object->character_physics_support()->movement()->JumpMinVelTime(target);
	// выполнить прыжок в соответствии с делителем времени
	float cur_factor	= (check_force_factor && m_data.force_factor > 0) ? 
						   m_data.force_factor : m_jump_factor;

	float JumpTime = ph_time/cur_factor;
	if (Evasive || is_flag(SControlJumpData::eEvasiveJump))
	{
		// Short evasions keep a readable animation instead of compressing it into a few frames.
		const float MotionTime = m_man->animation().motion_time(m_data.state_glide.motion, m_object->Visual());
		JumpTime = std::max(JumpTime, MotionTime / EvasiveAnimationSpeedMax);
	}
	return JumpTime;
}

void CControlJump::on_event(ControlCom::EEventType type, ControlCom::IEventData *data)
{
	if (type == ControlCom::eventVelocityBounce)
	{
		SEventVelocityBounce *event_data = (SEventVelocityBounce *)data;
		// !TEMP!
		if (  (event_data->m_ratio < 0) && !m_velocity_bounced && (m_jump_time != 0))
		{
 			if ( is_on_the_ground() ) 
			{
				m_velocity_bounced	= true;
				grounding();
			}
			else if (!IsEvasiveFlight())
			{
				// Ground-path bounce notifications must not cancel an airborne point-directed evasion.
				// Physical collision remains active; touchdown still ends the ballistic controller.
				stop();
			}
		}
	} else if (type == ControlCom::eventAnimationEnd) {
		const auto* Animation = static_cast<SControlAnimationData*>(m_man->data(this, ControlCom::eControlAnimation));
		// A queued callback from the preceding motion cannot finish a newly selected stage.
		if (!Animation->global.actual || (m_anim_state_prev == eStateGlide && !FlightStarted))
		{
			return;
		}
		select_next_anim_state();
	} else if (type == ControlCom::eventAnimationStart) {
		
		// start new animation
		SControlAnimationData		*ctrl_data = (SControlAnimationData*)m_man->data(this, ControlCom::eControlAnimation); 
		VERIFY						(ctrl_data);
		
		if (!ctrl_data->global.actual)
		{
			return;
		}
		if ((m_anim_state_current == eStateGlide) && (m_anim_state_prev == eStateGlide)) {
			// Start events from other skeleton parts must not reapply the impulse or change its speed.
			if (FlightStarted || ctrl_data->global.get_motion() != m_data.state_glide.motion)
			{
				return;
			}
			// Revalidate at the actual impulse, after animation startup and any preceding movement.
			if (is_flag(SControlJumpData::eEvasiveJump) && !IsEvasiveDistanceValid(m_target_position))
			{
				stop();
				return;
			}
			FlightStarted = true;

			//---------------------------------------------------------------------------------
			// start jump here
			//---------------------------------------------------------------------------------
			// получить врем€ физ.прыжка
			m_jump_time = calculate_jump_time(m_target_position, true);
			FlightDuration = m_jump_time;
			FlightStartedAt = Device.dwTimeGlobal;

			CPHMovementControl* Movement = m_object->character_physics_support()->movement();
			if (is_flag(SControlJumpData::eEvasiveJump))
			{
				// Remove the last grounded steering command before the first physics step of flight.
				Movement->PHCharacter()->SetAcceleration(Fvector().set(0.f, 0.f, 0.f));
			}
			Movement->Jump(m_target_position, m_jump_time);
			m_time_started		= time();
			m_time_next_allowed	= m_time_started + m_delay_after_jump;
			//---------------------------------------------------------------------------------
			
			// set angular speed in exclusive force mode
			SControlDirectionData					*ctrl_data_dir = (SControlDirectionData*)m_man->data(this, ControlCom::eControlDir); 
			VERIFY									(ctrl_data_dir);	

			if ( !m_data.flags.test(SControlJumpData::eUseAutoAim) || !m_data.target_object )
			{
				ctrl_data_dir->heading.target_angle	= m_man->direction().angle_to_target(m_target_position);
			}

			float cur_yaw,target_yaw;
			m_man->direction().get_heading			(cur_yaw, target_yaw);
			ctrl_data_dir->heading.target_speed		= angle_difference(cur_yaw,target_yaw)/ m_jump_time;
			ctrl_data_dir->linear_dependency		= false;
			//---------------------------------------------------------------------------------

			CBlend* Blend = m_man->animation().current_blend();
			ctrl_data->set_speed(is_flag(SControlJumpData::eEvasiveJump) ? Blend->timeTotal / m_jump_time : Blend->timeTotal / Blend->speed / m_jump_time);

		} else 
			ctrl_data->set_speed	(-1.f);

	}
}

void CControlJump::hit_test() 
{
	if (m_object_hitted)	return;
	if (!m_data.target_object)	return;

	// ѕроверить на нанесение хита во врем€ прыжка
	Fvector trace_from;
	m_object->Center(trace_from);

	collide::rq_result	l_rq;

	if (Level().ObjectSpace.RayPick(trace_from, m_object->Direction(), m_hit_trace_range * m_object->GetVisualScale(), collide::rqtObject, l_rq, m_object)) {
		if ((l_rq.O == m_data.target_object) && (l_rq.range < m_hit_trace_range * m_object->GetVisualScale())) {
			m_object_hitted = true;
		}
	}

	if (!m_object_hitted && m_data.target_object) {
		
		m_object_hitted = true;
		// определить дистанцию до врага
		Fvector d;
		d.sub(m_data.target_object->Position(),m_object->Position());
		if (d.magnitude() > m_hit_trace_range * m_object->GetVisualScale())
			m_object_hitted = false;

		// проверка на  Field-Of-Hit
		float my_h,my_p;
		float h,p;

		m_object->Direction().getHP(my_h,my_p);
		d.getHP(h,p);

		float from	= angle_normalize(my_h - PI_DIV_6);
		float to	= angle_normalize(my_h + PI_DIV_6);

		if (!is_angle_between(h, from, to)) m_object_hitted = false;

		from	= angle_normalize(my_p - PI_DIV_6);
		to		= angle_normalize(my_p + PI_DIV_6);

		if (!is_angle_between(p, from, to)) m_object_hitted = false;

	} 

	if (m_object_hitted)
	{
		Fvector TargetCenter;
		m_data.target_object->Center(TargetCenter);
		Fvector Direction;
		Direction.sub(TargetCenter, trace_from);
		const float Range = Direction.magnitude();
		if (Range > EPS_L)
		{
			Direction.div(Range);
			collide::rq_result Obstacle;
			if (Level().ObjectSpace.RayPick(trace_from, Direction, Range, collide::rqtStatic, Obstacle, m_object))
			{
				m_object_hitted = false;
				return;
			}
		}
	}
	if (m_object_hitted)
		m_object->HitEntityInJump(m_data.target_object != nullptr ? m_data.target_object->cast_entity() : nullptr);
}

bool CControlJump::can_jump(CObject *target)
{
	if (!AttackJumpEnabled || !target)
	{
		return false;
	}
	const bool	aggressive_jump	=	m_object->can_use_agressive_jump(target);
	Fvector		target_position;
	target->Center					(target_position);

	return							can_jump(target_position, aggressive_jump);
}

bool CControlJump::jump_intersect_geometry (Fvector const & target, CObject * const ignored_object, bool Evasive)
{
	const float JumpTime = calculate_jump_time(target, false, Evasive);

	Fvector velocity;
	velocity.sub					(target, m_object->Position());

	TransferenceToThrowVel			(velocity,JumpTime,physics_world()->Gravity());

	Fvector collide_position;
	collide::rq_results	temp_rq_results;
	xr_vector<trajectory_pick> *	pass_jump_picks		=	nullptr;
	xr_vector<Fvector> *	pass_collide_tris	=	nullptr;
#ifdef DEBUG		
	xr_vector<trajectory_pick>	jump_picks;
	pass_jump_picks		=	&jump_picks;
	xr_vector<Fvector>		collide_tris;
	pass_collide_tris	=	&collide_tris;
#endif // #ifdef DEBUG		

	const float Scale = m_object->GetVisualScale();
	Fvector Sizes = {0.8f, 1.4f, 0.8f};
	if (Scale != 1.f)
	{
		m_object->character_physics_support()->movement()->Box().getsize(Sizes);
	}

	Fvector const start_to_target	=	target - m_object->Position();
	if ( magnitude(start_to_target) < Scale )
		return false;

	Fvector const traj_start	=	m_object->Position() + Fvector().set(0, 1.2f * Scale, 0);
	Fvector const traj_target	=	target + Fvector().set(0, 1.2f * Scale, 0) - (normalize(start_to_target) * Scale);

	if ( trajectory_intersects_geometry	(JumpTime,
										 traj_start,
										 traj_target,
								 		 velocity,
								 		 collide_position,
								 		 m_object,
								 		 ignored_object,
								 		 temp_rq_results,
								 		 pass_jump_picks,
										 pass_collide_tris,
										 Sizes) )
	{
#ifdef DEBUG		
		m_object->m_jump_picks			=	jump_picks;
		m_object->m_jump_collide_tris	=	collide_tris;
#endif // #ifdef DEBUG		

		return						true;
	}

#ifdef DEBUG
	m_object->m_jump_picks				=	jump_picks;
	m_object->m_jump_collide_tris		=	collide_tris;
#endif // #ifdef DEBUG
	
	return							false;
}

bool CControlJump::can_jump(Fvector const& target, bool const aggressive_jump, float MaxHeadingAngle)
{
	if (s32(Device.dwTimeGlobal - EvasionRecoveryUntil) < 0)
	{
		return false;
	}
	if (MaxHeadingAngle >= 0.f && !IsEvasiveDistanceValid(target))
	{
		return false;
	}
	if ( m_time_next_allowed != 0 )
	{
		// in aggressive mode we can jump after 1/3 of m_delay_after_jump
		if ( m_time_next_allowed - (int)aggressive_jump*(2*m_delay_after_jump/3) > Device.dwTimeGlobal) 
		{
			return					false;
		}
	}

	if (!m_object->movement().restrictions().accessible(target))
		return						false;

	Fvector source_position		=	m_object->Position	();
	Fvector target_position		=	target;

	// проверка на dist
	float dist = source_position.distance_to(target_position);

	// in aggressive mode we can jump from distance >= 1
	const float test_min_distance = aggressive_jump ? std::min(1.f, get_min_distance()) : get_min_distance();
	if ( (dist < test_min_distance) || (dist > get_max_distance()) )
		return						false;

	// получить вектор направлени€ и его мир угол
	float		dir_yaw = Fvector().sub(target_position, source_position).getH();
	dir_yaw						=	angle_normalize(-dir_yaw);

	// проверка на angle
	float yaw_current, yaw_target;
	m_object->control().direction().get_heading(yaw_current, yaw_target);

	// Evasive point jumps may use a separate heading limit; ordinary attacks keep their species limit.
	const float HeadingLimit = MaxHeadingAngle >= 0.f ? clampr(MaxHeadingAngle, 0.f, PI) : m_max_angle;
	if (angle_difference(yaw_current, dir_yaw) > HeadingLimit)
		return						false;
	
	// check if target on the same floor etc
	if (std::abs(target_position.y-source_position.y) > m_max_height)
		return						false;

	// проверка prepare
	if (!is_flag(SControlJumpData::ePrepareSkip) && !is_flag(SControlJumpData::eGlideOnPrepareFailed)) {
		if (!is_flag(SControlJumpData::ePrepareInMove)) {
			VERIFY(m_data.state_prepare.motion.valid());
		} else {
			VERIFY(m_data.state_prepare_in_move.motion.valid());
			VERIFY(m_data.state_prepare_in_move.velocity_mask != u32(-1));

			// try to trace distance according to prepare animation
			bool good_trace_res = false;

			// get animation time
			float time			= m_man->animation().motion_time(m_data.state_prepare_in_move.motion, m_object->Visual());
			// set acceleration and velocity
			SVelocityParam &vel	= m_object->move().get_velocity(m_data.state_prepare_in_move.velocity_mask);
			float dist_ = time * vel.velocity.linear;

			// check nodes in direction
			Fvector target_point;
			target_point.mad(m_object->Position(), m_object->Direction(), dist_);

			if (m_man->path_builder().accessible(target_point)) {
				// нода в пр€мой видимости?
				m_man->path_builder().restrictions().add_border(m_object->Position(), target_point);
				u32 node = ai().level_graph().check_position_in_direction(m_object->ai_location().level_vertex_id(),m_object->Position(),target_point);
				m_man->path_builder().restrictions().remove_border();
				
				if (ai().level_graph().valid_vertex_id(node) && m_man->path_builder().accessible(node)) 
					good_trace_res = true;
			}

			if (!good_trace_res) {
				// cannot prepare in move, so check if can prepare in stand state
				if (!m_data.state_prepare.motion.valid()) return false;
			}
		}
	}

	return true;
}


Fvector CControlJump::predict_position(CObject *obj, const Fvector &pos)
{
	return pos;
}

