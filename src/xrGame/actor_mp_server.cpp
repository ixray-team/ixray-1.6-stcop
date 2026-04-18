#include "StdAfx.h"
#include "actor_mp_server.h"
#include "Level.h"

CSE_ActorMP::CSE_ActorMP		(const char* section) : 
	inherited				(section)
{
	m_ready_to_update		= false;
}

void CSE_ActorMP::STATE_Read	(NET_Packet &packet, u16 size)
{
	inherited::STATE_Read	(packet,size);
	
#ifdef DEBUG
	Msg("--- Actor %d[%s] STATE_Read, health is: %2.04f", this->ID, this->name_replace(),
		m_state_holder.state().health);
#endif // #ifdef DEBUG
}

void CSE_ActorMP::STATE_Write	(NET_Packet &packet)
{
	inherited::STATE_Write	(packet);
#ifdef DEBUG
	Msg("--- Actor %d[%s] STATE_Write, health is: %2.04f", this->ID, this->name_replace(),
		m_state_holder.state().health);
#endif // #ifdef DEBUG
}

bool CSE_ActorMP::Net_Relevant	()
{
	if (get_health() <= 0) return (false);
	return !IsGameTypeSingle();
}

void CSE_ActorMP::fill_state(actor_mp_state& state)
{
	state.physics_quaternion = m_AliveState.quaternion;
	state.physics_angular_velocity = m_AliveState.angular_vel;
	state.physics_linear_velocity = m_AliveState.linear_vel;
	state.physics_force = m_AliveState.force;
	state.physics_torque = m_AliveState.torque;
	state.physics_position = m_AliveState.position;

	state.position = o_Position;

	state.logic_acceleration = accel;

	state.model_yaw = angle_normalize(o_model);
	state.camera_yaw = angle_normalize(o_torso.yaw);
	state.camera_pitch = angle_normalize(o_torso.pitch);
	state.camera_roll = angle_normalize(o_torso.roll);

	state.time = timestamp;

	state.inventory_active_slot = weapon;
	state.body_state_flags = mstate;
	state.health = get_health();
	state.radiation = fRadiation;
	state.physics_state_enabled = m_AliveState.enabled ? 1 : 0;

	m_ready_to_update = true;
}

void CSE_ActorMP::UPDATE_Write(NET_Packet& packet)
{
	if (!m_ready_to_update)
	{
		actor_mp_state state;
		fill_state(state);
		m_state_holder.relevant(state);
	}

	m_state_holder.write(packet);
}

void CSE_ActorMP::UPDATE_Read(NET_Packet& packet)
{
	flags = 0;
	m_u16NumItems = 1;
	velocity.set(0.f, 0.f, 0.f);

	if (get_health() <= 0)
	{
		actor_mp_state_holder	tmp_state_holder;
		tmp_state_holder.read(packet);
		return;
	}

	m_state_holder.read(packet);

	m_AliveState.quaternion = m_state_holder.state().physics_quaternion;
	m_AliveState.angular_vel = m_state_holder.state().physics_angular_velocity;
	m_AliveState.linear_vel = m_state_holder.state().physics_linear_velocity;
	m_AliveState.force = m_state_holder.state().physics_force;
	m_AliveState.torque = m_state_holder.state().physics_torque;
	m_AliveState.position = m_state_holder.state().physics_position;

	o_Position = m_state_holder.state().position;

	accel = m_state_holder.state().logic_acceleration;

	o_model = m_state_holder.state().model_yaw;
	o_torso.yaw = m_state_holder.state().camera_yaw;
	o_torso.pitch = m_state_holder.state().camera_pitch;
	o_torso.roll = m_state_holder.state().camera_roll;

	timestamp = m_state_holder.state().time;

	weapon = m_state_holder.state().inventory_active_slot;
	mstate = m_state_holder.state().body_state_flags;
	set_health(m_state_holder.state().health);
	fRadiation = m_state_holder.state().radiation;
	m_AliveState.enabled = m_state_holder.state().physics_state_enabled;

	m_ready_to_update = true;
}

#ifdef XRGAME_EXPORTS
void CSE_ActorMP::on_death(CSE_Abstract* killer)
{
	inherited::on_death(killer);

	actor_mp_state				state;
	fill_state(state);
	m_state_holder.relevant(state);
}
#endif