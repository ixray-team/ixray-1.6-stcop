#include "StdAfx.h"
#include "net_physics_state.h"

void net_physics_state::fill(SPHNetState &state, u32 time)
{
	dwTimeStamp = time;
	physics_linear_velocity = state.linear_vel;
	physics_position = state.position;
	physics_state_enabled = state.enabled;
}

void net_physics_state::write(NET_Packet &packet)
{
	packet << dwTimeStamp;
	packet << physics_position;
	packet << physics_state_enabled;
}

void net_physics_state::read(NET_Packet &packet)
{
	packet >> dwTimeStamp;
	packet >> physics_position;
	packet >> physics_state_enabled;

	physics_linear_velocity.set(0, 0, 0);
}
