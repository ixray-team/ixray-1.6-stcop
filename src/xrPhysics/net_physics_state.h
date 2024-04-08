#pragma once
#include "stdafx.h"
#include "..\xrServerEntities\PHNetState.h"

#define MIN_LINEAR_VELOCITY_COMPONENT -32.f
#define MAX_LINEAR_VELOCITY_COMPONENT 32.f

class XRPHYSICS_API net_physics_state
{
public:
	Fvector physics_linear_velocity = { 0, 0, 0 };
	Fvector physics_position = { 0, 0, 0 };
	bool	physics_state_enabled = false;
	u32		dwTimeStamp = 0;

public:
	void fill(SPHNetState &state, u32 time);
	void write(NET_Packet &packet);
	void read(NET_Packet &packet);
};
