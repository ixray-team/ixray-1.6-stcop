#pragma once
#include "ai_stalker_animations.h"
#include "../../../xrServerEntities/xrServer_Space.h"

class CAI_Stalker;
class CSE_ALifeHumanStalker;

class aistalker_state_net
{
public:
	template<typename T>
	IC	void write_bits(const u32& bit_count, const u32& value, u32& current, T& output)
	{
		output |= ((value & ((T(1) << bit_count) - 1)) << current);
		current += bit_count;
	}

	template<typename T>
	IC	u32 read_bits(const u32& bit_count, u32& current, const u8& output)
	{
		u32			result = (output >> current) & ((T(1) << bit_count) - 1);
		current += bit_count;
		return result;
	}

	u64 NextSyncronizeDialogs = 0;

	// PHYSIC STATE
	bool physics_state_enabled;
	bool canOpenDialog;

	Fvector physics_linear_velocity;
	Fvector physics_position;
	u32 PhysicDwTime = 0;
  
	u16								u_active_slot = 0;
	u16								u_active_id = 0;
	u8								u_active_stripped = 0;
	
	StalkerMotionData				motions_data;
 	//StalkerMotionData				motions_server;

	u8								phSyncFlag = 0;
	u8								m_wounded = 0;

	Fvector							Position;
	SRotation						o_torso;
	SRotation						o_head;
	float							health = 1.0f;

	u8								id_TeamCSE;
	u8								id_SquadCSE;
	u8								id_GroupCSE;

	u16								graph_vertex_id;
 	float							distance_lvgraph;
 
 	shared_str						m_sStartDialog;
	shared_str						script_logic_data;
	u32								script_dwUpdate = 0;
  

	void UpdateAIInternal(CAI_Stalker* stalker);

	void CSE_StateWrite(NET_Packet& p);
	void CSE_StateRead(NET_Packet& p);

	void FillState(CAI_Stalker* stalker);
	void GetState(CAI_Stalker* stalker);
	 
	void FillStateCSE(CSE_ALifeHumanStalker* stalker);
	void GetStateCSE(CSE_ALifeHumanStalker* stalker);
};

