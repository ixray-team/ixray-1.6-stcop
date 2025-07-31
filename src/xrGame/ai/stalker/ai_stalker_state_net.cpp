#include "stdafx.h"
#include "ai_stalker_state_net.h"
#include "stalker_movement_manager_smart_cover.h"
#include "Inventory.h"
#include "Weapon.h"
#include "ai/stalker/ai_stalker.h"

#include "CharacterPhysicsSupport.h"
#include "PHMovementControl.h"
#include "xrServer_Objects_ALife_Monsters.h"
#include "animation_movement_controller.h"
#include "physics_shell_animated.h"
#include "stalker_animation_manager.h"
 
void aistalker_state_net::UpdateAIInternal(CAI_Stalker* stalker)
{
	if (OnServer())
	{
		CSE_ALifeHumanStalker* cse_stalker =  smart_cast<CSE_ALifeHumanStalker*> ( Level().Server->ID_to_entity(stalker->ID()) );

		if (cse_stalker)
		{
			cse_stalker->s_team = id_TeamCSE;
			cse_stalker->s_group = id_GroupCSE;
			cse_stalker->s_squad = id_SquadCSE;

			cse_stalker->m_tNextGraphID = graph_vertex_id;
			cse_stalker->m_tPrevGraphID = graph_vertex_id;

			cse_stalker->m_fDistanceFromPoint = distance_lvgraph;
			cse_stalker->m_fDistanceToPoint = distance_lvgraph;
		}	
	}
}

// PACKET WRITE
void aistalker_state_net::CSE_StateWrite(NET_Packet& tNetPacket)
{	
	bool IsDead = health <= 0;

 	u8 flags = 0;
	u32 curvalue = 0;
	write_bits<u8>(1, m_wounded, curvalue, flags);
	write_bits<u8>(1, phSyncFlag, curvalue, flags);
	write_bits<u8>(1, IsDead, curvalue, flags);
	write_bits<u8>(1, canOpenDialog, curvalue, flags);
	tNetPacket.w_u8(flags);

	float whealth = health;
	clamp(whealth, 0.f, 1.f);

	tNetPacket.w_float_q8(whealth, 0, 1);												// 2

	// было все в Float Стало с квантованием (4 байт) было 16
	tNetPacket.w_angle8(o_torso.pitch);
	tNetPacket.w_angle8(o_torso.yaw);
	tNetPacket.w_angle8(o_head.pitch);
	tNetPacket.w_angle8(o_head.yaw);													// 6

	// 9 BYTE
 	if (IsDead)
	{
		tNetPacket.w_vec3(Position);													// 12 + 6 = 18 для трупа
		return;
	}

	// Slot
	tNetPacket.w_u16(u_active_slot);													// 6 + 5 = 11
 	if (u_active_slot != 0)
	{
		tNetPacket.w_u16(u_active_id);
		tNetPacket.w_u8(u_active_stripped);
	}

	if (phSyncFlag)		// 17 BYTE														// 11+17 = 28 Байт на нпс
	{
		tNetPacket.w_u8(physics_state_enabled);
		tNetPacket.w_vec3(physics_position);
		tNetPacket.w_u32(PhysicDwTime);
	}
	else
		tNetPacket.w_vec3(Position);
}

void aistalker_state_net::CSE_StateRead(NET_Packet& tNetPacket)
{
	u32 curvalue = 0;
	u8 flags = 0;
	tNetPacket.r_u8(flags);

	m_wounded = read_bits<u8>(1, curvalue, flags);
	phSyncFlag = read_bits<u8>(1, curvalue, flags);
	bool IsDead = read_bits<u8>(1, curvalue, flags);
	canOpenDialog = read_bits<u8>(1, curvalue, flags);
	
	tNetPacket.r_float_q8(health, 0, 1);
 	if (IsDead)
		health = 0;

	tNetPacket.r_angle8(o_torso.pitch);
	tNetPacket.r_angle8(o_torso.yaw);
 	tNetPacket.r_angle8(o_head.pitch);
	tNetPacket.r_angle8(o_head.yaw);			
	 
	if (IsDead)
	{
 		tNetPacket.r_vec3(Position);
		return;
	}
 
	// Slot		5 Байт
 	/* SINGLE */
	tNetPacket.r_u16(u_active_slot);
	if (u_active_slot != 0)
	{
		tNetPacket.r_u16(u_active_id);
		tNetPacket.r_u8(u_active_stripped);
	}
 
	if (phSyncFlag)		// Метод Fvector 1+12+4 = 17 байт
	{
		physics_state_enabled = tNetPacket.r_u8();
		tNetPacket.r_vec3(physics_position);
		PhysicDwTime = tNetPacket.r_u32();
		Position.set(physics_position);
	}
	else
		tNetPacket.r_vec3(Position);
}
 
// STATE READING CSE, STALKER_AI

#include "ai_space.h"
 
void aistalker_state_net::FillState(CAI_Stalker* stalker)
{
	motions_data.LastIsScript = stalker->animation().IsGlobalOrScriptPlaying;
	m_wounded				  = stalker->wounded();
	canOpenDialog			  = stalker->cast_inventory_owner()->IsTalkEnabled();

//	UpdateLogicScript(stalker);
	if (NextSyncronizeDialogs < Device.dwTimeGlobal)
	{
		NextSyncronizeDialogs = 1000 + Device.dwTimeGlobal;

		NET_Packet packet;
		stalker->u_EventGen(packet, GE_STALKER_DIALOG, stalker->ID());
		packet.w_stringZ(stalker->GetStartDialog());
		stalker->u_EventSend(packet);
	}


 	graph_vertex_id		= stalker->movement().game_dest_vertex_id();
	distance_lvgraph	= stalker->Position().distance_to(ai().game_graph().vertex(graph_vertex_id)->level_point());

	id_TeamCSE  = (u8) stalker->g_Team();
	id_SquadCSE = (u8) stalker->g_Squad();
	id_GroupCSE = (u8) stalker->g_Group();

 
	m_sStartDialog = stalker->GetStartDialog();
  
	// health
	health = stalker->GetfHealth();

	// agnles
 	o_torso.pitch = stalker->movement().m_body.current.pitch;
	o_torso.yaw = stalker->movement().m_body.current.yaw;
	//o_torso.roll = stalker->movement().m_body.current.roll;

	o_head.pitch = stalker->movement().m_head.current.pitch;
	o_head.yaw = stalker->movement().m_head.current.yaw;
	//o_head.roll = stalker->movement().m_head.current.roll;

	if (!stalker->g_Alive())
	{
		Position.set(stalker->Position());
 		return;
	}

	CPHSynchronize* sync = stalker->PHGetSyncItem(0);

	if (sync)
	{
		phSyncFlag = 1;

 		SPHNetState state;
		sync->get_State(state);
		// FILL STATE POS
		physics_state_enabled = state.enabled;
 		physics_position = state.position;
 		PhysicDwTime = Level().timeServer();
   	}
	else
	{
		phSyncFlag = 0;
 		Position = stalker->Position();
	}

	if (stalker->inventory().ActiveItem() != nullptr)
	{
		// inventory
		CWeapon* weapon = smart_cast<CWeapon*>(stalker->inventory().ActiveItem());

		if (weapon)
		{
			u_active_slot = stalker->inventory().ActiveItem()->CurrSlot();
			u_active_id = weapon->ID();
			u_active_stripped = weapon->strapped_mode();
		}
		else
		{
			u_active_slot = NO_ACTIVE_SLOT;
		}
	}
 	
//	motions_data.Initialize(motions_server);
}

void aistalker_state_net::GetState(CAI_Stalker* stalker)
{
	// ORIGINAL STATE
 
//	stalker->id_Team  = id_TeamCSE;
//	stalker->id_Squad = id_SquadCSE;
//	stalker->id_Group = id_GroupCSE;
 
	// NEW STATE
	{
		PIItem item = stalker->inventory().ItemFromSlot(u_active_slot);
		CWeapon* wpn = smart_cast<CWeapon*>(item);

		if (u_active_slot != 0)
		if (item && item->object_id() != u_active_id || !item)
		{
			CObject* obj = Level().Objects.net_Find(u_active_id);
			CInventoryItem* itemINV = smart_cast<CInventoryItem*>(obj);
			CGameObject* game_object = smart_cast<CGameObject*>(obj);

			if (itemINV && itemINV->parent_id() == stalker->ID())
			{
				stalker->inventory().Slot(u_active_slot, itemINV, true, true);
			}
		}

		stalker->inventory().SetActiveSlot(u_active_slot);
		if (wpn && wpn->strapped_mode() != !!u_active_stripped)
			wpn->strapped_mode(u_active_stripped);
	}

	stalker->m_wounded = m_wounded;
	//if (canOpenDialog)
	//	stalker->cast_inventory_owner()->EnableTalk();
	//else
	//	stalker->cast_inventory_owner()->DisableTalk();

 	stalker->SetfHealth(health);
	
	if (!stalker->g_Alive())
	{
 		stalker->movement().m_body.current.pitch = o_torso.pitch;
		stalker->movement().m_body.current.yaw = o_torso.yaw;
		stalker->movement().m_head.current.pitch = o_head.pitch;
		stalker->movement().m_head.current.yaw = o_head.yaw;
		stalker->Position().set(Position);
		return;
	}

	if (phSyncFlag)
	{
		stalker_interpolation::net_update_A N_A;
		N_A.State.enabled = physics_state_enabled;
		N_A.State.linear_vel = physics_linear_velocity;
		N_A.State.position = physics_position;
		N_A.o_torso = o_torso;
		N_A.head = o_head;
		N_A.dwTimeStamp = PhysicDwTime;
		 
		stalker->m_bInterpolate = true;
		stalker->postprocess_packet(N_A);
 	}
	else
	{

 		stalker->movement().m_body.current.pitch = o_torso.pitch;
		stalker->movement().m_body.current.yaw = o_torso.yaw;
 		stalker->movement().m_head.current.pitch = o_head.pitch;
		stalker->movement().m_head.current.yaw = o_head.yaw;
 		
  		stalker->Position().set(Position);

		stalker->NET_A.clear();
		//TODO: disable interpolation?
	}
		
	// Pavel: create structure for animation?
 
	if (stalker->g_Alive())
	{
		stalker->ApplyAnimation(motions_data);
	}
 
	if (Position.distance_to(stalker->Position()) > 10) // Апдейтим позицию по причине что не обновляемся )) (если не обновлять UpdateCL не запустится)
	{
		stalker->Position().set(Position);
		stalker->character_physics_support()->movement()->SetPosition(Position); // we need it ?
	}
 	
	
	stalker->character_physics_support()->movement()->CollisionEnable(false);
}


void aistalker_state_net::FillStateCSE(CSE_ALifeHumanStalker* stalker)
{
	//ForSave			= stalker->net_SaveMP;

//  id_TeamCSE		= stalker->s_team;
// 	id_GroupCSE		= stalker->s_group;
// 	id_SquadCSE		= stalker->s_squad;
	health			= stalker->get_health();
	Position		= stalker->o_Position;
	o_torso			= stalker->o_torso;
	
	//m_sStartDialog	= stalker->m_start_dialog;
}

void aistalker_state_net::GetStateCSE(CSE_ALifeHumanStalker* stalker)
{
	//ForSave = stalker->net_SaveMP;

	stalker->set_health(health);

	stalker->o_model = o_torso.yaw;
	stalker->o_torso = o_torso;

	stalker->o_Position = Position;

//	stalker->s_team = id_TeamCSE;
//	stalker->s_group = id_GroupCSE;
//	stalker->s_squad = id_SquadCSE;
//
//	stalker->m_tNextGraphID = graph_vertex_id;
//	stalker->m_tPrevGraphID = graph_vertex_id;
//
//	stalker->m_fDistanceFromPoint = distance_lvgraph;
//	stalker->m_fDistanceToPoint = distance_lvgraph;

	// Затычка чтобы много не жрало сети
	CAI_Stalker* sta = smart_cast<CAI_Stalker*>(Level().Objects.net_Find(stalker->ID));
	if (sta)
	{
		stalker->m_start_dialog = sta->GetStartDialog();
	}
}
 