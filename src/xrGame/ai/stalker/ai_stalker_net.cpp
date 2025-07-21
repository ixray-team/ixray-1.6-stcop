#include "stdafx.h"
#include "stdafx.h"
#include "ai_stalker.h"

#include "ai_object_location.h"
#include "CharacterPhysicsSupport.h"
#include "stalker_movement_manager_smart_cover.h"


void CAI_Stalker::net_Save(NET_Packet& P)
{
	inherited::net_Save(P);
	m_pPhysics_support->in_NetSave(P);
}

BOOL CAI_Stalker::net_SaveRelevant()
{
	return (inherited::net_SaveRelevant() || BOOL(PPhysicsShell() != NULL));
}

void CAI_Stalker::net_Export(NET_Packet& P)
{
	if (!IsGameTypeSingle())
	{
		state_manager.FillState(this);
		state_manager.UpdateAIInternal(this);

		state_manager.CSE_StateWrite(P);
		return;
	}

	R_ASSERT(Local());

	// export last known packet
	R_ASSERT(!NET.empty());
	net_update& N = NET.back();

	P.w_float(GetfHealth());

	P.w_u32(N.dwTimeStamp);
	P.w_u8(0);
	P.w_vec3(N.p_pos);
	P.w_float(N.o_model);
	P.w_float(N.o_torso.yaw);
	P.w_float(N.o_torso.pitch);
	P.w_float(N.o_torso.roll);
	P.w_u8(u8(g_Team()));
	P.w_u8(u8(g_Squad()));
	P.w_u8(u8(g_Group()));


	float f1 = 0;
	GameGraph::_GRAPH_ID l_game_vertex_id = ai_location().game_vertex_id();
	P.w(&l_game_vertex_id, sizeof(l_game_vertex_id));
	P.w(&l_game_vertex_id, sizeof(l_game_vertex_id));

	if (ai().game_graph().valid_vertex_id(l_game_vertex_id)) 
	{
		f1 = Position().distance_to(ai().game_graph().vertex(l_game_vertex_id)->level_point());
		P.w(&f1, sizeof(f1));

		f1 = Position().distance_to(ai().game_graph().vertex(l_game_vertex_id)->level_point());
		P.w(&f1, sizeof(f1));
	}
	else 
	{
		P.w(&f1, sizeof(f1));
		P.w(&f1, sizeof(f1));
	}

	P.w_stringZ(m_sStartDialog);
}
 
void CAI_Stalker::net_Import(NET_Packet& P)
{
	state_manager.CSE_StateRead(P);
	state_manager.GetState(this);
	setVisible(TRUE);
	setEnabled(TRUE);
}

#include "relation_registry.h"
#include "animation_movement_controller.h"
#include "HUDManager.h"

#define MATPUSH(a)	\
((a)._11), ((a)._12), ((a)._13), ((a)._14), \
((a)._21), ((a)._22), ((a)._23), ((a)._24), \
((a)._31), ((a)._32), ((a)._33), ((a)._34), \
((a)._41), ((a)._42), ((a)._43), ((a)._44)

// ANIMATION CALLBACKS (ДЛЯ СИНХРОНИЗАЦИИ)
void SetDataMotion(Motions_NUM& data, const MotionID& motionID, u8 boneID = 0)
{
	if (data.IDX >= 255)	// 16 Изза Квантования
		data.IDX = 0;
	data.IDX++;
	data.id = motionID;
	data.boneID = boneID;
	//data.dwTimeGlobal = Device.dwTimeGlobal;
}

void CAI_Stalker::OnAnimationChangeTorso(const MotionID& motionID, bool mix, bool continue_position, bool m_step_dependence, bool anim_stand)
{
	Motions_NUM data;
	state_manager.motions_data.GetTorso(data);
	SetDataMotion(data, motionID);
	state_manager.motions_data.SetTorso(data);
	if (!g_Alive())
		return;

	if (OnServer())
	{
		NET_Packet  packet;
		Level().Server->game->u_EventGen(packet, GE_STALKER_ANIMATION, this->ID());
		packet.w_u32(1);
		packet.w_u16(data.id.idx);
		packet.w_u16(data.id.slot);
		packet.w_u32(data.IDX);
		Level().Server->game->u_EventSend(packet);
	}
}

void CAI_Stalker::OnAnimationChangeHead(const MotionID& motionID, bool mix, bool continue_position, bool m_step_dependence, bool anim_stand)
{
	Motions_NUM data;
	state_manager.motions_data.GetHead(data);
	SetDataMotion(data, motionID);
	state_manager.motions_data.SetHead(data);
	if (!g_Alive())
		return;

	if (OnServer())
	{
		NET_Packet  packet;
		Level().Server->game->u_EventGen(packet, GE_STALKER_ANIMATION, this->ID());
		packet.w_u32(2);
		packet.w_u16(data.id.idx);
		packet.w_u16(data.id.slot);
		packet.w_u32(data.IDX);
		Level().Server->game->u_EventSend(packet);
	}	
}

void CAI_Stalker::OnAnimationChangeLegs(const MotionID& motionID, bool mix, bool continue_position, bool m_step_dependence, bool anim_stand)
{
	Motions_NUM data;
	state_manager.motions_data.GetLegs(data);
	SetDataMotion(data, motionID);
	state_manager.motions_data.SetLegs(data);
	if (!g_Alive())
		return;

	if (OnServer())
	{
		// SCRIPT TO PLAY ANY FASTER
		NET_Packet  packet;
		Level().Server->game->u_EventGen(packet, GE_STALKER_ANIMATION, this->ID());
		packet.w_u32(3);
		packet.w_u16(data.id.idx);
		packet.w_u16(data.id.slot);
		packet.w_u32(data.IDX);
		Level().Server->game->u_EventSend(packet);
	}
}

void CAI_Stalker::OnAnimationChangeScript(u8 boneID, const MotionID& motionID, bool mix, bool use_controller, bool local_animation, Fmatrix* target_matrix)
{
	Motions_NUM data;
	state_manager.motions_data.GetScript(data);
	SetDataMotion(data, motionID, boneID);
	data.motion_matrix = *target_matrix;
	data.AnimationCtrl = use_controller;
	state_manager.motions_data.SetScript(data);


	state_manager.motions_data.script_dwReciveTime = Level().timeServer();
	if (!g_Alive())
		return;

	if (OnServer())
	{
		// SCRIPT TO PLAY ANY FASTER
		NET_Packet  packet;
		Level().Server->game->u_EventGen(packet, GE_STALKER_ANIMATION, this->ID());
		packet.w_u32(4);
		packet.w_u16(data.id.idx);
		packet.w_u16(data.id.slot);
		packet.w_u32(data.IDX);
		packet.w_u8(data.boneID);
		packet.w_u8(data.AnimationCtrl);
		packet.w_matrix(*target_matrix);
		Level().Server->game->u_EventSend(packet);
	}
}

void CAI_Stalker::OnAnimationChangeGlobal(u8 boneID, const MotionID& motionID, bool mix, bool use_controller, bool local_animation, Fmatrix* target_matrix)
{
	if (!g_Alive())
		return;

	Motions_NUM data;
	state_manager.motions_data.GetGlobal(data);
	SetDataMotion(data, motionID, boneID);
	data.AnimationCtrl = use_controller;	
	state_manager.motions_data.SetGlobal(data);

	// TODO SYNC FOR GLOBAL (НО Нужно ли ) в Аномалии нужно для Зомби некоторых  
}

// APPLY
extern int g_stalker_event_anim;
extern int g_stalker_activate_animator;
 
void ApplyBYParts_Event(IKinematicsAnimated* ka, CAI_Stalker* stalker, StalkerMotionData& anims, MotionID& motion, u8 bone_part)
{ 	 	
	CBlend* m_blend = 0;
	for (u16 i = 0; i < MAX_PARTS; ++i)
	{
		if (!(bone_part & (1 << i)))
			continue;

		if (true) // g_stalker_activate_animator
		{
			CBlend* blend = 0;
			if (!m_blend)
			{
				blend = ka->LL_PlayCycle(i, motion, TRUE, 0, 0);
				if (blend && !m_blend)
					m_blend = blend;

				if (anims.ScriptAnimCtrl && blend)
					stalker->create_anim_mov_ctrl(blend, &stalker->UpdateMatrix, true);
			}
			else
			{
				ka->LL_PlayCycle(i, motion, TRUE, 0, 0);
			}
		}
		else
		{
			ka->LL_PlayCycle(i, motion, TRUE, 0, 0);
		}
	}
}

void ApplyBYParts(IKinematicsAnimated* ka, CAI_Stalker* stalker, StalkerMotionData& anims, MotionID& motion, u8 bone_part, bool Script)
{
	CBlend* m_blend = 0;
	for (u16 i = 0; i < MAX_PARTS; ++i)
	{
		//		if (bone_part != 255)
		//		if (!(bone_part & (1 << i)))
		//			continue;

		CBlend* blend = 0;
		if (!m_blend)
		{
			blend = ka->LL_PlayCycle(i, motion, TRUE, 0, 0);

			if (blend && !m_blend)
				m_blend = blend;

			if (anims.ScriptAnimCtrl && Script)
			{
				stalker->UpdateMatrix.set(stalker->XFORM());
				stalker->create_anim_mov_ctrl(blend, &stalker->UpdateMatrix, true);
			}
			else
			{
				if (stalker->animation_movement() && !Script)
					stalker->animation_movement()->stop();
			}
		}
		else
		{
			ka->LL_PlayCycle(i, motion, TRUE, 0, 0);
		}
	}
}

/*
bone_or_part:
0: legs
1: torso
2: head
*/

enum EventTypeSTALKER
{
	eRecvestIDX = 0,
};

void CAI_Stalker::ApplyAnimation(StalkerMotionData& anims)
{
	IKinematicsAnimated* ka = smart_cast<IKinematicsAnimated*>(Visual());
	if (!ka)
		return;

	// Если нет установки анимы в течении 2х сек то ставим	

	if (LastUpdateAnims < Device.dwTimeGlobal)
	{
		LastUpdateAnims = Device.dwTimeGlobal + 3000;
		auto blendLegs = ka->LL_PartBlend(0, 0);
		auto blendTors = ka->LL_PartBlend(1, 0);  
		auto blendHead = ka->LL_PartBlend(2, 0);

		if (!Level().game)
			return;	

		auto data = state_manager.motions_data;
		bool ALL_UNDEFINED = !data.torso.valid() && !data.head.valid() && !data.legs.valid() && !data.script.valid();

		//|| (blendTors && blendTors->motionID.idx == 602) || (blendLegs && blendLegs->motionID.idx == 602) || (blendHead && blendHead->motionID.idx == 602)

		if ( ALL_UNDEFINED )
		{
			// Msg("Recvest Sync Packet From NPC: %s", cName().c_str());
			NET_Packet packet;
			u_EventGen(packet, GE_STALKER_ANIMATION, this->ID());
			packet.w_u32(eRecvestIDX);
			packet.w_clientID(Level().game->local_svdpnid);
			u_EventSend(packet);
		}
	}
}
  
void CAI_Stalker::UpdateScriptAnim(NET_Packet& packet)
{
	if (OnServer())
	{
		// RECIVE RECVEST
		u32 type = packet.r_u32();
		ClientID clID; packet.r_clientID(clID);
		if (type == eRecvestIDX)
		{
			Motions_NUM datascript;
			state_manager.motions_data.GetScript(datascript);

			bool SendScript = false;
			if (animation_movement() && animation_movement()->IsActive() && datascript.id.valid())
			{
				SendScript = true;
			}

			if (!SendScript)
			{
				Motions_NUM data;
				// LEGS
				{
					NET_Packet packet;
					state_manager.motions_data.GetLegs(data);
					Level().Server->game->u_EventGen(packet, GE_STALKER_ANIMATION, this->ID());
					packet.w_u32(3);
					packet.w_u16(data.id.idx);
					packet.w_u16(data.id.slot);
					packet.w_u32(data.IDX);
					Level().Server->SendTo(clID, packet, net_flags(true, true));
				}

				state_manager.motions_data.GetTorso(data);
				// TORSO
				{
					NET_Packet packet;
					state_manager.motions_data.GetLegs(data);
					Level().Server->game->u_EventGen(packet, GE_STALKER_ANIMATION, this->ID());
					packet.w_u32(1);
					packet.w_u16(data.id.idx);
					packet.w_u16(data.id.slot);
					packet.w_u32(data.IDX);
					Level().Server->SendTo(clID, packet, net_flags(true, true));
				}
				state_manager.motions_data.GetHead(data);
				// HEAD
				{
					NET_Packet packet;
					state_manager.motions_data.GetLegs(data);
					Level().Server->game->u_EventGen(packet, GE_STALKER_ANIMATION, this->ID());
					packet.w_u32(2);
					packet.w_u16(data.id.idx);
					packet.w_u16(data.id.slot);
					packet.w_u32(data.IDX);
					Level().Server->SendTo(clID, packet, net_flags(true, true));
				}
			}
			else
			{
				state_manager.motions_data.GetScript(datascript);

				// SCRIPT TO PLAY ANY FASTER
				NET_Packet  packet;
				Level().Server->game->u_EventGen(packet, GE_STALKER_ANIMATION, this->ID());
				packet.w_u32(4);
				packet.w_u16(datascript.id.idx);
				packet.w_u16(datascript.id.slot);
				packet.w_u32(datascript.IDX);
				packet.w_u8(datascript.boneID);
				packet.w_u8(datascript.AnimationCtrl);
				packet.w_matrix(datascript.motion_matrix);

				Level().Server->SendTo(clID, packet, net_flags(true, true));
			}
		}


		return;
	}

	if (!RDEVICE.b_is_Ready)
	{
		Msg("Skip Process Event: NPC: %d", ID());
		return;
	}


	IKinematicsAnimated* ka = smart_cast<IKinematicsAnimated*>(Visual());
	if (!ka)
		return;

	if (!g_Alive())
		return;

	LastUpdateAnims = Device.dwTimeGlobal;

	auto& anims = state_manager.motions_data;
	 
	u32 TYPE = packet.r_u32();
	Motions_NUM data;
	data.id.idx		  = packet.r_u16();
	data.id.slot	  = packet.r_u16();
	data.IDX		  = packet.r_u32();
	
	if (!data.id.valid())
		return;

	if (data.id.slot >= 4)
		return;

	if (data.id.idx > 4096)
		return;

	Msg("UpdateScriptAnim Type[%u] Motion[%u] Slot[%u] IDX[%u]", TYPE, data.id.idx, data.id.slot, data.IDX);

	 
	if (TYPE == 1) // TORSO
	{
		//if (ka->get_animation_valid(data.id))
		{
			anims.SetTorso(data);
			try 
			{
				ka->LL_PlayCycle(1, data.id, TRUE, 0, 0);
			}
			catch (...)
			{
				Msg("Cant PlayCycle TORSO: %s, IDX: %d, SLOT: %d", cNameVisual().c_str(), data.id.idx, data.id.slot);
			}
		}
	}

	if (TYPE == 2) // HEAD
	{
		//if (ka->get_animation_valid(data.id))
		{
			anims.SetHead(data);
			try
			{
				ka->LL_PlayCycle(2, data.id, TRUE, 0, 0);
			}
			catch (...)
			{
				Msg("Cant PlayCycle HEAD: %s, IDX: %d, SLOT: %d", cNameVisual().c_str(), data.id.idx, data.id.slot);
			}
		}
	}

	if (TYPE == 3) // LEGS
	{
		//if (ka->get_animation_valid(data.id))
		{
			anims.SetLegs(data);
			try
			{
				ka->LL_PlayCycle(0, data.id, TRUE, 0, 0);
			}
			catch (...)
			{
				Msg("Cant PlayCycle: LEGS: %s, IDX: %d, SLOT: %d", cNameVisual().c_str(), data.id.idx, data.id.slot);
			}
		}
	}

	if (TYPE == 4)
	{
		data.boneID = packet.r_u8();
		data.AnimationCtrl = packet.r_u8();
		packet.r_matrix(data.motion_matrix);

		 
		//if (ka->get_animation_valid(data.id))
		{
			// SET ANIM
			anims.SetScript(data);
			// APPLY
			UpdateMatrix = data.motion_matrix;
			
			try
			{
				ApplyBYParts_Event(ka, this, anims, anims.script, data.boneID);
			}
			catch (...)
			{
				Msg("Cant APPLY Script: %s, Anim: IDX: %d, SLOT: %d, BONE: %d", cNameVisual().c_str(), data.id, data.id.slot, data.boneID);
			}
		}
	}
}

 