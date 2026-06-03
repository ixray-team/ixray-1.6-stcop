#include "stdafx.h"
#include "actor_mp_client.h"
#include "ActorCondition.h"
#include "../xrEngine/CameraBase.h"
#include "../xrEngine/CameraManager.h"

#include "game_cl_base.h"
#include "ui/UIActorMenu.h"
#include "ui/UIDragDropReferenceList.h"
#include "UIGameCustom.h"
#include "eatable_item.h"
#include "Inventory.h"
#include "CharacterPhysicsSupport.h"

//if we are not current control entity we use this value
const float	CActorMP::cam_inert_value = 0.7f;

CActorMP::CActorMP			()
{
	//m_i_am_dead				= false;
}

void CActorMP::OnEvent		( NET_Packet &P, u16 type)
{
	if (type == GEG_PLAYER_USE_BOOSTER)
	{
		use_booster(P);
		return;
	}
	inherited::OnEvent(P,type);
}

void CActorMP::Die			(CObject *killer)
{
	//m_i_am_dead				= true;
	//conditions().health()	= 0.f;
	conditions().SetHealth( 0.f );
	inherited::Die			(killer);
}

void CActorMP::use_booster(NET_Packet &packet)
{
	if (OnServer())
		return;

	u16 tmp_booster_id;
	packet.r_u16			(tmp_booster_id);
	CObject* tmp_booster =	Level().Objects.net_Find(tmp_booster_id);
	VERIFY2(tmp_booster, "using unknown or deleted booster");
	if (!tmp_booster)
	{
		Msg("! ERROR: trying to use unkown booster object, ID = %d", tmp_booster_id);
		return;
	}

	CEatableItem* tmp_eatable = tmp_booster->cast_eatable_item();
	VERIFY2(tmp_eatable, "using not eatable object");
	if (!tmp_eatable)
	{
		Msg("! ERROR: trying to use not eatable object, ID = %d", tmp_booster_id);
		return;
	}
	tmp_eatable->UseBy(this);
}

void CActorMP::On_SetEntity()
{
	prev_cam_inert_value = psCamInert;
	if (this != Level().CurrentControlEntity())
	{
		psCamInert = cam_inert_value;
	}
	inherited::On_SetEntity();
}

void CActorMP::On_LostEntity()
{
	psCamInert = prev_cam_inert_value;
}

void CActorMP::fill_state(actor_mp_state& state)
{
	if (PHGetSyncItem(0) != nullptr)
	{
		SPHNetState State;

		PHGetSyncItem(0)->get_State(State);
		state.physics_quaternion = State.quaternion;
		state.physics_angular_velocity = State.angular_vel;
		state.physics_linear_velocity = State.linear_vel;
		state.physics_force = State.force;
		state.physics_torque = State.torque;
		state.physics_position = State.position;
		state.physics_state_enabled = State.enabled ? 1 : 0;
	}

	state.position = Position();

	state.logic_acceleration = NET_SavedAccel;

	state.model_yaw = angle_normalize(r_model_yaw);
	state.camera_yaw = angle_normalize(unaffected_r_torso.yaw);
	state.camera_pitch = angle_normalize(unaffected_r_torso.pitch);
	state.camera_roll = angle_normalize(unaffected_r_torso.roll);

	state.time = Level().timeServer();

	state.inventory_active_slot = inventory().GetActiveSlot();
	state.body_state_flags = mstate_real & 0x0000ffff;
	state.health = GetfHealth();
	//because after packing to 1 byte, this value can be positive...
	if (state.health < EPS)
		state.health = 0;

	state.radiation = g_Radiation() / 100.0f;
}

bool CActorMP::net_Relevant()
{
	if (Holder())
	{
		return false;
	}

	if (character_physics_support()->IsRemoved())
	{
		return (false);
	}

	actor_mp_state state;
	fill_state(state);
	return (m_state_holder.relevant(state));
}

void CActorMP::net_Export(NET_Packet& packet)
{
	m_state_holder.write(packet);
}

void CActorMP::net_Import(NET_Packet& P)
{
	net_update N;

	m_state_holder.read(P);

	if (OnClient())
	{
		game_PlayerState* ps = Game().GetPlayerByGameID(this->object_id());
		float new_health = m_state_holder.state().health;
		if (GetfHealth() < new_health)
		{
			SetfHealth(new_health);
		}
		else
		{
			if (!ps || !ps->testFlag(GAME_PLAYER_FLAG_INVINCIBLE))
			{
				SetfHealth(new_health);
			}
		}
	}

	if (PPhysicsShell() != nullptr)
	{
		return;
	}

	if (OnClient())
	{
		SetfRadiation(m_state_holder.state().radiation * 100.0f);
	}

	u16	ActiveSlot = m_state_holder.state().inventory_active_slot;

	if (OnClient() && (inventory().GetActiveSlot() != ActiveSlot))
	{
#ifdef DEBUG
		Msg("Client-SetActiveSlot[%d][%d]", ActiveSlot, Device.dwFrame);
#endif // #ifdef DEBUG
		inventory().SetActiveSlot(ActiveSlot);
	}

	N.mstate = m_state_holder.state().body_state_flags;

	N.dwTimeStamp = m_state_holder.state().time;
	N.p_pos = m_state_holder.state().position;

	N.o_model = m_state_holder.state().model_yaw;
	N.o_torso.yaw = m_state_holder.state().camera_yaw;
	N.o_torso.pitch = m_state_holder.state().camera_pitch;
	N.o_torso.roll = m_state_holder.state().camera_roll;

	if (N.o_torso.roll > PI)
	{
		N.o_torso.roll -= PI_MUL_2;
	}

	if (Level().IsDemoPlay() || OnServer() || Remote())
	{
		unaffected_r_torso.yaw = N.o_torso.yaw;
		unaffected_r_torso.pitch = N.o_torso.pitch;
		unaffected_r_torso.roll = N.o_torso.roll;

		cam_Active()->yaw = -N.o_torso.yaw;
		cam_Active()->pitch = N.o_torso.pitch;
	}

	//CSE_ALifeCreatureActor
	N.p_accel = m_state_holder.state().logic_acceleration;

	process_packet(N);

	net_update_A			N_A;
	m_States.clear();

	N_A.State.enabled = m_state_holder.state().physics_state_enabled;
	N_A.State.angular_vel = m_state_holder.state().physics_angular_velocity;
	N_A.State.linear_vel = m_state_holder.state().physics_linear_velocity;
	N_A.State.force = m_state_holder.state().physics_force;
	N_A.State.torque = m_state_holder.state().physics_torque;
	N_A.State.position = m_state_holder.state().physics_position;
	N_A.State.quaternion = m_state_holder.state().physics_quaternion;

	// interpolcation
	postprocess_packet(N_A);
}

void CActorMP::postprocess_packet(net_update_A& N_A)
{
	if (!NET.empty())
		N_A.dwTimeStamp = NET.back().dwTimeStamp;
	else
		N_A.dwTimeStamp = Level().timeServer();

	N_A.State.previous_position = N_A.State.position;
	N_A.State.previous_quaternion = N_A.State.quaternion;

	if (Local() && OnClient() || !g_Alive()) return;

	{
		//-----------------------------------------------
		if (!NET_A.empty() && N_A.dwTimeStamp < NET_A.back().dwTimeStamp) return;
		if (!NET_A.empty() && N_A.dwTimeStamp == NET_A.back().dwTimeStamp)
		{
			NET_A.back() = N_A;
		}
		else
		{
			NET_A.push_back(N_A);
			if (NET_A.size() > 5) NET_A.pop_front();
		};

		if (!NET_A.empty()) m_bInterpolate = true;
	};

	Level().AddObject_To_Objects4CrPr(this);
	CrPr_SetActivated(false);
	CrPr_SetActivationStep(0);
}

void CActorMP::process_packet(net_update& N)
{
	if (Local() && OnClient())
		return;

	if (!NET.empty() && (N.dwTimeStamp < NET.back().dwTimeStamp))
		return;

	if (g_Alive()) {
		setVisible((bool)!HUDview());
		setEnabled(true);
	};

	if (!NET.empty() && (N.dwTimeStamp == NET.back().dwTimeStamp)) {
		NET.back() = N;
		return;
	}

	NET.push_back(N);

	if (NET.size() > 5)
		NET.pop_front();
}
