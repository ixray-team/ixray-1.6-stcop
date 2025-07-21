#include "StdAfx.h"
#include "script_game_object.h"
#include "HudItem.h"
#include "physic_item.h"
#include "Actor.h"
#include "ActorEffector.h"
#include "Missile.h"
#include "xrMessages.h"
#include "Level.h"
#include "Inventory.h"
#include "../xrEngine/CameraBase.h"
#include "player_hud.h"
#include "../xrEngine/SkeletonMotions.h"
#include "script_game_object.h"
#include "../../xrUI/ui_base.h"
#include "HUDManager.h"
#include "../xrScripts/script_callback_ex.h"

ENGINE_API extern float psHUD_FOV_def;

CHudItem::CHudItem()
{
	RenderHud					(TRUE);
	m_bStopAtEndAnimIsRunning	= false;
	m_current_motion_def		= nullptr;
	m_started_rnd_anim_idx		= u8(-1);
	m_nearwall_last_hud_fov		= psHUD_FOV_def;
	m_eDevicesFlags.zero();
}

DLL_Pure *CHudItem::_construct	()
{
	m_object			= smart_cast<CPhysicItem*>(this);
	VERIFY				(m_object);

	m_item				= smart_cast<CInventoryItem*>(this);
	VERIFY				(m_item);

	return				(m_object);
}

CHudItem::~CHudItem()
{
}

void CHudItem::Load(LPCSTR section)
{
	hud_sect				= pSettings->r_string		(section,"hud");
	hud_sect_cache = hud_sect;

	m_animation_slot		= pSettings->r_u32			(section,"animation_slot");

	m_nearwall_dist_min = READ_IF_EXISTS(pSettings, r_float, section, "nearwall_dist_min", .2f);
	m_nearwall_dist_max = READ_IF_EXISTS(pSettings, r_float, section, "nearwall_dist_max", 1.f);
	m_nearwall_target_hud_fov = READ_IF_EXISTS(pSettings, r_float, section, "nearwall_target_hud_fov", 0.27f);
	m_nearwall_speed_mod = READ_IF_EXISTS(pSettings, r_float, section, "nearwall_speed_mod", 10.f);

	m_fHudFov = READ_IF_EXISTS(pSettings, r_float, hud_sect, "hud_fov", 0.0f);

	m_current_inertion.PitchOffsetR = READ_IF_EXISTS(pSettings, r_float, hud_sect, "inertion_pitch_offset_r", PITCH_OFFSET_R);
	m_current_inertion.PitchOffsetD = READ_IF_EXISTS(pSettings, r_float, hud_sect, "inertion_pitch_offset_d", PITCH_OFFSET_D);
	m_current_inertion.PitchOffsetN = READ_IF_EXISTS(pSettings, r_float, hud_sect, "inertion_pitch_offset_n", PITCH_OFFSET_N);

	m_current_inertion.OriginOffset = READ_IF_EXISTS(pSettings, r_float, hud_sect, "inertion_origin_offset", ORIGIN_OFFSET);
	m_current_inertion.TendtoSpeed = READ_IF_EXISTS(pSettings, r_float, hud_sect, "inertion_tendto_speed", TENDTO_SPEED);

	m_jitter_params.pos_amplitude = READ_IF_EXISTS(pSettings, r_float, "gunslinger_base", "base_jitter_pos_amplitude", 0.001f);
	m_jitter_params.rot_amplitude = READ_IF_EXISTS(pSettings, r_float, "gunslinger_base", "base_jitter_rot_amplitude", 0.1f);

	m_jitter_params.pos_amplitude = READ_IF_EXISTS(pSettings, r_float, hud_sect, "jitter_pos_amplitude", m_jitter_params.pos_amplitude);
	m_jitter_params.rot_amplitude = READ_IF_EXISTS(pSettings, r_float, hud_sect, "jitter_rot_amplitude", m_jitter_params.rot_amplitude);

	m_jitter_params.stop_time = floor(READ_IF_EXISTS(pSettings, r_float, hud_sect, "jitter_stop_time", 3.0f) * 1000.f);

	m_bDisableBore = READ_IF_EXISTS(pSettings, r_bool, hud_sect, "disable_bore", false);

	m_HudLight.SetInstalled(READ_IF_EXISTS(pSettings, r_bool, section, "torch_installed", false));
	m_HudLight.NewTorchlight(section);

	LoadSounds(section);
}

void CHudItem::LoadSounds(LPCSTR section)
{
	m_eSoundsFlags.zero();

	if (!m_bDisableBore && SoundExist(section, "snd_bore"))
	{
		m_sounds.LoadSound(section, "snd_bore", "sndBore", true);
	}

	if (SoundExist(section, "snd_switch_device"))
	{
		m_sounds.LoadSound(section, "snd_switch_device", "sndSwitchDevice", false);
	}

	if (SoundExist(section, "snd_headlamp_on"))
	{
		m_eSoundsFlags.set(ESoundsFlags::sf_headlamp, TRUE);
		m_sounds.LoadSound(section, "snd_headlamp_on", "sndHeadlampOn", false);
		m_sounds.LoadSound(section, "snd_headlamp_off", "sndHeadlampOff", false);
	}

	if (SoundExist(section, "snd_nv_on"))
	{
		m_eSoundsFlags.set(ESoundsFlags::sf_nv, TRUE);
		m_sounds.LoadSound(section, "snd_nv_on", "sndNVOn", false);
		m_sounds.LoadSound(section, "snd_nv_off", "sndNVOff", false);
	}

	if (SoundExist(section, "snd_gasmask"))
	{
		m_sounds.LoadSound(section, "snd_gasmask", "sndGasmask", false);
	}

	if (SoundExist(section, "snd_prepare_detector"))
	{
		m_eSoundsFlags.set(ESoundsFlags::sf_prepare_detector, TRUE);
		m_sounds.LoadSound(section, "snd_prepare_detector", "sndPrepareDet", false);
	}

	if (SoundExist(section, "snd_finish_detector"))
	{
		m_eSoundsFlags.set(ESoundsFlags::sf_finish_detector, TRUE);
		m_sounds.LoadSound(section, "snd_finish_detector", "sndFinishDet", false);
	}
}

void CHudItem::PlaySound(LPCSTR alias, const Fvector& position, bool allowOverlap)
{
	m_sounds.PlaySound(alias, position, object().H_Root(), !!GetHUDmode(), false, allowOverlap);
}

void CHudItem::renderable_Render()
{
	UpdateXForm					();
	BOOL _hud_render			= ::Render->get_HUD() && GetHUDmode();
	
	if(_hud_render  && !IsHidden())
	{ 
	}
	else 
	{
		if (!object().H_Parent() || (!_hud_render && !IsHidden()))
		{
			on_renderable_Render		();
			debug_draw_firedeps			();
		}else
		if (m_object&&object().H_Parent())
		{
			if ((m_object->H_Parent()->cast_inventory_owner() && 
				m_object->H_Parent()->cast_inventory_owner()->attached(m_object->cast_inventory_item())) 
				||
				(item().BaseSlot() == INV_SLOT_3 /*|| item().BaseSlot() == INV_SLOT_2*/))
				on_renderable_Render();
		}
	}
}

void CHudItem::SwitchState(u32 S)
{
	if (OnClient()) 
		return;

	SetNextState( S );

	if (object().Local() && !object().getDestroy())	
	{
		// !!! Just single entry for given state !!!
		NET_Packet				P;
		object().u_EventGen		(P,GE_WPN_STATE_CHANGE,object().ID());
		P.w_u8					(u8(S));
		object().u_EventSend	(P);
	}
}

void CHudItem::OnEvent(NET_Packet& P, u16 type)
{
	switch (type)
	{
	case GE_WPN_STATE_CHANGE:
		{
			u8				S;
			P.r_u8			(S);
			OnStateSwitch	(u32(S));
		}
		break;
	}
}

void CHudItem::OnStateSwitch(u32 S)
{
	u32 old_state = GetState();
	SetState			(S);
	
	if(object().Remote()) 
		SetNextState	(S);

	switch (S)
	{
	case eShowing:
	{
		if (object().H_Parent() != nullptr && object().H_Parent() == Level().CurrentControlEntity())
		{
			if (g_player_hud->attached_item(1) != nullptr && g_player_hud->attached_item(1) != HudItemData())
			{
				CCustomDetector* det = smart_cast<CCustomDetector*>(g_player_hud->attached_item(1)->m_parent_hud_item);
				if (det != nullptr && det->CanDrawHand() && old_state != CMissile::EMissileStates::eThrowEnd)
				{
					det->SwitchState(CCustomDetector::eHandDraw);
				}
			}
		}
		break;
	}
	case eHiding:
	{
		if (object().H_Parent() != nullptr && object().H_Parent() == Level().CurrentControlEntity())
		{
			if (g_player_hud->attached_item(1) != nullptr && g_player_hud->attached_item(1) != HudItemData())
			{
				CCustomDetector* det = smart_cast<CCustomDetector*>(g_player_hud->attached_item(1)->m_parent_hud_item);
				if (det != nullptr && det->CanHideHand())
				{
					det->SwitchState(CCustomDetector::eHandHide);
				}
			}
		}
		break;
	}
	case eBore:
	{
		SetPending		(FALSE);

		PlayAnimBore	();
		if(HudItemData())
		{
			Fvector P		= HudItemData()->m_item_transform.c;
			m_sounds.PlaySound("sndBore", P, object().H_Root(), !!GetHUDmode(), false, m_started_rnd_anim_idx);
		}

		break;
	}
	case eSprintStart:
	{
		m_bSwitchSprint = true;
		SetPending(true);
		PlayHUDMotion(SetCurrentStateAnimation("anm_idle_sprint_start"), true, eSprintStart);
		break;
	}
	case eSprintEnd:
	{
		m_bSwitchSprint = false;
		SetPending(true);
		PlayHUDMotion(SetCurrentStateAnimation("anm_idle_sprint_end"), true, eSprintEnd);
		break;
	}
	case eDeviceSwitch:
	{
		PlayAnimDeviceSwitch();
		break;
	}
	case ePrepareDetector:
	{
		SetPending(true);
		PlayHUDMotion(SetCurrentStateAnimation("anm_prepare_detector"), true, ePrepareDetector);

		if (m_eSoundsFlags.test(ESoundsFlags::sf_prepare_detector))
		{
			PlaySound("sndPrepareDet", m_object->Position());
		}
		break;
	}
	case ePrepareDetectorEnd:
	{
		SetPending(true);
		PlayHUDMotion(SetCurrentStateAnimation("anm_draw_detector"), true, ePrepareDetectorEnd);
		if (CActor* pActor = m_object && m_object->H_Parent() ? m_object->H_Parent()->cast_actor() : nullptr)
		{
			if (CCustomDetector* det = pActor->GetDetector(true))
			{
				det->SwitchState(eShowing);
				det->TurnDetectorInternal(true);
			}
		}
		break;
	}
	case eFinishDetector:
	{
		SetPending(true);
		PlayHUDMotion(SetCurrentStateAnimation("anm_finish_detector"), true, eFinishDetector);

		if (m_eSoundsFlags.test(ESoundsFlags::sf_finish_detector))
		{
			PlaySound("sndFinishDet", m_object->Position());
		}
		break;
	}
	};

	if (S != eIdle && S != eSprintStart && S != eSprintEnd)
	{
		m_bSwitchSprint = false;
	}
}

void CHudItem::OnAnimationEnd(u32 state)
{
	if (CActor* pActor = m_object&&m_object->H_Parent() ? m_object->H_Parent()->cast_actor() : NULL)
	{
		pActor->callback(GameObject::eActorHudAnimationEnd)(smart_cast<CGameObject*>(this)->lua_game_object(), hud_sect.c_str(), m_current_motion.c_str(), state, animation_slot());
	}

	switch(state)
	{
	case eSprintStart:
	case eSprintEnd:
	case eBore:
	case eDeviceSwitch:
	case ePrepareDetectorEnd:
	case eFinishDetector:
	{
		SwitchState(eIdle);
		break;
	}
	case ePrepareDetector:
	{
		if (m_eAnimationsFlags.test(af_prepare_detector_end))
		{
			SwitchState(ePrepareDetectorEnd);
		}
		else
		{
			SwitchState(eIdle);
		}
		break;
	}
	};
}

void CHudItem::PlayAnimBore()
{
	PlayHUDMotion(SetCurrentStateAnimation("anm_bore"), TRUE, GetState());
}

bool CHudItem::ActivateItem() 
{
	OnActiveItem	();
	return			true;
}

void CHudItem::DeactivateItem() 
{
	OnHiddenItem();
}

void CHudItem::OnMoveToRuck(const SInvItemPlace& prev)
{
	SwitchState(eHidden);
}

bool CHudItem::SendDeactivateItem()
{
	if (GetState() == eHiding)
		return false;

	SendHiddenItem();

	return true;
}

void CHudItem::SendHiddenItem()
{
	if (!object().getDestroy())
	{
		NET_Packet				P;
		object().u_EventGen		(P,GE_WPN_STATE_CHANGE,object().ID());
		P.w_u8					(u8(eHiding));
		object().u_EventSend	(P, net_flags(TRUE, TRUE, FALSE, TRUE));
	}
}

void CHudItem::UpdateHudAdditonal(Fmatrix& trans)
{
	//TODO: Implement new yaw & pitch inertion
}

void CHudItem::UpdateCL()
{
	if(m_current_motion_def)
	{
		if(m_bStopAtEndAnimIsRunning)
		{
			const xr_vector<motion_marks>&	marks = m_current_motion_def->marks;
			if(!marks.empty())
			{
				float motion_prev_time = ((float)m_dwMotionCurrTm - (float)m_dwMotionStartTm)/1000.0f;
				float motion_curr_time = ((float)Device.dwTimeGlobal - (float)m_dwMotionStartTm)/1000.0f;
				
				xr_vector<motion_marks>::const_iterator it = marks.begin();
				xr_vector<motion_marks>::const_iterator it_e = marks.end();
				for(;it!=it_e;++it)
				{
					const motion_marks&	M = (*it);
					if(M.is_empty())
						continue;
	
					const motion_marks::interval* Iprev = M.pick_mark(motion_prev_time);
					const motion_marks::interval* Icurr = M.pick_mark(motion_curr_time);
					if(Iprev==nullptr && Icurr!=nullptr /* || M.is_mark_between(motion_prev_time, motion_curr_time)*/)
					{
						OnMotionMark				(m_startedMotionState, M);
					}
				}
			
			}

			m_dwMotionCurrTm					= Device.dwTimeGlobal;
			if(m_dwMotionCurrTm > m_dwMotionEndTm)
			{
				m_current_motion_def				= nullptr;
				m_dwMotionStartTm					= 0;
				m_dwMotionEndTm						= 0;
				m_dwMotionCurrTm					= 0;
				m_bStopAtEndAnimIsRunning			= false;
				OnAnimationEnd						(m_startedMotionState);
			}
		}
	}

	m_HudLight.UpdateTorchFromObject(this);
}

void CHudItem::OnH_A_Chield		()
{}

void CHudItem::OnH_B_Chield		()
{
	StopCurrentAnimWithoutCallback();
}

void CHudItem::OnH_B_Independent	(bool just_before_destroy)
{
	m_sounds.StopAllSounds	();
	UpdateXForm				();
	
	// next code was commented 
	/*
	if(HudItemData() && !just_before_destroy)
	{
		object().XFORM().set( HudItemData()->m_item_transform );
	}
	
	if (HudItemData())
	{
		g_player_hud->detach_item(this);
		Msg("---Detaching hud item [%s][%d]", this->HudSection().c_str(), this->object().ID());
	}*/
	//SetHudItemData			(nullptr);
}

void CHudItem::OnH_A_Independent	()
{
	if(HudItemData())
		g_player_hud->detach_item(this);
	StopCurrentAnimWithoutCallback();
}

void CHudItem::on_b_hud_detach()
{
	m_sounds.StopAllSounds	();
}

void CHudItem::on_a_hud_attach()
{
	if (m_current_motion_def)
	{
		PlayHUDMotion_noCB(m_current_motion, FALSE);
	}

	m_eAnimationsFlags.set(EAnimationsFlags::af_torch, HudAnimationExist("anm_switch_device"));
	m_eAnimationsFlags.set(EAnimationsFlags::af_nvg, m_eAnimationsFlags.test(EAnimationsFlags::af_torch));
	m_eAnimationsFlags.set(EAnimationsFlags::af_clear_mask, HudAnimationExist("anm_gasmask"));
	m_eAnimationsFlags.set(EAnimationsFlags::af_prepare_detector, HudAnimationExist("anm_prepare_detector"));
	m_eAnimationsFlags.set(EAnimationsFlags::af_prepare_detector_end, HudAnimationExist("anm_draw_detector"));
	m_eAnimationsFlags.set(EAnimationsFlags::af_finish_detector, HudAnimationExist("anm_finish_detector"));
	m_eAnimationsFlags.set(EAnimationsFlags::af_det_hand_draw, HudAnimationExist("anm_hand_draw"));
	m_eAnimationsFlags.set(EAnimationsFlags::af_det_hand_hide, HudAnimationExist("anm_hand_hide"));
	m_eAnimationsFlags.set(EAnimationsFlags::af_det_hand_throw_start, HudAnimationExist("anm_hand_throw_start"));
	m_eAnimationsFlags.set(EAnimationsFlags::af_det_hand_throw_idle, HudAnimationExist("anm_hand_throw_idle"));
	m_eAnimationsFlags.set(EAnimationsFlags::af_det_hand_throw_end, HudAnimationExist("anm_hand_throw_end"));
	m_eAnimationsFlags.set(EAnimationsFlags::af_det_hand_kick, HudAnimationExist("anm_kick") && HudAnimationExist("anm_kick2"));
	m_eAnimationsFlags.set(EAnimationsFlags::af_det_hand_lam, HudAnimationExist("anm_lam"));
	m_eAnimationsFlags.set(EAnimationsFlags::af_firemode, (HudAnimationExist("anm_firemode") || HudAnimationExist("anm_changefiremode_from_1_to_a") || HudAnimationExist("anm_changefiremode_from_a_to_1")));
}

bool CHudItem::HudAnimationExist(const shared_str& anim_name)
{
	if (Level().CurrentControlEntity() != object().H_Parent())
		return false;

	auto HID = HudItemData();
	if (HID)
	{
		return HID->m_hand_motions.has_motion(anim_name);
	}
	else
	{
		return g_player_hud->motion_length(anim_name, HudSection(), m_current_motion_def) > 100;
	}
}

u32 CHudItem::PlayHUDMotion(const shared_str& M, BOOL bMixIn, u32 state)
{
	if (HudItemData() && !HudAnimationExist(M.c_str()))
	{
		Msg("! model [%s] has no motion alias defined [%s]", hud_sect.c_str(), M.c_str());
		return 0;
	}

	shared_str snd;
	snd.printf("snd_%s", *M);
	if (m_object->H_Parent() != nullptr && pSettings->line_exist(HudSection(), snd))
	{
		m_sounds.LoadSound(*HudSection(), *snd.printf("snd_%s", *M), "sndByMotion", false);
		PlaySound("sndByMotion", m_object->Position());
	}

	u32 anim_time = PlayHUDMotion_noCB(M.c_str(), bMixIn);
	if (anim_time>0)
	{
		m_bStopAtEndAnimIsRunning	= true;
		m_dwMotionStartTm			= Device.dwTimeGlobal;
		m_dwMotionCurrTm			= m_dwMotionStartTm;
		m_dwMotionEndTm				= m_dwMotionStartTm + anim_time;
		m_startedMotionState		= state;
	}else
		m_bStopAtEndAnimIsRunning	= false;

	return anim_time;
}

bool CHudItem::AddSuffixName(shared_str& anim, LPCSTR suffix, LPCSTR test_suffix)
{
	string128 new_name = {};
	xr_strconcat(new_name, anim.c_str(), suffix, test_suffix);

	if (HudAnimationExist(new_name))
	{
		anim = new_name;
		return true;
	}

	return false;
}

u32 CHudItem::PlayHUDMotion_noCB(const shared_str& motion_name, BOOL bMixIn)
{
	m_current_motion					= motion_name;

	if(bDebug && item().m_pInventory)
	{
		Msg("-[%s] as[%d] [%d]anim_play [%s][%d]",
			HudItemData()?"HUD":"Simulating", 
			item().m_pInventory->GetActiveSlot(), 
			item().object_id(),
			motion_name.c_str(), 
			Device.dwFrame);
	}
	if( HudItemData() )
	{
		return HudItemData()->anim_play		(motion_name, bMixIn, m_current_motion_def, m_started_rnd_anim_idx);
	}else
	{
		m_started_rnd_anim_idx				= 0;
		return g_player_hud->motion_length	(motion_name, HudSection(), m_current_motion_def );
	}
}

void CHudItem::StopCurrentAnimWithoutCallback()
{
	m_dwMotionStartTm			= 0;
	m_dwMotionEndTm				= 0;
	m_dwMotionCurrTm			= 0;
	m_bStopAtEndAnimIsRunning	= false;
	m_current_motion_def		= nullptr;
}

BOOL CHudItem::GetHUDmode()
{
	if (m_object && m_object->H_Parent())
	{
		CActor* A = m_object->H_Parent()->cast_actor();
		return (A && A->HUDview() && HudItemData());
	}
	else
		return FALSE;
}

void CHudItem::PlayAnimIdle()
{
	if (TryPlayAnimIdle())
	{
		return;
	}

	PlayHUDMotion(SetCurrentIdleAnimation(), TRUE, GetState());
}

shared_str CHudItem::SetCurrentIdleAnimation()
{
	shared_str new_name = "anm_idle";
	if (Level().CurrentControlEntity() == nullptr)
		return new_name;

	CActor* pActor = Level().CurrentControlEntity()->cast_actor();

	if (pActor && pActor == object().H_Parent())
	{
		u32 state = pActor->GetMovementState(ACTOR_DEFS::EMovementStates::eReal);

		if (state & ACTOR_DEFS::EMoveCommand::mcAccel && state & ACTOR_DEFS::EMoveCommand::mcCrouch)
		{
			AddSuffixName(new_name, "_crouch_slow");
		}
		else if (state & ACTOR_DEFS::EMoveCommand::mcAccel)
		{
			AddSuffixName(new_name, "_slow");
		}
		else if (state & ACTOR_DEFS::EMoveCommand::mcCrouch)
		{
			AddSuffixName(new_name, "_crouch");
		}
	}

	return new_name;
}

bool CHudItem::TryPlayAnimIdle()
{
	if (MovingAnimAllowedNow())
	{
		CActor* pActor = smart_cast<CActor*>(object().H_Parent());
		if (pActor)
		{
			u32 state = pActor->GetMovementState(eReal);
			if (state & ACTOR_DEFS::EMoveCommand::mcSprint)
			{
				if (!m_bSwitchSprint && HudAnimationExist("anm_idle_sprint_start"))
				{
					SwitchState(eSprintStart);
					return true;
				}

				PlayAnimIdleSprint();
				return true;
			}
			else if (m_bSwitchSprint && HudAnimationExist("anm_idle_sprint_end"))
			{
				SwitchState(eSprintEnd);
				return true;
			}
			else if (state & ACTOR_DEFS::EMoveCommand::mcAnyMove)
			{
				if (state & ACTOR_DEFS::EMoveCommand::mcCrouch && (HudAnimationExist("anm_idle_moving_crouch_slow") || HudAnimationExist("anm_idle_moving_crouch")))
				{
					if (state & ACTOR_DEFS::EMoveCommand::mcAccel && HudAnimationExist("anm_idle_moving_crouch_slow"))
						PlayAnimIdleMovingCrouchSlow();
					else
						PlayAnimIdleMovingCrouch();

					return true;
				}
				else
				{
					if (state & ACTOR_DEFS::EMoveCommand::mcAccel && HudAnimationExist("anm_idle_moving_slow"))
						PlayAnimIdleMovingSlow();
					else
						PlayAnimIdleMoving();

					return true;
				}
			}
		}
	}
	return false;
}

void CHudItem::PlayAnimIdleMoving()
{
	PlayHUDMotion(SetCurrentStateAnimation("anm_idle_moving"), TRUE, GetState());
}

void CHudItem::PlayAnimIdleMovingSlow()
{
	PlayHUDMotion(SetCurrentStateAnimation("anm_idle_moving_slow"), TRUE, GetState());
}

void CHudItem::PlayAnimIdleMovingCrouch()
{
	PlayHUDMotion(SetCurrentStateAnimation("anm_idle_moving_crouch"), TRUE, GetState());
}

void CHudItem::PlayAnimIdleMovingCrouchSlow()
{
	PlayHUDMotion(SetCurrentStateAnimation("anm_idle_moving_crouch_slow"), TRUE, GetState());
}

void CHudItem::PlayAnimIdleSprint()
{
	PlayHUDMotion(SetCurrentStateAnimation("anm_idle_sprint"), TRUE, GetState());
}

void CHudItem::PlayAnimDeviceSwitch()
{
	SetPending(TRUE);
	shared_str anim_name;
	shared_str sound_name;

	if (m_eDevicesFlags.test(EDevicesFlags::df_torch))
	{
		anim_name = SetCurrentStateAnimation("anm_switch_device");

		if (m_eSoundsFlags.test(ESoundsFlags::sf_headlamp))
		{
			if (CActor* pActor = m_object->H_Parent() ? m_object->H_Parent()->cast_actor() : nullptr)
			{
				if (CTorch* pTorch = smart_cast<CTorch*>(pActor->inventory().ItemFromSlot(TORCH_SLOT)))
				{
					sound_name = pTorch->IsSwitched() ? "sndHeadlampOff" : "sndHeadlampOn";
				}
			}
		}
		else
		{
			sound_name = "sndSwitchDevice";
		}
	}
	else if (m_eDevicesFlags.test(EDevicesFlags::df_nvg))
	{
		anim_name = SetCurrentStateAnimation("anm_switch_device");

		if (m_eSoundsFlags.test(ESoundsFlags::sf_nv))
		{
			if (CActor* pActor = m_object->H_Parent() ? m_object->H_Parent()->cast_actor() : nullptr)
			{
				if (pActor->GetNightVisionEffector() != nullptr)
				{
					sound_name = pActor->GetNightVisionEffector()->GetStatus() ? "sndNVOff" : "sndNVOn";
				}
			}
		}
		else
		{
			sound_name = "sndSwitchDevice";
		}
	}
	else if (m_eDevicesFlags.test(EDevicesFlags::df_clear_mask))
	{
		anim_name = SetCurrentStateAnimation("anm_gasmask");
		sound_name = "sndGasmask";
	}

	if (g_player_hud->attached_item(0) == nullptr && g_player_hud->attached_item(1) != nullptr || g_player_hud->attached_item(0) != nullptr && g_player_hud->attached_item(0) == HudItemData())
	{
		PlaySound(*sound_name, m_object->Position());
	}

	PlayHUDMotion(anim_name, true, eDeviceSwitch);
}

void CHudItem::OnMovementChanged(ACTOR_DEFS::EMoveCommand cmd)
{
	if (GetState() == eIdle && !m_bStopAtEndAnimIsRunning)
	{
		PlayAnimIdle();
		ResetSubStateTime();
	}
}

attachable_hud_item* CHudItem::HudItemData()
{
	attachable_hud_item* hi = nullptr;
	if(!g_player_hud)		
		return				hi;

	hi = g_player_hud->attached_item(0);
	if (hi && hi->m_parent_hud_item == this)
		return hi;

	hi = g_player_hud->attached_item(1);
	if (hi && hi->m_parent_hud_item == this)
		return hi;

	return nullptr;
}

float CHudItem::GetHudFov()
{
	if (Level().CurrentViewEntity() == object().H_Parent())
	{
		float dist = HUD().GetCurrentRayQuery().range;

		clamp(dist, m_nearwall_dist_min, m_nearwall_dist_max);
		float fDistanceMod = ((dist - m_nearwall_dist_min) / (m_nearwall_dist_max - m_nearwall_dist_min));

		float fBaseFov = m_fHudFov ? m_fHudFov : psHUD_FOV_def;
		clamp(fBaseFov, 5.f, 180.f);
		const static bool isCollision = EngineExternal()[EEngineExternalGame::EnableWeaponCollision];
		if (isCollision)
		{

			float src = m_nearwall_speed_mod * Device.fTimeDelta;
			clamp(src, 0.f, 1.f);

			float fTrgFov = m_nearwall_target_hud_fov + fDistanceMod * (fBaseFov - m_nearwall_target_hud_fov);
			m_nearwall_last_hud_fov = m_nearwall_last_hud_fov * (1.f - src) + fTrgFov * src;
		}
		else
			m_nearwall_last_hud_fov = fBaseFov;
	}

	return m_nearwall_last_hud_fov;
}

void CHudItem::SetModelBoneStatus(const char* bone, BOOL show)
{
	if (HudItemData())
	{
		HudItemData()->set_bone_visible(bone, show, TRUE);
	}

	if (IKinematics* pWeaponVisual = m_object ? m_object->Visual()->dcast_PKinematics() : NULL)
	{
		if (auto BoneID = pWeaponVisual->LL_BoneID(bone); BoneID != BI_NONE)
		{
			pWeaponVisual->LL_SetBoneVisible(BoneID, show, FALSE);
		}
	}
}

void CHudItem::SetMultipleBonesStatus(const char* section, const char* line, BOOL show)
{
	if (!pSettings->section_exist(section))
	{
		return;
	}

	if (!!pSettings->line_exist(section, line))
	{
		LPCSTR	S = pSettings->r_string(section, line);
		if (S && S[0])
		{
			string128 _Item = {};
			int count = _GetItemCount(S);
			for (int it = 0; it < count; ++it)
			{
				_GetItem(S, it, _Item);
				SetModelBoneStatus(_Item, show);
			}
		}
	}
}

void CHudItem::OnMotionMark(u32 state, const motion_marks& mark)
{
	if (state == eDeviceSwitch && mark.name == "Left")
	{
		if (g_player_hud->attached_item(1) != nullptr && g_player_hud->attached_item(1) == HudItemData() && g_player_hud->attached_item(0) != nullptr)
		{
			m_eDevicesFlags.zero();
		}

		if (m_eDevicesFlags.test(EDevicesFlags::df_torch))
		{
			if (CActor* pActor = m_object->H_Parent() ? m_object->H_Parent()->cast_actor() : nullptr)
			{
				if (CTorch* pTorch = smart_cast<CTorch*>(pActor->inventory().ItemFromSlot(TORCH_SLOT)))
				{
					pTorch->Switch();
				}
			}
		}
		else if (m_eDevicesFlags.test(EDevicesFlags::df_nvg))
		{
			if (CActor* pActor = m_object->H_Parent() ? m_object->H_Parent()->cast_actor() : nullptr)
			{
				if (pActor->GetNightVisionEffector() != nullptr)
				{
					pActor->GetNightVisionEffector()->SwitchNightVision();
				}
			}
		}
		else if (m_eDevicesFlags.test(EDevicesFlags::df_clear_mask))
		{
			if (CActor* pActor = m_object->H_Parent() ? m_object->H_Parent()->cast_actor() : nullptr)
			{
				pActor->ClearMaskCB();
			}
		}

		m_eDevicesFlags.zero();
	}

	if (state == ePrepareDetector && mark.name == "Left")
	{
		if (CActor* pActor = m_object && m_object->H_Parent() ? m_object->H_Parent()->cast_actor() : nullptr)
		{
			if (CCustomDetector* det = pActor->GetDetector(true))
			{
				det->SwitchState(eShowing);
				det->TurnDetectorInternal(true);
			}
		}
	}
}

bool CHudItem::SoundExist(LPCSTR section, LPCSTR sound_name)
{
	if (!pSettings->line_exist(section, sound_name))
	{
		return false;
	}

	LPCSTR str = pSettings->r_string(section, sound_name);
	if (str == nullptr || xr_strlen(str) == 0)
	{
		return false;
	}

	return true;
}