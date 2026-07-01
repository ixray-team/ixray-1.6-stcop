#include "StdAfx.h"
#include "HudAnimatorManager.h"
#include "player_hud.h"
#include "CustomDevice.h"
#include "Actor.h"
#include "Inventory.h"
#include "InventoryWeaponSlotLayout.h"
#include "ui/UIGameCustom.h"

#include "ai_space.h"
#include "../../xrScripts/script_engine.h"

bool m_AnimatorForceHideItems = false;


CHudAnimatorBase::~CHudAnimatorBase()
{
	StopAnimator();
	m_sounds.StopAllSounds();
}

void CHudAnimatorBase::Load()
{
	m_sounds.Clear();

	m_bCanSprint = READ_IF_EXISTS(pSettings, r_bool, m_section, "can_sprint", false);
	m_bHideUI = READ_IF_EXISTS(pSettings, r_bool, m_section, "hide_ui", true);

	m_fHudFov = READ_IF_EXISTS(pSettings, r_float, m_section, "hud_fov", 0.0f);
	m_fHudFovFactor = READ_IF_EXISTS(pSettings, r_float, m_section, "hud_fov_factor", 1.0f);

	m_sLuaLeftCallback = READ_IF_EXISTS(pSettings, r_string, m_section, "left_lua_callback", "null");
	m_sLuaLeft2Callback = READ_IF_EXISTS(pSettings, r_string, m_section, "left2_lua_callback", "null");
	m_sLuaRightCallback = READ_IF_EXISTS(pSettings, r_string, m_section, "right_lua_callback", "null");
	m_sLuaRight2Callback = READ_IF_EXISTS(pSettings, r_string, m_section, "right2_lua_callback", "null");
	m_sLuaStartCallback = READ_IF_EXISTS(pSettings, r_string, m_section, "start_lua_callback", "null");
	m_sLuaEndCallback = READ_IF_EXISTS(pSettings, r_string, m_section, "end_lua_callback", "null");

	pSettings->read_if_exists<bool>(m_bBlendMovement, m_section, "use_blending_movement");
}

void CHudAnimatorBase::StopAnimator()
{
	m_bIsPlaying = false;
	m_manager->Parent()->set_inventory_disabled(false);
	m_manager->Parent()->set_pda_disabled(false);
	g_player_hud->delete_animator_item();

	m_manager->SetCurrentAnimator(nullptr);

	m_left_callback.clear();
	m_left2_callback.clear();
	m_right_callback.clear();
	m_right2_callback.clear();
	m_start_callback.clear();
	m_end_callback.clear();

	m_sLuaLeftCallback = "null";
	m_sLuaLeft2Callback = "null";
	m_sLuaRightCallback = "null";
	m_sLuaRight2Callback = "null";
	m_sLuaStartCallback = "null";
	m_sLuaEndCallback = "null";
	m_sLuaModifySect = "null";
}

ENGINE_API extern float psHUD_FOV_def;

float CHudAnimatorBase::GetHudFov() const
{
	if (!m_fHudFov || !m_bIsPlaying)
	{
		return psHUD_FOV_def * m_fHudFovFactor;
	}

	return m_fHudFov * m_fHudFovFactor;
}

bool CHudAnimatorBase::HudAnimationExist(const shared_str& name)
{
	if (g_player_hud->GetAnimator() != nullptr)
	{
		return g_player_hud->GetAnimator()->m_hand_motions.has_motion(name);
	}

	return false;
}

void CHudAnimatorBase::CallLeftCallback()
{
	if (m_left_callback)
	{
		m_left_callback();
		m_left_callback.clear();
	}

	if (m_sLuaLeftCallback != "null")
	{
		luabind::functor<void> lua_func;
		if (ai().script_engine().functor(*m_sLuaLeftCallback, lua_func))
		{
			lua_func();
			m_sLuaLeftCallback = "null";
		}
		else
		{
			Msg("Error to call left script callback [%s] in animator [%s]", *m_sLuaLeftCallback, *m_section);
		}
	}
}

void CHudAnimatorBase::CallLeft2Callback()
{
	if (m_left2_callback)
	{
		m_left2_callback();
		m_left2_callback.clear();
	}

	if (m_sLuaLeft2Callback != "null")
	{
		luabind::functor<void> lua_func;
		if (ai().script_engine().functor(*m_sLuaLeft2Callback, lua_func))
		{
			lua_func();
			m_sLuaLeft2Callback = "null";
		}
		else
		{
			Msg("Error to call left2 script callback [%s] in animator [%s]", *m_sLuaLeft2Callback, *m_section);
		}
	}
}

void CHudAnimatorBase::CallRightCallback()
{
	if (m_right_callback)
	{
		m_right_callback();
		m_right_callback.clear();
	}

	if (m_sLuaRightCallback != "null")
	{
		luabind::functor<void> lua_func;
		if (ai().script_engine().functor(*m_sLuaRightCallback, lua_func))
		{
			lua_func();
			m_sLuaRightCallback = "null";
		}
		else
		{
			Msg("Error to call right script callback [%s] in animator [%s]", *m_sLuaRightCallback, *m_section);
		}
	}
}

void CHudAnimatorBase::CallRight2Callback()
{
	if (m_right2_callback)
	{
		m_right2_callback();
		m_right2_callback.clear();
	}

	if (m_sLuaRight2Callback != "null")
	{
		luabind::functor<void> lua_func;
		if (ai().script_engine().functor(*m_sLuaRight2Callback, lua_func))
		{
			lua_func();
			m_sLuaRight2Callback = "null";
		}
		else
		{
			Msg("Error to call right2 script callback [%s] in animator [%s]", *m_sLuaRight2Callback, *m_section);
		}
	}
}

void CHudAnimatorBase::CallEndCallback()
{
	if (m_end_callback)
	{
		m_end_callback();
		m_end_callback.clear();
	}

	if (m_sLuaEndCallback != "null")
	{
		luabind::functor<void> lua_func;
		if (ai().script_engine().functor(*m_sLuaEndCallback, lua_func))
		{
			lua_func();
			m_sLuaEndCallback = "null";
		}
		else
		{
			Msg("Error to call end script callback [%s] in animator [%s]", *m_sLuaEndCallback, *m_section);
		}
	}
}

void CHudAnimatorBase::CallStartCallback()
{
	if (m_start_callback)
	{
		m_start_callback();
		m_start_callback.clear();
	}

	if (m_sLuaStartCallback != "null")
	{
		luabind::functor<void> lua_func;
		if (ai().script_engine().functor(*m_sLuaStartCallback, lua_func))
		{
			lua_func();
			m_sLuaStartCallback = "null";
		}
		else
		{
			Msg("Error to call start script callback [%s] in animator [%s]", *m_sLuaStartCallback, *m_section);
		}
	}
}

CHudStateAnimator::CHudStateAnimator(CHudAnimatorManager* manager) : CHudAnimatorBase(manager)
{
	m_eSoundsFlags.zero();
	m_eDevicesFlags.zero();
	m_eAnimationsFlags.zero();
}

void CHudStateAnimator::Load()
{
	CHudAnimatorBase::Load();

	if (pSettings->line_exist(m_section, "snd_draw"))
	{
		m_sounds.LoadSound(m_section.c_str(), "snd_draw", "sndDraw", true);
	}

	if (pSettings->line_exist(m_section, "snd_holster"))
	{
		m_sounds.LoadSound(m_section.c_str(), "snd_holster", "sndHide", true);
	}

	if (pSettings->line_exist(m_section, "snd_bore"))
	{
		m_sounds.LoadSound(m_section.c_str(), "snd_bore", "sndBore", true);
	}

	if (pSettings->line_exist(m_section, "snd_switch_device"))
	{
		m_sounds.LoadSound(*m_section, "snd_switch_device", "sndSwitchDevice", false);
	}

	if (pSettings->line_exist(m_section, "snd_headlamp_on"))
	{
		m_eSoundsFlags.set(ESoundsFlags::sf_headlamp, true);
		m_sounds.LoadSound(*m_section, "snd_headlamp_on", "sndHeadlampOn", false);
		m_sounds.LoadSound(*m_section, "snd_headlamp_off", "sndHeadlampOff", false);
	}

	if (pSettings->line_exist(m_section, "snd_nv_on"))
	{
		m_eSoundsFlags.set(ESoundsFlags::sf_nv, true);
		m_sounds.LoadSound(*m_section, "snd_nv_on", "sndNVOn", false);
		m_sounds.LoadSound(*m_section, "snd_nv_off", "sndNVOff", false);
	}

	if (pSettings->line_exist(m_section, "snd_gasmask"))
	{
		m_sounds.LoadSound(*m_section, "snd_gasmask", "sndGasmask", false);
	}

	if (pSettings->line_exist(m_section, "snd_sprint_start"))
	{
		m_eSoundsFlags.set(ESoundsFlags::sf_sprint_start, true);
		m_sounds.LoadSound(*m_section, "snd_sprint_start", "sndSprintStart", false);
	}
	if (pSettings->line_exist(m_section, "snd_sprint_end"))
	{
		m_eSoundsFlags.set(ESoundsFlags::sf_sprint_end, true);
		m_sounds.LoadSound(*m_section, "snd_sprint_end", "sndSprintEnd", false);
	}

	m_bDisableBore = READ_IF_EXISTS(pSettings, r_bool, m_section, "disable_bore", true);
}

void CHudStateAnimator::Update()
{
	if (m_bNeedActivated)
	{
		m_manager->SetTargetAnimator(this);

		bool wpn_hide = !g_player_hud->attached_item(0) && !m_manager->Parent()->inventory().ActiveItem() && !m_manager->Parent()->inventory().GetNextActiveSlot() && !m_manager->Parent()->inventory().GetActiveSlot();
		if (wpn_hide && g_player_hud->GetAnimator() == nullptr && !g_player_hud->attached_item(1))
		{
			m_bNeedActivated = false;
			SetState(eShowing);
		}
		else
		{
			CHudAnimatorBase* current_animator = m_manager->Parent()->HudAnimator()->CurrentAnimator();
			if (current_animator != nullptr && this != current_animator)
			{
				if (CHudStateAnimator* state_animator = current_animator->cast_hud_state_animator())
				{
					if (state_animator->GetState() != CHudStateAnimator::EAnimatorStates::eHidden && state_animator->GetState() != CHudStateAnimator::EAnimatorStates::eHiding)
					{
						if (m_AnimatorForceHideItems)
						{
							state_animator->StopAnimator();
						}
						else
						{
							state_animator->SetState(CHudStateAnimator::EAnimatorStates::eHiding);
						}
					}
				}
			}

			CHudItem* active_item = m_manager->Parent()->inventory().ActiveItem() ? m_manager->Parent()->inventory().ActiveItem()->cast_hud_item() : nullptr;
			if (active_item != nullptr)
			{
				u16 slot = m_manager->Parent()->inventory().GetActiveSlot();
				m_manager->SlotToRestore() = slot;

				if (m_AnimatorForceHideItems)
				{
					m_manager->Parent()->inventory().SetActiveSlot(NO_ACTIVE_SLOT);
					active_item->SwitchState(CHUDState::EHudStates::eHidden, false);
					active_item->SetState(CHUDState::EHudStates::eHidden);
					g_player_hud->detach_item_idx(0);
				}
				else if (active_item->GetState() != CHUDState::EHudStates::eHiding)
				{
					m_manager->Parent()->inventory().Activate(NO_ACTIVE_SLOT);
				}
			}

			if (CCustomDevice* dev = m_manager->Parent()->GetDevice())
			{
				m_manager->RestoreDevice() = true;

				if (m_AnimatorForceHideItems)
				{
					dev->SwitchState(CHUDState::EHudStates::eHidden, false);
					dev->SetState(CHUDState::EHudStates::eHidden);
					g_player_hud->detach_item_idx(1);
				}
				else if (dev->GetState() != CHUDState::EHudStates::eHiding)
				{
					dev->HideDetector(true, true);
				}
			}
		}
	}
	else
	{
		if (GetState() == eIdle)
		{
			if (!m_bDisableBore && Device.dwTimeGlobal - m_dw_curr_substate_time > 20000)
			{
				SetState(eBore);
				ResetSubStateTime();
			}
		}
	}

	UpdateAnimation();
}

void CHudStateAnimator::PlayMotion(const shared_str& name, bool blend, u8 state)
{
	m_on_animation_end_state = state;

	u32 ret = g_player_hud->GetAnimator()->anim_play(name, blend, m_current_motion_def);

	if (ret > 0)
	{
		m_dwMotionStartTm = Device.dwTimeGlobal;
		m_dwMotionCurrTm = m_dwMotionStartTm;
		m_dwMotionEndTm = m_dwMotionStartTm + ret;
		m_bStopAtEndAnimIsRunning = true;
	}
	else
	{
		m_bStopAtEndAnimIsRunning = false;
	}
}

void CHudStateAnimator::UpdateAnimation()
{
	if (m_current_motion_def)
	{
		if (m_bStopAtEndAnimIsRunning)
		{
			auto& marks = m_current_motion_def->marks;
			if (!marks.empty())
			{
				float motion_prev_time = ((float)m_dwMotionCurrTm - (float)m_dwMotionStartTm) / 1000.0f;
				float motion_curr_time = ((float)Device.dwTimeGlobal - (float)m_dwMotionStartTm) / 1000.0f;

				for (auto& M : marks)
				{
					if (M.is_empty())
					{
						continue;
					}

					auto Iprev = M.pick_mark(motion_prev_time);
					auto Icurr = M.pick_mark(motion_curr_time);
					if (Iprev == nullptr && Icurr != nullptr)
					{
						OnMotionMark(M, m_on_animation_end_state);
					}
				}

			}

			m_dwMotionCurrTm = Device.dwTimeGlobal;
			if (m_dwMotionCurrTm > m_dwMotionEndTm)
			{
				m_current_motion_def = nullptr;
				m_dwMotionStartTm = 0;
				m_dwMotionEndTm = 0;
				m_dwMotionCurrTm = 0;
				m_bStopAtEndAnimIsRunning = false;
				OnAnimationEnd(m_on_animation_end_state);
			}
		}
	}
}

void CHudStateAnimator::OnMotionMark(const motion_marks& mark, u8 state)
{
	if (state == eDeviceSwitch)
	{
		if (mark.name == "Right")
		{
			if (m_eDevicesFlags.test(EDevicesFlags::df_nvg))
			{
				if (CActor* pActor = m_manager->Parent())
				{
					pActor->StartNVPPE();
				}
			}
		}

		if (mark.name == "Left")
		{
			if (m_eDevicesFlags.test(EDevicesFlags::df_torch))
			{
				if (CActor* pActor = m_manager->Parent())
				{
					PIItem torch_item = pActor->inventory().ItemFromSlot(TORCH_SLOT);
					if (CTorch* pTorch = torch_item != nullptr ? torch_item->cast_torch() : nullptr)
					{
						pTorch->Switch();
					}
				}
			}
			else if (m_eDevicesFlags.test(EDevicesFlags::df_nvg))
			{
				if (CActor* pActor = m_manager->Parent())
				{
					if (pActor->GetNightVisionEffector() != nullptr)
					{
						pActor->GetNightVisionEffector()->SwitchNightVision();
					}
				}
			}
			else if (m_eDevicesFlags.test(EDevicesFlags::df_clear_mask))
			{
				if (CActor* pActor = m_manager->Parent())
				{
					pActor->ClearMaskCB();
				}
			}

			m_eDevicesFlags.zero();
		}
	}
}

void CHudStateAnimator::OnAnimationEnd(u8 state)
{
	switch (state)
	{
	case eHiding:
	{
		SetState(eHidden);

		m_manager->Parent()->set_pda_disabled(false);
		m_manager->Parent()->set_inventory_disabled(false);

		if (m_manager->TargetAnimator() != nullptr)
		{
			break;
		}

		if (!g_player_hud->m_need_reload)
		{
			break;
		}

		u8& slot_to_restore = m_manager->SlotToRestore();
		bool& restore_device = m_manager->RestoreDevice();

		PIItem item_to_restore = m_manager->Parent()->inventory().ItemFromSlot(slot_to_restore);
		if (slot_to_restore > 0 && item_to_restore != nullptr)
		{
			u16 real_slot = item_to_restore->BaseSlot();
			if (!IsSidearmPhysicalSlot(real_slot) && real_slot != KNIFE_SLOT && real_slot != BOLT_SLOT)
			{
				restore_device = false;
			}

			m_manager->Parent()->inventory().Activate(slot_to_restore);
			slot_to_restore = 0;
		}

		if (restore_device && m_manager->Parent()->GetDevice(true))
		{
			m_manager->Parent()->GetDevice(true)->ToggleDetector(true, true);
			restore_device = false;
		}
	}break;
	case eShowing:
	case eIdle:
	case eBore:
	case eSprintStart:
	case eSprintEnd:
	case eDeviceSwitch:
	{
		SetState(eIdle);
	}break;
	}
}

void CHudStateAnimator::OnStateSwitch(u8 state)
{
	m_current_state = state;

	if (state != eIdle)
	{
		m_dw_curr_state_time = Device.dwTimeGlobal;
		ResetSubStateTime();
	}

	switch (state)
	{
	case eShowing:
	{
		g_player_hud->create_animator_item(this, m_section);
		m_eAnimationsFlags.set(EAnimationsFlags::af_torch, HudAnimationExist("anm_switch_device"));
		m_eAnimationsFlags.set(EAnimationsFlags::af_nvg, m_eAnimationsFlags.test(EAnimationsFlags::af_torch));
		m_eAnimationsFlags.set(EAnimationsFlags::af_clear_mask, HudAnimationExist("anm_gasmask"));
		PlayMotion(SetCurrentStateAnimation("anm_show"), false, eShowing);

		m_bIsPlaying = true;

		if (m_manager->TargetAnimator() == this)
		{
			m_manager->SetTargetAnimator(nullptr);
		}

		m_manager->SetCurrentAnimator(this);

		if (m_sounds.FindSoundItem("sndDraw", false))
		{
			m_sounds.PlaySound("sndDraw", zero_vel, m_manager->Parent(), true);
		}
	}break;
	case eHiding:
	{
		if (m_sounds.FindSoundItem("sndHide", false))
		{
			m_sounds.PlaySound("sndHide", zero_vel, m_manager->Parent(), true);
		}
		PlayMotion(SetCurrentStateAnimation("anm_hide"), true, eHiding);
	}break;
	case eIdle:
	{
		PlayAnimIdle();
	}break;
	case eHidden:
	{
		StopAnimator();
	}break;
	case eBore:
	{
		PlayMotion(SetCurrentStateAnimation("anm_bore"), true, eBore);
		if (m_sounds.FindSoundItem("sndBore", false))
		{
			m_sounds.PlaySound("sndBore", zero_vel, m_manager->Parent(), true);
		}
	}break;
	case eSprintStart:
	{
		m_bSwitchSprint = true;
		PlayMotion(SetCurrentStateAnimation("anm_idle_sprint_start"), true, eSprintStart);
		if (m_sounds.FindSoundItem("sndSprintStart", false))
		{
			m_sounds.PlaySound("sndSprintStart", zero_vel, m_manager->Parent(), true);
		}
		break;
	}
	case eSprintEnd:
	{
		m_bSwitchSprint = false;
		PlayMotion(SetCurrentStateAnimation("anm_idle_sprint_end"), true, eSprintEnd);
		if (m_sounds.FindSoundItem("sndSprintEnd", false))
		{
			m_sounds.PlaySound("sndSprintEnd", zero_vel, m_manager->Parent(), true);
		}
		break;
	}
	case eDeviceSwitch:
	{
		PlayAnimDeviceSwitch();
		break;
	}
	};

	if (state != eIdle && state != eSprintStart && state != eSprintEnd)
	{
		m_bSwitchSprint = false;
	}

	g_player_hud->UpdateMovementLayers();
}

void CHudStateAnimator::OnMovementChanged()
{
	if (GetState() == eIdle && !m_bStopAtEndAnimIsRunning)
	{
		PlayAnimIdle();
		ResetSubStateTime();
	}
}

void CHudStateAnimator::PlayAnimIdle()
{
	if (TryPlayAnimIdle())
	{
		return;
	}

	PlayMotion(SetCurrentStateAnimation("anm_idle"), true, eIdle);
}

bool CHudStateAnimator::TryPlayAnimIdle()
{
	u32 state = m_manager->Parent()->GetMovementState(eReal);
	if (state & ACTOR_DEFS::EMoveCommand::mcSprint && CanSprint() && HudAnimationExist("anm_idle_sprint"))
	{
		if (!m_bSwitchSprint && HudAnimationExist("anm_idle_sprint_start"))
		{
			SetState(eSprintStart);
			return true;
		}

		PlayAnimIdleSprint();
		return true;
	}
	else if (m_bSwitchSprint && HudAnimationExist("anm_idle_sprint_end"))
	{
		SetState(eSprintEnd);
		return true;
	}
	else if (state & ACTOR_DEFS::EMoveCommand::mcAnyMove)
	{
		if (state & ACTOR_DEFS::EMoveCommand::mcCrouch && (HudAnimationExist("anm_idle_moving_crouch_slow") || HudAnimationExist("anm_idle_moving_crouch")))
		{
			if (state & ACTOR_DEFS::EMoveCommand::mcAccel && HudAnimationExist("anm_idle_moving_crouch_slow"))
			{
				PlayAnimIdleMovingCrouchSlow();
				return true;
			}
			else if (HudAnimationExist("anm_idle_moving_crouch"))
			{
				PlayAnimIdleMovingCrouch();
				return true;
			}
		}
		else
		{
			if (state & ACTOR_DEFS::EMoveCommand::mcAccel && HudAnimationExist("anm_idle_moving_slow"))
			{
				PlayAnimIdleMovingSlow();
				return true;
			}
			else if (HudAnimationExist("anm_idle_moving"))
			{
				PlayAnimIdleMoving();
				return true;
			}
		}
	}

	return false;
}

void CHudStateAnimator::PlayAnimIdleMoving()
{
	PlayMotion(SetCurrentStateAnimation("anm_idle_moving"), true, eIdle);
}

void CHudStateAnimator::PlayAnimIdleMovingSlow()
{
	PlayMotion(SetCurrentStateAnimation("anm_idle_moving_slow"), true, eIdle);
}

void CHudStateAnimator::PlayAnimIdleMovingCrouch()
{
	PlayMotion(SetCurrentStateAnimation("anm_idle_moving_crouch"), true, eIdle);
}

void CHudStateAnimator::PlayAnimIdleMovingCrouchSlow()
{
	PlayMotion(SetCurrentStateAnimation("anm_idle_moving_crouch_slow"), true, eIdle);
}

void CHudStateAnimator::PlayAnimIdleSprint()
{
	PlayMotion(SetCurrentStateAnimation("anm_idle_sprint"), true, eIdle);
}

void CHudStateAnimator::PlayAnimDeviceSwitch()
{
	shared_str anim_name;
	shared_str sound_name;

	if (m_eDevicesFlags.test(EDevicesFlags::df_torch))
	{
		anim_name = SetCurrentStateAnimation("anm_switch_device");

		if (m_eSoundsFlags.test(ESoundsFlags::sf_headlamp))
		{
			if (CActor* pActor = m_manager->Parent())
			{
				PIItem torch_item = pActor->inventory().ItemFromSlot(TORCH_SLOT);
				if (CTorch* pTorch = torch_item != nullptr ? torch_item->cast_torch() : nullptr)
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
			if (CActor* pActor = m_manager->Parent())
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

	m_sounds.PlaySound(*sound_name, m_manager->Parent()->Position(), m_manager->Parent(), true, false);
	PlayMotion(anim_name, true, eDeviceSwitch);
}

void CHudStateAnimator::SwitchAnimator()
{
	if (GetState() == eIdle)
	{
		SetState(eHiding);
	}
	else if (!m_bNeedActivated && GetState() == eHidden && g_player_hud->GetAnimator() == nullptr)
	{
		m_sLuaModifySect = READ_IF_EXISTS(pSettings, r_string, m_section, "modify_sect_lua_callback", "null");

		if (m_sLuaModifySect != "null")
		{
			luabind::functor<const char*> lua_func;
			if (ai().script_engine().functor(*m_sLuaModifySect, lua_func))
			{
				m_section = lua_func(m_section.c_str());
				m_sLuaModifySect = "null";
			}
			else
			{
				Msg("Error to call section modify script [%s] in animator [%s]", *m_sLuaModifySect, *m_section);
			}
		}

		Load();

		m_bNeedActivated = true;

		if (m_bHideUI)
		{
			m_manager->Parent()->set_pda_disabled(true);
			m_manager->Parent()->set_inventory_disabled(true);

			if (auto ui = CurrentGameUI())
			{
				ui->HideShownDialogs();
			}
		}
	}
}

void CHudStateAnimator::ShowStateAnimator(const shared_str& section)
{
	m_section = section;

	m_sLuaModifySect = READ_IF_EXISTS(pSettings, r_string, m_section, "modify_sect_lua_callback", "null");

	if (m_sLuaModifySect != "null")
	{
		luabind::functor<const char*> lua_func;
		if (ai().script_engine().functor(*m_sLuaModifySect, lua_func))
		{
			m_section = lua_func(m_section.c_str());
			m_sLuaModifySect = "null";
		}
		else
		{
			Msg("Error to call section modify script [%s] in animator [%s]", *m_sLuaModifySect, *m_section);
		}
	}

	Load();

	m_bNeedActivated = true;

	if (m_bHideUI)
	{
		m_manager->Parent()->set_pda_disabled(true);
		m_manager->Parent()->set_inventory_disabled(true);

		if (auto ui = CurrentGameUI())
		{
			ui->HideShownDialogs();
		}
	}
}

void CHudStateAnimator::HideStateAnimator()
{
	SetState(eHiding);
}

void CHudStateAnimator::StopAnimator()
{
	m_current_state = eHidden;
	m_on_animation_end_state = eHidden;
	m_current_motion_def = nullptr;
	m_dwMotionStartTm = 0;
	m_dwMotionEndTm = 0;
	m_dwMotionCurrTm = 0;
	m_bStopAtEndAnimIsRunning = false;

	CHudAnimatorBase::StopAnimator();
}

CHudAnimatorManager::CHudAnimatorManager(CActor* actor) : m_actor(actor)
{
	m_item_animator = new CHudItemAnimator(this);
	m_hud_state_animator = new CHudStateAnimator(this);

	if (pGameGlobals->line_exist("backpack", "backpack_animator"))
	{
		const char* backpack_animator = pGameGlobals->r_string("backpack", "backpack_animator");
		if (pSettings->section_exist(backpack_animator))
		{
			m_backpack_animator = new CBackpackAnimator(this, backpack_animator);
		}
	}

	if (pGameGlobals->line_exist("burn", "burn_animator"))
	{
		const char* burn_animator = pGameGlobals->r_string("burn", "burn_animator");
		if (pSettings->section_exist(burn_animator))
		{
			m_burn_animator = new CBurnAnimator(this, burn_animator);
		}
	}

	static const bool Use3DPDA = EngineExternal()[EEngineExternalGame::Enable3DPDA];

	if (Use3DPDA && pGameGlobals->line_exist("pda", "pda_animator"))
	{
		const char* pda_animator = pGameGlobals->r_string("pda", "pda_animator");
		if (pSettings->section_exist(pda_animator))
		{
			m_pda_animator = new CHudPdaAnimator(this, pda_animator);
		}
	}
}

CHudAnimatorManager::~CHudAnimatorManager()
{
	xr_delete(m_item_animator);
	xr_delete(m_backpack_animator);
	xr_delete(m_pda_animator);
	xr_delete(m_burn_animator);

	m_actor = nullptr;
	m_iRestoreSlot = NO_ACTIVE_SLOT;
	m_bRestoreDevice = false;
}

void CHudAnimatorManager::Update()
{
	if (ItemAnimator() != nullptr)
	{
		ItemAnimator()->Update();
	}

	if (BackpackAnimator() != nullptr)
	{
		BackpackAnimator()->Update();
	}

	if (HudStateAnimator() != nullptr)
	{
		HudStateAnimator()->Update();
	}

	if (PdaAnimator() != nullptr)
	{
		PdaAnimator()->Update();
	}

	if (BurnAnimator() != nullptr)
	{
		BurnAnimator()->Update();
	}
}

bool CHudAnimatorManager::IsAnyAnimatorActive()
{
	if (CurrentAnimator() != nullptr && CurrentAnimator()->IsActive())
	{
		return true;
	}

	return false;
}

bool CHudAnimatorManager::CanSprint()
{
	if (CurrentAnimator() != nullptr && CurrentAnimator()->IsActive())
	{
		return CurrentAnimator()->CanSprint();
	}

	return true;
}

float CHudAnimatorManager::GetHudFov()
{
	if (CurrentAnimator() != nullptr && CurrentAnimator()->IsActive())
	{
		return CurrentAnimator()->GetHudFov();
	}

	return psHUD_FOV_def;
}

void CHudAnimatorManager::StopGetAnimator()
{
	if (CurrentAnimator() != nullptr && CurrentAnimator()->IsActive())
	{
		CurrentAnimator()->StopAnimator();
	}

	if (TargetAnimator() != nullptr && TargetAnimator()->IsActive())
	{
		TargetAnimator()->StopAnimator();
	}
}

bool& CHudAnimatorManager::ForceHideItems()
{
	return m_AnimatorForceHideItems;
}

void CHudAnimatorManager::OnMovementChanged()
{
	CHudAnimatorBase* current_animator = CurrentAnimator();

	if (current_animator == nullptr)
	{
		return;
	}

	CHudStateAnimator* state_animator = current_animator->cast_hud_state_animator();

	if (state_animator == nullptr)
	{
		return;
	}

	if (state_animator->IsActive())
	{
		state_animator->OnMovementChanged();
	}
}

bool CHudAnimatorManager::InputKeyPress(int cmd)
{
	if (CurrentAnimator() != nullptr)
	{
		return CurrentAnimator()->InputKeyPress(cmd);
	}
	
	return false;
}

bool CHudAnimatorManager::InputKeyRelease(int cmd)
{
	if (CurrentAnimator() != nullptr)
	{
		return CurrentAnimator()->InputKeyRelease(cmd);
	}

	return false;
}

bool CHudAnimatorManager::InputKeyHold(int cmd)
{
	if (CurrentAnimator() != nullptr)
	{
		return CurrentAnimator()->InputKeyHold(cmd);
	}

	return false;
}