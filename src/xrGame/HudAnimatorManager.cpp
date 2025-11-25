#include "StdAfx.h"
#include "HudAnimatorManager.h"
#include "player_hud.h"
#include "CustomDevice.h"
#include "Actor.h"
#include "Inventory.h"
#include "ai_space.h"
#include "ui/UIGameCustom.h"

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

	m_fHudFov = READ_IF_EXISTS(pSettings, r_float, m_section, "hud_fov", 0.0f);
	m_fHudFovFactor = READ_IF_EXISTS(pSettings, r_float, m_section, "hud_fov_factor", 1.0f);
}

void CHudAnimatorBase::StopAnimator()
{
	m_bIsPlaying = false;
	m_actor->set_inventory_disabled(false);
	m_actor->set_pda_disabled(false);
	g_player_hud->delete_animator_item();
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

CHudStateAnimator::CHudStateAnimator(CActor* parent, const shared_str& section) : CHudAnimatorBase(parent)
{
	m_section = section;
	Load();
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

	m_bDisableBore = READ_IF_EXISTS(pSettings, r_bool, m_section, "disable_bore", true);
}

void CHudStateAnimator::Update()
{
	if (m_bNeedActivated)
	{
		bool wpn_hide = !g_player_hud->attached_item(0) && !m_actor->inventory().ActiveItem() && !m_actor->inventory().GetNextActiveSlot() && !m_actor->inventory().GetActiveSlot();
		if (wpn_hide && !g_player_hud->attached_item(1))
		{
			m_bNeedActivated = false;
			SetState(eShowing);
		}
		else
		{
			CHudItem* active_item = m_actor->inventory().ActiveItem() ? m_actor->inventory().ActiveItem()->cast_hud_item() : nullptr;
			if (active_item != nullptr)
			{
				u16 slot = m_actor->inventory().GetActiveSlot();
				m_iRestoreSlot = slot;

				if (m_AnimatorForceHideItems)
				{
					m_actor->inventory().SetActiveSlot(NO_ACTIVE_SLOT);
					active_item->SwitchState(CHUDState::EHudStates::eHidden);
					active_item->SetState(CHUDState::EHudStates::eHidden);
					g_player_hud->detach_item_idx(0);
				}
				else if (active_item->GetState() != CHUDState::EHudStates::eHiding)
				{
					m_actor->inventory().Activate(NO_ACTIVE_SLOT);
				}
			}

			if (CCustomDevice* dev = m_actor->GetDevice())
			{
				m_bRestoreDetector = true;

				if (m_AnimatorForceHideItems)
				{
					dev->SwitchState(CHUDState::EHudStates::eHidden);
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

void CHudStateAnimator::PlayMotion(const shared_str& name, bool blend, u32 state)
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
						//OnMotionMark(M);
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

void CHudStateAnimator::OnAnimationEnd(u32 state)
{
	switch (state)
	{
	case eHiding:
	{
		SetState(eHidden);

		if (m_iRestoreSlot > 0 && m_actor->inventory().ItemFromSlot(m_iRestoreSlot))
		{
			m_actor->inventory().Activate(m_iRestoreSlot);
			m_iRestoreSlot = 0;
		}

		if (m_bRestoreDetector && m_actor->GetDevice(true))
		{
			m_actor->GetDevice(true)->ToggleDetector(true, true);
			m_bRestoreDetector = false;
		}
	}break;
	case eShowing:
	case eIdle:
	case eBore:
	case eSprintStart:
	case eSprintEnd:
	{
		SetState(eIdle);
	}break;
	}
}

void CHudStateAnimator::OnStateSwitch(u32 state)
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
		g_player_hud->create_animator_item(m_section);
		PlayMotion("anm_show", false, eShowing);

		m_bIsPlaying = true;

		if (m_sounds.FindSoundItem("sndDraw", false))
		{
			m_sounds.PlaySound("sndDraw", zero_vel, m_actor, true);
		}
	}break;
	case eHiding:
	{
		if (m_sounds.FindSoundItem("sndHide", false))
		{
			m_sounds.PlaySound("sndHide", zero_vel, m_actor, true);
		}
		PlayMotion("anm_hide", true, eHiding);
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
		PlayMotion("anm_bore", true, eBore);
		if (m_sounds.FindSoundItem("sndBore", false))
		{
			m_sounds.PlaySound("sndBore", zero_vel, m_actor, true);
		}
	}break;
	case eSprintStart:
	{
		m_bSwitchSprint = true;
		PlayMotion("anm_idle_sprint_start", true, eSprintStart);
		break;
	}
	case eSprintEnd:
	{
		m_bSwitchSprint = false;
		PlayMotion("anm_idle_sprint_end", true, eSprintEnd);
		break;
	}
	};

	if (state != eIdle && state != eSprintStart && state != eSprintEnd)
	{
		m_bSwitchSprint = false;
	}
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

	PlayMotion("anm_idle", true, eIdle);
}

bool CHudStateAnimator::TryPlayAnimIdle()
{
	u32 state = m_actor->GetMovementState(eReal);
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
	PlayMotion("anm_idle_moving", true, eIdle);
}

void CHudStateAnimator::PlayAnimIdleMovingSlow()
{
	PlayMotion("anm_idle_moving_slow", true, eIdle);
}

void CHudStateAnimator::PlayAnimIdleMovingCrouch()
{
	PlayMotion("anm_idle_moving_crouch", true, eIdle);
}

void CHudStateAnimator::PlayAnimIdleMovingCrouchSlow()
{
	PlayMotion("anm_idle_moving_crouch_slow", true, eIdle);
}

void CHudStateAnimator::PlayAnimIdleSprint()
{
	PlayMotion("anm_idle_sprint", true, eIdle);
}

void CHudStateAnimator::SwitchAnimator()
{
	if (GetState() == eIdle)
	{
		SetState(eHiding);
		m_actor->set_pda_disabled(false);
		m_actor->set_inventory_disabled(false);
	}
	else if (!m_bNeedActivated && GetState() == eHidden && g_player_hud->GetAnimator() == nullptr)
	{
		m_bNeedActivated = true;
		m_actor->set_pda_disabled(true);
		m_actor->set_inventory_disabled(true);

		if (auto ui = CurrentGameUI())
		{
			ui->HideShownDialogs();
		}
	}
}

CHudAnimatorManager::CHudAnimatorManager(CActor* actor) : m_actor(actor)
{
	m_item_animator = new CHudItemAnimator(actor);

	if (pGameGlobals->line_exist("backpack", "backpack_animator"))
	{
		LPCSTR backpack_animator = pGameGlobals->r_string("backpack", "backpack_animator");
		if (pSettings->section_exist(backpack_animator))
		{
			m_backpack_animator = new CBackpackAnimator(actor, backpack_animator);
		}
	}

	//m_pda_animator = new CHudPdaAnimator(actor, "pda_show_animator_hud");
}

CHudAnimatorManager::~CHudAnimatorManager()
{
	xr_delete(m_item_animator);
	xr_delete(m_backpack_animator);
	//xr_delete(m_pda_animator);

	m_actor = nullptr;
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

	//if (PdaAnimator() != nullptr)
	//{
	//	PdaAnimator()->Update();
	//}
}

bool CHudAnimatorManager::IsAnyAnimatorActive()
{
	if (ItemAnimator() != nullptr && ItemAnimator()->IsActive())
	{
		return true;
	}

	if (BackpackAnimator() != nullptr && BackpackAnimator()->IsActive())
	{
		return true;
	}

	//if (PdaAnimator() != nullptr)
	//{
	//	return PdaAnimator()->IsActive();
	//}

	return false;
}

bool CHudAnimatorManager::CanSprint()
{
	if (ItemAnimator() != nullptr && ItemAnimator()->IsActive())
	{
		return ItemAnimator()->CanSprint();
	}

	if (BackpackAnimator() != nullptr && BackpackAnimator()->IsActive())
	{
		return BackpackAnimator()->CanSprint();
	}

	//if (PdaAnimator() != nullptr && PdaAnimator()->IsActive())
	//{
	//	return PdaAnimator()->CanSprint();
	//}

	return true;
}

float CHudAnimatorManager::GetHudFov()
{
	if (ItemAnimator() != nullptr && ItemAnimator()->IsActive())
	{
		return ItemAnimator()->GetHudFov();
	}

	if (BackpackAnimator() != nullptr && BackpackAnimator()->IsActive())
	{
		return BackpackAnimator()->GetHudFov();
	}

	//if (PdaAnimator() != nullptr && PdaAnimator()->IsActive())
	//{
	//	return PdaAnimator()->GetHudFov();
	//}

	return psHUD_FOV_def;
}

void CHudAnimatorManager::StopGetAnimator()
{
	if (ItemAnimator() != nullptr && ItemAnimator()->IsActive())
	{
		ItemAnimator()->StopAnimator();
	}

	if (BackpackAnimator() != nullptr && BackpackAnimator()->IsActive())
	{
		BackpackAnimator()->StopAnimator();
	}

	//if (PdaAnimator() != nullptr && PdaAnimator()->IsActive())
	//{
	//	PdaAnimator()->StopAnimator();
	//}
}

void CHudAnimatorManager::SetForceHideItems(bool value)
{
	m_AnimatorForceHideItems = value;
}

bool CHudAnimatorManager::IsForceHideItems()
{
	return m_AnimatorForceHideItems;
}

void CHudAnimatorManager::OnMovementChanged()
{
	if (BackpackAnimator() != nullptr && BackpackAnimator()->IsActive())
	{
		BackpackAnimator()->OnMovementChanged();
	}
}