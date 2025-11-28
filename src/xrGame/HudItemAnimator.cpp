#include "StdAfx.h"
#include "HudItemAnimator.h"
#include "player_hud.h"
#include "UIGameCustom.h"
#include "Inventory.h"
#include "ai_space.h"
#include "../../xrScripts/script_engine.h"

extern bool m_AnimatorForceHideItems;

void CHudItemAnimator::StopAnimator()
{
	CHudAnimatorBase::StopAnimator();

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

void CHudItemAnimator::Load()
{
	CHudAnimatorBase::Load();

	if (pSettings->line_exist(m_section, "sound_1"))
	{
		m_sounds.LoadSound(m_section.c_str(), "sound_1", "sndSnd", true);
	}

	m_bBlend = READ_IF_EXISTS(pSettings, r_bool, m_section, "blend", false);

	m_sLuaLeftCallback = READ_IF_EXISTS(pSettings, r_string, m_section, "left_lua_callback", "null");
	m_sLuaLeft2Callback = READ_IF_EXISTS(pSettings, r_string, m_section, "left2_lua_callback", "null");
	m_sLuaRightCallback = READ_IF_EXISTS(pSettings, r_string, m_section, "right_lua_callback", "null");
	m_sLuaRight2Callback = READ_IF_EXISTS(pSettings, r_string, m_section, "right2_lua_callback", "null");
	m_sLuaStartCallback = READ_IF_EXISTS(pSettings, r_string, m_section, "start_lua_callback", "null");
	m_sLuaEndCallback = READ_IF_EXISTS(pSettings, r_string, m_section, "end_lua_callback", "null");
}

void CHudItemAnimator::Update()
{
	if (m_bNeedActivated)
	{
		bool wpn_hide = !g_player_hud->attached_item(0) && !m_actor->inventory().ActiveItem() && !m_actor->inventory().GetNextActiveSlot() && !m_actor->inventory().GetActiveSlot();
		if (wpn_hide && g_player_hud->GetAnimator() == nullptr && !g_player_hud->attached_item(1))
		{
			PlayMotion();
		}
		else
		{
			if (CBackpackAnimator* backpack_animator = m_actor->HudAnimator()->BackpackAnimator())
			{
				if (backpack_animator->IsActive() && backpack_animator->GetState() != CHudStateAnimator::EAnimatorStates::eHiding)
				{
					if (m_AnimatorForceHideItems)
					{
						backpack_animator->StopAnimator();
					}
					else
					{
						backpack_animator->SetState(CHudStateAnimator::EAnimatorStates::eHiding);
					}

					m_iRestoreSlot = backpack_animator->GetSlotToRestore();
					m_bRestoreDetector = backpack_animator->NeedRestoreDetector();
				}
			}

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

	UpdateAnimation();
}

void CHudItemAnimator::StartAnimator(const shared_str& section)
{
	if (m_bNeedActivated || m_bIsPlaying)
	{
		return;
	}

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

	m_sLuaPrecondFunc = READ_IF_EXISTS(pSettings, r_string, m_section, "precondition_functor", "null");

	if (m_sLuaPrecondFunc != "null")
	{
		luabind::functor<bool> precondition;
		if (ai().script_engine().functor(*m_sLuaPrecondFunc, precondition))
		{
			m_sLuaPrecondFunc = "null";
			if (!precondition())
			{
				return;
			}
		}
		else
		{
			Msg("Error to call precondition script [%s] in animator [%s]", *m_sLuaPrecondFunc, *m_section);
		}
	}

	Load();

	m_bNeedActivated = true;

	if (CurrentGameUI() && CurrentGameUI()->TopInputReceiver())
	{
		CurrentGameUI()->TopInputReceiver()->HideDialog();
	}

	m_actor->set_inventory_disabled(true);
	m_actor->set_pda_disabled(true);
}

void CHudItemAnimator::PlayMotion()
{
	g_player_hud->create_animator_item(m_section);

	u32 ret = g_player_hud->GetAnimator()->anim_play("anm_show", m_bBlend, m_current_motion_def);

	m_bNeedActivated = false;
	m_bIsPlaying = true;

	CallStartCallback();

	if (m_sounds.FindSoundItem("sndSnd", false))
	{
		m_sounds.PlaySound("sndSnd", m_actor->Position(), m_actor, !!m_actor->HUDview(), !!(ret == 0));
	}

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

void CHudItemAnimator::UpdateAnimation()
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
						continue;

					auto Iprev = M.pick_mark(motion_prev_time);
					auto Icurr = M.pick_mark(motion_curr_time);
					if (Iprev == nullptr && Icurr != nullptr)
					{
						OnMotionMark(M);
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
				OnAnimationEnd();
			}
		}
	}
}

void CHudItemAnimator::OnMotionMark(const motion_marks& mark)
{
	if (mark.name == "Left")
	{
		CallLeftCallback();
	}
	else if (mark.name == "Left2")
	{
		CallLeft2Callback();
	}
	else if (mark.name == "Right")
	{
		CallRightCallback();
	}
	else if (mark.name == "Right2")
	{
		CallRight2Callback();
	}
}

void CHudItemAnimator::OnAnimationEnd()
{
	CallLeftCallback();
	CallLeft2Callback();
	CallRightCallback();
	CallRight2Callback();
	CallEndCallback();

	StopAnimator();

	if (m_iRestoreSlot > 0 && m_actor->inventory().ItemFromSlot(m_iRestoreSlot))
	{
		m_actor->inventory().Activate(m_iRestoreSlot);
		m_iRestoreSlot = 0;
	}

	if (m_bRestoreDetector && m_actor->GetDevice(true))
	{
		m_actor->GetDevice(true)->ToggleDetector(true);
		m_bRestoreDetector = false;
	}
}

void CHudItemAnimator::CallLeftCallback()
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

void CHudItemAnimator::CallLeft2Callback()
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

void CHudItemAnimator::CallRightCallback()
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

void CHudItemAnimator::CallRight2Callback()
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

void CHudItemAnimator::CallEndCallback()
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

void CHudItemAnimator::CallStartCallback()
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

void CBackpackAnimator::OnAnimationEnd(u32 state)
{
	switch (state)
	{
		case eShowing:
		{
			if (auto ui = CurrentGameUI())
			{
				ui->ShowActorMenu();
			}
			SetState(eIdle);
		}break;
		default:
		{
			CHudStateAnimator::OnAnimationEnd(state);
			break;
		}
	};
}

void CBackpackAnimator::SwitchAnimator()
{
	if (GetState() == eIdle)
	{
		SetState(eHiding);
		m_actor->set_pda_disabled(false);

		if (auto ui = CurrentGameUI())
		{
			ui->HideActorMenu();
		}
	}
	else if (!m_bNeedActivated && GetState() == eHidden && g_player_hud->GetAnimator() == nullptr)
	{
		m_bNeedActivated = true;
		m_actor->set_pda_disabled(true);

		if (auto ui = CurrentGameUI())
		{
			ui->HideShownDialogs();
		}
	}
}