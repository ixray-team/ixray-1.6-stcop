#include "StdAfx.h"
#include "HudItemAnimator.h"
#include "player_hud.h"
#include "UIGameCustom.h"
#include "Inventory.h"
#include "InventoryWeaponSlotLayout.h"
#include "ai_space.h"
#include "UIActorMenu.h"
#include "ParticlesObject.h"
#include "Actor.h"

extern bool m_AnimatorForceHideItems;

void CHudItemAnimator::StopAnimator()
{
	CHudAnimatorBase::StopAnimator();
}

void CHudItemAnimator::Load()
{
	CHudAnimatorBase::Load();

	if (pSettings->line_exist(m_section, "sound_1"))
	{
		m_sounds.LoadSound(m_section.c_str(), "sound_1", "sndSnd", true);
	}

	m_bBlend = READ_IF_EXISTS(pSettings, r_bool, m_section, "blend", false);
}

void CHudItemAnimator::Update()
{
	if (m_bNeedActivated)
	{
		m_manager->SetTargetAnimator(this);
		bool wpn_hide = !g_player_hud->attached_item(0) && !m_manager->Parent()->inventory().ActiveItem() && !m_manager->Parent()->inventory().GetNextActiveSlot() && !m_manager->Parent()->inventory().GetActiveSlot();
		if (wpn_hide && g_player_hud->GetAnimator() == nullptr && !g_player_hud->attached_item(1))
		{
			PlayMotion();
		}
		else
		{

			CHudAnimatorBase* current_animator = m_manager->Parent()->HudAnimator()->CurrentAnimator();
			if (CHudStateAnimator* state_animator = current_animator != nullptr ? current_animator->cast_hud_state_animator() : nullptr)
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

			CHudItem* active_item = m_manager->Parent()->inventory().ActiveItem() ? m_manager->Parent()->inventory().ActiveItem()->cast_hud_item() : nullptr;
			if (active_item != nullptr)
			{
				u16 slot = m_manager->Parent()->inventory().GetActiveSlot();
				m_manager->SlotToRestore() = slot;

				if (m_AnimatorForceHideItems)
				{
					m_manager->Parent()->inventory().SetActiveSlot(NO_ACTIVE_SLOT);
					active_item->SwitchState(CHUDState::EHudStates::eHidden);
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

	if (m_bHideUI)
	{
		if (auto ui = CurrentGameUI())
		{
			ui->HideShownDialogs();
		}

		m_manager->Parent()->set_inventory_disabled(true);
		m_manager->Parent()->set_pda_disabled(true);
	}
}

void CHudItemAnimator::PlayMotion()
{
	g_player_hud->create_animator_item(this, m_section);

	u32 ret = g_player_hud->GetAnimator()->anim_play("anm_show", m_bBlend, m_current_motion_def);

	m_bNeedActivated = false;
	m_bIsPlaying = true;

	if (m_manager->TargetAnimator() == this)
	{
		m_manager->SetTargetAnimator(nullptr);
	}

	m_manager->SetCurrentAnimator(this);

	if (m_bHideUI)
	{
		if (auto ui = CurrentGameUI())
		{
			ui->HideShownDialogs();
		}

		m_manager->Parent()->set_inventory_disabled(true);
		m_manager->Parent()->set_pda_disabled(true);
	}

	CallStartCallback();

	if (m_sounds.FindSoundItem("sndSnd", false))
	{
		m_sounds.PlaySound("sndSnd", m_manager->Parent()->Position(), m_manager->Parent(), !!m_manager->Parent()->HUDview(), !!(ret == 0));
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

		g_player_hud->UpdateMovementLayers();
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

	if (!g_player_hud->m_need_reload)
	{
		return;
	}

	u8& restore_slot = m_manager->SlotToRestore();
	bool& restore_device = m_manager->RestoreDevice();

	PIItem item_to_restore = m_manager->Parent()->inventory().ItemFromSlot(restore_slot);
	if (restore_slot > 0 && item_to_restore != nullptr)
	{
		u16 real_slot = item_to_restore->BaseSlot();
		if (!IsSidearmPhysicalSlot(real_slot) && real_slot != KNIFE_SLOT && real_slot != BOLT_SLOT)
		{
			restore_device = false;
		}

		m_manager->Parent()->inventory().Activate(restore_slot);
		restore_slot = 0;
	}

	if (restore_device && m_manager->Parent()->GetDevice(true))
	{
		m_manager->Parent()->GetDevice(true)->ToggleDetector(true);
		restore_device = false;
	}
}

CBackpackAnimator::CBackpackAnimator(CHudAnimatorManager* m_manager, const shared_str& section) : CHudStateAnimator(m_manager)
{
	m_section = section;
	Load();
}

void CBackpackAnimator::OnAnimationEnd(u8 state)
{
	switch (state)
	{
		case eShowing:
		{
			if (auto ui = CurrentGameUI())
			{
				if (!ui->ActorMenu()->IsShown())
				{
					ui->ShowActorMenu();
				}
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

		if (auto ui = CurrentGameUI())
		{
			ui->HideActorMenu();
		}
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
				Load();
			}
			else
			{
				Msg("Error to call section modify script [%s] in animator [%s]", *m_sLuaModifySect, *m_section);
			}
		}

		m_bNeedActivated = true;

		if (m_bHideUI)
		{
			if (auto ui = CurrentGameUI())
			{
				ui->HideShownDialogs();
			}

			m_manager->Parent()->set_pda_disabled(true);
		}
	}
}

void CBackpackAnimator::OnMotionMark(const motion_marks& mark, u8 state)
{
	if (state == eShowing && mark.name == "Left")
	{
		if (auto ui = CurrentGameUI())
		{
			ui->ShowActorMenu();
		}
	}
}

CBurnAnimator::CBurnAnimator(CHudAnimatorManager* m_manager, const shared_str& section) : CHudAnimatorBase(m_manager)
{
	m_section = section;
	Load();
}

void CBurnAnimator::Load()
{
	CHudAnimatorBase::Load();
	m_burn_restore = pSettings->r_float(m_section, "burn_restore");

	m_pFlameParticles = Particles::Details::Create(pSettings->r_string(m_section, "flame_particles"), false);
	m_pFlameParticles->m_bAutoStop = true;
	m_pFlameParticles->SetLiveUpdate(true);

	m_sounds.LoadSound(m_section.c_str(), "snd_burn", "sndBurned", true);
}

void CBurnAnimator::Update()
{
	if (m_bNeedActivated)
	{
		m_manager->SetTargetAnimator(this);
		bool wpn_hide = !g_player_hud->attached_item(0) && !m_manager->Parent()->inventory().ActiveItem() && !m_manager->Parent()->inventory().GetNextActiveSlot() && !m_manager->Parent()->inventory().GetActiveSlot();
		if (wpn_hide && g_player_hud->GetAnimator() == nullptr && !g_player_hud->attached_item(1))
		{
			PlayAnimBurn();
		}
		else
		{
			CHudAnimatorBase* current_animator = m_manager->Parent()->HudAnimator()->CurrentAnimator();
			if (CHudStateAnimator* state_animator = current_animator != nullptr ? current_animator->cast_hud_state_animator() : nullptr)
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

			CHudItem* active_item = m_manager->Parent()->inventory().ActiveItem() ? m_manager->Parent()->inventory().ActiveItem()->cast_hud_item() : nullptr;
			if (active_item != nullptr)
			{
				u16 slot = m_manager->Parent()->inventory().GetActiveSlot();
				m_manager->SlotToRestore() = slot;

				if (m_AnimatorForceHideItems)
				{
					m_manager->Parent()->inventory().SetActiveSlot(NO_ACTIVE_SLOT);
					active_item->SwitchState(CHUDState::EHudStates::eHidden);
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

	if (m_pFlameParticles->m_bPlaying)
	{
		Fmatrix pos;
		pos.set(get_ParticlesXFORM());
		pos.c.set(get_CurrentFirePoint());
		m_pFlameParticles->UpdateParent(pos, zero_vel);
	}
	UpdateFireDependencies();

	UpdateAnimation();
}

void CBurnAnimator::StartFlameParticle()
{
	m_pFlameParticles->Stop(false);

	Fmatrix pos;
	pos.set(get_ParticlesXFORM());
	pos.c.set(get_CurrentFirePoint());

	m_pFlameParticles->UpdateParent(pos, zero_vel);
	m_pFlameParticles->Play(true);
}

void CBurnAnimator::UpdateFireDependencies_internal()
{
	if (Device.dwFrame == dwFP_Frame)
		return;

	dwFP_Frame = Device.dwFrame;

	if (g_player_hud->GetAnimator())
		g_player_hud->GetAnimator()->setup_firedeps(m_current_firedeps);
	VERIFY(_valid(m_current_firedeps.m_FireParticlesXForm));
}

void CBurnAnimator::PlayAnimBurn()
{
	g_player_hud->create_animator_item(this, m_section);

	u32 ret = g_player_hud->GetAnimator()->anim_play("anm_show", false, m_current_motion_def);

	m_bNeedActivated = false;
	m_bIsPlaying = true;

	if (m_manager->TargetAnimator() == this)
	{
		m_manager->SetTargetAnimator(nullptr);
	}

	m_manager->SetCurrentAnimator(this);

	if (m_bHideUI)
	{
		if (auto ui = CurrentGameUI())
		{
			ui->HideShownDialogs();
		}

		m_manager->Parent()->set_inventory_disabled(true);
		m_manager->Parent()->set_pda_disabled(true);
	}

	CallStartCallback();

	m_sounds.PlaySound("sndBurned", m_manager->Parent()->Position(), m_manager->Parent(), !!m_manager->Parent()->HUDview(), !!(ret == 0));

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

		g_player_hud->UpdateMovementLayers();
}

void CBurnAnimator::UpdateAnimation()
{
	if (m_current_motion_def)
	{
		if (m_bStopAtEndAnimIsRunning)
		{
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

void CBurnAnimator::OnAnimationEnd()
{
	StopAnimator();

	u8& restore_slot = m_manager->SlotToRestore();
	bool& restore_device = m_manager->RestoreDevice();

	if (restore_slot > 0 && m_manager->Parent()->inventory().ItemFromSlot(restore_slot))
	{
		m_manager->Parent()->inventory().Activate(restore_slot);
		restore_slot = 0;
	}

	if (restore_device && m_manager->Parent()->GetDevice(true))
	{
		m_manager->Parent()->GetDevice(true)->ToggleDetector(true);
		restore_device = false;
	}
}

void CBurnAnimator::StartAnimator()
{
	if (m_bNeedActivated || m_bIsPlaying)
	{
		return;
	}

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

	m_bNeedActivated = true;

	if (m_bHideUI)
	{
		if (auto ui = CurrentGameUI())
		{
			ui->HideShownDialogs();
		}

		m_manager->Parent()->set_inventory_disabled(true);
		m_manager->Parent()->set_pda_disabled(true);
	}
}
