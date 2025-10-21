#include "StdAfx.h"
#include "CustomDevice.h"
#include "Inventory.h"
#include "Actor.h"
#include "player_hud.h"
#include "Weapon.h"

CCustomDevice::~CCustomDevice()
{
	TurnDetectorInternal(false);
}

void CCustomDevice::UpdateXForm()
{
	CInventoryItem::UpdateXForm();
}

void CCustomDevice::LoadSounds(LPCSTR section)
{
	inherited::LoadSounds(section);

	m_sounds.LoadSound(section, "snd_draw", "sndShow");
	m_sounds.LoadSound(section, "snd_holster", "sndHide");
}

BOOL CCustomDevice::net_Spawn(CSE_Abstract* DC)
{
	TurnDetectorInternal(false);
	return inherited::net_Spawn(DC);
}

bool CCustomDevice::CheckCompatibilityInt(CHudItem* itm, u16* slot_to_activate)
{
	if (itm == nullptr)
	{
		return true;
	}

	CInventoryItem& iitm = itm->item();
	u32 slot = iitm.BaseSlot();
	bool bres = (slot == INV_SLOT_2 || slot == KNIFE_SLOT || slot == BOLT_SLOT);

	if (!bres && slot_to_activate)
	{
		*slot_to_activate = NO_ACTIVE_SLOT;

		if (m_pInventory->ItemFromSlot(BOLT_SLOT))
		{
			*slot_to_activate = BOLT_SLOT;
		}
		else if (m_pInventory->ItemFromSlot(INV_SLOT_3) && m_pInventory->ItemFromSlot(INV_SLOT_3)->BaseSlot() != INV_SLOT_3)
		{
			*slot_to_activate = INV_SLOT_3;
		}
		else if (m_pInventory->ItemFromSlot(INV_SLOT_2) && m_pInventory->ItemFromSlot(INV_SLOT_2)->BaseSlot() != INV_SLOT_3)
		{
			*slot_to_activate = INV_SLOT_2;
		}
		else if (m_pInventory->ItemFromSlot(KNIFE_SLOT))
		{
			*slot_to_activate = KNIFE_SLOT;
		}

		if (*slot_to_activate != NO_ACTIVE_SLOT)
		{
			bres = true;
		}
	}

	if (!bres && itm->GetState() != CHUDState::eShowing)
	{
		bres = bres && !itm->IsPending();
	}

	if (bres)
	{
		if (CWeapon* W = itm->cast_weapon())
		{
			bres = bres && (W->GetState() != CHUDState::eBore) && (W->GetState() != CWeapon::eReload) && (W->GetState() != CWeapon::eSwitch) && !W->IsZoomed();
		}
	}
	return bres;
}

bool CCustomDevice::CheckCompatibility(CHudItem* itm)
{
	if (!inherited::CheckCompatibility(itm))
	{
		return false;
	}

	if (!CheckCompatibilityInt(itm, nullptr))
	{
		HideDetector(true);
		return false;
	}

	return true;
}

void CCustomDevice::HideAndSetCallback(detector_fn_t fn)
{
	m_bNeedActivation = false;
	m_bFastAnimMode = true;
	SwitchState(eHiding);

	hide_callback = fn;
}

void CCustomDevice::HideDetector(bool bFastMode, bool force)
{
	if (force)
	{
		m_bFastAnimMode = bFastMode;
		SwitchState(eHiding);
		return;
	}

	const CHUDState::EHudStates CurrentState = (CHUDState::EHudStates)GetState();
	const bool bClimb = Actor()->GetMovementState(eReal) & ACTOR_DEFS::EMoveCommand::mcClimb;

	if (bClimb && CurrentState == CHUDState::EHudStates::eShowing)
	{
		StopCurrentAnimWithoutCallback();
		SetState(eIdle);
		ToggleDetector(bFastMode);
	}
	else
	{
		ToggleDetector(bFastMode);
	}
}

void CCustomDevice::ShowDetector(bool bFastMode)
{
	if (GetState() == eHidden)
	{
		ToggleDetector(bFastMode);
	}
}

void CCustomDevice::ToggleDetector(bool bFastMode, bool switching)
{
	m_bNeedActivation = false;
	m_bFastAnimMode = bFastMode;

	if (GetState() == eHidden)
	{
		PIItem iitem = m_pInventory->ActiveItem();
		CHudItem* itm = (iitem) ? iitem->cast_hud_item() : nullptr;
		u16 slot_to_activate = NO_ACTIVE_SLOT;

		if (CheckCompatibilityInt(itm, &slot_to_activate))
		{
			if (slot_to_activate != NO_ACTIVE_SLOT)
			{
				if (OnServer())
				{
					// Пытаемся достать допустимый предмет: нож, оружие или тп
					// при этом будет спрятано текущее оружие
					m_pInventory->Activate(slot_to_activate);
				}
				else
				{
					if (H_Parent() && H_Parent() == Level().CurrentViewEntity())
					{
						NET_Packet P;
						CGameObject::u_EventGen(P, GEG_PLAYER_ACTIVATE_SLOT, H_Parent()->ID());
						P.w_u16(slot_to_activate);
						CGameObject::u_EventSend(P);
					}
				}
				m_pInventory->Activate(slot_to_activate);
				m_bNeedActivation = true;
			}
			else
			{
				if (itm != nullptr && !itm->bDisablePrepareAnimation && itm->m_eAnimationsFlags.test(EAnimationsFlags::af_prepare_detector))
				{
					if (itm->GetState() == CHUDState::eIdle)
					{
						itm->SwitchState(CHUDState::ePrepareDetector);
					}
				}
				else
				{
					SwitchState(eShowing);
					TurnDetectorInternal(true);

					if (itm != nullptr && itm->bDisablePrepareAnimation)
					{
						itm->bDisablePrepareAnimation = false;
					}
				}
			}
		}
	}
	else if (GetState() != eHiding && GetState() != eShowing && !m_bIsZoomed)
	{
		SwitchState(eHiding);
	}
}

void CCustomDevice::SwitchState(u32 S)
{
	if (IsGameTypeSingle() || OnServer())
	{
		inherited::SwitchState(S);
		return;
	}

	if (!IsGameTypeSingle() && OnClient())
	{
		SetNextState(S);
		OnStateSwitch(u32(S));

		switch (S)
		{
		case eHidden:
			if (hide_callback)
			{
				hide_callback();
			}
			ClearCallback();
			break;
		case eShowing:
		case eIdle:
			ClearCallback();
			break;
		default:
			break;
		}
	}
}

void CCustomDevice::ShowingCallback(CBlend* B)
{
	ToggleDetector(g_player_hud->attached_item(0) != nullptr, true);
	g_player_hud->ResetBlockedPartID();
	g_player_hud->OnMovementChanged(mcAnyMove);
	g_player_hud->RestoreHandBlends("right_hand");
}

void CCustomDevice::switch_device()
{
	CObject* control_entity = Level().CurrentControlEntity();
	CActor* actor = control_entity != nullptr ? control_entity->cast_actor() : nullptr;
	if (actor != nullptr && actor->HudAnimator() && actor->HudAnimator()->IsActive())
	{
		return;
	}

	PIItem active_item = m_pInventory->ActiveItem();

	bool need_fx = active_item == nullptr || active_item->cast_hud_item() == nullptr || !active_item->cast_hud_item()->m_eAnimationsFlags.test(af_prepare_detector);

	if (GetState() == eHidden && g_player_hud->attached_item(0) && need_fx && active_item && active_item->BaseSlot() == INV_SLOT_2)
	{
		if (g_player_hud->animator_play(g_player_hud->check_anim("anm_hide", 0) ? "anm_hide" : "anm_hide_0", 0, 1, TRUE, 1.5f, 0, false, true, [](CBlend* B) {static_cast<CCustomDevice*>(B->CallbackParam)->ShowingCallback(B); }, this, 0))
			g_player_hud->animator_fx_play(g_player_hud->check_anim("anm_hide", 0) ? "anm_hide" : "anm_hide_0", 0, 2, 0, 3.f, 1.f, 1.f, 0.5f);
	}
	else
	{
		ToggleDetector(g_player_hud->attached_item(0) != nullptr, true);
	}
}

bool CCustomDevice::need_renderable()
{
	return m_pInventory && (!m_pInventory->ActiveItem() || (m_pInventory->ActiveItem() && m_pInventory->ActiveItem()->cast_hud_item() && m_pInventory->ActiveItem()->cast_hud_item()->need_renderable()));
}

void CCustomDevice::OnStateSwitch(u32 S)
{
	inherited::OnStateSwitch(S);

	switch (S)
	{
	case eShowing:
	{
		g_player_hud->attach_item(this);
		m_sounds.PlaySound("sndShow", Fvector().set(0, 0, 0), this, true, false);
		PlayHUDMotion(m_bFastAnimMode ? "anm_show_fast" : "anm_show", FALSE, S);
		SetPending(TRUE);
	}break;
	case eHiding:
	{
		m_sounds.PlaySound("sndHide", Fvector().set(0, 0, 0), this, true, false);
		PlayHUDMotion(m_bFastAnimMode ? "anm_hide_fast" : "anm_hide", TRUE, S);
		SetPending(TRUE);
		PlayWpnFinishDetector();
	}break;
	case eIdle:
	{
		PlayAnimIdle();
		SetPending(FALSE);
	}break;
	case eHandDraw:
	{
		PlayHUDMotion("anm_hand_draw", true, eHandDraw);
		break;
	}
	case eHandHide:
	{
		PlayHUDMotion("anm_hand_hide", true, eHandHide);
		break;
	}
	case eHandThrowStart:
	{
		PlayHUDMotion("anm_hand_throw_start", true, eHandThrowStart);
		break;
	}
	case eHandThrowIdle:
	{
		PlayHUDMotion("anm_hand_throw_idle", true, eHandThrowIdle);
		break;
	}
	case eHandThrowEnd:
	{
		PlayHUDMotion("anm_hand_throw_end", true, eHandThrowEnd);
		break;
	}
	case eHandKick1:
	{
		PlayHUDMotion("anm_kick", true, eHandKick1);
		break;
	}
	case eHandKick2:
	{
		PlayHUDMotion("anm_kick2", true, eHandKick2);
		break;
	}
	case eHandLam:
	{
		PlayHUDMotion("anm_lam", true, eHandLam);
		break;
	}
	}
}

void CCustomDevice::PlayWpnFinishDetector()
{
	if (!m_pInventory)
	{
		return;
	}

	if (m_pInventory->GetNextActiveSlot() == NO_ACTIVE_SLOT)
	{
		return;
	}

	PIItem iitem = m_pInventory->ActiveItem();
	CHudItem* itm = (iitem) ? iitem->cast_hud_item() : nullptr;
	if (itm != nullptr && itm->GetState() == CHUDState::eIdle && itm->m_eAnimationsFlags.test(af_finish_detector))
	{
		itm->SwitchState(CHUDState::eFinishDetector);
	}
}

void CCustomDevice::OnAnimationEnd(u32 state)
{
	inherited::OnAnimationEnd(state);
	switch (state)
	{
	case eShowing:
	case eHandDraw:
	case eHandHide:
	case eHandThrowEnd:
	case eHandKick1:
	case eHandKick2:
	case eHandLam:
	{
		SwitchState(eIdle);
	} break;
	case eHiding:
	{
		SwitchState(eHidden);
		TurnDetectorInternal(false);
		g_player_hud->detach_item(this);
		m_bIsZoomed = false;
	} break;
	case eHandThrowStart:
	case eHandThrowIdle:
	{
		SwitchState(eHandThrowIdle);
		break;
	}
	}
}

bool CCustomDevice::NeedBlockSprint() const
{
	return GetState() == eSprintEnd;
}

bool CCustomDevice::CanDrawHand() const
{
	return m_eAnimationsFlags.test(EAnimationsFlags::af_det_hand_draw) && (GetState() == eIdle || GetState() == eHandHide);
}

bool CCustomDevice::CanHideHand() const
{
	return m_eAnimationsFlags.test(EAnimationsFlags::af_det_hand_hide) && (GetState() == eIdle || GetState() == eHandDraw);
}

bool CCustomDevice::CanThrowHand() const
{
	bool has_anims = m_eAnimationsFlags.test(EAnimationsFlags::af_det_hand_throw_start) && m_eAnimationsFlags.test(EAnimationsFlags::af_det_hand_throw_idle) && m_eAnimationsFlags.test(EAnimationsFlags::af_det_hand_throw_end);
	return has_anims && GetState() != eHidden && GetState() != eShowing && GetState() != eHiding;
}

bool CCustomDevice::CanKick() const
{
	return m_eAnimationsFlags.test(EAnimationsFlags::af_det_hand_kick) &&
		(GetState() == eIdle || GetState() == eHandKick1 || GetState() == eHandKick2 || GetState() == eShowing || GetState() == eSprintEnd || GetState() == eSprintStart || GetState() == eHandDraw || GetState() == eHandHide);
}

bool CCustomDevice::CanLam() const
{
	return m_eAnimationsFlags.test(EAnimationsFlags::af_det_hand_lam) &&
		(GetState() == eIdle || GetState() == eHandLam || GetState() == eShowing || GetState() == eSprintEnd || GetState() == eSprintStart || GetState() == eHandDraw || GetState() == eHandHide);
}

void CCustomDevice::shedule_Update(u32 dt)
{
	PROF_EVENT(__FUNCTION__)

	inherited::shedule_Update(dt);

	if (!IsWorking())
	{
		return;
	}

	Position().set(H_Parent()->Position());

	Fvector P;
	P.set(H_Parent()->Position());
}

bool CCustomDevice::IsWorking()
{
	return m_bWorking && H_Parent() && H_Parent() == Level().CurrentViewEntity();
}

void CCustomDevice::UpdateHudAdditonal(Fmatrix& trans)
{
	if (m_pInventory)
	{
		PIItem active_item = m_pInventory->ActiveItem();
		if (CWeapon* pWeap = active_item != nullptr ? active_item->cast_weapon() : nullptr)
		{
			if (pWeap->IsZoomed())
			{
				return;
			}
		}
	}

	CHudItem::UpdateHudAdditonal(trans);
}

void CCustomDevice::UpdateVisibility()
{
	//check visibility

	if (!m_pInventory)
	{
		return;
	}

	if (!Actor())
	{
		return;
	}

	if (m_bNeedActivation)
	{
		CActor* actor = Level().CurrentControlEntity()->cast_actor();
		if (actor && actor->HudAnimator() && actor->HudAnimator()->IsActive())
		{
			m_bNeedActivation = false;
			return;
		}
	}

	if ((g_player_hud->attached_item(0) == nullptr || g_player_hud->attached_item(0)->m_parent_hud_item->cast_missile() == nullptr) && GetState() >= EDeviceStates::eHandThrowStart && GetState() <= EDeviceStates::eHandThrowEnd)
	{
		SwitchState(eIdle);
	}

	attachable_hud_item* i0 = g_player_hud->attached_item(0);
	CHudItem* parent_hud_item = i0 != nullptr ? i0->m_parent_hud_item : nullptr;

	if (parent_hud_item != nullptr && HudItemData() != nullptr)
	{
		bool bClimborTalking = ((Actor()->GetMovementState(eReal) & mcClimb) != 0 || Actor()->IsTalking());
		if (bClimborTalking)
		{
			HideDetector(true);
			m_bNeedActivation = true;
		}
		else
		{
			CWeapon* wpn = parent_hud_item != nullptr ? parent_hud_item->cast_weapon() : nullptr;
			if (wpn != nullptr && (wpn->IsZoomed() || wpn->GetState() == CWeapon::eReload || wpn->GetState() == CWeapon::eSwitch))
			{
				HideDetector(true);
				m_bNeedActivation = true;
			}
		}
	}
	else if (m_bNeedActivation)
	{
		bool bClimborTalking = ((Actor()->GetMovementState(eReal) & mcClimb) != 0 || Actor()->IsTalking());
		if (!bClimborTalking)
		{
			bool bChecked = parent_hud_item == nullptr || CheckCompatibilityInt(parent_hud_item, 0);

			if (bChecked)
			{
				ShowDetector(true);
			}
		}
	}
}

extern u32 hud_adj_mode;

void CCustomDevice::UpdateCL()
{
	PROF_EVENT(__FUNCTION__)

	inherited::UpdateCL();

	if (H_Parent() != Level().CurrentEntity())
	{
		return;
	}

	enable(!IsHidden());

	/* TODO: Drombeys to Rawlik: Let's redo it in the gunslinger branch
	if (HudAnimationExist("anm_bore") && AllowBore())
	{
		CActor* pActor = smart_cast<CActor*>(H_Parent());
		if (pActor && !pActor->AnyMove())
		{
			if (hud_adj_mode == 0 && GetState() == eIdle && (Device.dwTimeGlobal - m_dw_curr_substate_time > 20000))
			{
				SwitchState(eBore);
				ResetSubStateTime();
			}
		}
	}
	*/

	UpdateVisibility();

	if (m_HudLight.GetTorchInstalled())
	{
		if (attachable_hud_item* item = HudItemData())
		{
			for (const shared_str& bone : m_HudLight.ConeBones)
			{
				item->set_bone_visible(bone, m_HudLight.GetTorchActive(), TRUE);
			}
		}
	}

	if (!IsWorking())
	{
		return;
	}

	UpdateWork();
}

bool CCustomDevice::can_be_attached() const
{
	if (smart_cast<CActor*>(H_Parent()) && m_pInventory)
	{
		return m_pInventory->InSlot(this) && !IsHidden();
	}

	return inherited::can_be_attached();
}

void CCustomDevice::OnH_B_Independent(bool just_before_destroy)
{
	inherited::OnH_B_Independent(just_before_destroy);

	SwitchState(eHidden);
	m_HudLight.SwitchTorchlight(false);
	m_HudLight.UpdateTorchFromObject(this);
	m_bIsZoomed = false;
}

void CCustomDevice::OnMoveToRuck(const SInvItemPlace& prev)
{
	inherited::OnMoveToRuck(prev);

	if (prev.type == eItemPlaceSlot)
	{
		SwitchState(eHidden);
		g_player_hud->detach_item(this);
		m_bNeedActivation = false;
		m_bIsZoomed = false;
	}

	TurnDetectorInternal(false);
	StopCurrentAnimWithoutCallback();
}

void CCustomDevice::TurnDetectorInternal(bool b)
{
	m_bWorking = b;
}

void CCustomDevice::OnMotionMark(u32 state, const motion_marks& mark)
{
	inherited::OnMotionMark(state, mark);

	if ((state == eShowing || state == eHiding) && mark.name == "Left")
	{
		m_HudLight.SwitchTorchlight(!m_HudLight.GetTorchActive());
	}
}