#include "stdafx.h"
#include "CustomDetector.h"
#include "ui/ArtefactDetectorUI.h"
#include "HUDManager.h"
#include "Inventory.h"
#include "Level.h"
#include "map_manager.h"
#include "ActorEffector.h"
#include "Actor.h"
#include "ui/UIWindow.h"
#include "player_hud.h"
#include "Weapon.h"
#include "WeaponKnife.h"
#include "HUDAnimItem.h"

ITEM_INFO::ITEM_INFO()
{
	pParticle	= nullptr;
	curr_ref	= nullptr;
}

ITEM_INFO::~ITEM_INFO()
{
	if(pParticle)
		CParticlesObject::Destroy(pParticle);
}
#include "WeaponBinoculars.h"
bool CCustomDetector::CheckCompatibilityInt(CHudItem* itm, u16* slot_to_activate)
{
	if(itm==nullptr)
		return true;

	if (CHUDAnimItem* ha = smart_cast<CHUDAnimItem*>(itm))
	{
		if (!ha->SupportsDetector)
			return false;
	}

	CInventoryItem& iitm			= itm->item();
	u32 slot						= iitm.BaseSlot();
	
	bool isGuns = EngineExternal().isModificationGunslinger();
	
	bool bres = (slot==INV_SLOT_2 || slot==KNIFE_SLOT || slot==BOLT_SLOT || !isGuns && (slot==BINOCULAR_SLOT || slot==GRENADE_SLOT) || slot==ANIM_SLOT);
	if(!bres && slot_to_activate)
	{
		*slot_to_activate = NO_ACTIVE_SLOT;

		if(m_pInventory->ItemFromSlot(BOLT_SLOT))
			*slot_to_activate = BOLT_SLOT;
		else if(m_pInventory->ItemFromSlot(INV_SLOT_3) && m_pInventory->ItemFromSlot(INV_SLOT_3)->BaseSlot()!=INV_SLOT_3)
			*slot_to_activate = INV_SLOT_3;
		else if(m_pInventory->ItemFromSlot(INV_SLOT_2) && m_pInventory->ItemFromSlot(INV_SLOT_2)->BaseSlot()!=INV_SLOT_3)
			*slot_to_activate = INV_SLOT_2;
		else if(m_pInventory->ItemFromSlot(KNIFE_SLOT))
			*slot_to_activate = KNIFE_SLOT;

		if(*slot_to_activate != NO_ACTIVE_SLOT)
			bres = true;
	}

	if(!bres && itm->GetState()!=CHUDState::eShowing)
		bres = bres && !itm->IsPending();

	if (bres)
	{
		CWeapon* W = smart_cast<CWeapon*>(itm);
		if (W && !smart_cast<CWeaponBinoculars*>(W))
			bres = bres && !W->IsZoomed() && !W->IsPending();
	}

	return bres;
}

bool  CCustomDetector::CheckCompatibility(CHudItem* itm)
{
	if(!inherited::CheckCompatibility(itm) )	
		return false;

	if(!CheckCompatibilityInt(itm, nullptr))
	{
		HideDetector	(true);
		return			false;
	}

	return true;
}

void CCustomDetector::HideDetector(bool bFastMode)
{
	const CHUDState::EHudStates CurrentState = (CHUDState::EHudStates) GetState();
	switch (CurrentState) {
		case CHUDState::EHudStates::eIdle:
		case CHUDState::EHudStates::eShowing: {
			bool bClimb = Actor()->GetMovementState(eReal) & ACTOR_DEFS::EMoveCommand::mcClimb;
			if (bClimb && CurrentState==CHUDState::EHudStates::eShowing) {
				StopCurrentAnimWithoutCallback();
				SetState(eIdle);
				ToggleDetector(bFastMode);
			}
			else
				ToggleDetector(bFastMode);
			break;
		}
		default:
			break;
	}
}

void CCustomDetector::ShowDetector(bool bFastMode)
{
	bool isGuns = EngineExternal().isModificationGunslinger();
	
	if (GetState()==eHidden || !isGuns && GetState()==eHiding)
		ToggleDetector(bFastMode);
}

void CCustomDetector::ToggleDetector(bool bFastMode)
{
	m_bNeedActivation = false;
	m_bFastAnimMode	= bFastMode;

	PIItem iitem = m_pInventory->ActiveItem();
	CHudItem* itm = (iitem) ? iitem->cast_hud_item() : nullptr;
	CWeapon* wpn = smart_cast<CWeapon*>(itm);

	bool isGuns = EngineExternal().isModificationGunslinger();

	if (GetState() == eHidden || !isGuns && GetState() == eHiding)
	{
		u16 slot_to_activate = NO_ACTIVE_SLOT;

		if (CheckCompatibilityInt(itm, &slot_to_activate))
		{
			if (slot_to_activate != NO_ACTIVE_SLOT)
			{
				m_pInventory->Activate(isGuns ? BOLT_SLOT : slot_to_activate);
				m_bNeedActivation = true;
			}
			else
			{
				CWeaponKnife* knf = smart_cast<CWeaponKnife*>(wpn);
				if (wpn)
				{
					if (knf || wpn->bIsNeedCallDet || !isGuns)
					{
						SwitchState(eShowing);
						TurnDetectorInternal(true);
						wpn->bIsNeedCallDet = false;
					}
					else if (isGuns && wpn->GetState() == CWeapon::eIdle)
						wpn->SwitchState(CWeapon::eShowingDet);
				}
				else
				{
					SwitchState(eShowing);
					TurnDetectorInternal(true);
				}
			}
		}
	}
	else if (GetState()==eIdle || GetState()==eShowing)
	{
		if (wpn && isGuns)
		{
			u32 state = wpn->GetState();

			if (state == CWeapon::eEmptyClick || state == CWeapon::eShowing || state == CWeapon::eCheckMisfire || state == CWeapon::eFire || state == CWeapon::eFire2 || (state == CWeapon::eSwitchMode && wpn->GetAmmoElapsed() != 0) || wpn->IsZoomed())
				return;
		}

		if (GetState() == eIdle || !isGuns && GetState() == eShowing)
			SwitchState(eHiding);
	}

}

void CCustomDetector::ShowingCallback(CBlend*B)
{
	ToggleDetector(g_player_hud->attached_item(0)!=nullptr);
	g_player_hud->ResetBlockedPartID();
	g_player_hud->OnMovementChanged(mcAnyMove);
	g_player_hud->RestoreHandBlends("right_hand");
}

void CCustomDetector::switch_detector()
{
	bool isGuns = EngineExternal().isModificationGunslinger();
	if (!isGuns && GetState() == eHidden && g_player_hud->attached_item(0) && m_pInventory->ActiveItem() && m_pInventory->ActiveItem()->BaseSlot() == INV_SLOT_2)
	{
		if(g_player_hud->animator_play(g_player_hud->check_anim("anm_hide", 0)?"anm_hide":"anm_hide_0", 0, 1, TRUE, 1.5f, 0, false, true, [](CBlend*B){static_cast<CCustomDetector*>(B->CallbackParam)->ShowingCallback(B);}, this, 0))
			g_player_hud->animator_fx_play(g_player_hud->check_anim("anm_hide", 0)?"anm_hide":"anm_hide_0", 0, 2, 0, 3.f, 1.f, 1.f, 0.5f);
	}
	else
		ToggleDetector(g_player_hud->attached_item(0) != nullptr);
}

bool  CCustomDetector::need_renderable()
{
	return m_pInventory && m_pInventory->ItemFromSlot(DETECTOR_SLOT) && m_pInventory->ItemFromSlot(DETECTOR_SLOT)->object().cast_inventory_item() == cast_inventory_item();
}

void CCustomDetector::OnStateSwitch(u32 S)
{
	inherited::OnStateSwitch(S);

	switch(S)
	{
		case eShowing:
		{
			m_sounds.PlaySound			("sndShow", Fvector().set(0,0,0), this, true, false);
			PlayHUDMotion				(m_bFastAnimMode?"anm_show_fast":"anm_show", !!(m_old_state != eHidden), this, S);
			SetPending					(TRUE);
		}break;
		case eHiding:
		{
			m_sounds.PlaySound			("sndHide", Fvector().set(0,0,0), this, true, false);
			PlayHUDMotion				(m_bFastAnimMode?"anm_hide_fast":"anm_hide", TRUE, this, S);
			SetPending					(TRUE);
			SetHideDetStateInWeapon();
		}break;
		case eIdle:
		{
			PlayAnimIdle();
			SetPending(FALSE);
		}break;
		case eDetThrowStart:
		{
			PlayHUDMotion("anm_throw_start", true, this, eDetThrowStart);
			SetPending(FALSE);
		}break;
		case eDetThrowIdle:
		{
			PlayHUDMotion("anm_throw_idle", true, this, eDetThrowIdle);
			SetPending(FALSE);
		}break;
		case eDetThrowEnd:
		{
			PlayHUDMotion("anm_throw_end", true, this, eDetThrowEnd);
			SetPending(FALSE);
		}break;
		case eDetAimStart:
		{
			PlayHUDMotion("anm_idle_aim_start", true, this, eDetAimStart);
			SetPending(FALSE);
		}break;
		case eDetAimEnd:
		{
			PlayHUDMotion("anm_idle_aim_end", true, this, eDetAimEnd);
			SetPending(FALSE);
		}break;
		case eDetKick:
		{
			PlayHUDMotion("anm_kick", true, this, eDetKick);
			SetPending(FALSE);
		}break;
		case eDetKick2:
		{
			PlayHUDMotion("anm_kick2", true, this, eDetKick2);
			SetPending(FALSE);
		}break;
		case eDetHideHand:
		{
			PlayHUDMotion("anm_hide_hand", true, this, eDetHideHand);
			SetPending(FALSE);
		}break;
		case eDetShowHand:
		{
			PlayHUDMotion("anm_show_hand", true, this, eDetShowHand);
			SetPending(FALSE);
		}break;
	}
	m_old_state=S;
}

void CCustomDetector::PlayAnimIdle()
{
	CWeapon* wpn = smart_cast<CWeapon*>(m_pInventory->ActiveItem());

	bool isGuns = EngineExternal().isModificationGunslinger();

	if (wpn && wpn->IsZoomed() && isGuns)
		PlayAnimAim();
	else
	{
		if (TryPlayAnimIdle())
			return;

		PlayHUDMotion("anm_idle", true, this, eIdle);
	}
}

void CCustomDetector::PlayAnimAim()
{
	u32 state = Actor()->GetMovementState(eReal);
	xr_string anim_name = "anm_idle_aim";

	if (state & ACTOR_DEFS::EMoveCommand::mcAnyMove)
		anim_name += "_moving";

	if (state & ACTOR_DEFS::EMoveCommand::mcFwd)
		anim_name += "_forward";
	else if (state & ACTOR_DEFS::EMoveCommand::mcBack)
		anim_name += "_back";
	else if (state & ACTOR_DEFS::EMoveCommand::mcLStrafe)
		anim_name += "_left";
	else if (state & ACTOR_DEFS::EMoveCommand::mcRStrafe)
		anim_name += "_right";

	PlayHUDMotion(anim_name, TRUE, this, eIdle);
}

void CCustomDetector::SetHideDetStateInWeapon() const
{
	bool isGuns = EngineExternal().isModificationGunslinger();
	
	if (!isGuns)
		return;

	CWeapon* wpn = smart_cast<CWeapon*>(m_pInventory->ActiveItem());

	if (wpn == nullptr)
		return;

	CWeaponKnife* knf = smart_cast<CWeaponKnife*>(wpn);

	if (knf != nullptr)
		return;

	if (wpn->GetState() == CWeapon::eIdle)
		wpn->SwitchState(CWeapon::eHideDet);
}

void CCustomDetector::OnAnimationEnd(u32 state)
{
	inherited::OnAnimationEnd	(state);
	switch(state)
	{
	case eDetThrowEnd:
	case eDetAimStart:
	case eDetAimEnd:
	case eDetKick:
	case eDetKick2:
	case eDetHideHand:
	case eDetShowHand:
	case eIdle:
	{
		SwitchState(eIdle);
	}break;
	case eDetThrowStart:
	case eDetThrowIdle:
		SwitchState(eDetThrowIdle);
	break;
	case eShowing:
		{
			SwitchState					(eIdle);
		} break;
	case eHiding:
		{
			SwitchState					(eHidden);
			TurnDetectorInternal		(false);
			g_player_hud->detach_item	(this);
		} break;
	}
}

void CCustomDetector::UpdateXForm()
{
	CInventoryItem::UpdateXForm();
}

void CCustomDetector::OnActiveItem()
{
	return;
}

void CCustomDetector::OnHiddenItem()
{
}

CCustomDetector::CCustomDetector() 
{
	m_ui				= nullptr;
	m_bFastAnimMode		= false;
	m_bNeedActivation	= false;
	m_old_state			= eHidden;
}

CCustomDetector::~CCustomDetector() 
{
	m_artefacts.destroy		();
	TurnDetectorInternal	(false);
	xr_delete				(m_ui);
}

BOOL CCustomDetector::net_Spawn(CSE_Abstract* DC) 
{
	TurnDetectorInternal(false);
	return		(inherited::net_Spawn(DC));
}

void CCustomDetector::Load(LPCSTR section) 
{
	inherited::Load			(section);

	m_fAfDetectRadius		= pSettings->r_float(section,"af_radius");
	m_fAfVisRadius			= pSettings->r_float(section,"af_vis_radius");
	m_artefacts.load		(section, "af");

	m_sounds.LoadSound( section, "snd_draw", "sndShow");
	m_sounds.LoadSound( section, "snd_holster", "sndHide");
}


void CCustomDetector::shedule_Update(u32 dt) 
{
	PROF_EVENT_DYNAMIC(cNameSect_str())
	inherited::shedule_Update(dt);
	
	if( !IsWorking() )			return;

	Position().set(H_Parent()->Position());

	Fvector						P; 
	P.set						(H_Parent()->Position());
	m_artefacts.feel_touch_update(P,m_fAfDetectRadius);
}


bool CCustomDetector::IsWorking()
{
	return m_bWorking && H_Parent() && H_Parent()==Level().CurrentViewEntity();
}

void CCustomDetector::UpfateWork()
{
	UpdateAf				();
	m_ui->update			();
}

void CCustomDetector::UpdateHudAdditonal(Fmatrix& trans)
{
	if (m_pInventory)
	{
		CWeapon* pWeap = smart_cast<CWeapon*>(m_pInventory->ActiveItem());
		if(pWeap)
		{
			if(pWeap->IsZoomed())
				return;
		}
	}

	CHudItem::UpdateHudAdditonal(trans);
}

void CCustomDetector::UpdateVisibility()
{
	if (!m_pInventory)
		return;

	attachable_hud_item* i0 = g_player_hud->attached_item(0);
	if (i0 && HudItemData())
	{
		bool bClimb = (Actor()->GetMovementState(eReal) & ACTOR_DEFS::EMoveCommand::mcClimb) || Actor()->IsTalking();
		if (bClimb)
		{
			HideDetector(true);
			m_bNeedActivation = true;
		}
		else
		{
			CWeapon* wpn = smart_cast<CWeapon*>(i0->m_parent_hud_item);
			if (wpn)
			{
				u32 state = wpn->GetState();
				bool isGuns = EngineExternal().isModificationGunslinger();
				if (!isGuns && wpn->IsZoomed() || state == CWeapon::eReload || state == CWeapon::eUnjam || state == CWeapon::eSwitch || (isGuns && state == CWeapon::eSwitchMode && wpn->GetAmmoElapsed() == 0))
				{
					HideDetector(true);
					m_bNeedActivation = true;
				}
			}
		}
	}
	else if (m_bNeedActivation)
	{
		attachable_hud_item* i0_ = g_player_hud->attached_item(0);
		bool bClimb = (Actor()->GetMovementState(eReal) & ACTOR_DEFS::EMoveCommand::mcClimb) || Actor()->IsTalking();
		if (!bClimb)
		{
			CHudItem* huditem = (i0_) ? i0_->m_parent_hud_item : NULL;
			bool bChecked = !huditem || CheckCompatibilityInt(huditem, 0);

			if (bChecked)
				ShowDetector(true);
		}
	}
}

extern u32 hud_adj_mode;

void CCustomDetector::UpdateCL() 
{
	PROF_EVENT_DYNAMIC(cNameSect_str())
	inherited::UpdateCL();

	if (H_Parent() != Level().CurrentEntity())
		return;

	enable(!IsHidden());

	if (GetState() != eHidden)
	{
		bool isGuns = EngineExternal().isModificationGunslinger();
		
		if (AllowBore() && !isGuns)
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
	}

	UpdateVisibility();

	if (!IsWorking())
		return;

	UpfateWork();
}

bool CCustomDetector::can_be_attached		() const
{
	if (smart_cast<CActor*>(H_Parent()) && m_pInventory)
		return m_pInventory->InSlot(this) && !IsHidden();

	return inherited::can_be_attached();
}

void CCustomDetector::OnH_A_Chield() 
{
	inherited::OnH_A_Chield		();
}

void CCustomDetector::OnH_B_Independent(bool just_before_destroy) 
{
	inherited::OnH_B_Independent(just_before_destroy);
	SwitchState					(eHidden);
	m_artefacts.clear			();
}


void CCustomDetector::OnMoveToRuck(const SInvItemPlace& prev)
{
	inherited::OnMoveToRuck	(prev);
	if(prev.type==eItemPlaceSlot)
	{
		SwitchState					(eHidden);
		g_player_hud->detach_item	(this);
	}
	TurnDetectorInternal			(false);
	StopCurrentAnimWithoutCallback	();
}

void CCustomDetector::OnMoveToSlot(const SInvItemPlace& prev)
{
	inherited::OnMoveToSlot	(prev);
}

void CCustomDetector::TurnDetectorInternal(bool b)
{
	m_bWorking				= b;
	if(b && m_ui==nullptr)
	{
		CreateUI			();
	}else
	{
//.		xr_delete			(m_ui);
	}

	UpdateNightVisionMode	(b);
}

#include "game_base_space.h"
void CCustomDetector::UpdateNightVisionMode(bool b_on)
{
}

bool CCustomDetector::NeedBlockSprint() const
{
	bool isGuns = EngineExternal().isModificationGunslinger();
	return isGuns && GetState() != eIdle && GetState() != eSprintStart && GetState() != eHidden;
}

void CCustomDetector::StartDetectorAction(u32 state)
{
	bool isGuns = EngineExternal().isModificationGunslinger();
	if (!isGuns)
		return;

	if (GetState() == eHidden || GetState() == eHiding || GetState() == eShowing)
		return;

	SwitchState(state);
}