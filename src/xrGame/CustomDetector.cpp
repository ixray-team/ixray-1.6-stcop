#include "stdafx.h"
#include "customdetector.h"
#include "ui/ArtefactDetectorUI.h"
#include "hudmanager.h"
#include "inventory.h"
#include "level.h"
#include "map_manager.h"
#include "ActorEffector.h"
#include "actor.h"
#include "ui/UIWindow.h"
#include "player_hud.h"
#include "weapon.h"
#include "WeaponKnife.h"

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

bool CCustomDetector::CheckCompatibilityInt(CHudItem* itm, u16* slot_to_activate)
{
	if(itm==nullptr)
		return true;

	CInventoryItem& iitm			= itm->item();
	u32 slot						= iitm.BaseSlot();
	bool bres = (slot==INV_SLOT_2 || slot==KNIFE_SLOT || slot==BOLT_SLOT);
	if(!bres && slot_to_activate)
	{
		*slot_to_activate	= NO_ACTIVE_SLOT;
		if(m_pInventory->ItemFromSlot(BOLT_SLOT))
			*slot_to_activate = BOLT_SLOT;

		if(m_pInventory->ItemFromSlot(KNIFE_SLOT))
			*slot_to_activate = KNIFE_SLOT;

		if(m_pInventory->ItemFromSlot(INV_SLOT_3) && m_pInventory->ItemFromSlot(INV_SLOT_3)->BaseSlot()!=INV_SLOT_3)
			*slot_to_activate = INV_SLOT_3;

		if(m_pInventory->ItemFromSlot(INV_SLOT_2) && m_pInventory->ItemFromSlot(INV_SLOT_2)->BaseSlot()!=INV_SLOT_3)
			*slot_to_activate = INV_SLOT_2;

		if(*slot_to_activate != NO_ACTIVE_SLOT)
			bres = true;
	}

	if(itm->GetState()!=CHUDState::eShowing)
		bres = bres && !itm->IsPending();

	if (bres)
	{
		CWeapon* W = smart_cast<CWeapon*>(itm);
		if (W)
			bres =	bres && W->GetState() != CHUDState::eBore && W->GetState() != CWeapon::eReload && W->GetState() != CWeapon::eSwitch && W->GetState() != CWeapon::eUnjam && !W->IsZoomed();
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
		case CHUDState::EHudStates::eIdle: {
			ToggleDetector(bFastMode);
			return;
		}
		case CHUDState::EHudStates::eShowing: {
			bool bClimb = Actor()->GetMovementState(eReal) & ACTOR_DEFS::EMoveCommand::mcClimb;
			if (bClimb) {
				StopCurrentAnimWithoutCallback();
				SetState(eIdle);
				ToggleDetector(bFastMode);
			}
			break;
		}
		default:
			break;
	}
}

void CCustomDetector::ShowDetector(bool bFastMode)
{
	if(GetState()==eHidden)
		ToggleDetector(bFastMode);
}

void CCustomDetector::ToggleDetector(bool bFastMode)
{
	isTryToToggle = true;
	bNeedHideDet = false;

	m_bNeedActivation		= false;
	m_bFastAnimMode			= bFastMode;

	PIItem iitem = m_pInventory->ActiveItem();
	CHudItem* itm = (iitem) ? iitem->cast_hud_item() : nullptr;
	CWeapon* wpn = smart_cast<CWeapon*>(itm);

	if(GetState()==eHidden)
	{
		u16 slot_to_activate = NO_ACTIVE_SLOT;

		if(CheckCompatibilityInt(itm, &slot_to_activate))
		{
			if(slot_to_activate!=NO_ACTIVE_SLOT)
			{
				m_pInventory->Activate(slot_to_activate);
				m_bNeedActivation		= true;
			}
			else
			{
				CWeaponKnife* knf = smart_cast<CWeaponKnife*>(wpn);
				if (wpn)
				{
					if (knf || wpn->bIsNeedCallDet)
					{
						isTryToToggle = false;
						SwitchState(eShowing);
						TurnDetectorInternal(true);
						wpn->bIsNeedCallDet = false;
					}
					else
					{
						if (wpn->GetState() == CWeapon::eIdle)
						{
							isTryToToggle = false;
							wpn->SwitchState(CWeapon::eShowingDet);
						}
					}
				}
				else
				{
					isTryToToggle = false;
					SwitchState(eShowing);
					TurnDetectorInternal(true);
				}
			}
		}
	}
	else
	{
		if (GetState() == eShowing)
			bNeedHideDet = true;

		if (wpn)
		{
			u32 state = wpn->GetState();

			if (state == CWeapon::eHiding || state == CWeapon::eEmptyClick || state == CWeapon::eShowing || state == CWeapon::eCheckMisfire)
			{
				if (state == CWeapon::eHiding)
				{
					bNeedHideDet = true;
					isTryToToggle = false;
				}
				return;
			}
		}

		if (GetState() == eIdle)
		{
			isTryToToggle = false;
			SwitchState(eHiding);
		}
	}

}

void CCustomDetector::OnStateSwitch(u32 S)
{
	inherited::OnStateSwitch(S);

	switch(S)
	{
	case eShowing:
		{
			g_player_hud->attach_item	(this);
			m_sounds.PlaySound			("sndShow", Fvector().set(0,0,0), this, true, false);
			PlayHUDMotion				(m_bFastAnimMode ? "anm_show_fast" : "anm_show", FALSE, this, GetState(), false);
			SetPending					(TRUE);
		}break;
	case eHiding:
		{
			m_sounds.PlaySound			("sndHide", Fvector().set(0,0,0), this, true, false);
			PlayHUDMotion				(m_bFastAnimMode ? "anm_hide_fast" : "anm_hide", TRUE, this, GetState(), false);
			SetPending					(TRUE);
			SetHideDetStateInWeapon();
		}break;
	case eIdle:
		{
			PlayAnimIdle				();
			SetPending					(FALSE);
		}break;
	}
}

void CCustomDetector::SetHideDetStateInWeapon()
{
	CWeapon* wpn = smart_cast<CWeapon*>(m_pInventory->ActiveItem());

	if (!wpn)
		return;

	CWeaponKnife* knf = smart_cast<CWeaponKnife*>(wpn);

	if (knf)
		return;

	if (wpn->GetState() == CWeapon::eIdle)
		wpn->SwitchState(CWeapon::eHideDet);
}

void CCustomDetector::OnAnimationEnd(u32 state)
{
	inherited::OnAnimationEnd	(state);
	switch(state)
	{
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
	isTryToToggle		= false;
	bNeedHideDet		= false;
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

void CCustomDetector::UpdateVisibility()
{
	//check visibility
	attachable_hud_item* i0		= g_player_hud->attached_item(0);
	if(i0 && HudItemData())
	{
		bool bClimb	= Actor()->GetMovementState(eReal) & ACTOR_DEFS::EMoveCommand::mcClimb;
		if(bClimb)
		{
			HideDetector		(true);
			m_bNeedActivation	= true;
		}else
		{
			CWeapon* wpn			= smart_cast<CWeapon*>(i0->m_parent_hud_item);
			if(wpn)
			{
				u32 state = wpn->GetState();
				bool TempTest = wpn->m_bAmmoInChamber ? wpn->iAmmoInChamberElapsed == 0 && wpn->GetAmmoElapsed() == 0 : wpn->GetAmmoElapsed() == 0;
				bool isGuns = EngineExternal()[EEngineExternalGunslinger::EnableGunslingerMode];
				if (wpn->IsZoomed() || state==CWeapon::eReload || state == CWeapon::eUnjam || state == CWeapon::eSwitch || (isGuns && state == CWeapon::eSwitchMode && TempTest))
				{
					HideDetector(true);
					m_bNeedActivation = true;
				}
			}
		}
	}else
	if(m_bNeedActivation)
	{
		attachable_hud_item* i0_		= g_player_hud->attached_item(0);
		bool bClimb	= Actor()->GetMovementState(eReal) & ACTOR_DEFS::EMoveCommand::mcClimb;
		if(!bClimb)
		{
			CHudItem* huditem		= (i0_)?i0_->m_parent_hud_item : nullptr;
			bool bChecked			= !huditem || CheckCompatibilityInt(huditem, 0);
			
			if(	bChecked )
				ShowDetector		(true);
		}
	}
}

void CCustomDetector::UpdateCL() 
{
	inherited::UpdateCL();

	if(H_Parent()!=Level().CurrentEntity() )			return;

	if (m_pInventory)
	{
		CWeapon* wpn = smart_cast<CWeapon*>(m_pInventory->ActiveItem());

		if (!wpn)
		{
			if (bNeedHideDet && GetState() == eIdle)
			{
				bNeedHideDet = false;
				HideDetector(false);
			}
		}

		if (isTryToToggle)
		{
			if (wpn)
			{
				if (wpn->GetState() == CWeapon::eIdle)
				{
					isTryToToggle = false;
					ToggleDetector(true);
				}
			}
			else
			{
				if (GetState() == eHidden)
				{
					isTryToToggle = false;
					ToggleDetector(false);
				}
			}
		}
	}

	UpdateVisibility		();

	if( !IsWorking() )		return;
	UpfateWork				();
}

void CCustomDetector::OnH_A_Chield() 
{
	inherited::OnH_A_Chield		();
}

void CCustomDetector::OnH_B_Independent(bool just_before_destroy) 
{
	inherited::OnH_B_Independent(just_before_destroy);
	
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