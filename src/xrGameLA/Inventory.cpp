#include "pch_script.h"
#include "inventory.h"
#include "actor.h"
#include "trade.h"
#include "weapon.h"

#include "ui/UIInventoryUtilities.h"

#include "eatable_item.h"
#include "script_engine.h"
#include "xrmessages.h"
//#include "game_cl_base.h"
#include "xr_level_controller.h"
#include "level.h"
#include "ai_space.h"
#include "entitycondition.h"
#include "game_base_space.h"
#include "clsid_game.h"
#include "ai/stalker/ai_stalker.h"
#include "weaponmagazined.h"
#include "game_object_space.h"
#include "script_callback_ex.h"
#include "script_game_object.h"
#include "player_hud.h"
#include "CustomOutfit.h"

using namespace InventoryUtilities;

// what to block
u32	INV_STATE_BLOCK_ALL		= 0xffffffff;
u32	INV_STATE_CAR			= INV_STATE_BLOCK_ALL ^ (1 << PISTOL_SLOT);
u32	INV_STATE_LADDER		= (1 << RIFLE_SLOT) | (1 << RIFLE_2_SLOT) | (1 << DETECTOR_SLOT);
u32	INV_STATE_INV_WND		= INV_STATE_BLOCK_ALL;
u32	INV_STATE_BUY_MENU		= INV_STATE_BLOCK_ALL;

CInventorySlot::CInventorySlot()
{
	m_pIItem				= NULL;
	m_bVisible				= true;
	m_bPersistent			= false;
	m_blockCounter			= 0;
}

CInventorySlot::~CInventorySlot()
{
}

bool CInventorySlot::CanBeActivated() const
{
	return (m_bVisible && !IsBlocked());
};

bool CInventorySlot::IsBlocked() const
{
	return (m_blockCounter>0);
}


CInventory::CInventory()
{
	
	m_fTakeDist									= pSettings->r_float	("inventory","take_dist");
	m_fMaxWeight								= pSettings->r_float	("inventory","max_weight");
	m_iMaxBelt									= pSettings->r_s32		("inventory","max_belt");

	m_slots.resize								(LAST_SLOT + 1); // first is [1]

	SetCurrentDetector							(NULL);
	m_iActiveSlot								= NO_ACTIVE_SLOT;
	m_iNextActiveSlot							= NO_ACTIVE_SLOT;
	m_iPrevActiveSlot							= NO_ACTIVE_SLOT;
	m_iLoadActiveSlot							= NO_ACTIVE_SLOT;
	m_ActivationSlotReason						= eGeneral;
	m_pTarget									= NULL;
	m_bHandsOnly								= false;

	string256 temp;
	for (TSlotId i = FirstSlot(); i <= LastSlot(); ++i)
	{
		xr_sprintf(temp, "slot_persistent_%d", i);
		if(pSettings->line_exist("inventory",temp))
			m_slots[i].m_bPersistent = !!pSettings->r_bool("inventory",temp);
	}

	m_slots[PDA_SLOT].m_bVisible				= false;
	m_slots[OUTFIT_SLOT].m_bVisible				= false;
	m_slots[TORCH_SLOT].m_bVisible				= false;

	m_bSlotsUseful								= true;
	m_bBeltUseful								= false;

	m_fTotalWeight								= -1.f;
	m_dwModifyFrame								= 0;
	m_drop_last_frame							= false;
	m_iLoadActiveSlotFrame						= u32(-1);
}


CInventory::~CInventory()
{
}

void CInventory::Clear()
{
	m_all.clear							();
	m_ruck.clear						();
	m_belt.clear						();

	for(TSlotId i=FirstSlot(); i<=LastSlot(); i++)
	{
		m_slots[i].m_pIItem				= NULL;
	}


	m_pOwner							= NULL;

	CalcTotalWeight						();
	InvalidateState						();
}

/*
void CInventory::repackAmmo(PIItem pIItem)
{
	CWeaponAmmo* ammo = smart_cast<CWeaponAmmo*>(pIItem);

	if (!ammo) {
		Msg("!! Can't convert to weapon ammo class obj with section '%s'!!!",pIItem->object().cNameSect_str());
		return;
	}


	if (!m_ruck.size()) return;
	TIItemContainer::const_iterator it_b	= m_ruck.begin();
	TIItemContainer::const_iterator it		= m_ruck.end();

	for(--it; it>=it_b; --it) {
		CInventoryItem* invAmmoObj = (*it);
		if (invAmmoObj->m_eItemPlace!=eItemPlaceBelt && invAmmoObj->object().cNameSect()==pIItem->object().cNameSect()) {
			CWeaponAmmo* invAmmo = smart_cast<CWeaponAmmo*>(invAmmoObj);
			R_ASSERT(invAmmo);

			u16 freeSpace = invAmmo->m_boxSize - invAmmo->m_boxCurr;
			if (!freeSpace) break; //Shouldnt this be a continue?

			if (freeSpace>=ammo->m_boxCurr) {
				invAmmo->m_boxCurr+=ammo->m_boxCurr;

				pIItem->SetDeleteManual(TRUE);
			} else {
				invAmmo->m_boxCurr=invAmmo->m_boxSize;
				ammo->m_boxCurr-=freeSpace;
			}
			break;
		}
	}
	return;
}
*/

void CInventory::Take(CGameObject *pObj, bool bNotActivate, bool strict_placement, bool duringSpawn)
{
	CInventoryItem *pIItem				= smart_cast<CInventoryItem*>(pObj);
	VERIFY								(pIItem);

	if(pIItem->m_pCurrentInventory)
	{
		Msg("! ERROR CInventory::Take but object has m_pCurrentInventory");
		Msg("! Inventory Owner is [%d]", GetOwner()->object_id());
		Msg("! Object Inventory Owner is [%d]", pIItem->m_pCurrentInventory->GetOwner()->object_id());

		CObject* p	= pObj->H_Parent();
		if(p)
			Msg("! object parent is [%s] [%d]", p->cName().c_str(), p->ID());
	}


	R_ASSERT							(CanTakeItem(pIItem));

	pIItem->m_pCurrentInventory			= this;
	pIItem->SetDropManual				(FALSE);

	if (Level().CurrentEntity())
	{
		u16 actor_id = Level().CurrentEntity()->ID();
	
		if (GetOwner()->object_id()==actor_id && this->m_pOwner->object_id()==actor_id)		//actors inventory
		{
	
			CWeaponMagazined*	pWeapon = smart_cast<CWeaponMagazined*>(pIItem);
			if (pWeapon && pWeapon->strapped_mode())
			{
				pWeapon->strapped_mode(false);
				Ruck(pWeapon);
			}
	
		}
	}

	m_all.push_back						(pIItem);

	if(!strict_placement)
		pIItem->SetCurrPlace			(eItemPlaceUndefined);

	bool result							= false;
	switch(pIItem->CurrPlace())
	{
	case eItemPlaceBelt:
		result							= Belt(pIItem);
#ifdef DEBUG
		if(!result)
			Msg("cant put in belt item %s", *pIItem->object().cName());
#endif

		break;
	case eItemPlaceRuck:
		result							= Ruck(pIItem);
#ifdef DEBUG
		if(!result)
			Msg("cant put in ruck item %s", *pIItem->object().cName());
#endif

		break;
	case eItemPlaceSlot:
		result							= Slot(pIItem->CurrSlot(), pIItem, bNotActivate);
#ifdef DEBUG
		if(!result)
			Msg("cant slot in ruck item %s", *pIItem->object().cName());
#endif

		break;
	default:
		
		if( !pIItem->RuckDefault() )
		{
			if (CanPutInSlot(pIItem, pIItem->BaseSlot()))
			{
				result						= Slot(pIItem->BaseSlot(), pIItem, bNotActivate); VERIFY(result);
			}
			else if (pIItem->BaseSlot() == RIFLE_SLOT && CanPutInSlot(pIItem, RIFLE_2_SLOT))
			{
				result						= Slot(RIFLE_2_SLOT, pIItem, bNotActivate); VERIFY(result);
			}
			else if (CanPutInBelt(pIItem))
			{
				result						= Belt(pIItem); VERIFY(result);
			}
			else
			{
				result						= Ruck(pIItem); VERIFY(result);
			}
		}else
		{
			result						= Ruck(pIItem); VERIFY(result);
		}
	}

	m_pOwner->OnItemTake				(pIItem, duringSpawn);

	CWeaponMagazined*	pWeapon = smart_cast<CWeaponMagazined*>(pIItem);
	if (pWeapon)
		pWeapon->InitAddons(); //skyloader: need to do it as in CoP when UI will be ported | íàäî âçÿòü ðåàëèçàöèþ çóì òåêñòóð èç ÷í\çï, êîãäà áóäåò ïåðåñåí óè èç çï


	CalcTotalWeight						();
	InvalidateState						();

	pIItem->object().processing_deactivate();
	VERIFY								(pIItem->CurrPlace() != eItemPlaceUndefined);
}

bool CInventory::DropItem(CGameObject *pObj)
{
	CInventoryItem *pIItem				= smart_cast<CInventoryItem*>(pObj);
	VERIFY								(pIItem);
	if( !pIItem )						return false;

	if(pIItem->m_pCurrentInventory!=this)
	{
		Msg("ahtung !!! [%d]", Device.dwFrame);
		Msg("CInventory::DropItem pIItem->m_pCurrentInventory!=this");
		Msg("this = [%d]", GetOwner()->object_id());
		Msg("pIItem->m_pCurrentInventory = [%d]", pIItem->m_pCurrentInventory->GetOwner()->object_id());
	}

	R_ASSERT							(pIItem->m_pCurrentInventory);
	R_ASSERT							(pIItem->m_pCurrentInventory==this);
	VERIFY								(pIItem->CurrPlace() != eItemPlaceUndefined);

	pIItem->object().processing_activate();

	switch(pIItem->CurrPlace())
	{
	case eItemPlaceBelt:{
			R_ASSERT(InBelt(pIItem));
			m_belt.erase(std::find(m_belt.begin(), m_belt.end(), pIItem));
			pIItem->object().processing_deactivate();
		}break;
	case eItemPlaceRuck:{
			R_ASSERT(InRuck(pIItem));
			m_ruck.erase(std::find(m_ruck.begin(), m_ruck.end(), pIItem));
		}break;
	case eItemPlaceSlot:{
			VERIFY			(InSlot(pIItem));
			u32 currSlot = pIItem->CurrSlot();
			if (currSlot == NO_ACTIVE_SLOT)
				return false;
			if (m_iActiveSlot == currSlot)
				Activate	(NO_ACTIVE_SLOT);

			m_slots[currSlot].m_pIItem = NULL;
			pIItem->object().processing_deactivate();
		}break;
	default:
		NODEFAULT;
	};

	TIItemContainer::iterator	it = std::find(m_all.begin(), m_all.end(), pIItem);
	if ( it != m_all.end())
		m_all.erase				(it);
	else
		Msg						("! CInventory::Drop item not found in inventory!!!");

	pIItem->m_pCurrentInventory = NULL;

	m_pOwner->OnItemDrop			(smart_cast<CInventoryItem*>(pObj));

	pIItem->SetCurrPlace(eItemPlaceUndefined);

	CalcTotalWeight					();
	InvalidateState					();
	m_drop_last_frame				= true;
	return							true;
}

//ïîëîæèòü âåùü â ñëîò
bool CInventory::Slot(TSlotId slot_id, PIItem pIItem, bool bNotActivate)
{
	VERIFY(pIItem);
	//Msg("%s Inventory::Slot id: %d, %s[%d], notActivate: %d", m_pOwner->Name(), slot_id, *pIItem->object().cName(), pIItem->object().ID(), bNotActivate);
	if(ItemFromSlot(slot_id) == pIItem)
		return false;

	//tatarinrafa: block putting pnv or helmet in slot if outfit does not allow that Çäåñü ýòî íóæíî ÷òîáû ïíâ è øëåì íå âîëîæèëèñü àâòîìàòè÷åñêè â ñëîò ïðè èõ ïîëó÷åíèè
	if (pIItem->BaseSlot() == HELMET_SLOT || pIItem->BaseSlot() == PNV_SLOT){
		PIItem	itemformoutfitslot = ItemFromSlot(OUTFIT_SLOT);
		if (itemformoutfitslot)
		{
			CCustomOutfit* outfit = smart_cast<CCustomOutfit*>(itemformoutfitslot);//íà âñÿêèé ñëó÷àé ïðîâåðèì åñëè ýòî áðîíÿ â ñëîòå áðîíè, à òî..
			if (outfit)
			{
				if (pIItem->BaseSlot() == HELMET_SLOT && outfit->block_helmet_slot == 1){
					Ruck(pIItem);
					return false;
				}

				if (pIItem->BaseSlot() == PNV_SLOT && outfit->block_pnv_slot == 1){
					Ruck(pIItem);
					return false;
				}
			}
		}
	}

	if (!CanPutInSlot(pIItem, slot_id))
	{
#if 0//def _DEBUG
		Msg("there is item %s[%d,%x] in slot %d[%d,%x]",
				*m_slots[pIItem->BaseSlot()].m_pIItem->object().cName(),
				m_slots[pIItem->BaseSlot()].m_pIItem->object().ID(),
				m_slots[pIItem->BaseSlot()].m_pIItem,
				pIItem->BaseSlot(),
				pIItem->object().ID(),
				pIItem);
#endif
		if (m_slots[slot_id].m_pIItem == pIItem && !bNotActivate){
			Activate(slot_id);
		}

		return false;
	}

	// If item was in another slot already
	auto oldSlot = pIItem->CurrSlot();
	if (oldSlot != NO_ACTIVE_SLOT && oldSlot != slot_id)
	{
		if(GetActiveSlot() == oldSlot)
			Activate(NO_ACTIVE_SLOT);

		m_slots[oldSlot].m_pIItem = NULL;
	}

	m_slots[slot_id].m_pIItem = pIItem;

	//óäàëèòü èç ðþêçàêà èëè ïîÿñà
	TIItemContainer::iterator it = std::find(m_ruck.begin(), m_ruck.end(), pIItem);
	if(m_ruck.end() != it) m_ruck.erase(it);
	it = std::find(m_belt.begin(), m_belt.end(), pIItem);
	if(m_belt.end() != it) m_belt.erase(it);


	if (((m_iActiveSlot == slot_id) || (m_iActiveSlot == NO_ACTIVE_SLOT) && m_iNextActiveSlot == NO_ACTIVE_SLOT) && (!bNotActivate))
		Activate				(slot_id);

	m_pOwner->OnItemSlot(pIItem, pIItem->CurrPlace());

	pIItem->SetCurrPlace(eItemPlaceSlot);
	pIItem->SetCurrSlot(slot_id);
	pIItem->OnMoveToSlot();

#pragma todo("Change to CoP slot ids for scripts (0 no item, 1 knife, etc.).")
	if(IsGameTypeSingle() && Actor()->m_inventory == this)
		Actor()->callback(GameObject::eOnMoveToSlot)((smart_cast<CGameObject*>(pIItem))->lua_game_object(), slot_id - 1);

	pIItem->object().processing_activate();


//tatarinrafa: Ruck pnv and helmet if outfit is not compatable with them
	CCustomOutfit* outfit = smart_cast<CCustomOutfit*>(pIItem);
	if (outfit){
		if (outfit->block_pnv_slot == 1){
			CInventoryItem* pnv = ItemFromSlot(PNV_SLOT);
			if (pnv){
				Ruck(pnv);
			}
		}

		if (outfit->block_helmet_slot == 1){
			CInventoryItem* helmet = ItemFromSlot(HELMET_SLOT);
			if (helmet){
				Ruck(helmet);
			}
		}
	}


	return						true;
}

void CInventory::RepackBelt( PIItem pIItem )
{

	CWeaponAmmo* ammo = smart_cast<CWeaponAmmo*>(pIItem);
	R_ASSERT(ammo);

	if ( !m_belt.size() || pIItem->CurrPlace() == eItemPlaceBelt  ) return; //Belt is empty, nothing to repack.
	TIItemContainer::const_iterator it			= m_belt.begin();
	TIItemContainer::const_iterator it_end		= m_belt.end();
	
	for(it; it != it_end; it++) 
	{
		CInventoryItem* invAmmoObj = (*it);
		if( invAmmoObj->CurrPlace() == eItemPlaceBelt && invAmmoObj->object().cNameSect()== pIItem->object().cNameSect() ) //Is on belt, is the same type;
		{
			CWeaponAmmo* beltAmmo = smart_cast<CWeaponAmmo*>(invAmmoObj); //Cast to ammo obj
			R_ASSERT(beltAmmo);//Check the cast

			if( ammo == beltAmmo || beltAmmo->m_boxCurr == beltAmmo->m_boxSize )
				continue;		   //Just skip it.
			
			u16 empty_space = beltAmmo->m_boxSize - beltAmmo->m_boxCurr;
			if( empty_space > 0 )				//Is this box not full?
			{
				if( empty_space > ammo->m_boxCurr )
				{
					beltAmmo->m_boxCurr += ammo->m_boxCurr; 
					ammo->m_boxCurr = 0;
				}
				else								
				{
					beltAmmo->m_boxCurr += empty_space; 
					ammo->m_boxCurr -= empty_space;
				}
			}
			if( ammo->m_boxCurr == 0 )				//Its and empty Box, Discard it.
			{
				pIItem->SetDropManual( true );
				return;
			}
		}
	}
}

bool CInventory::Belt(PIItem pIItem)
{
	if(!CanPutInBelt(pIItem, false))	return false;

	//Nova: Here is my belt repacking code.
	auto ammo = smart_cast<CWeaponAmmo*>(pIItem);
	if (ammo)
	{
		int boxCurrBefore = ammo->m_boxCurr;
		RepackBelt(pIItem);
		if (!CanPutInBelt(pIItem, true) || ammo->m_boxCurr == 0)
		{
			// if we can't actually move the whole ammo pack to slot, then return
			// true if some ammo was actually moved (even if ammo object didn't), false if nothing changed
			return boxCurrBefore != ammo->m_boxCurr;
		}
	}

	//âåùü áûëà â ñëîòå
	auto currSlot = pIItem->CurrSlot();
	if (currSlot != NO_ACTIVE_SLOT)
	{
		if (m_iActiveSlot == currSlot) Activate(NO_ACTIVE_SLOT);
		m_slots[currSlot].m_pIItem = NULL;
	}

	m_belt.insert(m_belt.end(), pIItem);
	// phobos2077: sort items in belt after adding new item
	std::sort(m_belt.begin(), m_belt.end(), GreaterRoomInRuck);

	if (currSlot == NO_ACTIVE_SLOT)
	{
		TIItemContainer::iterator it = std::find(m_ruck.begin(), m_ruck.end(), pIItem);
		if(m_ruck.end() != it) m_ruck.erase(it);
	}

	CalcTotalWeight();
	InvalidateState						();

	EItemPlace p = pIItem->CurrPlace();
	pIItem->SetCurrPlace(eItemPlaceBelt);
	m_pOwner->OnItemBelt(pIItem, p);
	pIItem->OnMoveToBelt();


	if(IsGameTypeSingle() && Actor()->m_inventory == this)
		Actor()->callback(GameObject::eOnMoveToBelt)((smart_cast<CGameObject*>(pIItem))->lua_game_object());

	if (currSlot != NO_ACTIVE_SLOT)
		pIItem->object().processing_deactivate();

	pIItem->object().processing_activate();

	return true;
}

void CInventory::RepackRuck( PIItem pIItem )
{

	CWeaponAmmo* ammo = smart_cast<CWeaponAmmo*>(pIItem);
	R_ASSERT(ammo);

	if ( !m_ruck.size() || pIItem->CurrPlace() == eItemPlaceRuck ) return;
	TIItemContainer::const_iterator it			= m_ruck.begin();
	TIItemContainer::const_iterator it_end		= m_ruck.end();
	
	for(it; it != it_end; it++) 
	{
		CInventoryItem* invAmmoObj = (*it);
		if( invAmmoObj->CurrPlace() == eItemPlaceRuck && invAmmoObj->object().cNameSect()== pIItem->object().cNameSect() ) //Is in ruck, is the same type;
		{
			CWeaponAmmo* ruckAmmo = smart_cast<CWeaponAmmo*>(invAmmoObj); //Cast to ammo obj
			R_ASSERT(ruckAmmo);//Check the cast

			if( ammo == ruckAmmo || ruckAmmo->m_boxCurr == ruckAmmo->m_boxSize )
				continue;		   //Just skip it.
			
			u16 empty_space = ruckAmmo->m_boxSize - ruckAmmo->m_boxCurr;
			if( empty_space > ammo->m_boxCurr )
			{
				ruckAmmo->m_boxCurr += ammo->m_boxCurr; 
				ammo->m_boxCurr = 0;
			}
			else
			{
				ruckAmmo->m_boxCurr += empty_space; 
				ammo->m_boxCurr -= empty_space;
			}

			if( ammo->m_boxCurr == 0 )				//Its and empty Box, Discard it.
			{
				pIItem->SetDropManual( true );
				return;
			}
		}
	}
}

bool CInventory::Ruck(PIItem pIItem)
{
	if(!CanPutInRuck(pIItem)) return true;

	//Nova: Here is my belt repacking code.
	if ( pIItem->object().CLS_ID==CLSID_OBJECT_AMMO )
	{
		RepackRuck(pIItem);
	}

	auto currSlot = pIItem->CurrSlot();
	// item was in the slot
	if (currSlot != NO_ACTIVE_SLOT)
	{
		if (m_iActiveSlot == currSlot)
		{
			Activate(NO_ACTIVE_SLOT);
		}
		else if (m_currentDetectorInHand == pIItem)
		{
			m_currentDetectorInHand->HideDetector(false);
		}
		m_slots[currSlot].m_pIItem = NULL;
	}
	else
	{
		//âåùü áûëà íà ïîÿñå èëè âîîáùå òîëüêî ïîäíÿòà ñ çåìëè
		TIItemContainer::iterator it = std::find(m_belt.begin(), m_belt.end(), pIItem);
		if(m_belt.end() != it) m_belt.erase(it);
	}
	//Msg("%s Inventory::Ruck() %s from slot: %d", m_pOwner->Name(), pIItem->object().cNameSect_str(), currSlot);

	m_ruck.insert									(m_ruck.end(), pIItem);

	CalcTotalWeight									();
	InvalidateState									();

	m_pOwner->OnItemRuck							(pIItem, pIItem->CurrPlace());
	pIItem->SetCurrPlace							(eItemPlaceRuck);
	pIItem->OnMoveToRuck							();

	if(IsGameTypeSingle() && Actor()->m_inventory == this)
		Actor()->callback(GameObject::eOnMoveToRuck)((smart_cast<CGameObject*>(pIItem))->lua_game_object());

	if (currSlot != NO_ACTIVE_SLOT)
		pIItem->object().processing_deactivate();

	return true;
}

void CInventory::Activate_deffered	(TSlotId slot, u32 _frame)
{
	 m_iLoadActiveSlot			= slot;
	 m_iLoadActiveSlotFrame		= _frame;
}

void  CInventory::ActivateNextItemInActiveSlot()
{
	if(m_iActiveSlot==NO_ACTIVE_SLOT)	return;

	PIItem current_item		= m_slots[m_iActiveSlot].m_pIItem;
	PIItem new_item			= NULL;

	bool b = (current_item==NULL);

	TIItemContainer::const_iterator it		= m_all.begin();
	TIItemContainer::const_iterator it_e	= m_all.end();

	for(; it!=it_e; ++it)
	{
		PIItem _pIItem		= *it;
		if(_pIItem==current_item)
		{
			b = true;
			continue;
		}
		if(_pIItem->BaseSlot()==m_iActiveSlot)
			new_item = _pIItem;

		if(b && new_item)
			break;
	}

	if(new_item==NULL)
		return; //only 1 item for this slot

	bool res = Ruck						(current_item);
	R_ASSERT							(res);
	NET_Packet							P;
	current_item->object().u_EventGen	(P, GEG_PLAYER_ITEM2RUCK, current_item->object().H_Parent()->ID());
	P.w_u16								(current_item->object().ID());
	current_item->object().u_EventSend	(P);

	res = Slot							(m_iActiveSlot, new_item);
	R_ASSERT							(res);
	new_item->object().u_EventGen		(P, GEG_PLAYER_ITEM2SLOT, new_item->object().H_Parent()->ID());
	P.w_u16								(new_item->object().ID());
	P.w_u16								((u16)m_iActiveSlot);
	new_item->object().u_EventSend		(P);

	//activate
	new_item->object().u_EventGen		(P, GEG_PLAYER_ACTIVATE_SLOT, new_item->object().H_Parent()->ID());
	P.w_u16								((u16)m_iActiveSlot);
	new_item->object().u_EventSend		(P);
}

#pragma todo("Cannot use this function for left hand slots (detector), need to properly implement 2 active slots")
bool CInventory::Activate(TSlotId slot, EActivationReason reason, bool bForce)
{
	//Msg("CInventory::Activate, slot = %d, force: %d, reason: %d, activeSlot: %d, nextSlot: %d", slot, bForce, reason, m_iActiveSlot, m_iNextActiveSlot);
	if (slot == DETECTOR_SLOT || slot == ANOM_DET_SLOT || slot == HELMET_SLOT || slot == PNV_SLOT)
	{
		//Msg("Activate: Wrong slot");
		return false;
	}

	bool res = TryActivate(slot, reason, bForce);
	if (res)
		m_ActivationSlotReason	= reason;

	return res;
}

bool CInventory::TryActivate(TSlotId slot, EActivationReason reason, bool bForce)
{
	R_ASSERT2(slot == NO_ACTIVE_SLOT || slot < m_slots.size(), "wrong slot number");

	CInventoryItem* tmp_item = nullptr;
	CHudItem* tmp_hud_item = nullptr;
	if (slot != NO_ACTIVE_SLOT)
	{
		tmp_item = ItemFromSlot(slot);
		if (tmp_item)
			tmp_hud_item = tmp_item->cast_hud_item();
	}

	// Åñëè â ðóêå íàõîäèòñÿ äåòåêòîð è îðöæèå íå ñîâìåñòèìî ñ íèì, òî îòëîæèòü äîñòàâàíèå îðóæèÿ, ñïðÿòàòü äåòåêòîð, à ïîòîì äîñòàòü îðóæèå
	if (slot != NO_ACTIVE_SLOT && slot <= LAST_SLOT)
	{
		if (m_currentDetectorInHand && tmp_item && !tmp_item->IsSingleHand())
		{
			m_currentDetectorInHand->HideDetector(false);
		}
	}

	//Msg("TryActivate, slot: %d, force: %d, currentSlot: %d", slot, bForce, m_iActiveSlot);

	if (m_ActivationSlotReason == eKeyAction	&& reason == eImportUpdate)
	{
		//Msg("Activate: Wrong reason");
		return false;
	}

	if(Device.dwFrame == m_iLoadActiveSlotFrame)
	{
		if ((m_iLoadActiveSlot == slot) && tmp_item)
		{
			m_iLoadActiveSlotFrame = u32(-1);
		}
		else
		{
			//Msg("Activate: loadACtiveslotframe, res = false");
			return false;
		}
	}

	if ((slot!=NO_ACTIVE_SLOT && m_slots[slot].IsBlocked()) && !bForce)
	{
		//Msg("Activate: Slot is blocked");
		return false;
	}

	if (slot != NO_ACTIVE_SLOT && !m_slots[slot].m_bVisible)
	{
		//Msg("Activate: Slot not visible");
		return false;
	}

	/*
	if (GetActiveSlot() == slot || (GetNextActiveSlot()==slot && tmp_hud_item && tmp_hud_item->IsHiding() && !bForce))
	{
		Msg("Activate: the same slot so set next.");
		m_iNextActiveSlot = slot;
		return true;
	}*/

	/*
	if (m_iActiveSlot == slot ||
		(m_iNextActiveSlot == slot &&
		 m_iActiveSlot != NO_ACTIVE_SLOT &&
		 m_slots[m_iActiveSlot].m_pIItem &&
		 (m_slots[m_iActiveSlot].m_pIItem->cast_hud_item() && m_slots[m_iActiveSlot].m_pIItem->cast_hud_item()->IsHiding())
		 )
	   )
	{
		Msg("Activate: Item is hiding or same slot");
		return false;
	}
	*/

	//àêòèâíûé ñëîò íå âûáðàí
	if(m_iActiveSlot == NO_ACTIVE_SLOT)
	{
		if(tmp_item)
		{
			m_iNextActiveSlot		= slot;
			m_ActivationSlotReason	= reason;
			//Msg("Activate: No active slot but have item");
			return true;
		}
		else
		{
			PIItem active_item = ActiveItem();
			if (slot==GRENADE_SLOT)//fake for grenade
			{
				PIItem gr = SameSlot(GRENADE_SLOT, NULL, true);
				if (gr)
				{
					Slot(slot, gr);
					//Msg("Activate: Grenade to slot");
				}
			}
			//Msg("Activate: No item");
		}
	}
	//àêòèâíûé ñëîò çàäåéñòâîâàí
	else if(slot == NO_ACTIVE_SLOT || tmp_item)
	{
		PIItem active_item = ActiveItem();

		if (active_item && !bForce)
		{
			CHudItem* tempItem = active_item->cast_hud_item();
			R_ASSERT2(tempItem, active_item->object().cNameSect().c_str());

			if (GetActiveSlot() == slot)
			{
				//Msg("Activate: Activate item in same slot");
				tempItem->Activate();
			}
			else if (!tempItem->IsHiding())
			{
				//Msg("Activate: SendDeactivateItem");
				tempItem->SendDeactivateItem();
			}
		} else //in case where weapon is going to destroy
		{
			if (tmp_hud_item)
			{
				//Msg("call HudItem activate, slot: %d", slot);
				tmp_hud_item->Activate();
			}

			m_iActiveSlot = slot;
		}

		m_iNextActiveSlot		= slot;
		m_ActivationSlotReason	= reason;

		//Msg("Activate: Deactivated slot");
		return true;
	}

	return false;
}

PIItem CInventory::ItemFromSlot(TSlotId slot) const
{
	VERIFY(NO_ACTIVE_SLOT != slot);
	return m_slots[slot].m_pIItem;
}

void CInventory::SendActionEvent(u16 cmd, u32 flags)
{
	CActor *pActor = smart_cast<CActor*>(m_pOwner);
	if (!pActor) return;

	NET_Packet		P;
	pActor->u_EventGen		(P,GE_INV_ACTION, pActor->ID());
	P.w_s32					(cmd);
	P.w_u32					(flags);
	P.w_s32					(pActor->GetZoomRndSeed());
	P.w_s32					(pActor->GetShotRndSeed());
	pActor->u_EventSend		(P, net_flags(TRUE, TRUE, FALSE, TRUE));
};

bool CInventory::Action(u16 cmd, u32 flags)
{
	CActor *pActor = smart_cast<CActor*>(m_pOwner);

	if (pActor)
	{
		switch(cmd)
		{
			case kWPN_FIRE:
			{
				pActor->SetShotRndSeed();
			}break;
			case kWPN_ZOOM :
			{
				pActor->SetZoomRndSeed();
			}break;
		};
	};

	if (g_pGameLevel && OnClient() && pActor) {
		switch(cmd)
		{
		case kUSE:
			{
			}break;

		case kDROP:

			{
				SendActionEvent(cmd, flags);
				return true;
			}break;

		case kWPN_NEXT:
		case kWPN_RELOAD:
		case kWPN_FIRE:
		case kWPN_FUNC:
		case kWPN_FIREMODE_NEXT:
		case kWPN_FIREMODE_PREV:
		case kWPN_ZOOM :
		case kTORCH:
		case kNIGHT_VISION:

			{
				SendActionEvent(cmd, flags);
			}break;
		}
	}


	if (m_iActiveSlot < m_slots.size() &&
			m_slots[m_iActiveSlot].m_pIItem &&
			m_slots[m_iActiveSlot].m_pIItem->Action(cmd, flags))
											return true;
	bool b_send_event = false;
	switch(cmd)
	{
	case kWPN_1:
	case kWPN_2:
	case kWPN_3:
	case kWPN_3b:
	case kWPN_4:
	case kWPN_5:
	case kWPN_6:
       {
			if (cmd == kWPN_6 && !IsGameTypeSingle()) return false;
			if (flags&CMD_START && !m_bHandsOnly)
			{
				auto desiredSlot = GetSlotByKey(cmd);
				if ((int)m_iActiveSlot == desiredSlot && m_slots[m_iActiveSlot].m_pIItem)
				{
					if (IsGameTypeSingle()){
						b_send_event = Activate(NO_ACTIVE_SLOT);
					}
					else
					{
						ActivateNextItemInActiveSlot();
					}
				} else {
					if ((int)m_iActiveSlot == desiredSlot && !IsGameTypeSingle())
					{
						break;
					}
					else
					{
						b_send_event = Activate(desiredSlot, eKeyAction);
					}
				}
			}
		}break;
	case kARTEFACT:
		{
			if(flags&CMD_START)
			{
                if((int)m_iActiveSlot == ARTEFACT_SLOT &&
					m_slots[m_iActiveSlot].m_pIItem && IsGameTypeSingle())
				{
					b_send_event = Activate(NO_ACTIVE_SLOT);

				}else {
					b_send_event = Activate(ARTEFACT_SLOT);

				}
			}
		}break;
	case kDETECTOR:
		if(flags&CMD_START)
		{
			auto& slot = m_slots[DETECTOR_SLOT];
			if (slot.m_pIItem)
			{
				CCustomDetectorR* det = smart_cast<CCustomDetectorR*>(slot.m_pIItem);
				if (slot.CanBeActivated())
				{
					det->ToggleDetector(false);
				}
				else
				{
					det->HideDetector(true);
				}
			}
		}

		break;
	}

	if(b_send_event && g_pGameLevel && OnClient() && pActor)
			SendActionEvent(cmd, flags);

	return false;
}

TSlotId CInventory::GetSlotByKey(u16 cmd)
{
	switch (cmd)
	{
	case kWPN_1: return KNIFE_SLOT;
	case kWPN_2: return PISTOL_SLOT;
	case kWPN_3: return RIFLE_SLOT;
	case kWPN_3b:return RIFLE_2_SLOT;
	case kWPN_4: return GRENADE_SLOT;
	case kWPN_5: return APPARATUS_SLOT;
	case kWPN_6: return BOLT_SLOT;
	case kARTEFACT: return ARTEFACT_SLOT;
	case kDETECTOR: return DETECTOR_SLOT;
	default:	 return NO_ACTIVE_SLOT;
	}
}


void CInventory::Update()
{
	if (Level().CurrentViewEntity()->CLS_ID == CLSID_OBJECT_ACTOR)
	{
//		bool bActiveSlotVisible;
//		//tatarinrafa: Lox fix for potentialy null variable being checked for its properties. 
//		if (m_iActiveSlot == NO_ACTIVE_SLOT ||		// maybe no slot at all
//			!m_slots[m_iActiveSlot].m_pIItem ||		// maybe no item in it
//			(m_slots[m_iActiveSlot].m_pIItem && (!m_slots[m_iActiveSlot].m_pIItem->cast_hud_item()/* item is in slot, but maybe it has no hud-item */ || m_slots[m_iActiveSlot].m_pIItem->cast_hud_item() && (m_slots[m_iActiveSlot].m_pIItem->cast_hud_item()->IsHidden()/* item is in slot, it has hud item, may be it is hidden*/))))
//			{
//				bActiveSlotVisible = false;
//			}
//			else
//			{
//				bActiveSlotVisible = true;
//			}
//		}

		// ^above is to much for being checked each frame, lets just find the only way bActiveSlotVisible becomes true

		bool bActiveSlotVisible = false;

		if (m_iActiveSlot != NO_ACTIVE_SLOT && m_slots[m_iActiveSlot].m_pIItem && m_slots[m_iActiveSlot].m_pIItem->cast_hud_item() && !m_slots[m_iActiveSlot].m_pIItem->cast_hud_item()->IsHidden())
		{
			bActiveSlotVisible = true;
		}
		bool bDetectorHiding = m_currentDetectorInHand && m_currentDetectorInHand->IsHiding();
		if (m_iNextActiveSlot != m_iActiveSlot && !bActiveSlotVisible && !bDetectorHiding)
		{
			if (m_iNextActiveSlot != NO_ACTIVE_SLOT &&
				m_slots[m_iNextActiveSlot].m_pIItem &&
				m_slots[m_iNextActiveSlot].m_pIItem->cast_hud_item()){
				m_slots[m_iNextActiveSlot].m_pIItem->cast_hud_item()->Activate();
			}
			m_iActiveSlot = m_iNextActiveSlot;
		}
		//if (m_iNextActiveSlot != NO_ACTIVE_SLOT) && ActiveItem() && ActiveItem()->cast_hud_item()->IsHidden())
		//	ActiveItem()->cast_hud_item()->Activate();
	}

	UpdateDropTasks	();
}

void CInventory::UpdateDropTasks()
{
	for(TSlotId i=FirstSlot(); i<=LastSlot(); ++i)	
	{
		PIItem itm = ItemFromSlot(i);
		if(itm)
			UpdateDropItem		(itm);
	}

	for(TSlotId i = 0; i < 2; ++i)
	{
		TIItemContainer &list			= i?m_ruck:m_belt;
		TIItemContainer::iterator it	= list.begin();
		TIItemContainer::iterator it_e	= list.end();

		for( ;it!=it_e; ++it)
		{
			UpdateDropItem		(*it);
		}
	}

	if (m_drop_last_frame)
	{
		m_drop_last_frame			= false;
		m_pOwner->OnItemDropUpdate	();
	}
}

void CInventory::UpdateDropItem(PIItem pIItem)
{
	if(pIItem && pIItem->GetDropManual() )
	{
		pIItem->SetDropManual(FALSE);
		if ( OnServer() )
		{
			NET_Packet					P;
			pIItem->object().u_EventGen	(P, GE_OWNERSHIP_REJECT, pIItem->object().H_Parent()->ID());
			P.w_u16						(u16(pIItem->object().ID()));
			pIItem->object().u_EventSend(P);
		}
	}// dropManual
	if ( pIItem->GetDeleteManual() ) {
		pIItem->SetDeleteManual(FALSE);
		if ( OnServer() )
		{
			NET_Packet					P;
			pIItem->object().u_EventGen	(P, GE_DESTROY, u16(pIItem->object().ID()));
			pIItem->object().u_EventSend(P);
		}
	}
}

//èùåì íà ïîÿñå ãðàíàòó òàêîæå òèïà
PIItem CInventory::Same(const PIItem pIItem, bool bSearchRuck) const
{
	const TIItemContainer &list = bSearchRuck ? m_ruck : m_belt;

	for(TIItemContainer::const_iterator it = list.begin(); list.end() != it; ++it)
	{
		const PIItem l_pIItem = *it;

		if((l_pIItem != pIItem) &&
				!xr_strcmp(l_pIItem->object().cNameSect(),
				pIItem->object().cNameSect()))
			return l_pIItem;
	}
	return NULL;
}

//èùåì íà ïîÿñå âåùü äëÿ ñëîòà

PIItem CInventory::SameSlot(const TSlotId slot, PIItem pIItem, bool bSearchRuck) const
{
	if(slot == NO_ACTIVE_SLOT) 	return NULL;

	const TIItemContainer &list = bSearchRuck ? m_ruck : m_belt;

	for(TIItemContainer::const_iterator it = list.begin(); list.end() != it; ++it)
	{
		PIItem _pIItem = *it;
		if(_pIItem != pIItem && _pIItem->BaseSlot() == slot) return _pIItem;
	}

	return NULL;
}

static bool GetItemPredicate(PIItem item, const char* name)
{
	return item != nullptr
		&& !xr_strcmp(item->object().cNameSect(), name)
		&& item->Useful();
}

//íàéòè â èíâåíòîðå âåùü ñ óêàçàííûì èìåíåì
PIItem CInventory::Get(const char *name, bool bSearchRuck) const
{
	const TIItemContainer &list = bSearchRuck ? m_ruck : m_belt;
	for (auto it = list.cbegin(); list.cend() != it; ++it)
	{
		if (GetItemPredicate(*it, name)) return *it;
	}
	return NULL;
}

PIItem CInventory::Get(CLASS_ID cls_id, bool bSearchRuck) const
{
	const TIItemContainer &list = bSearchRuck ? m_ruck : m_belt;

	for(TIItemContainer::const_iterator it = list.begin(); list.end() != it; ++it)
	{
		PIItem pIItem = *it;
		if(pIItem && pIItem->object().CLS_ID == cls_id &&
								pIItem->Useful())
				return pIItem;
	}
	return NULL;
}

PIItem CInventory::Get(const u16 id, bool bSearchRuck) const
{
	const TIItemContainer &list = bSearchRuck ? m_ruck : m_belt;

	for(TIItemContainer::const_iterator it = list.begin(); list.end() != it; ++it)
	{
		PIItem pIItem = *it;
		if(pIItem && pIItem->object().ID() == id)
			return pIItem;
	}
	return NULL;
}

PIItem CInventory::GetAmmo(const char *name, bool bSearchRuck) const
{
	const TIItemContainer &list = bSearchRuck ? m_ruck : m_belt;

	for (auto it = list.crbegin(); list.crend() != it; ++it)
	{
		PIItem pIItem = *it;
		if (GetItemPredicate(pIItem, name))
		{
			//auto ammo = smart_cast<CWeaponAmmo*>(pIItem);
			// if (ammo && ammo->m_boxCurr < ammo->m_boxSize)
			return pIItem;
		}
	}
	return NULL;
}

//search both (ruck and belt)
PIItem CInventory::GetAny(const char *name) const
{
	PIItem itm = GetAmmo(name, false);
	//äëÿ ÃÃ èùåì òîëüêî íà ïîÿñå
	if (!itm && (this != &g_actor->inventory()))
	{
		itm = GetAmmo(name, true);
	}
	return itm;
}

PIItem CInventory::item(CLASS_ID cls_id) const
{
	const TIItemContainer &list = m_all;

	for(TIItemContainer::const_iterator it = list.begin(); list.end() != it; ++it)
	{
		PIItem pIItem = *it;
		if(pIItem && pIItem->object().CLS_ID == cls_id &&
			pIItem->Useful())
			return pIItem;
	}
	return NULL;
}

float CInventory::TotalWeight() const
{
	VERIFY(m_fTotalWeight>=0.f);
	return m_fTotalWeight;
}


float CInventory::CalcTotalWeight()
{
	float weight = 0;
	for(TIItemContainer::const_iterator it = m_all.begin(); m_all.end() != it; ++it)
		weight += (*it)->Weight();

	m_fTotalWeight = weight;
	return m_fTotalWeight;
}


u32 CInventory::dwfGetSameItemCount(LPCSTR caSection, bool SearchAll)
{
	u32			l_dwCount = 0;
	TIItemContainer	&l_list = SearchAll ? m_all : m_ruck;
	for(TIItemContainer::iterator l_it = l_list.begin(); l_list.end() != l_it; ++l_it)
	{
		PIItem	l_pIItem = *l_it;
		if (l_pIItem && !xr_strcmp(l_pIItem->object().cNameSect(), caSection))
            ++l_dwCount;
	}

	return		(l_dwCount);
}
u32		CInventory::dwfGetGrenadeCount(LPCSTR caSection, bool SearchAll)
{
	u32			l_dwCount = 0;
	TIItemContainer	&l_list = SearchAll ? m_all : m_ruck;
	for(TIItemContainer::iterator l_it = l_list.begin(); l_list.end() != l_it; ++l_it)
	{
		PIItem	l_pIItem = *l_it;
		if (l_pIItem && l_pIItem->object().CLS_ID == CLSID_GRENADE_F1 || l_pIItem->object().CLS_ID == CLSID_GRENADE_RGD5)
			++l_dwCount;
	}

	return		(l_dwCount);
}

bool CInventory::bfCheckForObject(ALife::_OBJECT_ID tObjectID)
{
	TIItemContainer	&l_list = m_all;
	for(TIItemContainer::iterator l_it = l_list.begin(); l_list.end() != l_it; ++l_it)
	{
		PIItem	l_pIItem = *l_it;
		if (l_pIItem && l_pIItem->object().ID() == tObjectID)
			return(true);
	}
	return		(false);
}

CInventoryItem *CInventory::get_object_by_id(ALife::_OBJECT_ID tObjectID)
{
	TIItemContainer	&l_list = m_all;
	for(TIItemContainer::iterator l_it = l_list.begin(); l_list.end() != l_it; ++l_it)
	{
		PIItem	l_pIItem = *l_it;
		if (l_pIItem && l_pIItem->object().ID() == tObjectID)
			return	(l_pIItem);
	}
	return		(0);
}

//ñêóøàòü ïðåäìåò
bool CInventory::Eat(PIItem pIItem)
{
	R_ASSERT(pIItem->m_pCurrentInventory==this);
	//óñòàíàîâèòü ñúåäîáíà ëè âåùü
	CEatableItem* pItemToEat = smart_cast<CEatableItem*>(pIItem);
	R_ASSERT				(pItemToEat);

	CEntityAlive *entity_alive = smart_cast<CEntityAlive*>(m_pOwner);
	R_ASSERT				(entity_alive);

	bool used = pItemToEat->UseBy (entity_alive);

	if (!used) return false;

	if(IsGameTypeSingle() && Actor()->m_inventory == this)
		Actor()->callback(GameObject::eUseObject)((smart_cast<CGameObject*>(pIItem))->lua_game_object());

	if(pItemToEat->Empty() && entity_alive->Local())
	{
		NET_Packet					P;
		CGameObject::u_EventGen		(P,GE_OWNERSHIP_REJECT,entity_alive->ID());
		P.w_u16						(pIItem->object().ID());
		P.w_u8						(1); // send just_before_destroy flag, so physical shell does not activates and disrupts nearby objects
		CGameObject::u_EventSend	(P);

		CGameObject::u_EventGen		(P,GE_DESTROY,pIItem->object().ID());
		CGameObject::u_EventSend	(P);
	}
	return			true;
}

bool CInventory::InSlot(PIItem pIItem) const
{
	if(pIItem->CurrPlace() != eItemPlaceSlot)	return false;

	VERIFY(m_slots[pIItem->CurrSlot()].m_pIItem == pIItem);

	return true;
}
bool CInventory::InBelt(PIItem pIItem) const
{
	if(Get(pIItem->object().ID(), false)) return true;
	return false;
}
bool CInventory::InRuck(PIItem pIItem) const
{
	if( Get(pIItem->object().ID(), true ) ) return true;
	return false; 
}

bool CInventory::CanPutInSlot(PIItem pIItem, TSlotId slot_id) const
{
	if(!m_bSlotsUseful) return false;

	if( !GetOwner()->CanPutInSlot(pIItem, slot_id) ) return false;

	if(slot_id!=NO_ACTIVE_SLOT && 
		NULL==ItemFromSlot(slot_id) )
		return true;

	return false;
}
//ïðîâåðÿåò ìîæåì ëè ïîìåñòèòü âåùü íà ïîÿñ,
//ïðè ýòîì ðåàëüíî íè÷åãî íå ìåíÿåòñÿ
bool CInventory::CanPutInBelt(PIItem pIItem, bool forceRoomCheck)
{
	if(!pIItem)					              return false;
	if(InBelt(pIItem))					      return false;
	if(!m_bBeltUseful)					      return false;
	if(!pIItem->Belt())		            return false;
	bool needRoom = (forceRoomCheck || smart_cast<CWeaponAmmo*>(pIItem) == nullptr);
	if(needRoom && m_belt.size() == BeltWidth())	return false;

	return !needRoom || FreeRoom_inBelt(m_belt, pIItem, BeltWidth(), 1);
}
//ïðîâåðÿåò ìîæåì ëè ïîìåñòèòü âåùü â ðþêçàê,
//ïðè ýòîì ðåàëüíî íè÷åãî íå ìåíÿåòñÿ
bool CInventory::CanPutInRuck(PIItem pIItem) const
{
	if(InRuck(pIItem)) return false;
	return true;
}

u32	CInventory::dwfGetObjectCount()
{
	return		(m_all.size());
}

CInventoryItem	*CInventory::tpfGetObjectByIndex(int iIndex)
{
	if ((iIndex >= 0) && (iIndex < (int)m_all.size())) {
		TIItemContainer	&l_list = m_all;
		int			i = 0;
		for(TIItemContainer::iterator l_it = l_list.begin(); l_list.end() != l_it; ++l_it, ++i)
			if (i == iIndex)
                return	(*l_it);
	}
	else {
		ai().script_engine().script_log	(ScriptStorage::eLuaMessageTypeError,"invalid inventory index!");
		return	(0);
	}
	R_ASSERT	(false);
	return		(0);
}

CInventoryItem	*CInventory::tpfGetBeltObjectById(int item_id)
{
	TIItemContainer	&list = m_belt;
	TIItemContainer::iterator it = std::find_if(list.begin(), list.end(), SBeltItemPred(item_id));
	return (it != list.end()) ? *it : NULL;
}

CInventoryItem	*CInventory::tpfGetBeltObjectByIndex(int iIndex)
{
	if ((iIndex >= 0) && (iIndex < (int)m_belt.size())) {
		TIItemContainer	&l_list = m_belt;
		int			i = 0;
		for(TIItemContainer::iterator l_it = l_list.begin(); l_list.end() != l_it; ++l_it, ++i)
			if (i == iIndex)
                return	(*l_it);
	}
	else {
		ai().script_engine().script_log	(ScriptStorage::eLuaMessageTypeError,"invalid belt index!");
		return	(0);
	}
	R_ASSERT	(false);
	return		(0);
}

CInventoryItem	*CInventory::GetItemFromInventory(LPCSTR caItemName)
{
	TIItemContainer	&l_list = m_all;

	u32 crc = crc32(caItemName, xr_strlen(caItemName));

	for(TIItemContainer::iterator l_it = l_list.begin(); l_list.end() != l_it; ++l_it)
		if ((*l_it)->object().cNameSect()._get()->dwCRC == crc){
			VERIFY(	0 == xr_strcmp( (*l_it)->object().cNameSect().c_str(), caItemName)  );
			return	(*l_it);
		}
	return	(0);
}


bool CInventory::CanTakeItem(CInventoryItem* inventory_item) const
{
	if (inventory_item->object().getDestroy()) return false;

	if (!inventory_item->CanTake()) return false;
	TIItemContainer::const_iterator it = m_all.begin();
	for (it = m_all.begin(); it != m_all.end(); it++)
		if ((*it)->object().ID() == inventory_item->object().ID()) break;
	VERIFY3(it == m_all.end(), "item already exists in inventory", *inventory_item->object().cName());

	CActor* pActor = smart_cast<CActor*>(m_pOwner);
	//актер всегда может взять вещь
	if (!pActor && (TotalWeight() + inventory_item->Weight() > m_pOwner->MaxCarryWeight()))
		return	false;

	return	true;
}


u32  CInventory::BeltWidth() const
{
	return m_iMaxBelt;
}

void  CInventory::AddAvailableItems(TIItemContainer& items_container, bool for_trade) const
{
	for(TIItemContainer::const_iterator it = m_ruck.begin(); m_ruck.end() != it; ++it)
	{
		PIItem pIItem = *it;
		if(!for_trade || pIItem->CanTrade())
			items_container.push_back(pIItem);
	}

	if(m_bBeltUseful)
	{
		for(TIItemContainer::const_iterator it = m_belt.begin(); m_belt.end() != it; ++it)
		{
			PIItem pIItem = *it;
			if(!for_trade || pIItem->CanTrade())
				items_container.push_back(pIItem);
		}
	}

	CAI_Stalker* pOwner = smart_cast<CAI_Stalker*>(m_pOwner);
	if (pOwner && !pOwner->g_Alive())
	{
		TISlotArr::const_iterator slot_it			= m_slots.begin();
		TISlotArr::const_iterator slot_it_e			= m_slots.end();
		for(;slot_it!=slot_it_e;++slot_it)
		{
			const CInventorySlot& S = *slot_it;
			if(S.m_pIItem && S.m_pIItem->BaseSlot()!=BOLT_SLOT)
				items_container.push_back(S.m_pIItem);
		}

	} else if (m_bSlotsUseful) {
		TISlotArr::const_iterator slot_it			= m_slots.begin();
		TISlotArr::const_iterator slot_it_e			= m_slots.end();
		for(;slot_it!=slot_it_e;++slot_it)
		{
			const CInventorySlot& S = *slot_it;
			if (S.m_pIItem && (!for_trade || S.m_pIItem->CanTrade()))
			{
				if(!S.m_bPersistent || S.m_pIItem->BaseSlot()==GRENADE_SLOT )
				{
					if (pOwner) {
						u32 slot = S.m_pIItem->BaseSlot();
						if (slot != PISTOL_SLOT && slot != RIFLE_SLOT && slot != RIFLE_2_SLOT)
							items_container.push_back(S.m_pIItem);
					} else {
						items_container.push_back(S.m_pIItem);
					}
				}
			}
		}
	}
}


void  CInventory::AddAvailableItems(TIItemContainer& items_container, SInventorySelectorPredicate& pred) const
{
	for (TIItemContainer::const_iterator it = m_ruck.begin(); m_ruck.end() != it; ++it)
	{
		PIItem pIItem = *it;
		if (pred(pIItem))
			items_container.push_back(pIItem);
	}

	if (m_bBeltUseful)
	{
		for (TIItemContainer::const_iterator it = m_belt.begin(); m_belt.end() != it; ++it)
		{
			PIItem pIItem = *it;
			if (pred(pIItem))
				items_container.push_back(pIItem);
		}
	}

	CAI_Stalker* pOwner = smart_cast<CAI_Stalker*>(m_pOwner);
	if (pOwner && !pOwner->g_Alive())
	{
		TISlotArr::const_iterator slot_it = m_slots.begin();
		TISlotArr::const_iterator slot_it_e = m_slots.end();
		for (; slot_it != slot_it_e; ++slot_it)
		{
			const CInventorySlot& S = *slot_it;
			if (S.m_pIItem && S.m_pIItem->BaseSlot() != BOLT_SLOT)
				items_container.push_back(S.m_pIItem);
		}

	}
	else if (m_bSlotsUseful) {
		TISlotArr::const_iterator slot_it = m_slots.begin();
		TISlotArr::const_iterator slot_it_e = m_slots.end();
		for (; slot_it != slot_it_e; ++slot_it)
		{
			const CInventorySlot& S = *slot_it;
			if (S.m_pIItem && pred(S.m_pIItem))
			{
				if (!S.m_bPersistent || S.m_pIItem->BaseSlot() == GRENADE_SLOT)
				{
					if (pOwner) {
						u32 slot = S.m_pIItem->BaseSlot();
						if (slot != PISTOL_SLOT && slot != RIFLE_SLOT && slot != RIFLE_2_SLOT)
							items_container.push_back(S.m_pIItem);
					}
					else {
						items_container.push_back(S.m_pIItem);
					}
				}
			}
		}
	}
}

bool CInventory::isBeautifulForActiveSlot	(CInventoryItem *pIItem)
{
	if (!IsGameTypeSingle()) return (true);
	TISlotArr::iterator it =  m_slots.begin();
	for( ; it!=m_slots.end(); ++it) {
		if ((*it).m_pIItem && (*it).m_pIItem->IsNecessaryItem(pIItem))
			return		(true);
	}
	return				(false);
}

void CInventory::Items_SetCurrentEntityHud(bool current_entity)
{
	TIItemContainer::iterator it;
	for(it = m_all.begin(); m_all.end() != it; ++it)
	{
		CWeapon* pWeapon = smart_cast<CWeapon*>(*it);
		if (pWeapon)
		{
			pWeapon->InitAddons();
			pWeapon->UpdateAddonsVisibility();
		}
	}
};
//call this only via Actor()->SetWeaponHideState()
void CInventory::SetSlotsBlocked(u16 mask, bool bBlock)
{
	bool bChanged = false;
	for(int i = FirstSlot(); i <= LastSlot(); ++i)
	{
		if(mask & (1<<i))
		{
			bool bCanBeActivated = m_slots[i].CanBeActivated();
			if(bBlock){
				++m_slots[i].m_blockCounter;
				if (m_slots[i].m_blockCounter > 5) m_slots[i].m_blockCounter = 1;
				VERIFY2(m_slots[i].m_blockCounter< 5,"block slots overflow");
			}else{
				--m_slots[i].m_blockCounter;
				if (m_slots[i].m_blockCounter < -5) m_slots[i].m_blockCounter = -1;
				VERIFY2(m_slots[i].m_blockCounter>-5,"block slots underflow");
			}
			if(bCanBeActivated != m_slots[i].CanBeActivated())
				bChanged = true;
		}
	}
	if (bChanged)
	{
		/*Msg("Slots blocked changed. Knife: %d, Pistol: %d, Rifle: %d, Grenade: %d, Detector: %d",
			m_slots[KNIFE_SLOT].m_blockCounter,
			m_slots[PISTOL_SLOT].m_blockCounter,
			m_slots[RIFLE_SLOT].m_blockCounter,
			m_slots[GRENADE_SLOT].m_blockCounter,
			m_slots[DETECTOR_SLOT].m_blockCounter);*/

		auto ActiveSlot		= GetActiveSlot();
		auto PrevActiveSlot	= GetPrevActiveSlot();

		if(ActiveSlot==NO_ACTIVE_SLOT)
		{//try to restore hidden weapon
			if(PrevActiveSlot!=NO_ACTIVE_SLOT && m_slots[PrevActiveSlot].CanBeActivated())
				if(Activate(PrevActiveSlot))
					SetPrevActiveSlot(NO_ACTIVE_SLOT);
		}else
		{//try to hide active weapon
			if(!m_slots[ActiveSlot].CanBeActivated())
			{
				if(Activate(NO_ACTIVE_SLOT))
					SetPrevActiveSlot(ActiveSlot);
			}
		}
		if (m_currentDetectorInHand != nullptr && !m_slots[DETECTOR_SLOT].CanBeActivated())
		{
			m_currentDetectorInHand->HideDetector(false, true);
		}
	}
}

bool CInventory::AreSlotsBlocked()
{
	for (int i = FirstSlot(); i <= LastSlot(); ++i)
	{
		if (!m_slots[i].IsBlocked())
			return false;
	}
	return true;
}

bool CInventory::SBeltItemPred::operator ()(PIItem &item)
{
	return item->object().ID() == m_id;
}
