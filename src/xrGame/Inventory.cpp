#include "StdAfx.h"
#include "pch_script.h"
#include "Inventory.h"
#include "InventoryWeaponSlotLayout.h"
#include "ui/UIActorMenu.h"
#include "ui/UIInventoryUtilities.h"
#include "UIGameCustom.h"
#include "clsid_game.h"
#include "WeaponMagazined.h"
#include "Grenade.h"
#include "RepackerInterface.h"
#include "Actor.h"
#include "InventoryVolumeSystem.h"
#include "../xrScripts/script_callback_ex.h"
#include "ui/UICarBodyWnd.h"

using namespace InventoryUtilities;

// what to block
u16	INV_STATE_LADDER = INV_STATE_BLOCK_ALL;
u16	INV_STATE_BLOCK_ALL = 0xffff;
u16	INV_STATE_INV_WND = INV_STATE_BLOCK_ALL;
u16	INV_STATE_BUY_MENU = INV_STATE_BLOCK_ALL;

// REMINDER: defaultSlot* index is slot_id - 1 for slot_id in [KNIFE_SLOT .. LAST_SLOT] (see inventory_space.h).
bool defaultSlotActive[] =
{
	true,		// 1 knife
	true,		// 2 INV_SLOT_2
	true,		// 3 INV_SLOT_3
	true,		// 4 grenade
	true,		// 5 binocular
	true,		// 6 bolt
	false,		// 7 outfit
	false,		// 8 pda
	false,		// 9 detector
	false,		// 10 torch
	true,		// 11 artefact
	false,		// 12 helmet
	false,		// 13 backpack
	false,		// 14 power_bank
	false,		// 15 nvg
	false,		// 16 PISTOL_SLOT_NEW
	false,		// 17 CUSTOM_SLOT_1
	false,		// 18 CUSTOM_SLOT_2
	false,		// 19 CUSTOM_SLOT_3
	false,		// 20 CUSTOM_SLOT_4
	false,		// 21 CUSTOM_SLOT_5
	false,		// 22 CUSTOM_SLOT_6
	false,		// 23 CUSTOM_SLOT_7
	false,		// 24 CUSTOM_SLOT_8
	false,		// 25 CUSTOM_SLOT_9
	false,		// 26 CUSTOM_SLOT_10
};

bool defaultSlotPersistent[] =
{
	true,		// 1 knife
	false,		// 2 INV_SLOT_2
	false,		// 3 INV_SLOT_3
	true,		// 4 grenade
	true,		// 5 binocular
	true,		// 6 bolt
	false,		// 7 outfit
	true,		// 8 pda
	true,		// 9 detector
	true,		// 10 torch
	false,		// 11 artefact
	false,		// 12 helmet
	true,		// 13 backpack
	true,		// 14 power_bank
	true,		// 15 nvg
	true,		// 16 PISTOL_SLOT_NEW
	true,		// 17 CUSTOM_SLOT_1
	true,		// 18 CUSTOM_SLOT_2
	true,		// 19 CUSTOM_SLOT_3
	true,		// 20 CUSTOM_SLOT_4
	true,		// 21 CUSTOM_SLOT_5
	true,		// 22 CUSTOM_SLOT_6
	true,		// 23 CUSTOM_SLOT_7
	true,		// 24 CUSTOM_SLOT_8
	true,		// 25 CUSTOM_SLOT_9
	true,		// 26 CUSTOM_SLOT_10
};

static_assert(sizeof(defaultSlotActive) / sizeof(defaultSlotActive[0]) == LAST_SLOT, "defaultSlotActive size must match LAST_SLOT");
static_assert(sizeof(defaultSlotPersistent) / sizeof(defaultSlotPersistent[0]) == LAST_SLOT, "defaultSlotPersistent size must match LAST_SLOT");

CInventory::CInventory()
{
	m_fMaxWeight = pSettings->r_float("inventory", "max_weight");

	u16 k = KNIFE_SLOT;
	for (; k <= LAST_SLOT; k++)
	{
		string256 slot_persistent = {};
		string256 slot_active = {};
		xr_sprintf(slot_persistent, "%s%d", "slot_persistent_", k);
		xr_sprintf(slot_active, "%s%d", "slot_active_", k);
		m_last_slot = k;

		m_slots[k].m_bPersistent = pSettings->read_if_exists<bool>("inventory", slot_persistent, defaultSlotPersistent[k - 1]);
		m_slots[k].m_bAct = pSettings->read_if_exists<bool>("inventory", slot_active, defaultSlotActive[k - 1]);
	}

	m_blocked_slots.resize(k + 1);

	for (u16 i = 0; i <= k; ++i)
	{
		m_blocked_slots[i] = 0;
	}

	if (!pSettings->line_exist("inventory", "slot_active_1"))
	{
		m_iMaxBelt = pSettings->r_s32("inventory", "max_belt");
	}


	InitPriorityGroupsForQSwitch();
	LoadCallbackGlobals(m_isItemAvailableToTrade, m_onItemAvailableToTrade, "OnItemAvailableToTrade");
	LoadCallbackGlobals(m_isInventoryEat, m_onInventoryEat, "OnInventoryEat");
}

void CInventory::Clear()
{
	m_all.clear();
	m_ruck.clear();
	m_belt.clear();

	for (u16 i = FirstSlot(); i <= LastSlot(); i++)
	{
		m_slots[i].m_pIItem = nullptr;
	}

	m_pOwner = nullptr;

	CalcTotalWeight();
	InvalidateState();
}

void CInventory::ClearItemFromAllSlots(PIItem pIItem)
{
	if (pIItem == nullptr)
	{
		return;
	}

	for (u16 i = FirstSlot(); i <= LastSlot(); ++i)
	{
		if (m_slots[i].m_pIItem != pIItem)
		{
			continue;
		}

		if (GetActiveSlot() == i)
		{
			Activate(NO_ACTIVE_SLOT);
		}

		m_slots[i].m_pIItem = nullptr;
	}
}

void CInventory::RemoveItemFromRuckAndBelt(PIItem pIItem)
{
	if (pIItem == nullptr)
	{
		return;
	}

	TIItemContainer::iterator it = std::find(m_ruck.begin(), m_ruck.end(), pIItem);
	if (it != m_ruck.end())
	{
		m_ruck.erase(it);
	}

	it = std::find(m_belt.begin(), m_belt.end(), pIItem);
	if (it != m_belt.end())
	{
		m_belt.erase(it);
	}
}

void CInventory::ReconcileItemPlacement(PIItem pIItem)
{
	if (pIItem == nullptr)
	{
		return;
	}

	switch (pIItem->CurrPlace())
	{
	case eItemPlaceSlot:
	{
		const u16 slotId = pIItem->CurrSlot();
		RemoveItemFromRuckAndBelt(pIItem);

		for (u16 i = FirstSlot(); i <= LastSlot(); ++i)
		{
			if (i != slotId && m_slots[i].m_pIItem == pIItem)
			{
				m_slots[i].m_pIItem = nullptr;
			}
		}
		break;
	}
	case eItemPlaceRuck:
	{
		ClearItemFromAllSlots(pIItem);
		break;
	}
	case eItemPlaceBelt:
	{
		ClearItemFromAllSlots(pIItem);

		TIItemContainer::iterator it = std::find(m_ruck.begin(), m_ruck.end(), pIItem);
		if (it != m_ruck.end())
		{
			m_ruck.erase(it);
		}
		break;
	}
	default:
		break;
	}
}

void CInventory::TryRestorePersistentSlotItem(PIItem pIItem)
{
	if (pIItem == nullptr || pIItem->CurrPlace() != eItemPlaceRuck)
	{
		return;
	}

	const u16 baseSlot = pIItem->BaseSlot();
	if (baseSlot == NO_ACTIVE_SLOT || !SlotIsPersistent(baseSlot))
	{
		return;
	}

	if (ItemFromSlot(baseSlot) != nullptr)
	{
		return;
	}

	if (CanPutInSlot(pIItem, baseSlot))
	{
		Slot(baseSlot, pIItem, true, true);
	}
}

void CInventory::RepairItemPlacements()
{
	for (PIItem item : m_all)
	{
		if (item == nullptr || item->CurrPlace() == eItemPlaceUndefined)
		{
			continue;
		}

		ReconcileItemPlacement(item);
	}

	CalcTotalWeight();
	InvalidateState();
}

void CInventory::Take(CGameObject* pObj, bool bNotActivate, bool strict_placement, bool bForce)
{
	CInventoryItem* pIItem = pObj->cast_inventory_item();
	VERIFY(pIItem);
	VERIFY(pIItem->m_pInventory == nullptr);
	VERIFY(bForce || CanTakeItem(pIItem));

	pIItem->m_pInventory = this;
	pIItem->SetDropManual(false);
	pIItem->AllowTrade();

	//if net_Import for pObj arrived then the pObj will pushed to CrPr list (correction prediction)
	//usually net_Import arrived for objects that not has a parent object..
	//for unknown reason net_Import arrived for object that has a parent, so correction prediction schema will crash
	Level().RemoveObject_From_4CrPr(pObj);

	CObject* current_entity = Level().CurrentEntity();

	if (current_entity != nullptr)
	{
		auto actor_id = current_entity->ID();
		if (GetOwner()->object_id() == actor_id && this->m_pOwner->object_id() == actor_id) // actors inventory
		{
			CWeaponMagazined* pWeapon = pIItem->cast_weapon_magazined();
			if (pWeapon && pWeapon->strapped_mode())
			{
				pWeapon->strapped_mode(false);
				Ruck(pWeapon);
			}
		}
	}

	if (EngineExternal()[EEngineExternalSystem::EngineAmmoRepacker])
	{
		if (auto Repacker = pIItem->cast_repacker_interface())
		{
			for (auto& elem : m_all)
			{
				if (elem->m_section_id != pIItem->m_section_id)
				{
					continue;
				}
				if (!Repacker->Repack(elem))
				{
					return;
				}
			}
		}
	}

	m_all.push_back(pIItem);

	if (!strict_placement)
	{
		pIItem->m_ItemCurrPlace.type = eItemPlaceUndefined;
	}

	bool result = false;
	switch (pIItem->m_ItemCurrPlace.type)
	{
		case eItemPlaceBelt:
		{
			result = Belt(pIItem, strict_placement);
			if (!result)
				pIItem->m_ItemCurrPlace.type = eItemPlaceUndefined;
#ifdef DEBUG
			if (!result)
				Msg("cant put in belt item %s", *pIItem->object().cName());
#endif
	
			break;
		}
		case eItemPlaceRuck:
		{
			result = Ruck(pIItem, strict_placement);
			if (!result)
				pIItem->m_ItemCurrPlace.type = eItemPlaceUndefined;
#ifdef DEBUG
			if (!result)
				Msg("cant put in ruck item %s", *pIItem->object().cName());
#endif
	
			break;
		}
		case eItemPlaceSlot:
		{
			result = Slot(pIItem->m_ItemCurrPlace.slot_id, pIItem, bNotActivate, strict_placement);
			if (!result)
				pIItem->m_ItemCurrPlace.type = eItemPlaceUndefined;
#ifdef DEBUG
			if (!result)
				Msg("cant slot in slot item %s", *pIItem->object().cName());
#endif
			break;
		}
	};

	if (pIItem->CurrPlace() == eItemPlaceUndefined)
	{
		bool placedPreferred = false;
		if (pIItem->HasPreferredSlotAfterPickup())
		{
			if (InventoryHolsterPistolSlotActiveInSettings() &&
				pIItem->PreferredSlotAfterPickup() == PISTOL_SLOT_NEW &&
				ItemFromSlot(PISTOL_SLOT_NEW) == nullptr &&
				CanPutInSlot(pIItem, PISTOL_SLOT_NEW))
			{
				placedPreferred = Slot(PISTOL_SLOT_NEW, pIItem, bNotActivate, strict_placement);
				if (placedPreferred)
				{
					result = true;
				}
			}
		}
		if (!placedPreferred)
		{
			if (!result && !pIItem->RuckDefault())
			{
				u16 const equipSlot = InventoryResolveSidearmEquipSlot(pIItem);
				if (CanPutInSlot(pIItem, equipSlot))
				{
					result = Slot(equipSlot, pIItem, bNotActivate, strict_placement); VERIFY(result);
				}
				else if (CanPutInBelt(pIItem))
				{
					result = Belt(pIItem, strict_placement); VERIFY(result);
				}
				else
				{
					result = Ruck(pIItem, strict_placement); VERIFY(result);
				}
			}
			else if (!result)
			{
				result = Ruck(pIItem, strict_placement); VERIFY(result);
			}
		}
	}

	ReconcileItemPlacement(pIItem);
	TryRestorePersistentSlotItem(pIItem);

	m_pOwner->OnItemTake(pIItem);

	CalcTotalWeight();
	InvalidateState();

	pIItem->object().processing_deactivate();
	VERIFY(pIItem->CurrPlace() != eItemPlaceUndefined);

	if (CUIGameCustom* current_ui = CurrentGameUI())
	{
		CObject* pActor_owner = m_pOwner != nullptr ? m_pOwner->cast_game_object()->dcast_CObject() : nullptr;

		if (Level().CurrentViewEntity() == pActor_owner)
		{
			current_ui->OnInventoryAction(pIItem, GE_OWNERSHIP_TAKE);
		}
		else if (current_ui->GetCarbodyMenu() && current_ui->GetCarbodyMenu()->GetMenuMode() == mmDeadBodySearch)
		{
			if (m_pOwner == current_ui->GetCarbodyMenu()->GetPartner())
			{
				current_ui->OnInventoryAction(pIItem, GE_OWNERSHIP_TAKE);
			}
		}
	};
}

bool CInventory::DropItem(CGameObject* pObj, bool just_before_destroy, bool dont_create_shell)
{
	CInventoryItem* pIItem = pObj->cast_inventory_item();
	VERIFY(pIItem);
	VERIFY(pIItem->m_pInventory);
	VERIFY(pIItem->m_pInventory == this);
	VERIFY(pIItem->m_ItemCurrPlace.type != eItemPlaceUndefined);

	pIItem->m_last_dropped_owner_id = pIItem->parent_id();

	pIItem->object().processing_activate();

	switch (pIItem->CurrPlace())
	{
	case eItemPlaceBelt:
	{
		VERIFY(InBelt(pIItem));
		TIItemContainer::iterator temp_iter = std::find(m_belt.begin(), m_belt.end(), pIItem);
		if (temp_iter != m_belt.end())
		{
			m_belt.erase(temp_iter);
		}
		else
		{
			Msg("! ERROR: CInventory::Drop item not found in belt...");
		}
		pIItem->object().processing_deactivate();
	}break;
	case eItemPlaceRuck:
	{
		VERIFY(InRuck(pIItem));
		TIItemContainer::iterator temp_iter = std::find(m_ruck.begin(), m_ruck.end(), pIItem);
		if (temp_iter != m_ruck.end())
		{
			m_ruck.erase(temp_iter);
		}
		else
		{
			Msg("! ERROR: CInventory::Drop item not found in ruck...");
		}
	}break;
	case eItemPlaceSlot:
	{
		VERIFY(InSlot(pIItem));
		if (pIItem->CurrSlot() == PISTOL_SLOT_NEW && InventoryHolsterPistolSlotActiveInSettings())
		{
			pIItem->SetPreferredSlotAfterPickup(PISTOL_SLOT_NEW);
		}
		if (m_iActiveSlot == pIItem->CurrSlot() && m_pOwner != nullptr)
		{
			CActor* pActor = m_pOwner->cast_actor();
			if (!pActor || pActor->g_Alive())
			{
				if (just_before_destroy)
				{
#ifdef DEBUG
					Msg("---DropItem activating slot [-1], forced, Frame[%d]", Device.dwFrame);
#endif // #ifdef DEBUG
					Activate(NO_ACTIVE_SLOT, true);
				}
				else
				{
#ifdef DEBUG
					Msg("---DropItem activating slot [-1], Frame[%d]", Device.dwFrame);
#endif // #ifdef DEBUG
					Activate(NO_ACTIVE_SLOT);
				}
			}
		}
		m_slots[pIItem->CurrSlot()].m_pIItem = nullptr;
		pIItem->object().processing_deactivate();
	}break;
	default:
		NODEFAULT;
	};

	TIItemContainer::iterator it = std::find(m_all.begin(), m_all.end(), pIItem);
	if (it != m_all.end())
	{
		m_all.erase(std::find(m_all.begin(), m_all.end(), pIItem));
	}
	else
	{
		Msg("! CInventory::Drop item not found in inventory!!!");
	}

	pIItem->m_pInventory = nullptr;

	if (m_pOwner != nullptr)
	{
		m_pOwner->OnItemDrop(pObj->cast_inventory_item(), just_before_destroy);

		CalcTotalWeight();
		InvalidateState();
		m_drop_last_frame = true;

		if (CUIGameCustom* current_ui = CurrentGameUI())
		{
			CObject* pActor_owner = m_pOwner != nullptr ? m_pOwner->cast_game_object()->dcast_CObject() : nullptr;

			if (Level().CurrentViewEntity() == pActor_owner)
			{
				current_ui->OnInventoryAction(pIItem, GE_OWNERSHIP_REJECT);
			}
		};

		pObj->H_SetParent(0, dont_create_shell);
	}
	else
	{
		return false;
	}

	return true;
}

//положить вещь в слот
bool CInventory::Slot(u16 slot_id, PIItem pIItem, bool bNotActivate, bool strict_placement)
{
	VERIFY(pIItem);

	if (ItemFromSlot(slot_id) == pIItem)
	{
		return false;
	}

	if (!IsGameTypeSingle())
	{
		auto real_parent = pIItem->object().H_Parent() ? pIItem->object().H_Parent()->ID() : ALife::INVALID_OBJECT_ID;
		if (GetOwner()->object_id() != real_parent)
		{
			Msg("! WARNING: CL: actor [%d] tries to place to slot not own item [%d], that has parent [%d]", GetOwner()->object_id(), pIItem->object_id(), real_parent);
			return false;
		}
	}

	if (!strict_placement && !CanPutInSlot(pIItem, slot_id))
	{
#ifdef _DEBUG
		Msg("there is item %s[%d,%x] in slot %d[%d,%x]",
			ItemFromSlot(pIItem->CurrSlot())->object().cName().c_str(),
			ItemFromSlot(pIItem->CurrSlot())->object().ID(),
			ItemFromSlot(pIItem->CurrSlot()),
			pIItem->CurrSlot(),
			pIItem->object().ID(),
			pIItem);
#endif
		return false;
	}

	m_slots[slot_id].m_pIItem = pIItem;

	//удалить из рюкзака или пояса
	TIItemContainer::iterator it_ruck = std::find(m_ruck.begin(), m_ruck.end(), pIItem);
	TIItemContainer::iterator it_belt = std::find(m_belt.begin(), m_belt.end(), pIItem);
	if (!IsGameTypeSingle())
	{
		if (it_ruck != m_ruck.end())
		{
			m_ruck.erase(it_ruck);
			R_ASSERT(it_belt == m_belt.end());
		}
		else if (it_belt != m_belt.end())
		{
			m_belt.erase(it_belt);
			R_ASSERT(it_ruck == m_ruck.end());
		}
		else
		{
			auto real_parent = pIItem->object().H_Parent() ? pIItem->object().H_Parent()->ID() : ALife::INVALID_OBJECT_ID;
			R_ASSERT2(GetOwner()->object_id() == real_parent,
				make_string<const char*>("! ERROR: CL: actor [%d] doesn't contain [%d], real parent is [%d]",
					GetOwner()->object_id(), pIItem->object_id(), real_parent)
			);
		}
#ifdef MP_LOGGING
		Msg("--- Actor [%d] places to slot item [%d]", GetOwner()->object_id(), pIItem->object_id());
#endif //#ifdef MP_LOGGING
	}
	else
	{
		if (it_ruck != m_ruck.end())
		{
			m_ruck.erase(it_ruck);
		}

		if (it_belt != m_belt.end())
		{
			m_belt.erase(it_belt);
		}
	}

	bool in_slot = InSlot(pIItem);
	if (in_slot && (pIItem->CurrSlot() != slot_id))
	{
		if (GetActiveSlot() == pIItem->CurrSlot())
		{
			Activate(NO_ACTIVE_SLOT);
		}

		m_slots[pIItem->CurrSlot()].m_pIItem = nullptr;
	}

	if (((m_iActiveSlot == slot_id) || (m_iActiveSlot == NO_ACTIVE_SLOT) && m_iNextActiveSlot == NO_ACTIVE_SLOT) && (!bNotActivate))
	{
#ifdef DEBUG
		Msg("---To Slot: activating slot [%d], Frame[%d]", slot_id, Device.dwFrame);
#endif // #ifdef DEBUG
		Activate(slot_id);
	}

	SInvItemPlace p = pIItem->m_ItemCurrPlace;
	m_pOwner->OnItemSlot(pIItem, pIItem->m_ItemCurrPlace);
	pIItem->m_ItemCurrPlace.type = eItemPlaceSlot;
	pIItem->m_ItemCurrPlace.slot_id = slot_id;
	pIItem->OnMoveToSlot(p);

	pIItem->object().processing_activate();

	ReconcileItemPlacement(pIItem);

	return true;
}

bool CInventory::Belt(PIItem pIItem, bool strict_placement)
{
	if (!strict_placement && !CanPutInBelt(pIItem))
	{
		return false;
	}

	//вещь была в слоте
	bool in_slot = InSlot(pIItem);
	if (in_slot)
	{
		if (GetActiveSlot() == pIItem->CurrSlot())
		{
			Activate(NO_ACTIVE_SLOT);
		}

		m_slots[pIItem->CurrSlot()].m_pIItem = nullptr;
	}

	m_belt.insert(m_belt.end(), pIItem);

	if (!in_slot)
	{
		TIItemContainer::iterator it = std::find(m_ruck.begin(), m_ruck.end(), pIItem);
		if (m_ruck.end() != it)
		{
			m_ruck.erase(it);
		}
	}

	CalcTotalWeight();
	InvalidateState();

	SInvItemPlace p = pIItem->m_ItemCurrPlace;
	pIItem->m_ItemCurrPlace.type = eItemPlaceBelt;
	m_pOwner->OnItemBelt(pIItem, p);
	pIItem->OnMoveToBelt(p);

	if (in_slot)
	{
		pIItem->object().processing_deactivate();
	}

	pIItem->object().processing_activate();

	ReconcileItemPlacement(pIItem);

	return true;
}

bool CInventory::Ruck(PIItem pIItem, bool strict_placement)
{
	if (!strict_placement && !CanPutInRuck(pIItem))
	{
		return false;
	}

	if (!IsGameTypeSingle())
	{
		auto real_parent = pIItem->object().H_Parent() ? pIItem->object().H_Parent()->ID() : ALife::INVALID_OBJECT_ID;
		if (GetOwner()->object_id() != real_parent)
		{
			Msg("! WARNING: CL: actor [%d] tries to place to ruck not own item [%d], that has parent [%d]", GetOwner()->object_id(), pIItem->object_id(), real_parent);
			return false;
		}
	}

	bool in_slot = InSlot(pIItem);
	//вещь была в слоте
	if (in_slot)
	{
		if (GetActiveSlot() == pIItem->CurrSlot())
		{
			Activate(NO_ACTIVE_SLOT);
		}

		m_slots[pIItem->CurrSlot()].m_pIItem = nullptr;
	}
	else
	{
		//вещь была на поясе или вообще только поднята с земли
		TIItemContainer::iterator it = std::find(m_belt.begin(), m_belt.end(), pIItem);
		if (m_belt.end() != it)
		{
			m_belt.erase(it);
		}

		if (!IsGameTypeSingle())
		{
			auto item_parent_id = pIItem->object().H_Parent() ? pIItem->object().H_Parent()->ID() : ALife::INVALID_OBJECT_ID;
			auto inventory_owner_id = GetOwner()->object_id();
			R_ASSERT2(item_parent_id == inventory_owner_id,
				make_string<const char*>("! ERROR: CL: Actor[%d] tries to place to ruck not own item [%d], real item owner is [%d]",
					inventory_owner_id, pIItem->object_id(), item_parent_id)
			);
#ifdef MP_LOGGING
			Msg("--- Actor [%d] place to ruck item [%d]", inventory_owner_id, pIItem->object_id());
#endif
		}
	}

	m_ruck.insert(m_ruck.end(), pIItem);

	CalcTotalWeight();
	InvalidateState();

	m_pOwner->OnItemRuck(pIItem, pIItem->m_ItemCurrPlace);
	SInvItemPlace prev_place = pIItem->m_ItemCurrPlace;
	pIItem->m_ItemCurrPlace.type = eItemPlaceRuck;
	pIItem->OnMoveToRuck(prev_place);

	if (in_slot)
	{
		pIItem->object().processing_deactivate();
	}

	ReconcileItemPlacement(pIItem);

	return true;
}

void CInventory::Activate(u16 slot, bool bForce, bool ForceHide)
{
	if (!OnServer())
	{
		return;
	}

	if (CActor* actor = m_pOwner->cast_actor())
	{
		if (actor->HudAnimator() != nullptr && actor->HudAnimator()->IsAnyAnimatorActive())
		{
			if (CHudItem* hud_item = ActiveItem() ? ActiveItem()->cast_hud_item() : nullptr)
			{
				if (hud_item->SendDeactivateItem(false))
				{
					m_iNextActiveSlot = NO_ACTIVE_SLOT;
				}
			}
			return;
		}
	}

	PIItem tmp_item = nullptr;
	if (slot != NO_ACTIVE_SLOT)
	{
		tmp_item = ItemFromSlot(slot);
	}

	if (tmp_item && IsSlotBlocked(tmp_item) && (!bForce))
	{
		//to restore after unblocking ...
		SetPrevActiveSlot(slot);
		return;
	}

	if (GetActiveSlot() == slot || (GetNextActiveSlot() == slot && !bForce))
	{
		m_iNextActiveSlot = slot;
		return;
	}

	//R_ASSERT2(slot<=LastSlot(), "wrong slot number");

	if (slot != NO_ACTIVE_SLOT && !m_slots[slot].CanBeActivated())
	{
		return;
	}

	//активный слот не выбран
	if (GetActiveSlot() == NO_ACTIVE_SLOT)
	{
		if (tmp_item)
		{
			m_iNextActiveSlot = slot;
		}
		else if (slot == GRENADE_SLOT)
		{
			EnsureSlotItemFromRuck(GRENADE_SLOT);
		}
	}
	//активный слот задействован
	else if (slot == NO_ACTIVE_SLOT || tmp_item)
	{
		PIItem active_item = ActiveItem();
		if (active_item && !bForce)
		{
			CHudItem* tempItem = active_item->cast_hud_item();
			R_ASSERT2(tempItem, active_item->object().cNameSect().c_str());

			if (tempItem == nullptr || !tempItem->SendDeactivateItem(ForceHide))
			{
				return;
			}
		}
		else
		{
			//in case where weapon is going to destroy
			if (tmp_item)
			{
				tmp_item->ActivateItem();
			}

			m_iActiveSlot = slot;
		}

		m_iNextActiveSlot = slot;
	}
}

void CInventory::PutGrenade(CGrenade* new_grenade)
{
	m_pNewGrenade = new_grenade;
	Activate(NO_ACTIVE_SLOT);
}

PIItem CInventory::EnsureSlotItemFromRuck(u16 slotId, PIItem itemToSkip)
{
	if (slotId == NO_ACTIVE_SLOT)
	{
		return nullptr;
	}

	PIItem itemInSlot = ItemFromSlot(slotId);
	if (itemInSlot != nullptr)
	{
		return itemInSlot;
	}

	PIItem candidate = SameSlot(slotId, itemToSkip, true);
	if (candidate == nullptr)
	{
		return nullptr;
	}

	if (!Slot(slotId, candidate, true))
	{
		return nullptr;
	}

	return ItemFromSlot(slotId);
}

PIItem CInventory::ItemFromSlot(u16 slot) const
{
	if (NO_ACTIVE_SLOT == slot)
	{
		return nullptr;
	}

	const auto& Slot = m_slots.find(slot);
	if (Slot == m_slots.end())
	{
		return nullptr;
	}

	return (*Slot).second.m_pIItem;
}

void CInventory::SendActionEvent(u16 cmd, u32 flags)
{
	CActor* pActor = m_pOwner->cast_actor();
	if (!pActor)
	{
		return;
	}

	NET_Packet P;
	pActor->u_EventGen(P, GE_INV_ACTION, pActor->ID());
	P.w_u16(cmd);
	P.w_u32(flags);
	P.w_s32(pActor->GetZoomRndSeed());
	P.w_s32(pActor->GetShotRndSeed());
	pActor->u_EventSend(P, net_flags(true, true, false, true));
};

bool CInventory::Action(u16 cmd, u32 flags)
{
	CActor* pActor = m_pOwner->cast_actor();

	if (pActor)
	{
		switch (cmd)
		{
		case kWPN_FIRE:
		{
			pActor->SetShotRndSeed();
		}break;
		case kWPN_ZOOM:
		{
			pActor->SetZoomRndSeed();
		}break;
		};
	};

	if (g_pGameLevel && OnClient() && pActor)
	{
		switch (cmd)
		{
		case kUSE: break;
		case kDROP:
		{
			if ((flags & CMD_STOP) && !IsGameTypeSingle())
			{
				PIItem tmp_item = ActiveItem();
				if (tmp_item)
				{
					tmp_item->DenyTrade();
				}
			}
			SendActionEvent(cmd, flags);
			return true;
		}break;

		case kWPN_NEXT:
		case kWPN_RELOAD:
		case kWPN_FIRE:
		case kWPN_FUNC:
		case kWPN_FIREMODE_NEXT:
		case kWPN_FIREMODE_PREV:
		case kWPN_ZOOM:
		case kTORCH:
		case kNIGHT_VISION:
		{
			SendActionEvent(cmd, flags);
		}break;
		}
	}

	if (ActiveItem() != nullptr && ActiveItem()->Action(cmd, flags))
	{
		return true;
	}

	bool b_send_event = false;
	switch (cmd)
	{
	case kWPN_1:
	case kWPN_2:
	case kWPN_3:
	case kWPN_4:
	case kWPN_5:
	case kWPN_6:
	{
		b_send_event = true;
		if (cmd == kWPN_6 && !IsGameTypeSingleCompatible())
		{
			return false;
		}

		u16 slot = u16(cmd - kWPN_1 + 1);
		if (flags & CMD_START)
		{
			ActiveWeapon(slot);
		}
	}break;
	case kARTEFACT:
	{
		b_send_event = true;
		if (flags & CMD_START)
		{
			if (GetActiveSlot() == ARTEFACT_SLOT && ActiveItem() != nullptr)
			{
				Activate(NO_ACTIVE_SLOT);
			}
			else
			{
				Activate(ARTEFACT_SLOT);
			}
		}
	}break;
	case kWPN_7:
	{
		b_send_event = true;
		if (flags & CMD_START)
		{
			if (GetActiveSlot() == PISTOL_SLOT_NEW && ActiveItem() != nullptr)
			{
				Activate(NO_ACTIVE_SLOT);
			}
			else
			{
				Activate(PISTOL_SLOT_NEW);
			}
		}
	}break;
	}


	if (b_send_event && g_pGameLevel && OnClient() && pActor)
	{
		const u16 slotForDevice = InventoryWeaponHotkeyToInventorySlot(cmd);
		// Pavel: для ножа и болта нам не нужны проверки
		// Они нормально достаются / убираются с детектором в руках
		if (flags & CMD_START && cmd != kWPN_1 && cmd != kWPN_6 && slotForDevice != NO_ACTIVE_SLOT)
		{
			// Pavel: Не достаем другое оружие, если прицеливаемся из текущего оружия
			attachable_hud_item* i0 = g_player_hud->attached_item(0);
			if (i0)
			{
				CWeapon* pWpn = i0->m_parent_hud_item->cast_weapon();
				if (pWpn && pWpn->IsZoomed())
				{
					return false;
				}
			}

			if (attachable_hud_item* i1 = g_player_hud->attached_item(1))
			{
				// Pavel: Не достаем оружие, если в данный момент уже достается или убирается детектор
				CHudItem* pHudItem = i1->m_parent_hud_item;
				if (pHudItem->GetState() != CHUDState::EHudStates::eIdle &&
					pHudItem->GetState() != CHUDState::EHudStates::eHidden)
				{
					return false;
				}

				if (CCustomDevice* pDevice = pHudItem->cast_custom_device())
				{
					PIItem pItem = ItemFromSlot(slotForDevice);
					// Pavel: достаем пушку только после того, как убрали детектор
					if (pItem && !IsSidearmPhysicalSlot(pItem->BaseSlot()))
					{
						pDevice->HideAndSetCallback([cmd, flags, this]() {
							this->SendActionEvent(cmd, flags);
							});
						return false;
					}
				}
			}
		}
		SendActionEvent(cmd, flags);
	}

	return false;
}

void CInventory::ActiveWeapon(u16 slot)
{
	// weapon is in active slot
	if (GetActiveSlot() == slot && ActiveItem())
	{
		if (IsGameTypeSingleCompatible())
		{
			Activate(NO_ACTIVE_SLOT);
		}
		else
		{
			ActivateNextItemInActiveSlot();
		}

		return;
	}

	Activate(slot);
}

void CInventory::Update()
{
	if (OnServer())
	{
		if (m_iActiveSlot != m_iNextActiveSlot)
		{
			CObject* pActor_owner = m_pOwner != nullptr ? m_pOwner->cast_game_object()->dcast_CObject() : nullptr;
			if (Level().CurrentViewEntity() == pActor_owner)
			{
				if ((m_iNextActiveSlot != NO_ACTIVE_SLOT) && ItemFromSlot(m_iNextActiveSlot) && !g_player_hud->allow_activation(ItemFromSlot(m_iNextActiveSlot)->cast_hud_item()))
				{
					return;
				}
			}

			if (ActiveItem() != nullptr)
			{
				CHudItem* hi = ActiveItem()->cast_hud_item();

				if (!hi->IsHidden())
				{
					if (hi->GetState() == CHUDState::eIdle && hi->GetNextState() == CHUDState::eIdle)
						hi->SendDeactivateItem(false);

					UpdateDropTasks();
					return;
				}
			}

			if (m_pNewGrenade != nullptr && ItemFromSlot(m_pNewGrenade->BaseSlot()))
				m_iNextActiveSlot = m_pNewGrenade->BaseSlot();

			if (GetNextActiveSlot() != NO_ACTIVE_SLOT)
			{
				PIItem tmp_next_active = ItemFromSlot(GetNextActiveSlot());
				if (tmp_next_active)
				{
					if (m_pNewGrenade != nullptr && tmp_next_active == ItemFromSlot(m_pNewGrenade->BaseSlot()))
					{
						Ruck(ItemFromSlot(m_pNewGrenade->BaseSlot()));
						Slot(m_pNewGrenade->BaseSlot(), m_pNewGrenade);
						m_pNewGrenade = nullptr;
					}

					if (IsSlotBlocked(tmp_next_active))
					{
						Activate(m_iActiveSlot);
						return;
					}
					else
					{
						tmp_next_active->ActivateItem();
					}
				}
			}

			m_iActiveSlot = GetNextActiveSlot();
		}

		if ((GetNextActiveSlot() != NO_ACTIVE_SLOT) && ActiveItem() && ActiveItem()->cast_hud_item() && ActiveItem()->cast_hud_item()->IsHidden())
		{
			ActiveItem()->ActivateItem();
		}
	}

	UpdateDropTasks();
}

void CInventory::AddExternalStorage(shared_str story_id, CTradeStorageBox* obj)
{
	m_TraderExternalStorageMode = true;
	if (I_ASSERT_M(!m_ExternalContainers.contains(story_id), "Attempt to register storage [%s] twice!", story_id.c_str()))
	{
		m_ExternalContainers[story_id] = obj;
	}
}

CTradeStorageBox* CInventory::FindSuitableStorage(const shared_str& section) const
{
	CTradeStorageBox* UniversalStorage = nullptr;
	for (auto& Storage : m_ExternalContainers)
	{
		if (!Storage.second->GetFilterSize())
		{
			I_ASSERT_M(!UniversalStorage, "Find more than one universal storages!");
			UniversalStorage = Storage.second;
			continue;
		}
		if (Storage.second->CanStoreItem(section))
		{
			return Storage.second;
		}
	}
	I_ASSERT_M(UniversalStorage, "Can't find universal storage!");
	return UniversalStorage;
}

void CInventory::UpdateDropTasks()
{
	//проверить слоты
	for (u16 i = FirstSlot(); i <= LastSlot(); ++i)
	{
		if (PIItem itm = ItemFromSlot(i))
		{
			UpdateDropItem(itm);
		}
	}

	for (int i = 0; i < 2; ++i)
	{
		TIItemContainer& list = i ? m_ruck : m_belt;

		for (const PIItem item : list)
		{
			UpdateDropItem(item);
		}
	}

	if (m_drop_last_frame)
	{
		m_drop_last_frame = false;
		m_pOwner->OnItemDropUpdate();
	}
}

void CInventory::UpdateDropItem(PIItem pIItem)
{
	if (pIItem->GetDropManual())
	{
		pIItem->SetDropManual(false);
		pIItem->DenyTrade();

		if (OnServer())
		{
			NET_Packet P;
			pIItem->object().u_EventGen(P, GE_OWNERSHIP_REJECT, pIItem->object().H_Parent()->ID());
			P << pIItem->object().ID();
			pIItem->object().u_EventSend(P);
		}
	}
}

//ищем на поясе гранату такоже типа
PIItem CInventory::Same(const PIItem pIItem, bool bSearchRuck) const
{
	const TIItemContainer& list = bSearchRuck ? m_ruck : m_belt;

	for (const PIItem item : list)
	{
		if ((item != pIItem) && !xr_strcmp(item->object().cNameSect(), pIItem->object().cNameSect()))
		{
			return item;
		}
	}

	return nullptr;
}

//ищем на поясе вещь для слота 

PIItem CInventory::SameSlot(const u16 slot, PIItem pIItem, bool bSearchRuck) const
{
	if (slot == NO_ACTIVE_SLOT) 
	{
		return nullptr;
	}

	const TIItemContainer& list = bSearchRuck ? m_ruck : m_belt;

	for (const PIItem item : list)
	{
		if (item != pIItem && item->BaseSlot() == slot)
		{
			return item;
		}
	}

	return nullptr;
}

//найти в инвенторе вещь с указанным именем
PIItem CInventory::Get(const char* name, bool bSearchRuck) const
{
	const TIItemContainer& list = bSearchRuck ? m_ruck : m_belt;

	for (const PIItem item : list)
	{
		if (!xr_strcmp(item->object().cNameSect(), name) && item->Useful())
		{
			return item;
		}
	}

	return nullptr;
}

PIItem CInventory::Get(CLASS_ID cls_id, bool bSearchRuck) const
{
	const TIItemContainer& list = bSearchRuck ? m_ruck : m_belt;

	for (const PIItem item : list)
	{
		if (item->object().CLS_ID == cls_id && item->Useful())
		{
			return item;
		}
	}

	return nullptr;
}

PIItem CInventory::Get(const ALife::_OBJECT_ID id, bool bSearchRuck) const
{
	const TIItemContainer& list = bSearchRuck ? m_ruck : m_belt;

	for (const PIItem item : list)
	{
		if (item->object().ID() == id)
		{
			return item;
		}
	}

	return nullptr;
}

//search both (ruck and belt)
PIItem CInventory::GetAny(const char* name) const
{
	PIItem itm = Get(name, false);
	if (itm == nullptr)
	{
		itm = Get(name, true);
	}

	return itm;
}

PIItem CInventory::item(CLASS_ID cls_id) const
{
	const TIItemContainer& list = m_all;

	for (const PIItem item : list)
	{
		if (item->object().CLS_ID == cls_id && item->Useful())
		{
			return item;
		}
	}

	return nullptr;
}

void CInventory::GetAll(LPCSTR name, xr_vector<PIItem>& Output) {
	Output.clear();
	for (TIItemContainer::const_iterator it = m_belt.begin(); m_belt.end() != it; ++it)
	{
		//PIItem pIItem = smart_cast<T*>(*it);
		PIItem pIItem = *it;
		if (pIItem
			&& !xr_strcmp(pIItem->object().cNameSect(), name)
			&& pIItem->Useful()) {
			Output.push_back(pIItem);
			}
	}
	for (TIItemContainer::const_iterator it = m_ruck.begin(); m_ruck.end() != it; ++it)
	{
		PIItem pIItem = *it;
		if (pIItem
			&& !xr_strcmp(pIItem->object().cNameSect(), name)
			&& pIItem->Useful()) {
			Output.push_back(pIItem);
			}
	}
}

float CInventory::TotalWeight() const
{
	VERIFY(m_fTotalWeight >= 0.f);
	return m_fTotalWeight;
}

float CInventory::CalcTotalWeight()
{
	float weight = 0.0f;
	for (const PIItem item : m_all)
	{
		weight += CalcItemWeight(item);
	}

	m_fTotalWeight = weight;
	return m_fTotalWeight;
}

float CInventory::CalcItemWeight(const CInventoryItem* item) const
{
	if (!item)
	{
		return 0.0f;
	}

	float weight = item->Weight();
	if (CActor* actor = m_pOwner != nullptr ? m_pOwner->cast_actor() : nullptr)
	{
		weight *= actor->GetArtefactInventoryWeightModifier();
	}

	return weight;
}

u32 CInventory::dwfGetSameItemCount(const char* caSection, bool SearchAll)
{
	u32 l_dwCount = 0;
	TIItemContainer& l_list = SearchAll ? m_all : m_ruck;

	for (PIItem item : l_list)
	{
		if (!xr_strcmp(item->object().cNameSect(), caSection))
		{
			++l_dwCount;
		}
	}

	return l_dwCount;
}

u32 CInventory::dwfGetGrenadeCount(const char* caSection, bool SearchAll)
{
	u32 l_dwCount = 0;
	TIItemContainer& l_list = SearchAll ? m_all : m_ruck;

	for (PIItem item : l_list)
	{
		if (item->object().CLS_ID == CLSID_GRENADE_F1 || item->object().CLS_ID == CLSID_GRENADE_RGD5)
		{
			++l_dwCount;
		}
	}

	return l_dwCount;
}

bool CInventory::bfCheckForObject(ALife::_OBJECT_ID tObjectID)
{
	TIItemContainer& l_list = m_all;

	for (PIItem item : l_list)
	{
		if (item->object().ID() == tObjectID)
		{
			return true;
		}
	}

	return false;
}

CInventoryItem* CInventory::get_object_by_id(ALife::_OBJECT_ID tObjectID)
{
	TIItemContainer& l_list = m_all;

	for (PIItem item : l_list)
	{
		if (item->object().ID() == tObjectID)
		{
			return item;
		}
	}

	return nullptr;
}

//скушать предмет 
#include "game_object_space.h"
#include "script_game_object.h"

bool CInventory::Eat(PIItem pIItem)
{
	//установить съедобна ли вещь
	CEatableItem* pItemToEat = pIItem->cast_eatable_item();
	if (pItemToEat == nullptr)
	{
		return false;
	}

	CEntityAlive* entity_alive = m_pOwner->cast_entity_alive();
	if (entity_alive == nullptr)
	{
		return false;
	}

	CInventoryOwner* IO = entity_alive->cast_inventory_owner();
	if (IO == nullptr)
	{
		return false;
	}

	CInventory* pInventory = pItemToEat->m_pInventory;
	if (pInventory == nullptr || pInventory != this)
	{
		return false;
	}

	if (pInventory != IO->m_inventory)
	{
		return false;
	}

	if (pItemToEat->object().H_Parent()->ID() != entity_alive->ID())
	{
		return false;
	}

	if (!pItemToEat->UseBy(entity_alive))
	{
		return false;
	}

#ifdef MP_LOGGING
	Msg("--- Actor [%d] use or eat [%d][%s]", entity_alive->ID(), pItemToEat->object().ID(), pItemToEat->object().cNameSect().c_str());
#endif // MP_LOGGING

	if (m_isInventoryEat)
	{
		luabind::functor<bool> funct;
		R_ASSERT2(ai().script_engine().functor(m_onInventoryEat, funct), "failed to get OnInventoryEat functor");

		CObject* object_parent = pItemToEat->object().H_Parent();
		CGameObject* object_parent_go = object_parent != nullptr ? object_parent->cast_game_object() : nullptr;
		if (!funct(object_parent_go != nullptr ? object_parent_go->lua_game_object() : 0, (pIItem->cast_game_object()->lua_game_object())))
		{
			return false;
		}

		if (Actor()->m_inventory == this)
		{
			if (IsGameTypeSingle())
			{
				Actor()->callback(GameObject::eUseObject)(pIItem->cast_game_object()->lua_game_object());
			}

			if (CurrentGameUI()->GetActiveInventoryWindow())
			{
				if (pItemToEat->IsUsingCondition() && pItemToEat->GetRemainingUses() < 1 && pItemToEat->GetMaxUses() > 1 && pItemToEat->CanDelete())
				{
					CurrentGameUI()->GetActiveInventoryWindow()->RefreshCurrentItemCell();
				}

				CurrentGameUI()->GetActiveInventoryWindow()->SetCurrentItem(nullptr);
			}
		}
	}
	else if (IsGameTypeSingle() && Actor()->m_inventory == this)
	{
		CGameObject* item_game_object = pIItem->cast_game_object();
		Actor()->callback(GameObject::eUseObject)(item_game_object->lua_game_object());
	}

	if (pItemToEat->Empty())
	{
		if (!pItemToEat->CanDelete())
		{
			return false;
		}

		pIItem->SetDropManual(true);
	}

	return true;
}

bool CInventory::ClientEat(PIItem pIItem)
{
	CEatableItem* pItemToEat = pIItem->cast_eatable_item();
	if (pItemToEat == nullptr)
	{
		return false;
	}

	CEntityAlive* entity_alive = m_pOwner->cast_entity_alive();
	if (entity_alive == nullptr)
	{
		return false;
	}

	CInventoryOwner* IO = entity_alive->cast_inventory_owner();
	if (IO == nullptr)
	{
		return false;
	}

	CInventory* pInventory = pItemToEat->m_pInventory;
	if (pInventory == nullptr || pInventory != this)
	{
		return false;
	}

	if (pInventory != IO->m_inventory)
	{
		return false;
	}

	if (pItemToEat->object().H_Parent()->ID() != entity_alive->ID())
	{
		return false;
	}

	NET_Packet P;
	CGameObject::u_EventGen(P, GEG_PLAYER_ITEM_EAT, pIItem->parent_id());
	P << pIItem->object().ID();
	CGameObject::u_EventSend(P);
	return true;
}

bool CInventory::InSlot(const CInventoryItem* pIItem) const
{
	return pIItem->CurrPlace() == eItemPlaceSlot;
}

bool CInventory::InBelt(const CInventoryItem* pIItem) const
{
	return pIItem->CurrPlace() == eItemPlaceBelt;
}

bool CInventory::InRuck(const CInventoryItem* pIItem) const
{
	return pIItem->CurrPlace() == eItemPlaceRuck;
}

bool CInventory::CanPutInSlot(PIItem pIItem, u16 slot_id, bool bAllowReplacement) const
{
	if (!m_bSlotsUseful)
	{
		return false;
	}

	if (!GetOwner()->CanPutInSlot(pIItem, slot_id))
	{
		return false;
	}

	if (slot_id == HELMET_SLOT)
	{
		CCustomOutfit* pOutfit = m_pOwner->GetOutfit();
		if (pOutfit != nullptr && !pOutfit->bIsHelmetAvaliable)
		{
			return false;
		}
	}

	if (slot_id != NO_ACTIVE_SLOT)
	{
		PIItem pItemInSlot = ItemFromSlot(slot_id);
		if (!pItemInSlot || (bAllowReplacement && pItemInSlot != pIItem))
			return true;
	}

	return false;
}
//проверяет можем ли поместить вещь на пояс,
//при этом реально ничего не меняется
bool CInventory::CanPutInBelt(PIItem pIItem)
{
	if (pIItem == nullptr || !pIItem->Belt())
	{
		return false;
	}

	if (InBelt(pIItem))
	{
		return false;
	}

	if (!m_bBeltUseful)
	{
		return false;
	}

	if (m_belt.size() >= BeltWidth())
	{
		return false;
	}

	return FreeRoom_inBelt(m_belt, pIItem, BeltWidth(), 1);
}

//проверяет можем ли поместить вещь в рюкзак,
//при этом реально ничего не меняется
bool CInventory::CanPutInRuck(PIItem pIItem) const
{
	if (InRuck(pIItem))
	{
		return false;
	}

	return m_pOwner == nullptr || CInventoryVolumeSystem::Get().CanAddToRuck(*m_pOwner, *pIItem);
}

u32	CInventory::dwfGetObjectCount()
{
	return (u32)(m_all.size());
}

CInventoryItem* CInventory::tpfGetObjectByIndex(int iIndex)
{
	if ((iIndex >= 0) && (iIndex < (int)m_all.size()))
	{
		TIItemContainer& l_list = m_all;
		int i = 0;

		for (PIItem item : l_list)
		{
			if (i++ == iIndex)
			{
				return item;
			}
		}
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "invalid inventory index!");
		return nullptr;
	}

	R_ASSERT(false);

	return nullptr;
}

CInventoryItem* CInventory::GetItemFromInventory(const char* caItemName)
{
	TIItemContainer& l_list = m_all;

	u32 crc = crc32(caItemName, xr_strlen(caItemName));

	for (PIItem item : l_list)
	{
		if (item->object().cNameSect()._get()->dwCRC == crc)
		{
			VERIFY(0 == xr_strcmp(item->object().cNameSect().c_str(), caItemName));
			return item;
		}
	}

	return nullptr;
}

bool CInventory::CanTakeItem(CInventoryItem* inventory_item) const
{
	VERIFY(inventory_item);
	VERIFY(m_pOwner);

	if (inventory_item->object().getDestroy())
	{
		return false;
	}

	if (!inventory_item->CanTake())
	{
		return false;
	}

	if (!CInventoryVolumeSystem::Get().CanAddToRuck(*m_pOwner, *inventory_item))
	{
		return false;
	}

#ifdef DEBUG
	TIItemContainer::const_iterator it = m_all.begin();
	for (; it != m_all.end(); it++)
		if ((*it)->object().ID() == inventory_item->object().ID()) break;
	VERIFY3(it == m_all.end(), "item already exists in inventory", *inventory_item->object().cName());
#endif

	CActor* pActor = m_pOwner->cast_actor();
	CCar* pCar = m_pOwner->cast_car();

	//актер всегда может взять вещь
	if (pCar == nullptr && pActor == nullptr && TotalWeight() + CalcItemWeight(inventory_item) > m_pOwner->MaxCarryWeight())
	{
		return false;
	}

	return true;
}

u32 CInventory::BeltWidth() const
{
	CActor* pActor = m_pOwner->cast_actor();
	if (pActor != nullptr && !m_iMaxBelt)
	{
		if (CCustomOutfit* outfit = pActor->GetOutfit())
		{
			return outfit->get_artefact_count();
		}
	}

	return m_iMaxBelt;
}

void CInventory::AddAvailableItems(TIItemContainer& items_container, bool for_trade) const
{
	if(for_trade && m_TraderExternalStorageMode)
	{
		for(auto& elem : m_ExternalContainers)
		{
			elem.second->AddAvailableItems(items_container);
		}
		return;
	}
	
	for (const PIItem item : m_ruck)
	{
		if (!for_trade || item->CanTrade())
		{
			if (m_isItemAvailableToTrade && m_pOwner->is_alive())
			{
				luabind::functor<bool> funct;
				R_ASSERT2(ai().script_engine().functor(m_onItemAvailableToTrade, funct), "failed to get OnItemAvailableToTrade functor");
				if (!funct(m_pOwner->cast_game_object()->lua_game_object(), item->cast_game_object()->lua_game_object()))
				{
					continue;
				}
			}

			items_container.push_back(item);
		}
	}

	if (m_bBeltUseful)
	{
		for (const PIItem item : m_belt)
		{
			if (!for_trade || item->CanTrade())
			{
				if (m_isItemAvailableToTrade && m_pOwner->is_alive())
				{
					luabind::functor<bool> funct;
					R_ASSERT2(ai().script_engine().functor(m_onItemAvailableToTrade, funct), "failed to get OnItemAvailableToTrade functor");
					if (!funct(m_pOwner->cast_game_object()->lua_game_object(), item->cast_game_object()->lua_game_object()))
					{
						continue;
					}

				}

				items_container.push_back(item);
			}
		}
	}

	CAI_Stalker* pOwner = m_pOwner->cast_stalker();
	if (pOwner != nullptr && !pOwner->g_Alive())
	{
		u16 I = FirstSlot();
		u16 E = LastSlot();
		for (; I <= E; ++I)
		{
			PIItem item = ItemFromSlot(I);
			if (item != nullptr && (item->BaseSlot() != BOLT_SLOT))
			{
				if (m_isItemAvailableToTrade && pOwner->is_alive())
				{
					luabind::functor<bool> funct;
					R_ASSERT2(ai().script_engine().functor(m_onItemAvailableToTrade, funct), "failed to get OnItemAvailableToTrade functor");

					if (!funct(pOwner->cast_game_object()->lua_game_object(), item->cast_game_object()->lua_game_object()))
					{
						continue;
					}

				}

				items_container.push_back(item);
			}
		}
	}
	else if (m_bSlotsUseful)
	{
		u16 I = FirstSlot();
		u16 E = LastSlot();
		for (; I <= E; ++I)
		{
			PIItem item = ItemFromSlot(I);
			if (item != nullptr && (!for_trade || item->CanTrade()))
			{
				const auto& Slot = m_slots.find(I);
				if (!(*Slot).second.m_bPersistent || item->BaseSlot() == GRENADE_SLOT)
				{
					if (pOwner)
					{
						u16 slot = item->BaseSlot();

						if (slot != INV_SLOT_3)
						{
							if (m_isItemAvailableToTrade && pOwner->is_alive())
							{
								luabind::functor<bool> funct;
								R_ASSERT2(ai().script_engine().functor(m_onItemAvailableToTrade, funct), "failed to get OnItemAvailableToTrade functor");
								if (!funct(pOwner->cast_game_object()->lua_game_object(), item->cast_game_object()->lua_game_object()))
								{
									continue;
								}

							}

							items_container.push_back(item);
						}
					}
					else
					{
						if (m_isItemAvailableToTrade && m_pOwner->is_alive())
						{
							luabind::functor<bool> funct;
							R_ASSERT2(ai().script_engine().functor(m_onItemAvailableToTrade, funct), "failed to get OnItemAvailableToTrade functor");

							if (!funct(m_pOwner->cast_game_object()->lua_game_object(), item->cast_game_object()->lua_game_object()))
							{
								continue;
							}

						}

						items_container.push_back(item);
					}
				}
			}
		}
	}
}

bool CInventory::isBeautifulForActiveSlot(CInventoryItem* pIItem)
{
	if (!IsGameTypeSingle())
	{
		return true;
	}

	u16 I = FirstSlot();
	u16 E = LastSlot();
	for (; I <= E; ++I)
	{
		PIItem item = ItemFromSlot(I);
		if (item != nullptr && item->IsNecessaryItem(pIItem))
		{
			return true;
		}
	}

	return false;
}

void CInventory::Items_SetCurrentEntityHud(bool current_entity)
{
	for (const PIItem pIItem : m_all)
	{
		if (CWeapon* pWeapon = pIItem->cast_weapon())
		{
			pWeapon->InitAddons();
			pWeapon->UpdateAddonsVisibility();
		}
	}
};

void CInventory::SetSlotsBlocked(u16 mask, bool bBlock)
{
	R_ASSERT(OnServer() || Level().IsDemoPlayStarted());

	for (u16 i = FirstSlot(), ie = LastSlot(); i <= ie; ++i)
	{
		if (mask & (1 << i))
		{
			if (bBlock)
			{
				BlockSlot(i);
			}
			else
			{
				UnblockSlot(i);
			}
		}
	}

	if (bBlock)
	{
		TryDeactivateActiveSlot(true);
	}
	else
	{
		TryActivatePrevSlot();
	}
}

void CInventory::TryActivatePrevSlot()
{
	u16 ActiveSlot = GetActiveSlot();
	u16 PrevActiveSlot = GetPrevActiveSlot();
	u16 NextActiveSlot = GetNextActiveSlot();

	if ((ActiveSlot == NO_ACTIVE_SLOT || NextActiveSlot == NO_ACTIVE_SLOT) && PrevActiveSlot != NO_ACTIVE_SLOT)
	{
		PIItem prev_active_item = ItemFromSlot(PrevActiveSlot);
		if (prev_active_item != nullptr && !IsSlotBlocked(prev_active_item) && m_slots[PrevActiveSlot].CanBeActivated())
		{
#ifndef MASTER_GOLD
			Msg("Set slots blocked: activating prev slot [%d], Frame[%d]", PrevActiveSlot, Device.dwFrame);
#endif // #ifndef MASTER_GOLD
			Activate(PrevActiveSlot);
			SetPrevActiveSlot(NO_ACTIVE_SLOT);
		}
	}
}

void CInventory::TryDeactivateActiveSlot(bool Force)
{
	u16 ActiveSlot = GetActiveSlot();
	u16 NextActiveSlot = GetNextActiveSlot();

	if ((ActiveSlot == NO_ACTIVE_SLOT) && (NextActiveSlot == NO_ACTIVE_SLOT))
	{
		return;
	}

	PIItem active_item = (ActiveSlot != NO_ACTIVE_SLOT) ? ItemFromSlot(ActiveSlot) : nullptr;
	PIItem next_active_item = (NextActiveSlot != NO_ACTIVE_SLOT) ? ItemFromSlot(NextActiveSlot) : nullptr;

	if (active_item != nullptr && (IsSlotBlocked(active_item) || !m_slots[ActiveSlot].CanBeActivated()) )
	{
#ifndef MASTER_GOLD
		Msg("Set slots blocked: activating slot [-1], Frame[%d]", Device.dwFrame);
#endif // #ifndef MASTER_GOLD
		ItemFromSlot(ActiveSlot)->DiscardState();
		Activate(NO_ACTIVE_SLOT, false, Force);
		SetPrevActiveSlot(ActiveSlot);
	}
	else if (next_active_item && (IsSlotBlocked(next_active_item) || !m_slots[NextActiveSlot].CanBeActivated()))
	{
		Activate(NO_ACTIVE_SLOT);
		SetPrevActiveSlot(NextActiveSlot);
	}
}

void CInventory::BlockSlot(u16 slot_id)
{
	++m_blocked_slots[slot_id];

	VERIFY2(m_blocked_slots[slot_id] < 5, make_string<const char*>("blocked slot [%d] overflow"));
}

void CInventory::UnblockSlot(u16 slot_id)
{
	VERIFY2(m_blocked_slots[slot_id] > 0, make_string<const char*>("blocked slot [%d] underflow"));

	--m_blocked_slots[slot_id];
}

bool CInventory::IsSlotBlocked(u16 slot_id) const
{
	if (m_blocked_slots.size() <= slot_id)
	{
		return false;
	}

	return m_blocked_slots[slot_id] > 0;
}

bool CInventory::IsSlotBlocked(PIItem const iitem) const
{
	VERIFY(iitem);
	return IsSlotBlocked(iitem->BaseSlot());
}
