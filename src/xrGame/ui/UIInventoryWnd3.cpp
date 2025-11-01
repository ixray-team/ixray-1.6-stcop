#include "stdafx.h"
#include "UIInventoryWnd.h"
#include "../Actor.h"
#include "../Silencer.h"
#include "../Scope.h"
#include "../GrenadeLauncher.h"
#include "../Artefact.h"
#include "../eatable_item.h"
#include "../BottleItem.h"
#include "../WeaponMagazined.h"
#include "../Inventory.h"
#include "../game_base.h"
#include "../game_cl_base.h"
#include "../../xrEngine/xr_level_controller.h"
#include "UICellItem.h"
#include "../../xrUI/Widgets/UIListBoxItem.h"
#include "../CustomOutfit.h"
#include "../../xrUI/UICursor.h"
#include "ui_drop_amount.h"
#include "../PDA.h"
#include "../game_sv_single.h"
#include "ai_object_location.h"
#include "../ActorHelmet.h"

void CUIInventoryWnd::EatItem(PIItem itm)
{
	SetCurrentItem							(nullptr);
	if(!itm->Useful())						return;

	CObject* current_entity = Level().CurrentEntity();
	CInventoryOwner* pInvOwner = current_entity != nullptr ? current_entity->cast_inventory_owner() : nullptr;
	SendEvent_Item_Eat						(itm, pInvOwner->object_id());

	PlaySnd									(eInvItemUse);
}

#include "../medkit.h"
#include "../antirad.h"
void CUIInventoryWnd::ActivatePropertiesBox()
{
	TryHidePropertiesBox();

	PIItem item = CurrentIItem();
	if (!item)
	{
		return;
	}

	CUICellItem* cell_item = CurrentItem();
	m_UIPropertiesBox->RemoveAll();
	bool b_show = false;

	PropertiesBoxForSlots(cell_item, item, b_show);
	PropertiesBoxForWeapon(cell_item, item, b_show);
	PropertiesBoxForAddon(item, b_show);
	PropertiesBoxForUsing(item, b_show);
	PropertiesBoxForPlaying(item, b_show);
	PropertiesBoxForDrop(cell_item, item, b_show);
	PropertiesBoxForParse(item, b_show);

	if (b_show)
	{
		m_UIPropertiesBox->AutoUpdateSize	();

		Frect								vis_rect;
		GetAbsoluteRect						(vis_rect);
		Fvector2							cursor_pos = GetUICursor().GetCursorPosition();
		cursor_pos.sub						(vis_rect.lt);
		m_UIPropertiesBox->Show				(vis_rect, cursor_pos);
		PlaySnd								(eInvProperties);
	}
}

void CUIInventoryWnd::ProcessPropertiesBoxClicked()
{
	PIItem			item		= CurrentIItem();
	CUICellItem*	cell_item	= CurrentItem();
	if (!m_UIPropertiesBox->GetClickedItem() || !item || !cell_item || !cell_item->OwnerList())
	{
		return;
	}

	CWeapon* weapon = item->cast_weapon();

	if (IAntigas* oAntigas = smart_cast<IAntigas*>(item->cast_inventory_item()))
	{
		if (oAntigas->OnProcessPropertiesBoxClicked(m_UIPropertiesBox))
		{
			return;
		}
	}

	switch (m_UIPropertiesBox->GetClickedItem()->GetTAG())
	{
	case INVENTORY_TO_SLOT_ACTION:	ToSlot( cell_item, true, item->BaseSlot() );		break;
	case INVENTORY_TO_BELT_ACTION:	ToBelt( cell_item, false );		break;
	case INVENTORY_TO_BAG_ACTION:	ToBag ( cell_item, false );		break;
	case INVENTORY_EAT_ACTION:
		TryUseItem( item );
		break;	
	case INVENTORY_EAT2_ACTION:
	{
		CGameObject* GO = item->cast_game_object();
		LPCSTR functor_name = READ_IF_EXISTS(pSettings, r_string, GO->cNameSect(), "use1_functor", 0);
		if (functor_name)
		{
			luabind::functor<bool>	funct1;
			if (ai().script_engine().functor(functor_name, funct1))
			{
				if (funct1(GO->lua_game_object()))
					TryUseItem(item);
			}
		}
		break;
	}
	case INVENTORY_EAT3_ACTION:
	{
		CGameObject* GO = item->cast_game_object();
		LPCSTR functor_name = READ_IF_EXISTS(pSettings, r_string, GO->cNameSect(), "use2_functor", 0);
		if (functor_name)
		{
			luabind::functor<bool>	funct2;
			if (ai().script_engine().functor(functor_name, funct2))
			{
				if (funct2(GO->lua_game_object()))
					TryUseItem(item);
			}
		}
		break;
	}
	case INVENTORY_EAT4_ACTION:
	{
		CGameObject* GO = item->cast_game_object();
		LPCSTR functor_name = READ_IF_EXISTS(pSettings, r_string, GO->cNameSect(), "use3_functor", 0);
		if (functor_name)
		{
			luabind::functor<bool>	funct3;
			if (ai().script_engine().functor(functor_name, funct3))
			{
				if (funct3(GO->lua_game_object()))
					TryUseItem(item);
			}
		}
		break;
	}
	case INVENTORY_EAT5_ACTION:
	{
		CGameObject* GO = item->cast_game_object();
		LPCSTR functor_name = READ_IF_EXISTS(pSettings, r_string, GO->cNameSect(), "use4_functor", 0);
		if (functor_name)
		{
			luabind::functor<bool>	funct4;
			if (ai().script_engine().functor(functor_name, funct4))
			{
				if (funct4(GO->lua_game_object()))
					TryUseItem(item);
			}
		}
		break;
	}
	case INVENTORY_DROP_ACTION:
		{
			void* d_ = m_UIPropertiesBox->GetClickedItem()->GetData();
			if(d_ == (void*)33)
			{
				DropAllCurrentItem(cell_item->ChildsCount() + 1);
			}
			else if (m_pItemDropAmountWnd != nullptr && d_ == (void*)77)
			{
				m_pItemDropAmountWnd->ShowDropAmount(cell_item->ChildsCount(), CUIItemDropAmountWnd::eModeDrop, item);
			}
			else
			{
				CObject* current_entity = Level().CurrentEntity();
				CInventoryOwner* pInvOwner = current_entity != nullptr ? current_entity->cast_inventory_owner() : nullptr;
				SendEvent_Item_Drop(item, pInvOwner->object_id());
			}
			break;
		}
	case INVENTORY_ATTACH_ADDON:
		{
			PIItem item_ = CurrentIItem(); // temporary storing because of AttachAddon is setting curiitem to nullptr
			AttachAddon((PIItem)(m_UIPropertiesBox->GetClickedItem()->GetData()));
			
			break;
		}
	case INVENTORY_DETACH_SCOPE_ADDON:
		if ( weapon )
		{
			DetachAddon( weapon->GetScopeName().c_str() );
			for ( u32 i = 0; i < cell_item->ChildsCount(); ++i )
			{
				CUICellItem*	child_itm	= cell_item->Child(i);
				PIItem			child_iitm	= (PIItem)(child_itm->m_pData);
				CWeapon* wpn = child_iitm != nullptr ? child_iitm->cast_weapon() : nullptr;
				if ( child_iitm && wpn )
				{
					DetachAddon(wpn->GetScopeName().c_str(), child_iitm);
				}
			}
		}
		break;
	case INVENTORY_DETACH_SILENCER_ADDON:
		if ( weapon )
		{
			DetachAddon( weapon->GetSilencerName().c_str() );
			for ( u32 i = 0; i < cell_item->ChildsCount(); ++i )
			{
				CUICellItem*	child_itm	= cell_item->Child(i);
				PIItem			child_iitm	= (PIItem)(child_itm->m_pData);
				CWeapon* wpn = child_iitm != nullptr ? child_iitm->cast_weapon() : nullptr;
				if ( child_iitm && wpn )
				{
					DetachAddon(wpn->GetSilencerName().c_str(), child_iitm);
				}
			}
		}
		break;
	case INVENTORY_DETACH_GRENADE_LAUNCHER_ADDON:
		if ( weapon )
		{
			DetachAddon( weapon->GetGrenadeLauncherName().c_str() );
			for ( u32 i = 0; i < cell_item->ChildsCount(); ++i )
			{
				CUICellItem*	child_itm	= cell_item->Child(i);
				PIItem			child_iitm	= (PIItem)(child_itm->m_pData);
				CWeapon* wpn = child_iitm != nullptr ? child_iitm->cast_weapon() : nullptr;
				if ( child_iitm && wpn )
				{
					DetachAddon(wpn->GetGrenadeLauncherName().c_str(), child_iitm);
				}
			}
		}
		break;
	case INVENTORY_RELOAD_MAGAZINE:
		if ( weapon )
		{
			weapon->Action( kWPN_RELOAD, CMD_START );
		}
		break;
	case INVENTORY_UNLOAD_MAGAZINE:
		{
			CWeapon* data = (CWeapon*)cell_item->m_pData;
			CWeaponMagazined* weap_mag = data != nullptr ? data->cast_weapon_magazined() : nullptr;
			if (weap_mag == nullptr)
			{
				break;
			}

			UnloadWeapon(weap_mag);
			weap_mag->UnloadChamber();
			for (u32 i = 0; i < cell_item->ChildsCount(); ++i)
			{
				CUICellItem* child_itm = cell_item->Child(i);
				data = (CWeapon*)child_itm->m_pData;
				CWeaponMagazined* child_weap_mag = data != nullptr ? data->cast_weapon_magazined() : nullptr;
				if (child_weap_mag != nullptr)
				{
					UnloadWeapon(child_weap_mag);
					child_weap_mag->UnloadChamber();
				}
			}
			break;
		}
	case INVENTORY_PLAY_ACTION:
		{
			CPda* pPda = item->cast_pda();
			if(!pPda)
				break;
			pPda->PlayScriptFunction();
			break;
		}
	case INVENTORY_PARSE_ITEM:
	{
		game_sv_Single* tpGame = Level().Server->game != nullptr ? Level().Server->game->cast_game_sv_single() : nullptr;
		if (tpGame == nullptr) {
			break;
		}

		CActor* actor = Level().CurrentEntity() ? Level().CurrentEntity()->cast_actor() : nullptr;
		if (actor == nullptr) {
			break;
		}

		extern CSE_Abstract* CALifeSimulator__spawn_item2(CALifeSimulator * self_, LPCSTR section, const Fvector & position, u32 level_vertex_id, GameGraph::_GRAPH_ID game_vertex_id, ALife::_OBJECT_ID id_parent);

		int Count = item->m_parse_params.m_items.size();
		int Count2 = item->m_parse_params.m_chances.size();

		for (int i = 0; i < Count; ++i)
		{
			float chance = 0.0f;

			if (i >= Count2)
			{
				chance = item->m_parse_params.m_chances.back();
			}
			else
			{
				chance = item->m_parse_params.m_chances[i];
			}

			if (chance >= ::Random.randF(0.0f, 1.0f))
			{
				CALifeSimulator__spawn_item2(&tpGame->alife(), *item->m_parse_params.m_items[i], actor->Position(), actor->ai_location().level_vertex_id(), actor->ai_location().game_vertex_id(), actor->ID());
			}
		}
		item->object().DestroyObject();
	}break;
	}//switch

//	SetCurrentItem( nullptr );

// to be implemented
//	UpdateItemsPlace();
//	UpdateConditionProgressBars();
}

bool CUIInventoryWnd::TryUseItem(PIItem itm)
{
	CBottleItem*		pBottleItem			= smart_cast<CBottleItem*>		(itm);
	CMedkit*			pMedkit				= smart_cast<CMedkit*>			(itm);
	CAntirad*			pAntirad			= smart_cast<CAntirad*>			(itm);
	CEatableItem*		pEatableItem		= smart_cast<CEatableItem*>		(itm);

	if(pMedkit || pAntirad || pEatableItem || pBottleItem)
	{
		EatItem(itm);
		return true;
	}
	return false;
}

bool CUIInventoryWnd::DropItem(PIItem itm, CUIDragDropListEx* lst)
{
	if(lst==m_pUIOutfitList)
	{
		return TryUseItem			(itm);
/*
		CCustomOutfit*		pOutfit		= smart_cast<CCustomOutfit*>	(CurrentIItem());
		if(pOutfit)
			ToSlot			(CurrentItem(), true);
		else
			EatItem				(CurrentIItem());

		return				true;
*/
	}
	CUICellItem*	_citem	= lst->ItemsCount() ? lst->GetItemIdx(0) : nullptr;
	PIItem _iitem	= _citem ? (PIItem)_citem->m_pData : nullptr;

	if(!_iitem)						return	false;
	if(!_iitem->CanAttach(itm))		return	false;
	AttachAddon						(_iitem);

	return							true;
}

void CUIInventoryWnd::PropertiesBoxForDrop(CUICellItem* cell_item, PIItem item, bool& b_show)
{
	if (!item->IsQuestItem())
	{
		m_UIPropertiesBox->AddItem("st_drop", nullptr, INVENTORY_DROP_ACTION);
		b_show = true;

		if (cell_item->ChildsCount())
		{
			if (m_pItemDropAmountWnd != nullptr)
			{
				m_UIPropertiesBox->AddItem("st_drop_amount", (void*)77, INVENTORY_DROP_ACTION);
			}
			m_UIPropertiesBox->AddItem("st_drop_all", (void*)33, INVENTORY_DROP_ACTION);
		}
	}
}
