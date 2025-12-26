#include "stdafx.h"
#include "UIInventoryWnd.h"
#include "../actor.h"
#include "../silencer.h"
#include "../scope.h"
#include "../grenadelauncher.h"
#include "../Artefact.h"
#include "../eatable_item.h"
#include "../BottleItem.h"
#include "../WeaponMagazined.h"
#include "../inventory.h"
#include "../game_base.h"
#include "../game_cl_base.h"
#include "../../xrEngine/xr_level_controller.h"
#include "UICellItem.h"
#include "../../xrUI/Widgets/UIListBoxItem.h"
#include "../CustomOutfit.h"
#include "../../xrUI/UICursor.h"
#include "ui_drop_amount.h"
#include "../pda.h"
#include "../game_sv_single.h"
#include "ai_object_location.h"
#include "../actorhelmet.h"

void CUIInventoryWnd::EatItem(PIItem itm)
{
	SetCurrentItem							(nullptr);
	if(!itm->Useful())						return;

	CObject* current_entity = Level().CurrentEntity();
	CInventoryOwner* pInvOwner = current_entity != nullptr ? current_entity->cast_inventory_owner() : nullptr;
	SendEvent_Item_Eat						(itm, pInvOwner->object_id());

	PlaySnd									(eInvItemUse);
}

#include "../Medkit.h"
#include "../Antirad.h"
void CUIInventoryWnd::ActivatePropertiesBox()
{
	TryHidePropertiesBox();

	PIItem item = CurrentIItem();
	if(!item) 
	{
		return;
	}

	CUICellItem* cell_item = CurrentItem();		
	UIPropertiesBox.RemoveAll();
	bool	b_show			= false;

	PropertiesBoxForSlots(item, b_show);
	PropertiesBoxForWeapon(cell_item, item, b_show);
	PropertiesBoxForAddon(item, b_show);
	PropertiesBoxForUsing(item, b_show);
	PropertiesBoxForPlaying(item, b_show);
	PropertiesBoxForDrop(cell_item, item, b_show);
	PropertiesBoxForParse(item, b_show);

	if(b_show)
	{
		UIPropertiesBox.AutoUpdateSize	();

		Fvector2						cursor_pos;
		Frect							vis_rect;
		GetAbsoluteRect					(vis_rect);
		cursor_pos						= GetUICursor().GetCursorPosition();
		cursor_pos.sub					(vis_rect.lt);
		UIPropertiesBox.Show			(vis_rect, cursor_pos);
		PlaySnd							(eInvProperties);
	}
}

void CUIInventoryWnd::ProcessPropertiesBoxClicked	()
{
	PIItem			item		= CurrentIItem();
	CUICellItem*	cell_item	= CurrentItem();
	if ( !UIPropertiesBox.GetClickedItem() || !item || !cell_item || !cell_item->OwnerList() )
	{
		return;
	}
	CWeapon* weapon = item->cast_weapon();

	switch ( UIPropertiesBox.GetClickedItem()->GetTAG() )
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
			void* d_ = UIPropertiesBox.GetClickedItem()->GetData();
			if(d_ == (void*)33) 
			{
				DropAllCurrentItem(cell_item->ChildsCount());
			}
			else if (d_ == (void*)77)
			{
				m_pItemDropAmountWnd->ShowDropAmount(cell_item->ChildsCount(), CUIItemDropAmountWnd::eModeDrop);
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
			AttachAddon((PIItem)(UIPropertiesBox.GetClickedItem()->GetData()));
			
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
		game_sv_Single* tpGame = Level().Server->game->cast_game_sv_single();
		if (tpGame == nullptr)
		{
			break;
		}

		CObject* current_entity = Level().CurrentEntity();
		CActor* actor = current_entity != nullptr ? current_entity->cast_actor() : nullptr;
		if (actor == nullptr)
		{
			break;
		}

		shared_str SpawnList = pSettings->r_string(item->m_section_id, "parse_spawn_items");
		shared_str ChanceList = pSettings->r_string(item->m_section_id, "parse_spawn_chances");

		int Count = _GetItemCount(SpawnList.c_str());
		int Count2 = _GetItemCount(ChanceList.c_str());

		extern CSE_Abstract* CALifeSimulator__spawn_item2(CALifeSimulator* self_, LPCSTR section, const Fvector& position, u32 level_vertex_id, GameGraph::_GRAPH_ID game_vertex_id, ALife::_OBJECT_ID id_parent);
		
		string128 sItem;
		string16 sItem2;

		for (int i = 0; i < Count; ++i)
		{
			_GetItem(SpawnList.c_str(), i, sItem);

			if (i < Count2)
				_GetItem(ChanceList.c_str(), i, sItem2);
			else
				_GetItem(ChanceList.c_str(), Count2 - 1, sItem2);

			float chance = static_cast<float>(atof(sItem2));

			if (chance >= ::Random.randF(0.0f, 1.0f))
				CALifeSimulator__spawn_item2(&tpGame->alife(), sItem, actor->Position(), actor->ai_location().level_vertex_id(), actor->ai_location().game_vertex_id(), actor->ID());
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

void CUIInventoryWnd::TryHidePropertiesBox()
{
	if (UIPropertiesBox.IsShown())
	{
		UIPropertiesBox.Hide();
	}
}

void CUIInventoryWnd::PropertiesBoxForSlots( PIItem item, bool& b_show )
{
	CObject* current_entity = Level().CurrentEntity();
	CInventoryOwner* pInvOwner = current_entity != nullptr ? current_entity->cast_inventory_owner() : nullptr;
	if(item->parent_id() != pInvOwner->object_id()) {
		return;
	}

	CCustomOutfit* pOutfit = item->cast_outfit();
	CHelmet* pHelmet		= item->cast_helmet();
	CInventory*  inv		= GetInventory();

	// Флаг-признак для невлючения пункта контекстного меню: Dreess Outfit, если костюм уже надет
	bool bAlreadyDressed	= false;
	u16 cur_slot			= item->BaseSlot();

	if(cur_slot == GRENADE_SLOT) {
		return;
	}

	if (	!pOutfit && !pHelmet &&
			cur_slot != NO_ACTIVE_SLOT &&
			!inv->SlotIsPersistent(cur_slot) &&
			inv->CanPutInSlot(item, cur_slot) )
	{
		UIPropertiesBox.AddItem( "st_move_to_slot",  nullptr, INVENTORY_TO_SLOT_ACTION );
		b_show = true;
	}
	if (	item->Belt() &&
			inv->CanPutInBelt( item ) )
	{
		UIPropertiesBox.AddItem( "st_move_on_belt",  nullptr, INVENTORY_TO_BELT_ACTION );
		b_show = true;
	}

	if (	item->Ruck() &&
			inv->CanPutInRuck(item) &&
			( cur_slot == NO_ACTIVE_SLOT || !inv->SlotIsPersistent(cur_slot) ) )
	{
		if( !pOutfit )
		{
			if( !pHelmet )
				UIPropertiesBox.AddItem( "st_move_to_bag",  nullptr, INVENTORY_TO_BAG_ACTION );
			else
				UIPropertiesBox.AddItem( "st_undress_helmet",  nullptr, INVENTORY_TO_BAG_ACTION );
		}
		else
			UIPropertiesBox.AddItem( "st_undress_outfit",  nullptr, INVENTORY_TO_BAG_ACTION );

		bAlreadyDressed = true;
		b_show			= true;
	}
	if ( pOutfit && !bAlreadyDressed )
	{
		UIPropertiesBox.AddItem( "st_dress_outfit",  nullptr, INVENTORY_TO_SLOT_ACTION );
		b_show			= true;
	}

	CCustomOutfit* outfit_in_slot = pInvOwner->GetOutfit();
	if ( pHelmet && !bAlreadyDressed && (!outfit_in_slot || outfit_in_slot->bIsHelmetAvaliable))
	{
		UIPropertiesBox.AddItem( "st_dress_helmet",  nullptr, INVENTORY_TO_SLOT_ACTION );
		b_show			= true;
	}
}

void CUIInventoryWnd::PropertiesBoxForWeapon( CUICellItem* cell_item, PIItem item, bool& b_show )
{
	//отсоединение аддонов от вещи
	CWeapon*	pWeapon = item->cast_weapon();
	if ( !pWeapon )
	{
		return;
	}

	if ( pWeapon->GrenadeLauncherAttachable() )
	{
		if ( pWeapon->IsGrenadeLauncherAttached() )
		{
			UIPropertiesBox.AddItem( "st_detach_gl",  nullptr, INVENTORY_DETACH_GRENADE_LAUNCHER_ADDON );
			b_show			= true;
		}
		else
		{
		}
	}
	if ( pWeapon->ScopeAttachable() )
	{
		if ( pWeapon->IsScopeAttached() )
		{
			UIPropertiesBox.AddItem( "st_detach_scope",  nullptr, INVENTORY_DETACH_SCOPE_ADDON );
			b_show			= true;
		}
		else
		{
		}
	}
	if ( pWeapon->SilencerAttachable() )
	{
		if ( pWeapon->IsSilencerAttached() )
		{
			UIPropertiesBox.AddItem( "st_detach_silencer",  nullptr, INVENTORY_DETACH_SILENCER_ADDON );
			b_show			= true;
		}
		else
		{
		}
	}
	if (pWeapon->cast_weapon_magazined() != nullptr && IsGameTypeSingleCompatible())
	{
		bool b = (pWeapon->GetAmmoElapsed() || pWeapon->IsChamber() && pWeapon->GetAmmoChamberElapsed());
		if (!b)
		{
			for (u32 i = 0; i < cell_item->ChildsCount(); ++i)
			{
				CWeapon* data = (CWeapon*)cell_item->Child(i)->m_pData;
				CWeaponMagazined* weap_mag = data != nullptr ? data->cast_weapon_magazined() : nullptr;
				if (weap_mag != nullptr && (weap_mag->GetAmmoElapsed() || weap_mag->IsChamber() && weap_mag->GetAmmoChamberElapsed()))
				{
					b = true;
					break; // for
				}
			}
		}
		if (b)
		{
			UIPropertiesBox.AddItem("st_unload_magazine", nullptr, INVENTORY_UNLOAD_MAGAZINE);
			b_show = true;
		}
	}
}
#include "../../xrEngine/string_table.h"
void CUIInventoryWnd::PropertiesBoxForAddon( PIItem item, bool& b_show )
{
	//присоединение аддонов к активному слоту (2 или 3)

	CScope* pScope = item->cast_addon_scope();
	CSilencer* pSilencer = item->cast_addon_silencer();
	CGrenadeLauncher* pGrenadeLauncher = item->cast_addon_grenade_launcher();
	CInventory* inv = GetInventory();

	PIItem	item_in_slot_2 = inv->ItemFromSlot(INV_SLOT_2);
	PIItem	item_in_slot_3 = inv->ItemFromSlot(INV_SLOT_3);
	PIItem	item_in_slot_pistol_new = inv->ItemFromSlot(PISTOL_SLOT_NEW);

	if(!item_in_slot_2 && !item_in_slot_3)	return;

	if ( pScope )
	{
		if (item_in_slot_pistol_new && item_in_slot_pistol_new->CanAttach(pScope))
		{
			shared_str str = g_pStringTable->translate("st_attach_scope_to_pistol");
			str.printf("%s %s", str.c_str(), item_in_slot_pistol_new->m_name.c_str());
			UIPropertiesBox.AddItem(str.c_str(), (void*)item_in_slot_pistol_new, INVENTORY_ATTACH_ADDON);
			b_show = true;
		}
		if ( item_in_slot_2 && item_in_slot_2->CanAttach(pScope) )
		{
			shared_str str = g_pStringTable->translate("st_attach_scope_to_pistol");
			str.printf("%s %s", str.c_str(), item_in_slot_2->m_name.c_str());
			UIPropertiesBox.AddItem( str.c_str(),  (void*)item_in_slot_2, INVENTORY_ATTACH_ADDON );
//			m_UIPropertiesBox->AddItem( "st_attach_scope_to_pistol",  (void*)item_in_slot_2, INVENTORY_ATTACH_ADDON );
			b_show			= true;
		}
		if ( item_in_slot_3 && item_in_slot_3->CanAttach(pScope) )
		{
			shared_str name = g_pStringTable->translate("st_attach_scope_to_rifle");
			shared_str add_name = g_pStringTable->translate("st_attach_scope_to_pistol");
			shared_str str;
			if (!xr_strcmp(name, "st_attach_scope_to_rifle") &&
				xr_strcmp(add_name, "st_attach_scope_to_pistol"))
			{
				str = add_name;
			}
			else
				str = name;

			str.printf("%s %s", str.c_str(), item_in_slot_3->m_name.c_str());
			UIPropertiesBox.AddItem( str.c_str(),  (void*)item_in_slot_3, INVENTORY_ATTACH_ADDON );
//			m_UIPropertiesBox->AddItem( "st_attach_scope_to_rifle",  (void*)item_in_slot_3, INVENTORY_ATTACH_ADDON );
			b_show			= true;
		}
		return;
	}

	if ( pSilencer )
	{
		if (item_in_slot_pistol_new && item_in_slot_pistol_new->CanAttach(pSilencer))
		{
			shared_str str = g_pStringTable->translate("st_attach_silencer_to_pistol");
			str.printf("%s %s", str.c_str(), item_in_slot_pistol_new->m_name.c_str());
			UIPropertiesBox.AddItem(str.c_str(), (void*)item_in_slot_pistol_new, INVENTORY_ATTACH_ADDON);
			b_show = true;
		}
		if ( item_in_slot_2 && item_in_slot_2->CanAttach(pSilencer) )
		{
			shared_str str = g_pStringTable->translate("st_attach_silencer_to_pistol");
			str.printf("%s %s", str.c_str(), item_in_slot_2->m_name.c_str());
			UIPropertiesBox.AddItem( str.c_str(),  (void*)item_in_slot_2, INVENTORY_ATTACH_ADDON );
			b_show			= true;
		}
		if ( item_in_slot_3 && item_in_slot_3->CanAttach(pSilencer) )
		{
			shared_str name = g_pStringTable->translate("st_attach_silencer_to_rifle");
			shared_str add_name = g_pStringTable->translate("st_attach_silencer_to_pistol");
			shared_str str;
			if (!xr_strcmp(name, "st_attach_silencer_to_rifle") &&
				xr_strcmp(add_name, "st_attach_silencer_to_pistol"))
			{
				str = add_name;
			}
			else
				str = name;

			str.printf("%s %s", str.c_str(), item_in_slot_3->m_name.c_str());
			UIPropertiesBox.AddItem( str.c_str(),  (void*)item_in_slot_3, INVENTORY_ATTACH_ADDON );
			b_show			= true;
		}
		return;
	}

	if ( pGrenadeLauncher )
	{
		if (item_in_slot_pistol_new && item_in_slot_pistol_new->CanAttach(item_in_slot_pistol_new))
		{
			shared_str str = g_pStringTable->translate("st_attach_gl_to_rifle");
			str.printf("%s %s", str.c_str(), item_in_slot_pistol_new->m_name.c_str());
			UIPropertiesBox.AddItem(str.c_str(), (void*)item_in_slot_pistol_new, INVENTORY_ATTACH_ADDON);
			b_show = true;
		}
		if ( item_in_slot_2 && item_in_slot_2->CanAttach(pGrenadeLauncher) )
		{
			shared_str str = g_pStringTable->translate("st_attach_gl_to_rifle");
			str.printf("%s %s", str.c_str(), item_in_slot_2->m_name.c_str());
			UIPropertiesBox.AddItem( str.c_str(),  (void*)item_in_slot_2, INVENTORY_ATTACH_ADDON );
//			m_UIPropertiesBox->AddItem( "st_attach_gl_to_pistol",  (void*)item_in_slot_2, INVENTORY_ATTACH_ADDON );
			b_show			= true;
		}
		if ( item_in_slot_3 && item_in_slot_3->CanAttach(pGrenadeLauncher) )
		{
			shared_str str = g_pStringTable->translate("st_attach_gl_to_rifle");
			str.printf("%s %s", str.c_str(), item_in_slot_3->m_name.c_str());
			UIPropertiesBox.AddItem( str.c_str(),  (void*)item_in_slot_3, INVENTORY_ATTACH_ADDON );
//			m_UIPropertiesBox->AddItem( "st_attach_gl_to_rifle",  (void*)item_in_slot_3, INVENTORY_ATTACH_ADDON );
			b_show			= true;
		}
	}
}

void CUIInventoryWnd::PropertiesBoxForUsing( PIItem item, bool& b_show )
{
	LPCSTR act_str = nullptr;
	CGameObject* GO = smart_cast<CGameObject*>(item);
	shared_str	section_name = GO->cNameSect();

	//ability to set eat string from settings
	act_str = READ_IF_EXISTS(pSettings, r_string, section_name, "default_use_text", 0);
	if (act_str)
	{
		UIPropertiesBox.AddItem(act_str, nullptr, INVENTORY_EAT_ACTION);
		b_show = true;
	}
	else {
		CMedkit*		pMedkit			= smart_cast<CMedkit*>		(item);
		CAntirad*		pAntirad		= smart_cast<CAntirad*>		(item);
		CEatableItem*	pEatableItem	= smart_cast<CEatableItem*>	(item);
		CBottleItem*	pBottleItem		= smart_cast<CBottleItem*>	(item);

		if ( pMedkit || pAntirad )
		{
			act_str = "st_use";
		}
		else if ( pBottleItem )
		{
			act_str = "st_drink";
		}
		else if ( pEatableItem )
		{
			act_str = *pEatableItem->UseText;
		}
		if ( act_str )
		{
			UIPropertiesBox.AddItem( act_str,  nullptr, INVENTORY_EAT_ACTION );
			b_show			= true;
		}
	}

	//1st Custom Use action
	act_str = READ_IF_EXISTS(pSettings, r_string, section_name, "use1_text", 0);
	if (act_str)
	{
		UIPropertiesBox.AddItem(act_str, NULL, INVENTORY_EAT2_ACTION);
		b_show = true;
	}

	//2nd Custom Use action
	act_str = READ_IF_EXISTS(pSettings, r_string, section_name, "use2_text", 0);
	if (act_str)
	{
		UIPropertiesBox.AddItem(act_str, NULL, INVENTORY_EAT3_ACTION);
		b_show = true;
	}
	
	//3rd Custom Use action
	act_str = READ_IF_EXISTS(pSettings, r_string, section_name, "use3_text", 0);
	if (act_str)
	{
		UIPropertiesBox.AddItem(act_str, NULL, INVENTORY_EAT4_ACTION);
		b_show = true;
	}

	//4th Custom Use action
	act_str = READ_IF_EXISTS(pSettings, r_string, section_name, "use4_text", 0);
	if (act_str)
	{
		UIPropertiesBox.AddItem(act_str, NULL, INVENTORY_EAT5_ACTION);
		b_show = true;
	}
}

void CUIInventoryWnd::PropertiesBoxForPlaying(PIItem item, bool& b_show)
{
	CPda* pPda = item->cast_pda();
	if(!pPda || !pPda->CanPlayScriptFunction())
		return;

	LPCSTR act_str = "st_play";
	UIPropertiesBox.AddItem(act_str,  nullptr, INVENTORY_PLAY_ACTION);
	b_show = true;
}

void CUIInventoryWnd::PropertiesBoxForDrop(CUICellItem* cell_item, PIItem item, bool& b_show)
{
	if (!item->IsQuestItem())
	{
		UIPropertiesBox.AddItem("st_drop", nullptr, INVENTORY_DROP_ACTION);
		b_show = true;

		if (cell_item->ChildsCount())
		{
			UIPropertiesBox.AddItem("st_drop_amount", (void*)77, INVENTORY_DROP_ACTION);
			UIPropertiesBox.AddItem("st_drop_all", (void*)33, INVENTORY_DROP_ACTION);
		}
	}
}

void CUIInventoryWnd::PropertiesBoxForParse(PIItem item, bool& b_show)
{
	if (pSettings->line_exist(item->m_section_id, "parse_spawn_items") && pSettings->line_exist(item->m_section_id, "parse_spawn_chances"))
	{
		UIPropertiesBox.AddItem("st_parse", nullptr, INVENTORY_PARSE_ITEM);
		b_show = true;
	}
}
