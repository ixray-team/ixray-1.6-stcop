#include "pch_script.h"
#include "UIInventoryWnd.h"
#include "../actor.h"
#include "../silencer.h"
#include "../scope.h"
#include "../grenadelauncher.h"
#include "../Artifact.h"
#include "../eatable_item.h"
#include "../BottleItem.h"
#include "../WeaponMagazined.h"
#include "../inventory.h"
#include "../game_base.h"
#include "../game_cl_base.h"
#include "../../xrEngine/xr_level_controller.h"
#include "UICellItem.h"
#include "UIListBoxItem.h"
#include "../CustomOutfit.h"
#include "../script_callback_ex.h"
#include "../Medkit.h"
#include "../Antirad.h"
#include "../battery.h"
#include "../UICursor.h"

void CUIInventoryWnd::EatItem(PIItem itm)
{
	SetCurrentItem							(NULL);
	if(!itm->Useful())						return;
	CActor *pActor							= smart_cast<CActor*>(Level().CurrentEntity());
	if(!pActor)								return;

	SendEvent_Item_Eat						(itm);

	//PlaySnd									(eInvItemUse);
}

void CUIInventoryWnd::ActivatePropertiesBox()
{
	// Флаг-признак для невлючения пункта контекстного меню: Dreess Outfit, если костюм уже надет
	bool bAlreadyDressed = false; 

		
	UIPropertiesBox.RemoveAll();
	
	auto iitem = CurrentIItem();
	CMedkit*			pMedkit				= smart_cast<CMedkit*>			(iitem);
	CAntirad*			pAntirad			= smart_cast<CAntirad*>			(iitem);
	CEatableItem*		pEatableItem		= smart_cast<CEatableItem*>		(iitem);
	CCustomOutfit*		pOutfit				= smart_cast<CCustomOutfit*>	(iitem);
	CWeapon*			pWeapon				= smart_cast<CWeapon*>			(iitem);
	CScope*				pScope				= smart_cast<CScope*>			(iitem);
	CSilencer*			pSilencer			= smart_cast<CSilencer*>		(iitem);
	CGrenadeLauncher*	pGrenadeLauncher	= smart_cast<CGrenadeLauncher*>	(iitem);
	CBottleItem*		pBottleItem			= smart_cast<CBottleItem*>		(iitem);
	CBattery*			pBattery			= smart_cast<CBattery*>			(iitem);
    
	bool	b_show			= false;
	auto	baseSlot		= iitem->BaseSlot();

	if(!pOutfit
	   && baseSlot != NO_ACTIVE_SLOT
	   && (CanPutInSlot(baseSlot, CurrentItem()) || (baseSlot == RIFLE_SLOT && CanPutInSlot(RIFLE_2_SLOT, CurrentItem())))
	   )
	{
		//tatarinrafa: block putting pnv or helmet in slot if outfit does not allow that
		bool show = true;
		if (baseSlot == HELMET_SLOT || baseSlot == PNV_SLOT){

			PIItem	itemformoutfitslot = GetInventory()->ItemFromSlot(OUTFIT_SLOT);
			if (itemformoutfitslot)
			{
				CCustomOutfit* outfit = smart_cast<CCustomOutfit*>(itemformoutfitslot);//на всякий случай проверим если это броня в слоте брони, а то..
				if (outfit)
				{
					if ((baseSlot == HELMET_SLOT && outfit->block_helmet_slot == 1) || (baseSlot == PNV_SLOT && outfit->block_pnv_slot == 1))
						show = false;
				}
			}
		}
		if (show){
			UIPropertiesBox.AddItem("st_move_to_slot", NULL, INVENTORY_TO_SLOT_ACTION);
			b_show = true;
		}
	}
	if(iitem->Belt() && m_pInv->CanPutInBelt(iitem, false))
	{
		UIPropertiesBox.AddItem("st_move_on_belt",  NULL, INVENTORY_TO_BELT_ACTION);
		b_show			= true;
	}

	if(iitem->Ruck() && m_pInv->CanPutInRuck(iitem) && (baseSlot == NO_ACTIVE_SLOT || !m_pInv->m_slots[baseSlot].m_bPersistent) )
	{
		if(!pOutfit)
			UIPropertiesBox.AddItem("st_move_to_bag",  NULL, INVENTORY_TO_BAG_ACTION);
		else
			UIPropertiesBox.AddItem("st_undress_outfit",  NULL, INVENTORY_TO_BAG_ACTION);
		bAlreadyDressed = true;
		b_show			= true;
	}
	if(pOutfit  && !bAlreadyDressed )
	{
		UIPropertiesBox.AddItem("st_dress_outfit",  NULL, INVENTORY_TO_SLOT_ACTION);
		b_show			= true;
	}
	
	//отсоединение аддонов от вещи
	if(pWeapon)
	{
		
		luabind::functor<bool>	lua_function;
		string256		fn;
		xr_strcpy		(fn, pSettings->r_string("lost_alpha_cfg", "on_checking_repair_wpn"));
		R_ASSERT2 (ai().script_engine().functor<bool>(fn,lua_function),make_string<const char*>("Can't find function %s",fn));
	
		if (lua_function(CurrentIItem()->object().ID())) {				//isRepairable?
			UIPropertiesBox.AddItem("st_repair_weapon",  NULL, INVENTORY_REPAIR);
		}
		if(pWeapon->GrenadeLauncherAttachable() && pWeapon->IsGrenadeLauncherAttached())
		{
			UIPropertiesBox.AddItem("st_detach_gl",  NULL, INVENTORY_DETACH_GRENADE_LAUNCHER_ADDON);
		b_show			= true;
		}
		if(pWeapon->ScopeAttachable() && pWeapon->IsScopeAttached())
		{
			UIPropertiesBox.AddItem("st_detach_scope",  NULL, INVENTORY_DETACH_SCOPE_ADDON);
		b_show			= true;
		}
		if(pWeapon->SilencerAttachable() && pWeapon->IsSilencerAttached())
		{
			UIPropertiesBox.AddItem("st_detach_silencer",  NULL, INVENTORY_DETACH_SILENCER_ADDON);
		b_show			= true;
		}
		if(smart_cast<CWeaponMagazined*>(pWeapon) && IsGameTypeSingle())
		{
			bool b = (0!=pWeapon->GetAmmoElapsed());

			if(!b)
			{
				CUICellItem * itm = CurrentItem();
				for(u32 i=0; i<itm->ChildsCount(); ++i)
				{
					pWeapon		= smart_cast<CWeaponMagazined*>((CWeapon*)itm->Child(i)->m_pData);
					if(pWeapon->GetAmmoElapsed())
					{
						b = true;
						break;
					}
				}
			}

			if(b){
				UIPropertiesBox.AddItem("st_unload_magazine",  NULL, INVENTORY_UNLOAD_MAGAZINE);
				b_show			= true;
			}
		}
	}
	
	//присоединение аддонов к активному слоту (2 или 3)
	if(pScope)
	{
		AttachActionToPropertyBox(PISTOL_SLOT,  pScope, "st_attach_scope_to_pistol");
		AttachActionToPropertyBox(RIFLE_SLOT,   pScope, "st_attach_scope_to_rifle");
		AttachActionToPropertyBox(RIFLE_2_SLOT, pScope, "st_attach_scope_to_rifle");
	}
	else if(pSilencer)
	{
		AttachActionToPropertyBox(PISTOL_SLOT,  pSilencer, "st_attach_silencer_to_pistol");
		AttachActionToPropertyBox(RIFLE_SLOT,   pSilencer, "st_attach_silencer_to_rifle");
		AttachActionToPropertyBox(RIFLE_2_SLOT, pSilencer, "st_attach_silencer_to_rifle");
	}
	else if(pGrenadeLauncher)
	{
		AttachActionToPropertyBox(RIFLE_SLOT,   pGrenadeLauncher, "st_attach_gl_to_rifle");
		AttachActionToPropertyBox(RIFLE_2_SLOT, pGrenadeLauncher, "st_attach_gl_to_rifle");
	}
	LPCSTR _action = NULL;

	if(pMedkit || pAntirad || pBattery)
	{
		_action					= "st_use";
	}
	else if(pEatableItem)
	{
		if(pBottleItem)
			_action					= "st_drink";
		else
			_action					= "st_eat";
	}

	if(_action){
		UIPropertiesBox.AddItem(_action,  NULL, INVENTORY_EAT_ACTION);
		b_show			= true;
	}

	bool disallow_drop	= (pOutfit&&bAlreadyDressed);
	disallow_drop		|= !!CurrentIItem()->IsQuestItem();

	if(!disallow_drop)
	{

		UIPropertiesBox.AddItem("st_drop", NULL, INVENTORY_DROP_ACTION);
		b_show			= true;

		if(CurrentItem()->ChildsCount())
			UIPropertiesBox.AddItem("st_drop_all", (void*)33, INVENTORY_DROP_ACTION);
	}

	if(b_show)
	{
		UIPropertiesBox.AutoUpdateSize	();
		UIPropertiesBox.BringAllToTop	();

		Fvector2						cursor_pos;
		Frect							vis_rect;
		GetAbsoluteRect					(vis_rect);
		cursor_pos						= GetUICursor().GetCursorPosition();
		cursor_pos.sub					(vis_rect.lt);
		UIPropertiesBox.Show			(vis_rect, cursor_pos);
		PlaySnd							(eInvProperties);
	}
}

bool CUIInventoryWnd::AttachActionToPropertyBox(TSlotId slot, CInventoryItem* addon, LPCSTR text)
{
	auto tgt = m_pInv->ItemFromSlot(slot);
	if (tgt != NULL && tgt->CanAttach(addon))
	{
		UIPropertiesBox.AddItem(text, (void*)tgt, INVENTORY_ATTACH_ADDON);
		return true;
	}
	return false;
}

void CUIInventoryWnd::ProcessPropertiesBoxClicked	()
{
	if(UIPropertiesBox.GetClickedItem())
	{
		switch(UIPropertiesBox.GetClickedItem()->GetTAG())
		{
		case INVENTORY_TO_SLOT_ACTION:	
			ToSlot(CurrentItem(), true, CurrentIItem()->BaseSlot());
			break;
		case INVENTORY_TO_BELT_ACTION:	
			ToBelt(CurrentItem(),false);
			break;
		case INVENTORY_TO_BAG_ACTION:	
			ToBag(CurrentItem(),false);
			break;
		case INVENTORY_DROP_ACTION:
			{
				void* d = UIPropertiesBox.GetClickedItem()->GetData();
				bool b_all = (d==(void*)33);

				DropCurrentItem(b_all);
			}break;
		case INVENTORY_EAT_ACTION:
			EatItem(CurrentIItem());
			break;
		case INVENTORY_ATTACH_ADDON:
			AttachAddon((PIItem)(UIPropertiesBox.GetClickedItem()->GetData()));
			break;
		case INVENTORY_DETACH_SCOPE_ADDON:
			DetachAddon(*(smart_cast<CWeapon*>(CurrentIItem()))->GetScopeName());
			break;
		case INVENTORY_DETACH_SILENCER_ADDON:
			DetachAddon(*(smart_cast<CWeapon*>(CurrentIItem()))->GetSilencerName());
			break;
		case INVENTORY_DETACH_GRENADE_LAUNCHER_ADDON:
			DetachAddon(*(smart_cast<CWeapon*>(CurrentIItem()))->GetGrenadeLauncherName());
			break;
		case INVENTORY_RELOAD_MAGAZINE:
			(smart_cast<CWeapon*>(CurrentIItem()))->Action(kWPN_RELOAD, CMD_START);
			break;
		case INVENTORY_REPAIR:
			{
				luabind::functor<void>	repair;
				string256		fn;
				xr_strcpy		(fn, pSettings->r_string("lost_alpha_cfg", "on_repair_wpn_clicked"));
				R_ASSERT2 (ai().script_engine().functor<void>(fn,repair),make_string<const char*>("Can't find function %s",fn));

				repair(CurrentIItem()->object().ID());																			//Repair
			}break;
		case INVENTORY_UNLOAD_MAGAZINE:
			{
				CUICellItem * itm = CurrentItem();
				(smart_cast<CWeaponMagazined*>((CWeapon*)itm->m_pData))->UnloadMagazine();
				for(u32 i=0; i<itm->ChildsCount(); ++i)
				{
					CUICellItem * child_itm			= itm->Child(i);
					(smart_cast<CWeaponMagazined*>((CWeapon*)child_itm->m_pData))->UnloadMagazine();
				}
			}break;
		}
	}
}

bool CUIInventoryWnd::TryUseItem(PIItem itm)
{
	CBottleItem*		pBottleItem			= smart_cast<CBottleItem*>		(itm);
	CMedkit*			pMedkit				= smart_cast<CMedkit*>			(itm);
	CAntirad*			pAntirad			= smart_cast<CAntirad*>			(itm);
	CEatableItem*		pEatableItem		= smart_cast<CEatableItem*>		(itm);
	CBattery*			pBattery			= smart_cast<CBattery*>			(itm);

#pragma todo("why not just smart cast to eatable item?")
	if(pMedkit || pAntirad || pEatableItem || pBottleItem || pBattery)
	{
		EatItem(itm);
		return true;
	}
	return false;
}

bool CUIInventoryWnd::DropItem(PIItem itm, CUIDragDropListEx* lst)
{
	if (lst == m_pUIOutfitList)
	{
		return TryUseItem			(itm);
	}
	CUICellItem*	_citem	= lst->ItemsCount() ? lst->GetItemIdx(0) : NULL;
	PIItem _iitem	= _citem ? (PIItem)_citem->m_pData : NULL;

	if(!_iitem)						return	false;
	if(!_iitem->CanAttach(itm))		return	false;
	AttachAddon						(_iitem);

	return							true;
}
