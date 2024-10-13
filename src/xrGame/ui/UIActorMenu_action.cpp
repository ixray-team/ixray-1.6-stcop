////////////////////////////////////////////////////////////////////////////
//	Module 		: UIActorMenu_action.cpp
//	Created 	: 14.10.2008
//	Author		: Evgeniy Sokolov (sea)
//	Description : UI ActorMenu actions implementation
////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "UIActorMenu.h"
#include "UIActorStateInfo.h"
#include "../actor.h"
#include "../uigamesp.h"
#include "../inventory.h"
#include "../inventory_item.h"
#include "../InventoryBox.h"
#include "object_broker.h"
#include "UIInventoryUtilities.h"
#include "game_cl_base.h"

#include "../../xrUI/UICursor.h"
#include "UICellItem.h"
#include "UICharacterInfo.h"
#include "UIItemInfo.h"
#include "UIDragDropListEx.h"
#include "UIInventoryUpgradeWnd.h"
#include "../../xrUI/Widgets/UI3tButton.h"
#include "../../xrUI/Widgets/UIBtnHint.h"
#include "UIMessageBoxEx.h"
#include "../../xrUI/Widgets/UIPropertiesBox.h"
#include "UIMainIngameWnd.h"


bool  CUIActorMenu::AllowItemDrops(EDDListType from, EDDListType to)
{
	xr_vector<EDDListType>& v = m_allowed_drops[to];
	xr_vector<EDDListType>::iterator it = std::find(v.begin(), v.end(), from);

	return(it!=v.end());
}
class CUITrashIcon :public ICustomDrawDragItem
{
	CUIStatic			m_icon;
public:
	CUITrashIcon		()
	{
		m_icon.SetWndSize		(Fvector2().set(29.0f*UI().get_current_kx(), 36.0f));
		m_icon.SetStretchTexture(true);
//		m_icon.SetAlignment		(waCenter);
		m_icon.InitTexture		("ui_inGame2_inv_trash");
	}
	virtual void		OnDraw		(CUIDragItem* drag_item)
	{
		Fvector2 pos			= drag_item->GetWndPos();
		Fvector2 icon_sz		= m_icon.GetWndSize();
		Fvector2 drag_sz		= drag_item->GetWndSize();

		pos.x			-= icon_sz.x;
		pos.y			+= drag_sz.y;

		m_icon.SetWndPos(pos);
//		m_icon.SetWndSize(sz);
		m_icon.Draw		();
	}

};
void CUIActorMenu::OnDragItemOnTrash(CUIDragItem* item, bool b_receive)
{
	if(b_receive && !CurrentIItem()->IsQuestItem())
		item->SetCustomDraw(new CUITrashIcon());
	else
		item->SetCustomDraw(nullptr);
}

bool CUIActorMenu::OnItemDrop(CUICellItem* itm)
{
	InfoCurItem( nullptr );
	CUIDragDropListEx*	old_owner		= itm->OwnerList();
	CUIDragDropListEx*	new_owner		= CUIDragDropListEx::m_drag_item->BackList();
	if ( !old_owner || !new_owner )
	{
		return false;
	}
	EDDListType t_new		= GetListType(new_owner);
	EDDListType t_old		= GetListType(old_owner);

	if ( !AllowItemDrops(t_old, t_new) )
	{
		Msg("incorrect action [%d]->[%d]",t_old, t_new);
		return true;
	}
	
	if (old_owner == new_owner){
		//Alundaio: Here we export the action of dragging one inventory item ontop of another! 
		luabind::functor<bool> funct1;
		if (ai().script_engine().functor("actor_menu_inventory.CUIActorMenu_OnItemDropped", funct1))
		{
			//If list only has 1 item, get it, otherwise try to get item at current drag position
			CUICellItem* _citem = (new_owner->ItemsCount() == 1) ? new_owner->GetItemIdx(0) : NULL;
			if (!_citem)
			{ 
				CUICellContainer* c = old_owner->GetContainer();
				Ivector2 c_pos = c->PickCell(old_owner->GetDragItemPosition());
				if (c->ValidCell(c_pos))
				{
					CUICell& ui_cell = c->GetCellAt(c_pos);
					if (!ui_cell.Empty())
						_citem = ui_cell.m_item;
				}
			}

			PIItem _iitem = _citem ? (PIItem)_citem->m_pData : NULL;

			CGameObject* GO1 = smart_cast<CGameObject*>(CurrentIItem());
			CGameObject* GO2 = _iitem ? smart_cast<CGameObject*>(_iitem) : NULL;
			if (funct1(GO1 ? GO1->lua_game_object() : (0), GO2 ? GO2->lua_game_object() : (0), (int)t_old, (int)t_new) == false)
				return false;
		}
		//-Alundaio
		return false;
	}

	switch(t_new)
	{
	case iTrashSlot:
		{
			if(CurrentIItem()->IsQuestItem())
				return true;

			if(t_old==iQuickSlot)	
			{
				old_owner->RemoveItem(itm, false);
				return true;
			}
			SendEvent_Item_Drop		(CurrentIItem(), m_pActorInvOwner->object_id());
			SetCurrentItem			(nullptr);
		}break;
	case iActorSlot:
		{
			//.			if(GetSlotList(CurrentIItem()->GetSlot())==new_owner)
			u16 slot_to_place;
			if( CanSetItemToList(CurrentIItem(), new_owner, slot_to_place) )
				ToSlot	(itm, true, slot_to_place);
		}break;
	case iActorBag:
		{
			ToBag	(itm, true);
		}break;
	case iActorBelt:
		{
			ToBelt	(itm, true);
		}break;
	case iActorTrade:
		{
			ToActorTrade(itm, true);
		}break;
	case iPartnerTrade:
		{
			if(t_old!=iPartnerTradeBag)	
				return false;
			ToPartnerTrade(itm, true);
		}break;
	case iPartnerTradeBag:
		{
			if(t_old!=iPartnerTrade)	
				return false;
			ToPartnerTradeBag(itm, true);
		}break;
	case iDeadBodyBag:
		{
			ToDeadBodyBag(itm, true);
		}break;
	case iQuickSlot:
		{
			ToQuickSlot(itm);
		}break;
	};

	OnItemDropped				(CurrentIItem(), new_owner, old_owner);

	//Alundaio: Here we export the action of dragging one inventory item ontop of another! 
	luabind::functor<bool> funct1;
	if (ai().script_engine().functor("actor_menu_inventory.CUIActorMenu_OnItemDropped", funct1))
	{
		//If list only has 1 item, get it, otherwise try to get item at current drag position
		CUICellItem* _citem = (new_owner->ItemsCount() == 1) ? new_owner->GetItemIdx(0) : NULL;
		if (!_citem)
		{
			CUICellContainer* c = old_owner->GetContainer();
			Ivector2 c_pos = c->PickCell(old_owner->GetDragItemPosition());
			if (c->ValidCell(c_pos))
			{
				CUICell& ui_cell = c->GetCellAt(c_pos);
				if (!ui_cell.Empty())
					_citem = ui_cell.m_item;
			}
		}

		PIItem _iitem = _citem ? (PIItem)_citem->m_pData : NULL;

		CGameObject* GO1 = smart_cast<CGameObject*>(CurrentIItem());
		CGameObject* GO2 = _iitem ? smart_cast<CGameObject*>(_iitem) : NULL;
		if (funct1(GO1 ? GO1->lua_game_object() : (0), GO2 ? GO2->lua_game_object() : (0), (int)t_old, (int)t_new) == false)
			return false;
	}
	//-Alundaio

	UpdateConditionProgressBars	();
	UpdateItemsPlace			();

	return true;
}

bool CUIActorMenu::OnItemStartDrag(CUICellItem* itm)
{
	InfoCurItem( nullptr );
	return false; //default behaviour
}

bool CUIActorMenu::OnItemDbClick(CUICellItem* itm)
{
	InfoCurItem( nullptr );
	CUIDragDropListEx*	old_owner		= itm->OwnerList();
	EDDListType t_old					= GetListType(old_owner);

	switch ( t_old )
	{
	case iActorSlot:
		{
			if ( m_currMenuMode == mmDeadBodySearch )
				ToDeadBodyBag	( itm, false );
			else
				ToBag			( itm, false );
			break;
		}
	case iActorBag:
		{
			if ( m_currMenuMode == mmTrade )
			{
				ToActorTrade( itm, false );
				break;
			}else
				if ( m_currMenuMode == mmDeadBodySearch )
				{
					ToDeadBodyBag( itm, false );
					break;
				}
				if(m_currMenuMode!=mmUpgrade && TryUseItem( itm ))
				{
					break;
				}
				if ( TryActiveSlot( itm ) )
				{
					break;
				}
				PIItem iitem_to_place = (PIItem)itm->m_pData;
				if ( !ToSlot( itm, false, iitem_to_place->BaseSlot() ) )
				{
					if ( !ToBelt( itm, false ) )
					{
						ToSlot( itm, true, iitem_to_place->BaseSlot() );
					}
				}
				break;
		}
	case iActorBelt:
		{
			ToBag( itm, false );
			break;
		}
	case iActorTrade:
		{
			ToBag( itm, false );
			break;
		}
	case iPartnerTradeBag:
		{
			ToPartnerTrade( itm, false );
			break;
		}
	case iPartnerTrade:
		{
			ToPartnerTradeBag( itm, false );
			break;
		}
	case iDeadBodyBag:
		{
			ToBag( itm, false );
			break;
		}
	case iQuickSlot:
		{
			ToQuickSlot(itm);
		}break;

	}; //switch 

	UpdateConditionProgressBars();
	UpdateItemsPlace();

	return true;
}

bool CUIActorMenu::OnItemSelected(CUICellItem* itm)
{
	SetCurrentItem		(itm);
	InfoCurItem			(nullptr);
	m_item_info_view	= false;
	return				false;
}

bool CUIActorMenu::OnItemRButtonClick(CUICellItem* itm)
{
	SetCurrentItem( itm );
	InfoCurItem( nullptr );
	ActivatePropertiesBox();
	m_item_info_view = false;
	return false;
}

bool CUIActorMenu::OnItemFocusReceive(CUICellItem* itm)
{
	InfoCurItem( nullptr );
	m_item_info_view = true;

	itm->m_selected = true;
	set_highlight_item( itm );

	luabind::functor<bool> funct1;
	if (ai().script_engine().functor("actor_menu_inventory.CUIActorMenu_OnItemFocusReceive", funct1))
	{
		PIItem _iitem = (PIItem)itm->m_pData;

		CGameObject* GO = _iitem ? smart_cast<CGameObject*>(_iitem) : NULL;
		if (GO)
			funct1(GO->lua_game_object());
	}

	return true;
}

bool CUIActorMenu::OnItemFocusLost(CUICellItem* itm)
{
	if ( itm )
	{
		itm->m_selected = false;
	}
	InfoCurItem( nullptr );
	clear_highlight_lists();

	luabind::functor<bool> funct1;
	if (ai().script_engine().functor("actor_menu_inventory.CUIActorMenu_OnItemFocusLost", funct1))
	{
		PIItem _iitem = (PIItem)itm->m_pData;

		CGameObject* GO = _iitem ? smart_cast<CGameObject*>(_iitem) : NULL;
		if (GO)
			funct1(GO->lua_game_object());
	}

	return true;
}

bool CUIActorMenu::OnItemFocusedUpdate(CUICellItem* itm)
{
	if ( itm )
	{
		itm->m_selected = true;
		if ( m_highlight_clear )
		{
			set_highlight_item( itm );
		}
	}
	VERIFY( m_ItemInfo );
	if ( Device.dwTimeGlobal < itm->FocusReceiveTime() + m_ItemInfo->delay )
	{
		return true; //false
	}
	if ( CUIDragDropListEx::m_drag_item || m_UIPropertiesBox->IsShown() || !m_item_info_view )
	{
		return true;
	}	

	InfoCurItem( itm );
	return true;
}

bool CUIActorMenu::OnMouseAction( float x, float y, EUIMessages mouse_action )
{
	inherited::OnMouseAction( x, y, mouse_action );
	return true; // no click`s
}

bool CUIActorMenu::OnKeyboardAction(int dik, EUIMessages keyboard_action)
{
	InfoCurItem( nullptr );
	if ( is_binded(kDROP, dik) )
	{
		if ( WINDOW_KEY_PRESSED == keyboard_action && CurrentIItem() && !CurrentIItem()->IsQuestItem()
			&& CurrentIItem()->parent_id()==m_pActorInvOwner->object_id() )
		{

			SendEvent_Item_Drop		(CurrentIItem(), m_pActorInvOwner->object_id());
			SetCurrentItem			(nullptr);
		}
		return true;
	}

	if ( is_binded(kSPRINT_TOGGLE, dik) )
	{
		if ( WINDOW_KEY_PRESSED == keyboard_action )
		{
			OnPressUserKey();
		}
		return true;
	}	

	if ( is_binded(kUSE, dik) || is_binded(kINVENTORY, dik) )
	{
		if ( WINDOW_KEY_PRESSED == keyboard_action )
		{
			g_btnHint->Discard();
			HideDialog();
		}
		return true;
	}	

	if ( is_binded(kQUIT, dik) )
	{
		if ( WINDOW_KEY_PRESSED == keyboard_action )
		{
			g_btnHint->Discard();
			HideDialog();
		}
		return true;
	}

#ifdef DEBUG
	if (WINDOW_KEY_PRESSED == keyboard_action)
	{
		{
			if (SDL_SCANCODE_KP_7 == dik && CurrentIItem() && CurrentIItem()->IsUsingCondition())
			{
				CurrentIItem()->ChangeCondition(-0.05f);
				UpdateConditionProgressBars();
				m_pCurrentCellItem->UpdateConditionProgressBar();
			}
			else if (SDL_SCANCODE_KP_8 == dik && CurrentIItem() && CurrentIItem()->IsUsingCondition())
			{
				CurrentIItem()->ChangeCondition(0.05f);
				UpdateConditionProgressBars();
				m_pCurrentCellItem->UpdateConditionProgressBar();
			}
		}
	}
#endif
	if( inherited::OnKeyboardAction(dik,keyboard_action) )return true;

	return false;
}

void CUIActorMenu::OnPressUserKey()
{
	switch ( m_currMenuMode )
	{
	case mmUndefined:		break;
	case mmInventory:		break;
	case mmTrade:			
//		OnBtnPerformTrade( this, 0 );
		break;
	case mmUpgrade:			
		TrySetCurUpgrade();
		break;
	case mmDeadBodySearch:	
		TakeAllFromPartner( this, 0 );
		break;
	default:
		R_ASSERT(0);
		break;
	}
}

void CUIActorMenu::OnBtnExitClicked(CUIWindow* w, void* d)
{
	g_btnHint->Discard();
	HideDialog();
}

void CUIActorMenu::OnMesBoxYes( CUIWindow*, void* )
{
	switch( m_currMenuMode )
	{
	case mmUndefined:
		break;
	case mmInventory:
		break;
	case mmTrade:
		break;
	case mmUpgrade:
		if ( m_repair_mode )
		{
			RepairEffect_CurItem();
			m_repair_mode = false;
		}
		else
		{
			m_pUpgradeWnd->OnMesBoxYes();
		}
		break;
	case mmDeadBodySearch:
		break;
	default:
		R_ASSERT(0);
		break;
	}
	UpdateItemsPlace();
}

void CUIActorMenu::OnMesBoxNo(CUIWindow*, void*)
{
	switch(m_currMenuMode)
	{
	case mmUndefined:
		break;
	case mmInventory:
		break;
	case mmTrade:
		break;
	case mmUpgrade:
		m_repair_mode = false;
		break;
	case mmDeadBodySearch:
		break;
	default:
		R_ASSERT(0);
		break;
	}
	UpdateItemsPlace();
}
