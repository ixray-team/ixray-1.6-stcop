#include "stdafx.h"
#include "pch_script.h"
#include "UIInventoryWnd.h"

#include "../../xrUI/xrUIXmlParser.h"
#include "../../xrUI/UIXmlInit.h"
#include "../../xrEngine/string_table.h"

#include "../Actor.h"
#include "UIGameSP.h"
#include "../HUDManager.h"

#include "../CustomOutfit.h"

#include "../Weapon.h"

#include "../../xrScripts/script_process.h"

#include "../eatable_item.h"
#include "../Inventory.h"

#include "UIInventoryUtilities.h"
using namespace InventoryUtilities;

#include "UIHelperGame.h"
#include "../InfoPortion.h"
#include "../Level.h"
#include "../EntityCondition.h"

#include "../game_cl_base.h"
#include "../ActorCondition.h"
#include "UIDragDropListEx.h"
#include "UIOutfitSlot.h"
#include "../../xrUI/Widgets/UI3tButton.h"
#include "ui_drop_amount.h"
#include "../WeaponMagazined.h"

#define				INVENTORY_ITEM_XML		"inventory_item.xml"
#define				INVENTORY_XML			"inventory_new.xml"


extern void move_item_from_to(u16 from_id, u16 to_id, u16 what_id);

CUIInventoryWnd*	g_pInvWnd = nullptr;

CUIInventoryWnd::CUIInventoryWnd()
{
	m_iCurrentActiveSlot				= NO_ACTIVE_SLOT;
	UIRank								= nullptr;
	m_pInvOwner							= nullptr;
	Init								();
	SetCurrentItem						(nullptr);

	g_pInvWnd							= this;	
	m_b_need_reinit						= false;
	Show								(false);	
}

void CUIInventoryWnd::Init()
{
	CUIXml								uiXml;
	uiXml.Load(CONFIG_PATH, UI_PATH, INVENTORY_XML);

	CUIXmlInit							xml_init;

	xml_init.InitWindow					(uiXml, "main", 0, this);

	AttachChild							(&UIBeltSlots);
	xml_init.InitStatic					(uiXml, "belt_slots", 0, &UIBeltSlots);

	AttachChild							(&UIBack);
	xml_init.InitStatic					(uiXml, "back", 0, &UIBack);

	AttachChild							(&UIStaticBottom);
	xml_init.InitStatic					(uiXml, "bottom_static", 0, &UIStaticBottom);

	AttachChild							(&UIBagWnd);
	xml_init.InitStatic					(uiXml, "bag_static", 0, &UIBagWnd);
	
	AttachChild							(&UIMoneyWnd);
	xml_init.InitStatic					(uiXml, "money_static", 0, &UIMoneyWnd);

	AttachChild							(&UIDescrWnd);
	xml_init.InitStatic					(uiXml, "descr_static", 0, &UIDescrWnd);


	UIDescrWnd.AttachChild				(&UIItemInfo);
	UIItemInfo.InitItemInfo				(Fvector2().set(0, 0), Fvector2().set(UIDescrWnd.GetWidth(), UIDescrWnd.GetHeight()), INVENTORY_ITEM_XML);

	AttachChild							(&UIPersonalWnd);
	xml_init.InitFrameWindow			(uiXml, "character_frame_window", 0, &UIPersonalWnd);

	AttachChild							(&UIProgressBack);
	xml_init.InitStatic					(uiXml, "progress_background", 0, &UIProgressBack);

	if (!IsGameTypeSingle()){
		AttachChild						(&UIProgressBack_rank);
		xml_init.InitStatic				(uiXml, "progress_back_rank", 0, &UIProgressBack_rank);

		UIProgressBack_rank.AttachChild	(&UIProgressBarRank);
		xml_init.InitProgressBar		(uiXml, "progress_bar_rank", 0, &UIProgressBarRank);
		UIProgressBarRank.SetProgressPos(100);

	}
	

	UIProgressBack.AttachChild (&UIProgressBarHealth);
	xml_init.InitProgressBar (uiXml, "progress_bar_health", 0, &UIProgressBarHealth);
	
	UIProgressBack.AttachChild	(&UIProgressBarPsyHealth);
	xml_init.InitProgressBar (uiXml, "progress_bar_psy", 0, &UIProgressBarPsyHealth);

	UIProgressBack.AttachChild	(&UIProgressBarRadiation);
	xml_init.InitProgressBar (uiXml, "progress_bar_radiation", 0, &UIProgressBarRadiation);

	UIPersonalWnd.AttachChild			(&UIStaticPersonal);
	xml_init.InitStatic					(uiXml, "static_personal",0, &UIStaticPersonal);
//	UIStaticPersonal.Init				(1, UIPersonalWnd.GetHeight() - 175, 260, 260);

	AttachChild							(&UIOutfitInfo);
	UIOutfitInfo.InitFromXml			(uiXml);
//.	xml_init.InitStatic					(uiXml, "outfit_info_window",0, &UIOutfitInfo);


	if (!IsGameTypeSingle()){
		UIRankFrame = new CUIStatic (); UIRankFrame->SetAutoDelete(true);
		UIRank = new CUIStatic (); UIRank->SetAutoDelete(true);

		CUIXmlInit::InitStatic(uiXml, "rank", 0, UIRankFrame);
		CUIXmlInit::InitStatic(uiXml, "rank:pic", 0, UIRank);
		AttachChild(UIRankFrame);
		UIRankFrame->AttachChild(UIRank);
	}

	m_pUIBagList						= new CUIDragDropListEx(); UIBagWnd.AttachChild(m_pUIBagList); m_pUIBagList->SetAutoDelete(true);
	CUIXmlInitGame::InitDragDropListEx	(uiXml, "dragdrop_bag", 0, m_pUIBagList);
	BindDragDropListEnents				(m_pUIBagList);

	m_pUIBeltList						= new CUIDragDropListEx(); AttachChild(m_pUIBeltList); m_pUIBeltList->SetAutoDelete(true);
	CUIXmlInitGame::InitDragDropListEx	(uiXml, "dragdrop_belt", 0, m_pUIBeltList);
	BindDragDropListEnents				(m_pUIBeltList);

	m_pUIOutfitList						= new CUIOutfitDragDropList(); AttachChild(m_pUIOutfitList); m_pUIOutfitList->SetAutoDelete(true);
	CUIXmlInitGame::InitDragDropListEx	(uiXml, "dragdrop_outfit", 0, m_pUIOutfitList);
	BindDragDropListEnents				(m_pUIOutfitList);

	m_pUIPistolList						= new CUIDragDropListEx(); AttachChild(m_pUIPistolList); m_pUIPistolList->SetAutoDelete(true);
	CUIXmlInitGame::InitDragDropListEx	(uiXml, "dragdrop_pistol", 0, m_pUIPistolList);
	BindDragDropListEnents				(m_pUIPistolList);

	m_pUIAutomaticList					= new CUIDragDropListEx(); AttachChild(m_pUIAutomaticList); m_pUIAutomaticList->SetAutoDelete(true);
	CUIXmlInitGame::InitDragDropListEx	(uiXml, "dragdrop_automatic", 0, m_pUIAutomaticList);
	BindDragDropListEnents				(m_pUIAutomaticList);

	//pop-up menu
	m_UIPropertiesBox					= new CUIPropertiesBox();
	AttachChild							(m_UIPropertiesBox);
	m_UIPropertiesBox->SetAutoDelete	(true);
	m_UIPropertiesBox->InitPropertiesBox(Fvector2().set(0,0),Fvector2().set(300,300));
	m_UIPropertiesBox->Hide				();

	AttachChild							(&UIStaticTime);
	xml_init.InitStatic					(uiXml, "time_static", 0, &UIStaticTime);

	UIStaticTime.AttachChild			(&UIStaticTimeString);
	xml_init.InitStatic					(uiXml, "time_static_str", 0, &UIStaticTimeString);

	UIExitButton						= new CUI3tButton();UIExitButton->SetAutoDelete(true);
	AttachChild							(UIExitButton);
	xml_init.Init3tButton				(uiXml, "exit_button", 0, UIExitButton);
	
	m_pItemDropAmountWnd				= new CUIItemDropAmountWnd();
	m_pItemDropAmountWnd->SetAutoDelete	(true);
	m_pItemDropAmountWnd->InitDropAmount();

//Load sounds

	XML_NODE* stored_root				= uiXml.GetLocalRoot		();
	uiXml.SetLocalRoot					(uiXml.NavigateToNode		("action_sounds",0));
	::Sound->create						(sounds[eInvSndOpen],		uiXml.Read("snd_open",			0,	nullptr),st_Effect,sg_SourceType);
	::Sound->create						(sounds[eInvSndClose],		uiXml.Read("snd_close",			0,	nullptr),st_Effect,sg_SourceType);
	::Sound->create						(sounds[eInvItemToSlot],	uiXml.Read("snd_item_to_slot",	0,	nullptr),st_Effect,sg_SourceType);
	::Sound->create						(sounds[eInvItemToBelt],	uiXml.Read("snd_item_to_belt",	0,	nullptr),st_Effect,sg_SourceType);
	::Sound->create						(sounds[eInvItemToRuck],	uiXml.Read("snd_item_to_ruck",	0,	nullptr),st_Effect,sg_SourceType);
	::Sound->create						(sounds[eInvProperties],	uiXml.Read("snd_properties",	0,	nullptr),st_Effect,sg_SourceType);
	::Sound->create						(sounds[eInvDropItem],		uiXml.Read("snd_drop_item",		0,	nullptr),st_Effect,sg_SourceType);
	::Sound->create						(sounds[eInvAttachAddon],	uiXml.Read("snd_attach_addon",	0,	nullptr),st_Effect,sg_SourceType);
	::Sound->create						(sounds[eInvDetachAddon],	uiXml.Read("snd_detach_addon",	0,	nullptr),st_Effect,sg_SourceType);
	::Sound->create						(sounds[eInvItemUse],		uiXml.Read("snd_item_use",		0,	nullptr),st_Effect,sg_SourceType);

	uiXml.SetLocalRoot					(stored_root);
	m_highlight_clear = true;
	clear_highlight_lists();
}

EListType CUIInventoryWnd::GetType(CUIDragDropListEx* l)
{
	if(l==m_pUIBagList)			return iwBag;
	if(l==m_pUIBeltList)		return iwBelt;

	if(l==m_pUIAutomaticList)	return iwSlot;
	if(l==m_pUIPistolList)		return iwSlot;
	if(l==m_pUIOutfitList)		return iwSlot;

	NODEFAULT;
#ifdef DEBUG
	return iwSlot;
#endif // DEBUG
}

void CUIInventoryWnd::PlaySnd(eInventorySndAction a)
{
	if (sounds[a].handle())
		sounds[a].play					(nullptr, sm_2D);
}

CUIInventoryWnd::~CUIInventoryWnd()
{
//.	ClearDragDrop(m_vDragDropItems);
	ClearAllLists						();
}

bool CUIInventoryWnd::OnMouseAction(float x, float y, EUIMessages mouse_action)
{
	if (m_b_need_reinit)
		return true;

	//вызов дополнительного меню по правой кнопке
	if (mouse_action == WINDOW_RBUTTON_DOWN)
	{
		if (m_UIPropertiesBox->IsShown())
		{
			m_UIPropertiesBox->Hide		();
			return						true;
		}
	}

	CUIWindow::OnMouseAction					(x, y, mouse_action);

	return true; // always returns true, because ::StopAnyMove() == true;
}

void CUIInventoryWnd::Draw()
{
	CUIWindow::Draw						();
}


void CUIInventoryWnd::Update()
{
	if(m_b_need_reinit)
		InitInventory					();

	CObject* current_entity = Level().CurrentEntity();
	CEntityAlive *pEntityAlive			= current_entity != nullptr ? current_entity->cast_entity_alive() : nullptr;

	if(pEntityAlive) 
	{
		float v = pEntityAlive->conditions().GetHealth()*100.0f;
		UIProgressBarHealth.SetProgressPos		(v);

		v = pEntityAlive->conditions().GetPsyHealth()*100.0f;
		UIProgressBarPsyHealth.SetProgressPos	(v);

		v = pEntityAlive->conditions().GetRadiation()*100.0f;
		UIProgressBarRadiation.SetProgressPos	(v);

		CInventoryOwner* pOurInvOwner	= pEntityAlive != nullptr ? pEntityAlive->cast_inventory_owner() : nullptr;
		u32 _money						= 0;

		if (!IsGameTypeSingle()){
			game_PlayerState* ps = Game().GetPlayerByGameID(pEntityAlive->ID());
			if (ps){
				//UIProgressBarRank.SetProgressPos(ps->experience_D*100);
				_money							= ps->money_for_round;
			}
		}else
		{
			_money							= pOurInvOwner->get_money();
		}
		// update money
		string64						sMoney;
		sprintf_s							(sMoney,"%d RU", _money);
		UIMoneyWnd.SetText				(sMoney);

		// update outfit parameters
		PIItem inv_item = pOurInvOwner->inventory().m_slots[OUTFIT_SLOT].m_pIItem;
		CCustomOutfit* outfit = inv_item != nullptr ? inv_item->cast_outfit() : nullptr;
		UIOutfitInfo.UpdateInfo			(outfit);		
	}

	UIStaticTimeString.SetText(*InventoryUtilities::GetGameTimeAsString(InventoryUtilities::etpTimeToMinutes));

	CUIWindow::Update					();
}

void CUIInventoryWnd::Show(bool status) 
{ 
	inherited::Show			(status);
	if (status)
	{
		InitInventory();

		if (!IsGameTypeSingle())
		{
			CObject* current_entity = Level().CurrentEntity();
			CActor* pActor = current_entity != nullptr ? current_entity->cast_actor() : nullptr;
			if (!pActor) return;

			pActor->SetWeaponHideState(INV_STATE_INV_WND, true);

			//rank icon		
			int team = Game().local_player->team;
			int rank = Game().local_player->rank;
			string256 _path;
			if (GameID() != eGameIDDeathmatch) {
				if (1 == team)
					sprintf_s(_path, "ui_hud_status_green_0%d", rank + 1);
				else
					sprintf_s(_path, "ui_hud_status_blue_0%d", rank + 1);
			}
			else
			{
				sprintf_s(_path, "ui_hud_status_green_0%d", rank + 1);
			}
			UIRank->InitTexture(_path);
		}

		SendInfoToActor("ui_inventory");

		Update();
		PlaySnd(eInvSndOpen);
	}
	else
	{
		PlaySnd(eInvSndClose);

		SendInfoToActor("ui_inventory_hide");
		ClearAllLists();

		//достать вещь в активный слот
		CObject* current_entity = Level().CurrentEntity();
		CActor* pActor = current_entity != nullptr ? current_entity->cast_actor() : nullptr;
		if (pActor && m_iCurrentActiveSlot != NO_ACTIVE_SLOT &&
			pActor->inventory().m_slots[m_iCurrentActiveSlot].m_pIItem)
		{
			pActor->inventory().Activate(m_iCurrentActiveSlot);
			m_iCurrentActiveSlot = NO_ACTIVE_SLOT;
		}

		if (!IsGameTypeSingle())
		{
			if (!pActor)			return;

			pActor->SetWeaponHideState(INV_STATE_INV_WND, false);
		}
	}
}

void CUIInventoryWnd::AttachAddon(PIItem item_to_upgrade)
{
	PlaySnd										(eInvAttachAddon);
	R_ASSERT									(item_to_upgrade);
	if (OnClient())
	{
		NET_Packet								P;
		item_to_upgrade->object().u_EventGen	(P, GE_ADDON_ATTACH, item_to_upgrade->object().ID());
		P.w_u32									(CurrentIItem()->object().ID());
		item_to_upgrade->object().u_EventSend	(P);
	};

	item_to_upgrade->Attach						(CurrentIItem(), true);


	//спрятать вещь из активного слота в инвентарь на время вызова менюшки
	CObject* current_entity = Level().CurrentEntity();
	CActor* pActor = current_entity != nullptr ? current_entity->cast_actor() : nullptr;
	if(pActor && item_to_upgrade == pActor->inventory().ActiveItem())
	{
			m_iCurrentActiveSlot				= pActor->inventory().GetActiveSlot();
			pActor->inventory().Activate		(NO_ACTIVE_SLOT);
	}
	SetCurrentItem								(nullptr);
}

void CUIInventoryWnd::DetachAddon(LPCSTR addon_name, PIItem itm)
{
	PlaySnd										(eInvDetachAddon);
	if (OnClient())
	{
		NET_Packet								P;
		if(itm==nullptr)
			CGameObject::u_EventGen				(P, GE_ADDON_DETACH, CurrentIItem()->object().ID());
		else
			CGameObject::u_EventGen				(P, GE_ADDON_DETACH, itm->object().ID());

		P.w_stringZ								(addon_name);
		CGameObject::u_EventSend				(P);
		return;
	}
	if(itm==nullptr)
		CurrentIItem()->Detach					(addon_name, true);
	else
		itm->Detach								(addon_name, true);
}

void CUIInventoryWnd::UnloadWeapon(CWeaponMagazined* pWpn)
{
	if (!pWpn) return;

	if (IsGameTypeSingle())
	{
		pWpn->UnloadMagazine();
	}
	else
	{
		NET_Packet	P;
		CGameObject::u_EventGen(P, GE_WPN_UNLOAD_AMMO, pWpn->ID());
		P.w_u8(0);
		CGameObject::u_EventSend(P);
	}
}

void CUIInventoryWnd::SendEvent_ActivateSlot(u16 slot, u16 recipient)
{
	NET_Packet						P;
	CGameObject::u_EventGen			(P, GEG_PLAYER_ACTIVATE_SLOT, recipient);
	P.w_u16							(slot);
	CGameObject::u_EventSend		(P);
	clear_highlight_lists			();
}

void CUIInventoryWnd::SendEvent_Item2Slot(PIItem pItem, u16 recipient, u16 slot_id)
{
	if(pItem->parent_id()!=recipient)
		move_item_from_to			(pItem->parent_id(), recipient, pItem->object_id());

	NET_Packet						P;
	CGameObject::u_EventGen			(P, GEG_PLAYER_ITEM2SLOT, pItem->object().H_Parent()->ID());
	P.w_u16							(pItem->object().ID());
	P.w_u16							(slot_id);
	CGameObject::u_EventSend		(P);
	clear_highlight_lists			();

	PlaySnd							(eInvItemToSlot);
};

void CUIInventoryWnd::SendEvent_Item2Belt(PIItem pItem, u16 recipient)
{
	if(pItem->parent_id()!=recipient)
		move_item_from_to			(pItem->parent_id(), recipient, pItem->object_id());

	NET_Packet						P;
	CGameObject::u_EventGen			(P, GEG_PLAYER_ITEM2BELT, pItem->object().H_Parent()->ID());
	P.w_u16							(pItem->object().ID());
	CGameObject::u_EventSend		(P);
	clear_highlight_lists			();

	PlaySnd							(eInvItemToBelt);
};

void	CUIInventoryWnd::SendEvent_Item2Ruck			(PIItem	pItem, u16 recipient)
{
	if(pItem->parent_id()!=recipient)
		move_item_from_to			(pItem->parent_id(), recipient, pItem->object_id());

	NET_Packet						P;
	CGameObject::u_EventGen			(P, GEG_PLAYER_ITEM2RUCK, pItem->object().H_Parent()->ID());
	P.w_u16							(pItem->object().ID());
	CGameObject::u_EventSend		(P);
	clear_highlight_lists			();

	PlaySnd							(eInvItemToRuck);
};

void CUIInventoryWnd::SendEvent_Item_Drop(PIItem pItem, u16 recipient)
{
	R_ASSERT(pItem->parent_id()==recipient);
	if (!IsGameTypeSingle())
		pItem->DenyTrade();
	//pItem->SetDropManual			(TRUE);
	NET_Packet					P;
	pItem->object().u_EventGen	(P,GE_OWNERSHIP_REJECT,pItem->parent_id());
	P.w_u16						(pItem->object().ID());
	pItem->object().u_EventSend	(P);
	PlaySnd						(eInvDropItem);
	clear_highlight_lists			();
}

void CUIInventoryWnd::SendEvent_Item_Eat(PIItem pItem, u16 recipient)
{
	if(pItem->parent_id()!=recipient)
		move_item_from_to			(pItem->parent_id(), recipient, pItem->object_id());

	NET_Packet						P;
	CGameObject::u_EventGen			(P, GEG_PLAYER_ITEM_EAT, recipient);
	P.w_u16							(pItem->object().ID());
	CGameObject::u_EventSend		(P);
	clear_highlight_lists			();
};


void CUIInventoryWnd::BindDragDropListEnents(CUIDragDropListEx* lst)
{
	lst->m_f_item_drop				= CUIDragDropListEx::DRAG_CELL_EVENT(this,&CUIInventoryWnd::OnItemDrop);
	lst->m_f_item_start_drag		= CUIDragDropListEx::DRAG_CELL_EVENT(this,&CUIInventoryWnd::OnItemStartDrag);
	lst->m_f_item_db_click			= CUIDragDropListEx::DRAG_CELL_EVENT(this,&CUIInventoryWnd::OnItemDbClick);
	lst->m_f_item_selected			= CUIDragDropListEx::DRAG_CELL_EVENT(this,&CUIInventoryWnd::OnItemSelected);
	lst->m_f_item_rbutton_click		= CUIDragDropListEx::DRAG_CELL_EVENT(this,&CUIInventoryWnd::OnItemRButtonClick);
	lst->m_f_item_focus_received	= CUIDragDropListEx::DRAG_CELL_EVENT(this,&CUIInventoryWnd::OnItemFocusReceive);
	lst->m_f_item_focus_lost		= CUIDragDropListEx::DRAG_CELL_EVENT(this,&CUIInventoryWnd::OnItemFocusLost);
	lst->m_f_item_focused_update	= CUIDragDropListEx::DRAG_CELL_EVENT(this,&CUIInventoryWnd::OnItemFocusedUpdate);
}


#include "../../xrEngine/xr_level_controller.h"

bool CUIInventoryWnd::OnKeyboardAction(int dik, EUIMessages keyboard_action)
{
	if(m_b_need_reinit)
		return true;

	if (m_UIPropertiesBox->GetVisible())
		m_UIPropertiesBox->OnKeyboardAction(dik, keyboard_action);

	if ( is_binded(kDROP, dik) )
	{
		if(WINDOW_KEY_PRESSED==keyboard_action)
			DropCurrentItem(false);
		return true;
	}

	if (WINDOW_KEY_PRESSED == keyboard_action)
	{
#ifdef DEBUG
		if(SDL_SCANCODE_KP_7 == dik && CurrentIItem())
		{
			CurrentIItem()->ChangeCondition(-0.05f);
			UIItemInfo.InitItem(CurrentItem());
		}
		else if(SDL_SCANCODE_KP_8 == dik && CurrentIItem())
		{
			CurrentIItem()->ChangeCondition(0.05f);
			UIItemInfo.InitItem(CurrentItem());
		}
#endif
	}
	if( inherited::OnKeyboardAction(dik,keyboard_action) )return true;

	return false;
}
