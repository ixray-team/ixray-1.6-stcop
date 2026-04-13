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
#include "../../xrUI/Widgets/UIGamepadLegend.h"
#include "../game_cl_base.h"
#include "../ActorCondition.h"
#include "UIDragDropListEx.h"
#include "UIOutfitSlot.h"
#include "../../xrUI/Widgets/UI3tButton.h"
#include "ui_drop_amount.h"
#include "../WeaponMagazined.h"

#define				INVENTORY_ITEM_XML		"inventory_item.xml"
#define				INVENTORY_XML			"inventory_new.xml"


CUIInventoryWnd::CUIInventoryWnd()
{
	m_iCurrentActiveSlot				= NO_ACTIVE_SLOT;
	UIRank								= nullptr;
	m_pInvOwner							= nullptr;
	Init								();
	SetCurrentItem						(nullptr);

	m_b_need_reinit						= false;
	Show								(false);	
	m_currMenuMode						= mmInventory;
}

void CUIInventoryWnd::Init()
{
	CUIXml								uiXml;
	uiXml.Load(CONFIG_PATH, UI_PATH, INVENTORY_XML);

	inherited::InitBase					(uiXml);

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

	m_ItemInfo							= new CUIItemInfo();
	UIDescrWnd.AttachChild				(m_ItemInfo);
	m_ItemInfo->InitItemInfo			(Fvector2().set(0, 0), Fvector2().set(UIDescrWnd.GetWidth(), UIDescrWnd.GetHeight()), INVENTORY_ITEM_XML);

	AttachChild							(&UIPersonalWnd);
	xml_init.InitFrameWindow			(uiXml, "character_frame_window", 0, &UIPersonalWnd);

	AttachChild							(&UIProgressBack);
	xml_init.InitStatic					(uiXml, "progress_background", 0, &UIProgressBack);

	if (!IsGameTypeSingle())
	{
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

	AttachChild							(&UIOutfitInfo);
	UIOutfitInfo.InitFromXml			(uiXml);

	if (!IsGameTypeSingle())
	{
		UIRankFrame = new CUIStatic (); 
		UIRankFrame->SetAutoDelete(true);
		UIRank = new CUIStatic (); 
		UIRank->SetAutoDelete(true);

		CUIXmlInit::InitStatic(uiXml, "rank", 0, UIRankFrame);
		CUIXmlInit::InitStatic(uiXml, "rank:pic", 0, UIRank);
		AttachChild(UIRankFrame);
		UIRankFrame->AttachChild(UIRank);
	}

	m_pUIBagList						= new CUIDragDropListEx(); UIBagWnd.AttachChild(m_pUIBagList); m_pUIBagList->SetAutoDelete(true);
	CUIXmlInitGame::InitDragDropListEx	(uiXml, "dragdrop_bag", 0, m_pUIBagList);
	BindDragDropListEvents				(m_pUIBagList);

	m_pUIBeltList						= new CUIDragDropListEx(); AttachChild(m_pUIBeltList); m_pUIBeltList->SetAutoDelete(true);
	CUIXmlInitGame::InitDragDropListEx	(uiXml, "dragdrop_belt", 0, m_pUIBeltList);
	BindDragDropListEvents				(m_pUIBeltList);

	inherited::InitSlots				(uiXml);
	
	int cols = m_pUIBeltList->CellsCapacity().x;
	int rows = m_pUIBeltList->CellsCapacity().y;

	m_ArtefactSlotsCount = cols * rows;

	m_ArtefactSlotsHighlight.resize(m_ArtefactSlotsCount, nullptr);
	m_belt_list_over.resize(m_ArtefactSlotsCount, nullptr);

	inherited::InitGamepadSelectors		();

	//pop-up menu
	m_UIPropertiesBox					= new CUIPropertiesBox();
	AttachChild							(m_UIPropertiesBox);
	m_UIPropertiesBox->InitPropertiesBox(Fvector2().set(0,0),Fvector2().set(300,300));
	m_UIPropertiesBox->Hide				();

	AttachChild							(&UIStaticTime);
	xml_init.InitStatic					(uiXml, "time_static", 0, &UIStaticTime);

	UIStaticTime.AttachChild			(&UIStaticTimeString);
	xml_init.InitStatic					(uiXml, "time_static_str", 0, &UIStaticTimeString);

	UIExitButton						= new CUI3tButton();UIExitButton->SetAutoDelete(true);
	AttachChild							(UIExitButton);
	xml_init.Init3tButton				(uiXml, "exit_button", 0, UIExitButton);
	
	CUIXml uiDropAmountXml;
	if (uiDropAmountXml.Load(CONFIG_PATH, UI_PATH, "custom_drop_amount.xml"))
	{
		m_pItemDropAmountWnd = new CUIItemDropAmountWnd();
		m_pItemDropAmountWnd->SetAutoDelete(true);
		m_pItemDropAmountWnd->InitDropAmount(uiDropAmountXml);
	}

	m_gamepad_legend					= UIHelper::CreateGamepadLegend(uiXml, "gamepad_legend", this, false);

	m_highlight_clear = true;
	clear_highlight_lists();
		
	const char* pSelectorTextureName = "ui_inv_item_selector_sec";
	m_pUIBagList->InitSelector(pSelectorTextureName);
	m_pUIBeltList->InitSelector(pSelectorTextureName);

	// Controller mode
	xr_map<xr_string, CUIWindow*> wndPointers;
	wndPointers["BeltList"]				= m_pUIBeltList;
	wndPointers["PistolList"]			= GetSidearmDragDropList();
	wndPointers["AutomaticList"]		= GetPrimaryDragDropList();
	wndPointers["OutfitList"]			= m_pInvList[OUTFIT_SLOT];
	wndPointers["BagList"]				= m_pUIBagList;

	ReadWndSelectorsInfo(uiXml, "ui_c_navi_inventory", m_ui_navigation_lists[mmInventory], wndPointers);
}

void CUIInventoryWnd::Update()
{
	if(m_b_need_reinit)
		InitInventory					();

	CObject* current_entity = Level().CurrentEntity();
	CEntityAlive *pEntityAlive			= current_entity != nullptr ? GetInventoryOwner()->cast_entity_alive() : nullptr;

	if (pEntityAlive) 
	{
		float v = pEntityAlive->conditions().GetHealth()*100.0f;
		UIProgressBarHealth.SetProgressPos		(v);

		v = pEntityAlive->conditions().GetPsyHealth()*100.0f;
		UIProgressBarPsyHealth.SetProgressPos	(v);

		v = pEntityAlive->conditions().GetRadiation()*100.0f;
		UIProgressBarRadiation.SetProgressPos	(v);

		CInventoryOwner* pOurInvOwner	= pEntityAlive != nullptr ? pEntityAlive->cast_inventory_owner() : nullptr;
		u32 _money						= pOurInvOwner->get_money();

		if (!IsGameTypeSingle())
		{
			game_PlayerState* ps = Game().GetPlayerByGameID(pEntityAlive->ID());
			if (ps)
			{
				_money							= ps->money_for_round;
			}
		}
		// update money
		string64						sMoney;
		xr_sprintf						(sMoney,"%d RU", _money);
		UIMoneyWnd.SetText				(sMoney);

		// update outfit parameters
		PIItem inv_item = pOurInvOwner->inventory().m_slots[OUTFIT_SLOT].m_pIItem;
		CCustomOutfit* outfit = inv_item != nullptr ? inv_item->cast_outfit() : nullptr;
		UIOutfitInfo.UpdateInfo			(outfit);		
	}

	UIStaticTimeString.SetText(*InventoryUtilities::GetGameTimeAsString(InventoryUtilities::etpTimeToMinutes));

	inherited::Update					();
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
			if (GameID() != eGameIDDeathmatch) 
			{
				if (1 == team)
					xr_sprintf(_path, "ui_hud_status_green_0%d", rank + 1);
				else
					xr_sprintf(_path, "ui_hud_status_blue_0%d", rank + 1);
			}
			else
			{
				xr_sprintf(_path, "ui_hud_status_green_0%d", rank + 1);
			}
			UIRank->InitTexture(_path);
		}

		SendInfoToActor("ui_inventory");

		Update();
		PlaySnd(eSndOpen);
	}
	else
	{
		PlaySnd(eSndClose);

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
			if (!pActor)			
				return;

			pActor->SetWeaponHideState(INV_STATE_INV_WND, false);
		}
	}
}

void CUIInventoryWnd::UpdateActor()
{	
	if (CActor* actor = GetInventoryOwner()->cast_actor())
	{
		if (CWeapon* wp = actor->inventory().ActiveItem() ? actor->inventory().ActiveItem()->cast_weapon() : nullptr)
		{
			wp->ForceUpdateAmmo();
		}
	}

	InventoryUtilities::UpdateWeight(UIBagWnd, true);
}
