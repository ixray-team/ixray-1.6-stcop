#include "StdAfx.h"
#include "UIActorMenu.h"
#include "../../xrUI/UIXmlInit.h"
#include "../../xrUI/xrUIXmlParser.h"
#include "UICharacterInfo.h"
#include "UIDragDropListEx.h"
#include "UIDragDropReferenceList.h"
#include "UIActorStateInfo.h"
#include "UIItemInfo.h"
#include "../../xrUI/Widgets/UIFrameLineWnd.h"
#include "UIMessageBoxEx.h"
#include "../../xrUI/Widgets/UIPropertiesBox.h"
#include "../../xrUI/Widgets/UI3tButton.h"
#include "../../xrEngine/xr_input.h"
#include "UIInventoryUpgradeWnd.h"
#include "UIInvUpgradeInfo.h"
#include "InventorySorter.h"

#include "ai_space.h"
#include "alife_simulator.h"
#include "object_broker.h"
#include "../../xrUI/Widgets/UIWndCallback.h"
#include "UIHelperGame.h"
#include "../../xrUI/Widgets/UIItemStateDisplay.h"
#include "../../xrUI/Widgets/UITabControl.h"
#include "../../xrUI/ui_base.h"
#include "../../xrEngine/string_table.h"
#include "ui_drop_amount.h"
#include "../xrUI/Widgets/UIProgressBar.h"

CUIActorMenu::CUIActorMenu()
{
	LoadCallbackGlobals(m_isCanDisassembleItem, m_onCanDisassembleItem, "OnCanDisassembleItem");
	LoadCallbackGlobals(m_isQuestionDisassembleItem, m_onQuestionDisassembleItem, "OnQuestionDisassembleItem");
	LoadCallbackGlobals(m_isEffectDisassemble, m_onEffectDisassemble, "OnEffectDisassemble");

	Construct						();
}

CUIActorMenu::~CUIActorMenu()
{
	xr_delete			(m_message_box_yes_no);
	xr_delete			(m_message_box_ok);
	xr_delete			(m_UIPropertiesBox);
	xr_delete			(m_hint_wnd);
	xr_delete			(m_ItemInfo);
}

void CUIActorMenu::Construct()
{
	CUIXml								uiXml;
	uiXml.Load							(CONFIG_PATH, UI_PATH, "actor_menu.xml");

	inherited::InitBase					(uiXml);

	CUIXmlInit							xml_init;

	xml_init.InitWindow					(uiXml, "main", 0, this);
	m_hint_wnd = UIHelper::CreateHint	(uiXml, "hint_wnd");

	m_LeftBackground					= new CUIStatic();
	m_LeftBackground->SetAutoDelete		(true);
	AttachChild							(m_LeftBackground);
	xml_init.InitStatic					(uiXml, "left_background", 0, m_LeftBackground);

	m_pUpgradeWnd						= new CUIInventoryUpgradeWnd(); 
	AttachChild							(m_pUpgradeWnd);
	m_pUpgradeWnd->SetAutoDelete		(true);
	m_pUpgradeWnd->Init					();

	m_ActorCharacterInfo				= new CUICharacterInfo();
	m_ActorCharacterInfo->SetAutoDelete	(true);
	AttachChild							(m_ActorCharacterInfo);
	m_ActorCharacterInfo->InitCharacterInfo(&uiXml, "actor_ch_info");

	m_PartnerCharacterInfo				= new CUICharacterInfo();
	m_PartnerCharacterInfo->SetAutoDelete(true);
	AttachChild							(m_PartnerCharacterInfo);
	m_PartnerCharacterInfo->InitCharacterInfo( &uiXml, "partner_ch_info" );
	
	m_RightDelimiter			= UIHelper::CreateStatic(uiXml, "right_delimiter", this);

	if (uiXml.NavigateToNode("right_delimiter:trade_caption", 0))
	{
		m_ActorTradeCaption = UIHelper::CreateStatic(uiXml, "right_delimiter:trade_caption", m_RightDelimiter);
		m_ActorTradeCaption->AdjustWidthToText();
	}
	m_ActorTradePrice			= UIHelper::CreateStatic(uiXml, "right_delimiter:trade_price", m_RightDelimiter);
	m_ActorTradeWeightMax		= UIHelper::CreateStatic(uiXml, "right_delimiter:trade_weight_max", m_RightDelimiter);
	
	m_LeftDelimiter				= UIHelper::CreateStatic(uiXml, "left_delimiter", this);

	if (uiXml.NavigateToNode("left_delimiter:trade_caption", 0))
	{
		m_PartnerTradeCaption		= UIHelper::CreateStatic(uiXml, "left_delimiter:trade_caption", m_LeftDelimiter);
		m_PartnerTradeCaption->AdjustWidthToText();
	}
	m_PartnerTradePrice			= UIHelper::CreateStatic(uiXml, "left_delimiter:trade_price", m_LeftDelimiter);
	m_PartnerTradeWeightMax		= UIHelper::CreateStatic(uiXml, "left_delimiter:trade_weight_max", m_LeftDelimiter);

	InitActorWeightSection				(uiXml, xml_init);
	InitActorVolumeSection				(uiXml, xml_init);
	m_ActorBottomInfo->AdjustWidthToText();

	m_PartnerBottomInfo			= UIHelper::CreateStatic(uiXml, "partner_weight_caption", this);
	m_PartnerWeight				= UIHelper::CreateStatic(uiXml, "partner_weight", this);
	m_PartnerBottomInfo->AdjustWidthToText();
	m_PartnerWeight_end_x		= m_PartnerWeight->GetWndPos().x;

	if (uiXml.NavigateToNode("quick_slot_highlight"))
	{
		m_QuickSlotsHighlight[0] = UIHelper::CreateStatic(uiXml, "quick_slot_highlight", this);
		m_QuickSlotsHighlight[0]->Show(false);
	}
	inherited::InitSlots		(uiXml);

	m_pInventoryBagList			= UIHelperGame::CreateDragDropListEx(uiXml, "dragdrop_bag", this);
	m_pInventoryBeltList		= UIHelperGame::CreateDragDropListEx(uiXml, "dragdrop_belt", this);

	m_pTradeActorList			= UIHelperGame::CreateDragDropListEx(uiXml, "dragdrop_actor_trade", this);
	m_pTradeActorBagList		= UIHelperGame::CreateDragDropListEx(uiXml, "dragdrop_actor_trade_bag", this);
	m_pTradePartnerList			= UIHelperGame::CreateDragDropListEx(uiXml, "dragdrop_partner_trade", this);
	m_pTradePartnerBagList		= UIHelperGame::CreateDragDropListEx(uiXml, "dragdrop_partner_bag", this);
	m_pDeadBodyBagList			= UIHelperGame::CreateDragDropListEx(uiXml, "dragdrop_deadbody_bag", this);
	if (uiXml.NavigateToNode("dragdrop_stack"))
	{
		m_pTradeActorBagList->SetConditionProgBarVisibility(false);
		m_pInventoryBagList->SetConditionProgBarVisibility(false);
		m_pInventoryStackList = UIHelperGame::CreateDragDropListEx(uiXml, "dragdrop_stack", this);
	}
	if (uiXml.NavigateToNode("dragdrop_quick_slots"))
	{
		m_pQuickSlot = UIHelperGame::CreateDragDropReferenceList(uiXml, "dragdrop_quick_slots", this);
		m_pQuickSlot->Initialize();
	}

	float dx = 0;
	float dy = 0;
	Fvector2 pos;
    if (m_QuickSlotsHighlight[0])
    {
        pos = m_QuickSlotsHighlight[0]->GetWndPos();
        dx = uiXml.ReadAttribFlt("quick_slot_highlight", 0, "dx", 0.0f);
        dy = uiXml.ReadAttribFlt("quick_slot_highlight", 0, "dy", 0.0f);

        for (u8 i = 1; i < 4; i++)
        {
            pos.x += dx;
            pos.y += dy;
            m_QuickSlotsHighlight[i] = UIHelper::CreateStatic(uiXml, "quick_slot_highlight", this);
            m_QuickSlotsHighlight[i]->SetWndPos(pos);
            m_QuickSlotsHighlight[i]->Show(false);
        }
    }

	int cols = m_pInventoryBeltList->CellsCapacity().x;
	int rows = m_pInventoryBeltList->CellsCapacity().y;

	m_ArtefactSlotsCount = cols * rows;

	m_ArtefactSlotsHighlight.resize(m_ArtefactSlotsCount, nullptr);
	m_belt_list_over.resize(m_ArtefactSlotsCount, nullptr);

	int counter = 1;

	if (m_ArtefactSlotsHighlight[0])
	{
		for (u8 i = 0; i < rows; ++i)
		{
			for (u8 j = 0; j < cols; ++j)
			{
				if (i == 0 && j == 0)
				{
					m_ArtefactSlotsHighlight[0] = UIHelper::CreateStatic(uiXml, "artefact_slot_highlight", this);
					pos = m_ArtefactSlotsHighlight[0]->GetWndPos();
					m_ArtefactSlotsHighlight[0]->Show(false);
					dx = uiXml.ReadAttribFlt("artefact_slot_highlight", 0, "dx", 24.0f);
					dy = uiXml.ReadAttribFlt("artefact_slot_highlight", 0, "dy", 24.0f);
				}
				else
				{
					if (j != 0)
						pos.x += dx;

					m_ArtefactSlotsHighlight[counter] = UIHelper::CreateStatic(uiXml, "artefact_slot_highlight", this);
					m_ArtefactSlotsHighlight[counter]->SetWndPos(pos);
					m_ArtefactSlotsHighlight[counter]->Show(false);
					counter++;
				}
			}

			pos.x = m_ArtefactSlotsHighlight[0]->GetWndPos().x;
			pos.y += dy;
		}
	}

	const char* pSelectorTextureName = "ui_inv_item_selector_sec";
	m_pInventoryBagList->InitSelector(pSelectorTextureName);
	m_pInventoryBeltList->InitSelector(pSelectorTextureName);
	m_pTradeActorBagList->InitSelector(pSelectorTextureName);
	m_pTradeActorList->InitSelector(pSelectorTextureName);
	m_pTradePartnerBagList->InitSelector(pSelectorTextureName);
	m_pTradePartnerList->InitSelector(pSelectorTextureName);
	m_pDeadBodyBagList->InitSelector(pSelectorTextureName);
	//m_pQuickSlot->InitSelector(pSelectorTextureName);

	m_pTrashList				= UIHelperGame::CreateDragDropListEx		(uiXml, "dragdrop_trash", this);
	m_pTrashList->m_f_item_drop	= CUIDragDropListEx::DRAG_CELL_EVENT	(this,&CUIActorMenu::OnItemDrop);
	m_pTrashList->m_f_drag_event= CUIDragDropListEx::DRAG_ITEM_EVENT	(this,&CUIActorMenu::OnDragItemOnTrash);

	counter = 1;

	for (u8 i = 0; i < rows; ++i)
	{
		for (u8 j = 0; j < cols; ++j)
		{
			if (i == 0 && j == 0)
			{
				m_belt_list_over[0] = UIHelper::CreateStatic(uiXml, "belt_list_over", this);
				pos = m_belt_list_over[0]->GetWndPos();
				dx = uiXml.ReadAttribFlt("belt_list_over", 0, "dx", 10.0f);
				dy = uiXml.ReadAttribFlt("belt_list_over", 0, "dy", 10.0f);
			}
			else
			{
				if (j != 0)
					pos.x += dx;

				m_belt_list_over[counter] = UIHelper::CreateStatic(uiXml, "belt_list_over", this);
				m_belt_list_over[counter]->SetWndPos(pos);
				counter++;
			}
		}

		pos.x = m_belt_list_over[0]->GetWndPos().x;
		pos.y += dy;
	}

	if (uiXml.NavigateToNode("helmet_over"))
	{
		m_HelmetOver = UIHelper::CreateStatic(uiXml, "helmet_over", this);
		m_HelmetOver->Show(false);
	}

	m_ActorMoney	= UIHelper::CreateStatic(uiXml, "actor_money_static", this);
	m_PartnerMoney	= UIHelper::CreateStatic(uiXml, "partner_money_static", this);

	if (uiXml.NavigateToNode("quick_slot1_text"))
		m_QuickSlot1	= UIHelper::CreateStatic(uiXml, "quick_slot1_text", this);

	if (uiXml.NavigateToNode("quick_slot2_text"))
		m_QuickSlot2	= UIHelper::CreateStatic(uiXml, "quick_slot2_text", this);

	if (uiXml.NavigateToNode("quick_slot3_text"))
		m_QuickSlot3	= UIHelper::CreateStatic(uiXml, "quick_slot3_text", this);

	if (uiXml.NavigateToNode("quick_slot4_text"))
		m_QuickSlot4	= UIHelper::CreateStatic(uiXml, "quick_slot4_text", this);

	if (uiXml.NavigateToNode("trade_button", 0))
		m_trade_button = UIHelper::Create3tButton(uiXml, "trade_button", this);

	if (uiXml.NavigateToNode("trade_buy_button", 0))
		m_trade_buy_button	= UIHelper::Create3tButton(uiXml, "trade_buy_button", this);

	if (uiXml.NavigateToNode("trade_sell_button", 0))
		m_trade_sell_button	= UIHelper::Create3tButton(uiXml, "trade_sell_button", this);

	m_takeall_button	= UIHelper::Create3tButton(uiXml, "takeall_button", this);

	if (uiXml.NavigateToNode("putall_button", 0))
	{
		m_putall_button = UIHelper::Create3tButton(uiXml, "putall_button", this);
	}
	
	m_exit_button		= UIHelper::Create3tButton(uiXml, "exit_button", this);

	if (uiXml.NavigateToNode("clock_value", 0))
	m_clock_value						= UIHelper::CreateStatic(uiXml, "clock_value", this);

/*
	m_pDeadBodyBagList					= new CUIDragDropListEx(); 
	AttachChild							(m_pDeadBodyBagList);
	m_pDeadBodyBagList->SetAutoDelete	(true);
	xml_init.InitDragDropListEx			(uiXml, "dragdrop_deadbody_bag", 0, m_pDeadBodyBagList);
*/
	m_ActorStateInfo					= new ui_actor_state_wnd();
	m_ActorStateInfo->init_from_xml		(uiXml, "actor_state_info");
	m_ActorStateInfo->SetAutoDelete		(true);
	AttachChild							(m_ActorStateInfo); 
	
	inherited::InitGamepadSelectors		();

	m_ItemInfo							= new CUIItemInfo();
//-	m_ItemInfo->SetAutoDelete			(true);
//-	AttachChild							(m_ItemInfo);
	m_ItemInfo->InitItemInfo			("actor_menu_item.xml");

	m_upgrade_info						= nullptr;
	if ( ai().get_alife() )
	{
		m_upgrade_info						= new UIInvUpgradeInfo();
		m_upgrade_info->SetAutoDelete		(true);
		AttachChild							(m_upgrade_info);
		m_upgrade_info->init_from_xml		("actor_menu_item.xml");
	}

	CUIXml uiDropAmountXml;
	if (uiDropAmountXml.Load(CONFIG_PATH, UI_PATH, "custom_drop_amount.xml"))
	{
		m_pItemDropAmountWnd = new CUIItemDropAmountWnd();
		m_pItemDropAmountWnd->SetAutoDelete(true);
		m_pItemDropAmountWnd->InitDropAmount(uiDropAmountXml);
	}

	m_pInventorySorter					= new CInventorySorter();

	const struct SSortTabsLayoutNode
	{
		ESortTabsLayoutSlot slot;
		const char* node;
		const char* windowName;
	};

	const SSortTabsLayoutNode layoutNodesCategories[] = {
		{ eSortTabsInventory, "inventory_sort_tabs", "inventory_sort_tabs" },
		{ eSortTabsUpgrade, "inventory_sort_tabs_container_upgrade", "inventory_sort_tabs_container_upgrade" },
		{ eSortTabsTradeActor, "inventory_sort_tabs_container_trade_actor_bag", "inventory_sort_tabs_container_trade_actor_bag" },
		{ eSortTabsTradePartner, "inventory_sort_tabs_container_trade_partner_bag", "inventory_sort_tabs_container_trade_partner_bag" },
		{ eSortTabsDeadBody, "inventory_sort_tabs_container_deadbody_bag", "inventory_sort_tabs_container_deadbody_bag" }
	};

	const SSortTabsLayoutNode layoutNodesOrdering[] = {
		{ eSortTabsInventory, "inventory_sort_order_tabs", "inventory_sort_order_tabs" },
		{ eSortTabsUpgrade, "inventory_sort_order_tabs_container_upgrade", "inventory_sort_order_tabs_container_upgrade" },
		{ eSortTabsTradeActor, "inventory_sort_order_tabs_container_trade_actor_bag", "inventory_sort_order_tabs_container_trade_actor_bag" },
		{ eSortTabsTradePartner, "inventory_sort_order_tabs_container_trade_partner_bag", "inventory_sort_order_tabs_container_trade_partner_bag" },
		{ eSortTabsDeadBody, "inventory_sort_order_tabs_container_deadbody_bag", "inventory_sort_order_tabs_container_deadbody_bag" }
	};

	const struct SSortTabsSystemInit
	{
		EInventorySortSystem system;
		u8 systemIndex;
		const char* baseSortTabsNode;
		const SSortTabsLayoutNode* layoutNodes;
		u32 layoutNodesCount;
	};

	const SSortTabsSystemInit systemsToInit[] = {
		{
			EInventorySortSystem::Categories,
			0,
			"inventory_sort_tabs",
			layoutNodesCategories,
			static_cast<u32>(std::size(layoutNodesCategories))
		},
		{
			EInventorySortSystem::Ordering,
			1,
			"inventory_sort_order_tabs",
			layoutNodesOrdering,
			static_cast<u32>(std::size(layoutNodesOrdering))
		}
	};

	for (const SSortTabsSystemInit& systemInit : systemsToInit)
	{
		if (!uiXml.NavigateToNode(systemInit.baseSortTabsNode, 0))
		{
			continue;
		}

		for (u32 layoutIndex = 0; layoutIndex < systemInit.layoutNodesCount; ++layoutIndex)
		{
			const SSortTabsLayoutNode& layoutNode = systemInit.layoutNodes[layoutIndex];
			const u8 systemIndex = systemInit.systemIndex;

			if (systemInit.system == EInventorySortSystem::Ordering)
			{
				m_orderMode[layoutNode.slot] = EInventoryOrderMode::General;
				m_orderModeId[layoutNode.slot] = "";
				m_orderOptions[layoutNode.slot] = {};
				m_orderOptions[layoutNode.slot].weightDesc = m_pInventorySorter->IsWeightDescending();
				m_orderOptions[layoutNode.slot].conditionDesc = m_pInventorySorter->IsConditionDescending();
				m_orderOptions[layoutNode.slot].costDesc = m_pInventorySorter->IsCostDescending();
				m_orderOptions[layoutNode.slot].noveltyDesc = m_pInventorySorter->IsNoveltyDescending();
				m_orderOptions[layoutNode.slot].typeCycle = 0;
			}
			else
			{
				m_sortCategory[layoutNode.slot] = EInventorySortCategory::All;
				m_sortCategoryId[layoutNode.slot] = "";
			}

			if (layoutNode.slot == eSortTabsInventory)
			{
				m_sortTabsLayoutPos[systemIndex][layoutNode.slot] = Fvector2().set(
					uiXml.ReadAttribFlt(systemInit.baseSortTabsNode, 0, "x", 0.0f),
					uiXml.ReadAttribFlt(systemInit.baseSortTabsNode, 0, "y", 0.0f));
				m_sortTabsLayoutSize[systemIndex][layoutNode.slot] = Fvector2().set(
					uiXml.ReadAttribFlt(systemInit.baseSortTabsNode, 0, "width", 0.0f),
					uiXml.ReadAttribFlt(systemInit.baseSortTabsNode, 0, "height", 0.0f));
				m_sortTabsLayoutDefined[systemIndex][layoutNode.slot] = true;
			}
			else if (uiXml.NavigateToNode(layoutNode.node, 0))
			{
				m_sortTabsLayoutPos[systemIndex][layoutNode.slot].x = uiXml.ReadAttribFlt(layoutNode.node, 0, "x", 0.0f);
				m_sortTabsLayoutPos[systemIndex][layoutNode.slot].y = uiXml.ReadAttribFlt(layoutNode.node, 0, "y", 0.0f);
				m_sortTabsLayoutSize[systemIndex][layoutNode.slot].x = uiXml.ReadAttribFlt(layoutNode.node, 0, "width", 0.0f);
				m_sortTabsLayoutSize[systemIndex][layoutNode.slot].y = uiXml.ReadAttribFlt(layoutNode.node, 0, "height", 0.0f);
				m_sortTabsLayoutDefined[systemIndex][layoutNode.slot] = true;
			}
			else
			{
				continue;
			}

			m_sortTabControl[systemIndex][layoutNode.slot] = new CUITabControl();
			m_sortTabControl[systemIndex][layoutNode.slot]->SetAutoDelete(true);
			AttachChild(m_sortTabControl[systemIndex][layoutNode.slot]);
			CUIXmlInit::InitTabControl(uiXml, systemInit.baseSortTabsNode, 0, m_sortTabControl[systemIndex][layoutNode.slot]);
			m_sortTabControl[systemIndex][layoutNode.slot]->SetWindowName(layoutNode.windowName);
			m_sortTabControl[systemIndex][layoutNode.slot]->SetWndPos(m_sortTabsLayoutPos[systemIndex][layoutNode.slot]);
			m_sortTabControl[systemIndex][layoutNode.slot]->SetWndSize(m_sortTabsLayoutSize[systemIndex][layoutNode.slot]);
			m_sortTabControl[systemIndex][layoutNode.slot]->Show(false);
			m_sortTabControl[systemIndex][layoutNode.slot]->Enable(false);
			if (systemInit.system == EInventorySortSystem::Ordering)
			{
				m_sortTabControl[systemIndex][layoutNode.slot]->SetAllowReselect(true);
			}
			Register(m_sortTabControl[systemIndex][layoutNode.slot]);
			AddCallbackStr(layoutNode.windowName, TAB_CHANGED, CUIWndCallback::void_function(this, &CUIActorMenu::OnSortTabChanged));
			ApplySortTabCaptions(m_sortTabControl[systemIndex][layoutNode.slot], systemInit.system);
		}
	}

	if (m_pInventorySorter->GetSystem() == EInventorySortSystem::Ordering)
	{
		for (u8 i = 0; i < eSortTabsLayoutCount; ++i)
		{
			UpdateOrderTabCaption(static_cast<ESortTabsLayoutSlot>(i));
		}
	}

	m_message_box_yes_no				= new CUIMessageBoxEx();	
	m_message_box_yes_no->InitMessageBox( "message_box_yes_no" );
	m_message_box_yes_no->SetAutoDelete	(true);
	m_message_box_yes_no->SetText		( "" );

	m_message_box_ok					= new CUIMessageBoxEx();	
	m_message_box_ok->InitMessageBox	( "message_box_ok" );
	m_message_box_ok->SetAutoDelete		(true);
	m_message_box_ok->SetText			( "" );

	m_UIPropertiesBox					= new CUIPropertiesBox();
	m_UIPropertiesBox->InitPropertiesBox(Fvector2().set(0,0),Fvector2().set(300,300));
	AttachChild							(m_UIPropertiesBox);
	m_UIPropertiesBox->Hide				();
	m_UIPropertiesBox->SetWindowName	( "property_box" );

	m_gamepad_legend					= UIHelper::CreateGamepadLegend(uiXml, "gamepad_legend", this, false);

	InitCallbacks						();

	BindDragDropListEvents(m_pInventoryBeltList);
	BindDragDropListEvents(m_pInventoryBagList);
	BindDragDropListEvents(m_pInventoryStackList);
	BindDragDropListEvents(m_pTradeActorBagList);
	BindDragDropListEvents(m_pTradeActorList);
	BindDragDropListEvents(m_pTradePartnerBagList);
	BindDragDropListEvents(m_pTradePartnerList);
	BindDragDropListEvents(m_pDeadBodyBagList);
	if (m_pQuickSlot)
	BindDragDropListEvents(m_pQuickSlot);

	m_upgrade_selected				= nullptr;
	SetCurrentItem					(nullptr);
	SetActor						(nullptr);
	SetPartner						(nullptr);
	SetInvBox						(nullptr);

	m_actor_trade					= nullptr;
	m_partner_trade					= nullptr;
	m_repair_mode					= 0;

	DeInitInventoryMode				();
	DeInitTradeMode					();
	DeInitUpgradeMode				();
	DeInitDeadBodySearchMode		();
	
	// Controller mode
	xr_map<xr_string, CUIWindow*> wndPointers;
	wndPointers["TradeActorBagList"]	= m_pTradeActorBagList;
	wndPointers["BeltList"]				= m_pInventoryBeltList;
	wndPointers["PistolList"]			= GetSidearmDragDropList();
	wndPointers["AutomaticList"]		= GetPrimaryDragDropList();
	wndPointers["OutfitList"]			= m_pInvList[OUTFIT_SLOT];
	wndPointers["HelmetList"]			= m_pInvList[HELMET_SLOT];
	wndPointers["DetectorList"]			= m_pInvList[DEVICE_SLOT];
	wndPointers["BagList"]				= m_pInventoryBagList;
	wndPointers["TradeActorList"]		= m_pTradeActorList;
	wndPointers["TradePartnerBagList"]	= m_pTradePartnerBagList;
	wndPointers["TradePartnerList"]		= m_pTradePartnerList;
	wndPointers["DeadBodyBagList"]		= m_pDeadBodyBagList;
	wndPointers["QuickSlot"]			= m_pQuickSlot;
	//wndPointers["UpgradeWnd"]			= m_pUpgradeWnd;

	ReadWndSelectorsInfo(uiXml, "ui_c_navi_inventory",	m_ui_navigation_lists[mmInventory], wndPointers);
	ReadWndSelectorsInfo(uiXml, "ui_c_navi_deadbody",	m_ui_navigation_lists[mmDeadBodySearch], wndPointers);
	ReadWndSelectorsInfo(uiXml, "ui_c_navi_trade",		m_ui_navigation_lists[mmTrade], wndPointers);
	ReadWndSelectorsInfo(uiXml, "ui_c_navi_upgrade",	m_ui_navigation_lists[mmUpgrade], wndPointers);
}

void CUIActorMenu::InitCallbacks()
{
	if (m_trade_button)
		Register(m_trade_button);
	if (m_trade_buy_button)
		Register						(m_trade_buy_button);
	if (m_trade_sell_button)
		Register						(m_trade_sell_button);
	Register						(m_takeall_button);

	if (m_putall_button != nullptr)
	{
		Register(m_putall_button);
	}

	Register						(m_exit_button);
	Register						(m_UIPropertiesBox);
	VERIFY							(m_pUpgradeWnd);
	Register						(m_pUpgradeWnd->m_btn_repair);
	if (m_pUpgradeWnd->m_btn_disassemble != nullptr)
	{
		Register(m_pUpgradeWnd->m_btn_disassemble);
	}

	if (m_trade_button)
	{
		AddCallback(m_trade_button, BUTTON_CLICKED,
			CUIWndCallback::void_function(this, &CUIActorMenu::OnBtnPerformTrade));
	}
	if (m_trade_buy_button)
		AddCallback(m_trade_buy_button,BUTTON_CLICKED,   CUIWndCallback::void_function(this, &CUIActorMenu::OnBtnPerformTradeBuy));
	if (m_trade_sell_button)
		AddCallback(m_trade_sell_button,BUTTON_CLICKED,   CUIWndCallback::void_function(this, &CUIActorMenu::OnBtnPerformTradeSell));
	AddCallback(m_takeall_button,  BUTTON_CLICKED,   CUIWndCallback::void_function(this, &CUIActorMenu::TakeAllFromPartner));

	if (m_putall_button != nullptr)
	{
		AddCallback(m_putall_button, BUTTON_CLICKED, CUIWndCallback::void_function(this, &CUIActorMenu::PutAllToPartner));
	}

	AddCallback(m_exit_button,     BUTTON_CLICKED,   CUIWndCallback::void_function(this, &CUIActorMenu::OnBtnExitClicked));
	AddCallback(m_UIPropertiesBox, PROPERTY_CLICKED, CUIWndCallback::void_function(this, &CUIActorMenuBase::ProcessPropertiesBoxClicked));
	AddCallback(m_pUpgradeWnd->m_btn_repair, BUTTON_CLICKED,   CUIWndCallback::void_function(this, &CUIActorMenu::TryRepairItem));
	if (m_pUpgradeWnd->m_btn_disassemble != nullptr)
	{
		AddCallback(m_pUpgradeWnd->m_btn_disassemble, BUTTON_CLICKED,   CUIWndCallback::void_function(this, &CUIActorMenu::TryDisassembleItem));

	}
}

void CUIActorMenu::UpdateButtonsLayout()
{
	if (m_trade_button)
	{
		Fvector2 btn_exit_pos;
		if (m_trade_button->IsShown() || m_takeall_button->IsShown())
		{
			btn_exit_pos = m_trade_button->GetWndPos();
			btn_exit_pos.x += m_trade_button->GetWndSize().x;
		}
		else
		{
			btn_exit_pos = m_trade_button->GetWndPos();
			btn_exit_pos.x += m_trade_button->GetWndSize().x / 2.0f;
		}

		m_exit_button->SetWndPos(btn_exit_pos);
	}

	string32 tmp;
	const char* str = g_pStringTable->translate("quick_use_str_1").c_str();
	strncpy_s(tmp, sizeof(tmp), str, 3);
	if(tmp[2]==',')
		tmp[1] = '\0';
	if (m_QuickSlot1)
		m_QuickSlot1->SetTextST(tmp);

	str = g_pStringTable->translate("quick_use_str_2").c_str();
	strncpy_s(tmp, sizeof(tmp), str, 3);
	if(tmp[2]==',')
		tmp[1] = '\0';
	if (m_QuickSlot2)
		m_QuickSlot2->SetTextST(tmp);

	str = g_pStringTable->translate("quick_use_str_3").c_str();
	strncpy_s(tmp, sizeof(tmp), str, 3);
	if(tmp[2]==',')
		tmp[1] = '\0';
	if (m_QuickSlot3)
		m_QuickSlot3->SetTextST(tmp);

	str = g_pStringTable->translate("quick_use_str_4").c_str();
	strncpy_s(tmp, sizeof(tmp), str, 3);
	if(tmp[2]==',')
		tmp[1] = '\0';
	if (m_QuickSlot4)
		m_QuickSlot4->SetTextST(tmp);

	UpdateConditionProgressBars		();
}
