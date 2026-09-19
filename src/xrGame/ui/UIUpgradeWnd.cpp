#include "stdafx.h"
#include "UIUpgradeWnd.h"
#include "UIHelperGame.h"
#include "../../xrUI/UIHelper.h"
#include "../../xrUI/Widgets/UIPropertiesBox.h"
#include "UICharacterInfo.h"
#include "UIInventoryUpgradeWnd.h"
#include "UIInvUpgradeInfo.h"
#include "../Level.h"
#include "../entity_alive.h"
#include "../../xrUI/Widgets/UI3tButton.h"
#include "UIItemInfo.h"
#include "UIMessageBoxEx.h"
#include "UICellItem.h"
#include "../../xrUI/Widgets/UIBtnHint.h"
#include "UIGameCustom.h"
#include "UITalkWnd.h"
#include "UITalkDialogWnd.h"

CUIUpgradeWnd::CUIUpgradeWnd()
{
	m_currMenuMode = mmUpgrade;
	Init();
}

void CUIUpgradeWnd::Init()
{
	CUIXml								uiXml;
	uiXml.Load(CONFIG_PATH, UI_PATH, "upgrade.xml");

	inherited::InitBase					(uiXml);

	CUIXmlInit							xml_init;

	xml_init.InitWindow					(uiXml, "main", 0, this);

	m_pUpgradeWnd						= new CUIInventoryUpgradeWnd(); 
	AttachChild							(m_pUpgradeWnd);
	m_pUpgradeWnd->SetAutoDelete		(true);
	m_pUpgradeWnd->Init					();

	//статические элементы интерфейса
	m_static_top						= UIHelper::CreateStatic(uiXml, "top_background", this);
	m_static_bottom						= UIHelper::CreateStatic(uiXml, "bottom_background", this);

	//иконки с изображение нас и партнера по торговле
	m_static_character_actor			= UIHelper::CreateStatic(uiXml, "static_icon", this, true, 0);
	m_static_character_partner			= UIHelper::CreateStatic(uiXml, "static_icon", this, true, 1);

	m_character_info_actor				= new CUICharacterInfo();
	m_static_character_actor->AttachChild(m_character_info_actor);
	m_character_info_actor->InitCharacterInfo(Fvector2().set(0, 0), m_static_character_actor->GetWndSize(), "trade_character.xml");
	m_character_info_partner			= new CUICharacterInfo();
	m_static_character_partner->AttachChild(m_character_info_partner);
	m_character_info_partner->InitCharacterInfo(Fvector2().set(0, 0), m_static_character_partner->GetWndSize(), "trade_character.xml");

	m_dragdrop_our_background			= UIHelper::CreateStatic(uiXml, "our_bag_static", this);
	m_our_money_static					= UIHelper::CreateStatic(uiXml, "our_money_static", m_dragdrop_our_background);

	m_dragdrop_our						= UIHelperGame::CreateDragDropListEx(uiXml, "dragdrop_list_our", m_dragdrop_our_background);


	BindDragDropListEvents				(m_dragdrop_our);

	m_description_frame				= UIHelper::CreateFrameWindow(uiXml, "frame_window", this);

	m_description_static			= UIHelper::CreateStatic(uiXml, "descr_static", m_description_frame);

	inherited::InitGamepadSelectors	();

	m_ItemInfo						= new CUIItemInfo(); 
	m_ItemInfo->SetAutoDelete		(true);
	m_description_frame->AttachChild(m_ItemInfo);
	m_ItemInfo->InitItemInfo		(Fvector2().set(0,0), m_description_frame->GetWndSize(), "carbody_item.xml");
	
	xml_init.InitAutoStaticGroup	(uiXml, "", 0, this);

	m_exit_button						= UIHelper::Create3tButton(uiXml, "button", this);
	
	m_message_box_yes_no				= new CUIMessageBoxEx();	
	m_message_box_yes_no->InitMessageBox( "message_box_yes_no" );
	m_message_box_yes_no->SetAutoDelete	(true);
	m_message_box_yes_no->SetText		( "" );

	m_message_box_ok					= new CUIMessageBoxEx();	
	m_message_box_ok->InitMessageBox	( "message_box_ok" );
	m_message_box_ok->SetAutoDelete		(true);
	m_message_box_ok->SetText			( "" );

	m_UIPropertiesBox					= new CUIPropertiesBox();
	AttachChild							(m_UIPropertiesBox);
	m_UIPropertiesBox->SetAutoDelete	(true);
	m_UIPropertiesBox->InitPropertiesBox(Fvector2().set(0,0),Fvector2().set(300,300));
	m_UIPropertiesBox->Hide				();
	
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
	m_gamepad_legend = UIHelper::CreateGamepadLegend(uiXml, "gamepad_legend", this, false);

	clear_highlight_lists();

	const char* pSelectorTextureName = "ui_inv_item_selector_sec";
	GetActorList()->InitSelector(pSelectorTextureName);

	// Controller mode
	xr_map<xr_string, CUIWindow*> wndPointers;
	wndPointers["BagList"]			= m_dragdrop_our;

	ReadWndSelectorsInfo(uiXml, "ui_c_navi_upgrade", m_ui_navigation_lists[mmUpgrade], wndPointers);

	Register(m_exit_button);
	Register(m_UIPropertiesBox);
	Register(m_pUpgradeWnd->m_btn_repair);
	AddCallback(m_exit_button, BUTTON_CLICKED, CUIWndCallback::void_function(this, &CUIUpgradeWnd::OnBtnExitClicked));
	AddCallback(m_UIPropertiesBox, PROPERTY_CLICKED, CUIWndCallback::void_function(this, &CUIActorMenuBase::ProcessPropertiesBoxClicked));
	AddCallback(m_pUpgradeWnd->m_btn_repair, BUTTON_CLICKED, CUIWndCallback::void_function(this, &CUIUpgradeWnd::TryRepairItem));
}

void CUIUpgradeWnd::SetCurrentItem(CUICellItem* itm)
{
	m_pCurrentCellItem = itm;
	m_repair_mode = 0;
	m_ItemInfo->InitItem(CurrentItem(), nullptr, CurrentIItem() ? CurrentIItem()->Cost() : u32(-1), nullptr, true);
	TryHidePropertiesBox();
	SetupUpgradeItem();
}

void CUIUpgradeWnd::StartUpgrade(CInventoryOwner* actor, CInventoryOwner* partner)
{
	m_actor_inv_owner = actor;
	m_partner_inv_owner = partner;

	m_character_info_actor->InitCharacter(m_actor_inv_owner);
	m_character_info_partner->InitCharacter(m_partner_inv_owner);
	
	InitInventoryContents(m_dragdrop_our);
	if (m_partner_inv_owner)
	{
		m_partner_inv_owner->StartTrading();
	}

	SetAreaSelectionTo(m_dragdrop_our);
}

void CUIUpgradeWnd::Update()
{
	CObject* current_entity = Level().CurrentEntity();
	CEntityAlive* pEntityAlive = current_entity != nullptr ? GetInventoryOwner()->cast_entity_alive() : nullptr;

	if (pEntityAlive)
	{
		CInventoryOwner* pOurInvOwner = pEntityAlive != nullptr ? pEntityAlive->cast_inventory_owner() : nullptr;
		u32 _money = pOurInvOwner->get_money();

		if (!IsGameTypeSingle())
		{
			game_PlayerState* ps = Game().GetPlayerByGameID(pEntityAlive->ID());
			if (ps)
			{
				_money = ps->money_for_round;
			}
		}
		// update money
		string64 sMoney;
		xr_sprintf(sMoney, "%u RU", _money);
		m_our_money_static->SetText(sMoney);
	}

	inherited::Update();
}

void CUIUpgradeWnd::SendMessage(CUIWindow* pWnd, s16 msg, void* pData)
{
	CUIWndCallback::OnEvent(pWnd, msg, pData);
}

void CUIUpgradeWnd::Show(bool status)
{
	inherited::Show(status);
	if (status)
	{
		SetCurrentItem(nullptr);
		ResetAll();
		PlaySnd(eSndOpen);
	}
	else
	{
		m_pUpgradeWnd->DeInitInventory();

		if (m_upgrade_selected)
		{
			m_upgrade_selected->Mark(false);
			m_upgrade_selected = nullptr;
		}

		PlaySnd(eSndClose);
	}
}

void CUIUpgradeWnd::OnBtnExitClicked(CUIWindow* w, void* d)
{
	g_btnHint->Discard();
	for (u8 i = 1; i <= LAST_SLOT; ++i)
	{
		if (m_pInvSlotHighlight[i])
		{
			m_pInvSlotHighlight[i]->Show(false);
		}
	}
	HideDialog();

	if (m_actor_inv_owner->IsTalking())
	{
		CurrentGameUI()->TalkMenu->UITalkDialogWnd->Show();
	}
}
