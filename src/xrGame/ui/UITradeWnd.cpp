#include "StdAfx.h"
#include "UITradeWnd.h"

#include "../../xrUI/xrUIXmlParser.h"
#include "../../xrUI/UIXmlInit.h"
#include "../../xrEngine/xr_input.h"
#include "../Entity.h"
#include "../HUDManager.h"
#include "../WeaponAmmo.h"
#include "../Actor.h"
#include "../trade.h"
#include "UIGameCustom.h"
#include "UIInventoryUtilities.h"
#include "../InventoryOwner.h"
#include "../eatable_item.h"
#include "../Inventory.h"
#include "../Level.h"
#include "../../xrEngine/string_table.h"
#include "character_info.h"
#include "../../xrUI/Widgets/UIMultiTextStatic.h"
#include "../../xrUI/Widgets/UI3tButton.h"
#include "UIItemInfo.h"
#include "UIHelperGame.h"
#include "../../xrUI/Widgets/UIBtnHint.h"
#include "UICharacterInfo.h"
#include "UIDragDropListEx.h"
#include "UICellItem.h"
#include "UICellItemFactory.h"
#include "UITalkWnd.h"
#include "../WeaponBinoculars.h"
#include "../WeaponKnife.h"
#include "../WeaponMagazinedWGrenade.h"
#include "../inventory_item.h"
#include "UITalkDialogWnd.h"

#define				TRADE_XML			"trade.xml"
#define				TRADE_CHARACTER_XML	"trade_character.xml"
#define				TRADE_ITEM_XML		"trade_item.xml"

CUITradeWnd::CUITradeWnd()
	:	m_bDealControlsVisible	(false),
		m_pTrade(nullptr),
		m_pOthersTrade(nullptr),
		bStarted(false)
{
	Init();
	Show(false);
	SetCurrentItem			(nullptr);
	m_currMenuMode			= mmTrade;
}

CUITradeWnd::~CUITradeWnd()
{
	UIOurBagList.ClearAll				(true);
	UIOurTradeList.ClearAll				(true);
	UIOthersBagList.ClearAll			(true);
	UIOthersTradeList.ClearAll			(true);
}

void CUITradeWnd::Init()
{
	CUIXml								uiXml;
	uiXml.Load(CONFIG_PATH, UI_PATH, TRADE_XML);

	inherited::InitBase					(uiXml);

	CUIXmlInit							xml_init;

	xml_init.InitWindow					(uiXml, "main", 0, this);

	//статические элементы интерфейса
	AttachChild							(&UIStaticTop);
	xml_init.InitStatic					(uiXml, "top_background", 0, &UIStaticTop);
	AttachChild							(&UIStaticBottom);
	xml_init.InitStatic					(uiXml, "bottom_background", 0, &UIStaticBottom);

	//иконки с изображение нас и партнера по торговле
	AttachChild							(&UIOurIcon);
	xml_init.InitStatic					(uiXml, "static_icon", 0, &UIOurIcon);
	AttachChild							(&UIOthersIcon);
	xml_init.InitStatic					(uiXml, "static_icon", 1, &UIOthersIcon);
	UIOurIcon.AttachChild		(&UICharacterInfoLeft);
	UICharacterInfoLeft.InitCharacterInfo	(Fvector2().set(0,0), Fvector2().set(UIOurIcon.GetWidth(), UIOurIcon.GetHeight()), TRADE_CHARACTER_XML);
	UIOthersIcon.AttachChild	(&UICharacterInfoRight);
	UICharacterInfoRight.InitCharacterInfo(Fvector2().set(0,0), Fvector2().set(UIOthersIcon.GetWidth(), UIOthersIcon.GetHeight()), TRADE_CHARACTER_XML);


	// Attach order defines z-order: trade panels first, bag panels last so the backpack stays on top
	// for drawing and hit-testing when a long deal list overlaps the inventory region
	AttachChild							(&UIOurTradeWnd);
	xml_init.InitStatic					(uiXml, "static", 0, &UIOurTradeWnd);
	AttachChild							(&UIOthersTradeWnd);
	xml_init.InitStatic					(uiXml, "static", 1, &UIOthersTradeWnd);

	UIOurTradeWnd.AttachChild	(&UIOurPriceCaption);
	CUIXmlInitGame::InitMultiTextStatic		(uiXml, "price_mt_static", 0, &UIOurPriceCaption);

	UIOthersTradeWnd.AttachChild(&UIOthersPriceCaption);
	CUIXmlInitGame::InitMultiTextStatic		(uiXml, "price_mt_static", 0, &UIOthersPriceCaption);

	UIOurTradeWnd.AttachChild	(&UIOurTradeList);	
	CUIXmlInitGame::InitDragDropListEx			(uiXml, "dragdrop_list", 2, &UIOurTradeList);

	UIOthersTradeWnd.AttachChild(&UIOthersTradeList);	
	CUIXmlInitGame::InitDragDropListEx			(uiXml, "dragdrop_list", 3, &UIOthersTradeList);

	AttachChild							(&UIOurBagWnd);
	xml_init.InitStatic					(uiXml, "our_bag_static", 0, &UIOurBagWnd);
	AttachChild							(&UIOthersBagWnd);
	xml_init.InitStatic					(uiXml, "others_bag_static", 0, &UIOthersBagWnd);

	UIOurBagWnd.AttachChild	(&UIOurMoneyStatic);
	xml_init.InitStatic					(uiXml, "our_money_static", 0, &UIOurMoneyStatic);

	UIOthersBagWnd.AttachChild(&UIOtherMoneyStatic);
	xml_init.InitStatic					(uiXml, "other_money_static", 0, &UIOtherMoneyStatic);

	UIOurBagWnd.AttachChild	(&UIOurBagList);	
	CUIXmlInitGame::InitDragDropListEx			(uiXml, "dragdrop_list", 0, &UIOurBagList);

	UIOthersBagWnd.AttachChild(&UIOthersBagList);	
	CUIXmlInitGame::InitDragDropListEx			(uiXml, "dragdrop_list", 1, &UIOthersBagList);

	
	AttachChild							(&UIDescWnd);
	xml_init.InitStatic					(uiXml, "desc_static", 0, &UIDescWnd);
	m_ItemInfo							= new CUIItemInfo();
	m_ItemInfo->SetAutoDelete			(true);
	UIDescWnd.AttachChild				(m_ItemInfo);
	m_ItemInfo->InitItemInfo			(Fvector2().set(0,0), Fvector2().set(UIDescWnd.GetWidth(), UIDescWnd.GetHeight()), TRADE_ITEM_XML);


	AttachChild							(&UIPerformTradeButton);
	xml_init.Init3tButton					(uiXml, "button", 0, &UIPerformTradeButton);

	AttachChild							(&UIToTalkButton);
	xml_init.Init3tButton					(uiXml, "button", 1, &UIToTalkButton);

	UIDealMsg					= nullptr;

	BindDragDropListEvents				(&UIOurBagList);
	BindDragDropListEvents				(&UIOthersBagList);
	BindDragDropListEvents				(&UIOurTradeList);
	BindDragDropListEvents				(&UIOthersTradeList);

	inherited::InitGamepadSelectors		();
	
	//pop-up menu
	m_UIPropertiesBox					= new CUIPropertiesBox();
	AttachChild							(m_UIPropertiesBox);
	m_UIPropertiesBox->SetAutoDelete	(true);
	m_UIPropertiesBox->InitPropertiesBox(Fvector2().set(0,0),Fvector2().set(300,300));
	m_UIPropertiesBox->Hide				();
	
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
	GetTradeActorBagList()->InitSelector(pSelectorTextureName);
	GetTradeActorList()->InitSelector(pSelectorTextureName);
	GetTradePartnerBagList()->InitSelector(pSelectorTextureName);
	GetTradePartnerList()->InitSelector(pSelectorTextureName);

	// Controller mode
	xr_map<xr_string, CUIWindow*> wndPointers;
	wndPointers["OurBagList"]			= &UIOurBagList;
	wndPointers["OurTradeList"]			= &UIOurTradeList;
	wndPointers["OthersBagList"]		= &UIOthersBagList;
	wndPointers["OthersTradeList"]		= &UIOthersTradeList;

	ReadWndSelectorsInfo(uiXml, "ui_c_navi_trade", m_ui_navigation_lists[mmTrade], wndPointers);
}

void CUITradeWnd::InitTrade(CInventoryOwner* pOur, CInventoryOwner* pOthers)
{
	VERIFY								(pOur);
	VERIFY								(pOthers);

	m_pInvOwner							= pOur;
	m_pOthersInvOwner					= pOthers;
	UIOthersPriceCaption.GetPhraseByIndex(0)->SetText(g_pStringTable->translate("ui_st_opponent_items").c_str());

	UICharacterInfoLeft.InitCharacter(m_pInvOwner);
	UICharacterInfoRight.InitCharacter(m_pOthersInvOwner);
	
	EnableAll							();

	m_pOthersInvOwner->StartTrading		();
	UpdateLists							(eBoth);
		
	m_pTrade							= pOur->GetTrade();
	m_pOthersTrade						= pOthers->GetTrade();

   	m_pTrade->StartTradeEx				(pOthers);
	m_pOthersTrade->StartTradeEx		(pOur);

	SetAreaSelectionTo					(&UIOurBagList);
}  

void CUITradeWnd::SendMessage(CUIWindow *pWnd, s16 msg, void *pData)
{
	if(pWnd == &UIToTalkButton && msg == BUTTON_CLICKED)
	{
		SwitchToTalk();
	}
	else if(pWnd == &UIPerformTradeButton && msg == BUTTON_CLICKED)
	{
		OnBtnPerformTrade(this, nullptr);
	}
	else if (pWnd == m_UIPropertiesBox && msg == PROPERTY_CLICKED)
	{
		ProcessPropertiesBoxClicked(this, nullptr);
	}

	CUIWindow::SendMessage(pWnd, msg, pData);
}

void CUITradeWnd::Draw()
{
	inherited::Draw				();
	if(UIDealMsg)		UIDealMsg->Draw();

}

extern void UpdateCameraDirection(CGameObject* pTo, bool isFocus);

void CUITradeWnd::Update()
{
	if (GetPartner()->inventory().ModifyFrame() != m_trade_partner_inventory_state)
		InitPartnerInventoryContents();

	inherited::Update				();

	UIPerformTradeButton.SetVisible(!pInput->GetControllerMode());
	UIToTalkButton.SetVisible(!pInput->GetControllerMode());

	if(UIDealMsg)
	{
		UIDealMsg->Update();
		if( !UIDealMsg->IsActual())
		{
			CurrentGameUI()->RemoveCustomStatic("not_enough_money_mine");
			CurrentGameUI()->RemoveCustomStatic("not_enough_money_other");
			UIDealMsg			= nullptr;
		}
	}
}

#include "UIInventoryUtilities.h"
void CUITradeWnd::Show(bool status)
{
	inherited::Show					(status);
	inherited::Enable				(status);
	if (status)
	{
		InventoryUtilities::SendInfoToActor("ui_trade");

		SetCurrentItem(nullptr);
		ResetAll();
		UIDealMsg = nullptr;
		PlaySnd(eSndOpen);
	}
	else
	{
		InventoryUtilities::SendInfoToActor("ui_trade_hide");
		if (bStarted)
			StopTrade();

		UIDealMsg = nullptr;

		if (CurrentGameUI()) {
			CurrentGameUI()->RemoveCustomStatic("not_enough_money_mine");
			CurrentGameUI()->RemoveCustomStatic("not_enough_money_other");
		}

		UIOurBagList.ClearAll(true);
		UIOurTradeList.ClearAll(true);
		UIOthersBagList.ClearAll(true);
		UIOthersTradeList.ClearAll(true);
		PlaySnd(eSndClose);
	}
}

void CUITradeWnd::StartTrade()
{
	if (m_pTrade)					m_pTrade->TradeCB(true);
	if (m_pOthersTrade)				m_pOthersTrade->TradeCB(true);
	bStarted						= true;
}

void CUITradeWnd::StopTrade()
{
	if (m_pTrade)					m_pTrade->TradeCB(false);
	if (m_pOthersTrade)				m_pOthersTrade->TradeCB(false);
	bStarted						= false;
}

void CUITradeWnd::TradeShowMessage(s64 money_actor, s64 money_patner) 
{
	if (money_patner < 0)
		UIDealMsg = CurrentGameUI()->AddCustomStatic("not_enough_money_other", true);
	else
		UIDealMsg = CurrentGameUI()->AddCustomStatic("not_enough_money_mine", true);


	UIDealMsg->m_endTime = Device.fTimeGlobal + 2.0f;// sec
}

void CUITradeWnd::DisableAll()
{
	UIOurBagWnd.Enable			(false);
	UIOthersBagWnd.Enable			(false);
	UIOurTradeWnd.Enable			(false);
	UIOthersTradeWnd.Enable		(false);
}

void CUITradeWnd::EnableAll()
{
	UIOurBagWnd.Enable			(true);
	UIOthersBagWnd.Enable			(true);
	UIOurTradeWnd.Enable			(true);
	UIOthersTradeWnd.Enable		(true);
}

void CUITradeWnd::UpdatePrices()
{
	m_iOurTradePrice	= CalcItemsPrice	(&UIOurTradeList,		m_pOthersTrade, true);
	m_iOthersTradePrice = CalcItemsPrice	(&UIOthersTradeList,	m_pOthersTrade, false);


	string256				buf;
	xr_sprintf				(buf, "%u RU", m_iOurTradePrice);
	UIOurPriceCaption.GetPhraseByIndex(2)->str = buf;
	xr_sprintf				(buf, "%u RU", m_iOthersTradePrice);
	UIOthersPriceCaption.GetPhraseByIndex(2)->str = buf;

	xr_sprintf				(buf, "%u RU", m_pInvOwner->get_money());
	UIOurMoneyStatic.SetText(buf);

	if(!m_pOthersInvOwner->InfinitiveMoney())
	{
		xr_sprintf			(buf, "%u RU", m_pOthersInvOwner->get_money());
		UIOtherMoneyStatic.SetText(buf);
	}
	else
	{
		UIOtherMoneyStatic.SetText("∞ RU");
	}
}

void CUITradeWnd::UpdateLists(EListType mode)
{
	if(mode==eBoth||mode==e1st)
	{
		InitInventoryContents(GetTradeActorBagList());
	}

	if(mode==eBoth||mode==e2nd)
	{
		InitPartnerInventoryContents();
	}
	UpdatePrices						();
	UpdateConditionProgressBars			();
}

void CUITradeWnd::SetCurrentItem(CUICellItem* itm)
{
	m_pCurrentCellItem				= itm;

	CUIDragDropListEx* owner	= itm ? itm->OwnerList() : nullptr;

	bool bBuying				= (owner==&UIOurBagList) || (owner==&UIOurTradeList);

	m_ItemInfo->InitItem	(CurrentItem(), nullptr, itm ? m_pOthersTrade->GetItemPrice(CurrentIItem(), bBuying) : u32(-1));
	TryHidePropertiesBox();
}

void CUITradeWnd::SwitchToTalk()
{
	g_btnHint->Discard();
	HideDialog();

	if (GetInventoryOwner()->IsTalking())
		CurrentGameUI()->TalkMenu->UITalkDialogWnd->Show();
}
