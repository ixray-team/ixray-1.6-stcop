#include "stdafx.h"
#include "UITradeWnd.h"

#include "../../xrUI/xrUIXmlParser.h"
#include "../../xrUI/UIXmlInit.h"

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

#include "UICharacterInfo.h"
#include "UIDragDropListEx.h"
#include "UICellItem.h"
#include "UICellItemFactory.h"

#include "../WeaponBinoculars.h"
#include "../WeaponKnife.h"
#include "../WeaponMagazinedWGrenade.h"
#include "../inventory_item.h"


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


	//Списки торговли
	AttachChild							(&UIOurBagWnd);
	xml_init.InitStatic					(uiXml, "our_bag_static", 0, &UIOurBagWnd);
	AttachChild							(&UIOthersBagWnd);
	xml_init.InitStatic					(uiXml, "others_bag_static", 0, &UIOthersBagWnd);

	UIOurBagWnd.AttachChild	(&UIOurMoneyStatic);
	xml_init.InitStatic					(uiXml, "our_money_static", 0, &UIOurMoneyStatic);

	UIOthersBagWnd.AttachChild(&UIOtherMoneyStatic);
	xml_init.InitStatic					(uiXml, "other_money_static", 0, &UIOtherMoneyStatic);

	AttachChild							(&UIOurTradeWnd);
	xml_init.InitStatic					(uiXml, "static", 0, &UIOurTradeWnd);
	AttachChild							(&UIOthersTradeWnd);
	xml_init.InitStatic					(uiXml, "static", 1, &UIOthersTradeWnd);

	UIOurTradeWnd.AttachChild	(&UIOurPriceCaption);
	CUIXmlInitGame::InitMultiTextStatic		(uiXml, "price_mt_static", 0, &UIOurPriceCaption);

	UIOthersTradeWnd.AttachChild(&UIOthersPriceCaption);
	CUIXmlInitGame::InitMultiTextStatic		(uiXml, "price_mt_static", 0, &UIOthersPriceCaption);

	//Списки Drag&Drop
	UIOurBagWnd.AttachChild	(&UIOurBagList);	
	CUIXmlInitGame::InitDragDropListEx			(uiXml, "dragdrop_list", 0, &UIOurBagList);

	UIOthersBagWnd.AttachChild(&UIOthersBagList);	
	CUIXmlInitGame::InitDragDropListEx			(uiXml, "dragdrop_list", 1, &UIOthersBagList);

	UIOurTradeWnd.AttachChild	(&UIOurTradeList);	
	CUIXmlInitGame::InitDragDropListEx			(uiXml, "dragdrop_list", 2, &UIOurTradeList);

	UIOthersTradeWnd.AttachChild(&UIOthersTradeList);	
	CUIXmlInitGame::InitDragDropListEx			(uiXml, "dragdrop_list", 3, &UIOthersTradeList);

	
	AttachChild							(&UIDescWnd);
	xml_init.InitStatic					(uiXml, "desc_static", 0, &UIDescWnd);
	m_ItemInfo							= new CUIItemInfo();
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
	
	//pop-up menu
	m_UIPropertiesBox					= new CUIPropertiesBox();
	AttachChild							(m_UIPropertiesBox);
	m_UIPropertiesBox->SetAutoDelete	(true);
	m_UIPropertiesBox->InitPropertiesBox(Fvector2().set(0,0),Fvector2().set(300,300));
	m_UIPropertiesBox->Hide				();

	m_highlight_clear = true;
	clear_highlight_lists();
}

void CUITradeWnd::InitTrade(CInventoryOwner* pOur, CInventoryOwner* pOthers)
{
	VERIFY								(pOur);
	VERIFY								(pOthers);

	m_pInvOwner							= pOur;
	m_pOthersInvOwner					= pOthers;
	UIOthersPriceCaption.GetPhraseByIndex(0)->SetText(*g_pStringTable->translate("ui_st_opponent_items"));

	UICharacterInfoLeft.InitCharacter(m_pInvOwner);
	UICharacterInfoRight.InitCharacter(m_pOthersInvOwner);

	m_pInv								= &m_pInvOwner->inventory();
	m_pOthersInv						= &m_pOthersInvOwner->inventory();
		
	m_pTrade							= pOur->GetTrade();
	m_pOthersTrade						= pOthers->GetTrade();

   	m_pTrade->StartTradeEx				(pOthers);
	m_pOthersTrade->StartTradeEx		(pOur);
	
	EnableAll							();

	UpdateLists							(eBoth);
}  

void CUITradeWnd::SendMessage(CUIWindow *pWnd, s16 msg, void *pData)
{
	if(pWnd == &UIToTalkButton && msg == BUTTON_CLICKED)
	{
		SwitchToTalk();
	}
	else if(pWnd == &UIPerformTradeButton && msg == BUTTON_CLICKED)
	{
		PerformTrade();
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

void CUITradeWnd::PerformTrade()
{

	if (UIOurTradeList.ItemsCount()==0 && UIOthersTradeList.ItemsCount()==0) 
		return;

	int our_money			= (int)m_pInvOwner->get_money();
	int others_money		= (int)m_pOthersInvOwner->get_money();

	int delta_price			= int(m_iOurTradePrice-m_iOthersTradePrice);

	our_money				+= delta_price;
	others_money			-= delta_price;

	if(our_money>=0 && others_money>=0 && (m_iOurTradePrice>=0 || m_iOthersTradePrice>0))
	{
		m_pOthersTrade->OnPerformTrade(m_iOthersTradePrice, m_iOurTradePrice);
		
		TransferItems		(&UIOurTradeList,		&UIOthersBagList, m_pOthersTrade,	true);
		TransferItems		(&UIOthersTradeList,	&UIOurBagList,	m_pOthersTrade,	false);
	}
	else
	{
		if(others_money<0)
			UIDealMsg		= CurrentGameUI()->AddCustomStatic("not_enough_money_other", true);
		else
			UIDealMsg		= CurrentGameUI()->AddCustomStatic("not_enough_money_mine", true);


		UIDealMsg->m_endTime	= Device.fTimeGlobal+2.0f;// sec
	}
	SetCurrentItem			(nullptr);
	UpdatePrices			();
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
	xr_sprintf				(buf, "%d RU", m_iOurTradePrice);
	UIOurPriceCaption.GetPhraseByIndex(2)->str = buf;
	xr_sprintf				(buf, "%d RU", m_iOthersTradePrice);
	UIOthersPriceCaption.GetPhraseByIndex(2)->str = buf;

	xr_sprintf				(buf, "%d RU", m_pInvOwner->get_money());
	UIOurMoneyStatic.SetText(buf);

	if(!m_pOthersInvOwner->InfinitiveMoney())
	{
		xr_sprintf			(buf, "%d RU", m_pOthersInvOwner->get_money());
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
}

void CUITradeWnd::FillList	(TIItemContainer& cont, CUIDragDropListEx& dragDropList, bool do_colorize)
{
	TIItemContainer::iterator it	= cont.begin();
	TIItemContainer::iterator it_e	= cont.end();

	for(; it != it_e; ++it) 
	{
		CUICellItem* itm			= create_cell_item	(*it);
		if(do_colorize)				ColorizeItem		(itm, CanMoveToPartner(*it));
		dragDropList.SetItem		(itm);
	}

}

void CUITradeWnd::SetCurrentItem(CUICellItem* itm)
{
	if(m_pCurrentCellItem == itm) return;
	m_pCurrentCellItem				= itm;

	CUIDragDropListEx* owner	= itm ? itm->OwnerList() : nullptr;

	bool bBuying				= (owner==&UIOurBagList) || (owner==&UIOurTradeList);

	m_ItemInfo->InitItem	(CurrentItem(), nullptr, itm ? m_pOthersTrade->GetItemPrice(CurrentIItem(), bBuying) : u32(-1));
}

void CUITradeWnd::SwitchToTalk()
{
	GetMessageTarget()->SendMessage		(this, TRADE_WND_CLOSED);
}
