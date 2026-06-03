#include "stdafx.h"
#include "pch_script.h"
#include "UICarBodyWnd.h"
#include "../xrUI/xrUIXmlParser.h"
#include "../xrUI/UIXmlInit.h"
#include "../HUDManager.h"
#include "../Level.h"
#include "UICharacterInfo.h"
#include "UIDragDropListEx.h"
#include "../xrUI/Widgets/UIFrameWindow.h"
#include "UIItemInfo.h"
#include "../xrUI/Widgets/UIPropertiesBox.h"
#include "../ai/monsters/basemonster/base_monster.h"
#include "../Inventory.h"
#include "UIInventoryUtilities.h"
#include "UICellItem.h"
#include "UICellItemFactory.h"
#include "../WeaponMagazined.h"
#include "../Actor.h"
#include "../eatable_item.h"
#include "../alife_registry_wrappers.h"
#include "../xrUI/Widgets/UI3tButton.h"
#include "../xrUI/Widgets/UIListBoxItem.h"
#include "../InventoryBox.h"
#include "../game_object_space.h"
#include "../../xrScripts/script_callback_ex.h"
#include "../script_game_object.h"
#include "../BottleItem.h"
#include "../../xrUI/UICursor.h"
#include "UIHelperGame.h"
#include "../../xrEngine/xr_input.h"
#include "../WeaponBinoculars.h"
#include "../WeaponKnife.h"
#include "../WeaponMagazinedWGrenade.h"
#include "../PDA.h"
#include "ui_drop_amount.h"
#include "../game_sv_single.h"
#include "ai_object_location.h"

#define				CAR_BODY_XML		"carbody_new.xml"
#define				CARBODY_ITEM_XML	"carbody_item.xml"

CUICarBodyWnd::CUICarBodyWnd()
{
	m_pInventoryBox		= nullptr;
	m_pUIPutAll			= nullptr;
	Init				();
	Show				(false);
	m_currMenuMode		= mmDeadBodySearch;
}

CUICarBodyWnd::~CUICarBodyWnd()
{
	m_pUIOurBagList->ClearAll					(true);
	m_pUIOthersBagList->ClearAll				(true);
}

void CUICarBodyWnd::Init()
{
	CUIXml						uiXml;
	uiXml.Load(CONFIG_PATH, UI_PATH, CAR_BODY_XML);

	inherited::InitBase			(uiXml);
	
	CUIXmlInit					xml_init;

	xml_init.InitWindow			(uiXml, "main", 0, this);

	m_pUIStaticTop				= new CUIStatic(); m_pUIStaticTop->SetAutoDelete(true);
	AttachChild					(m_pUIStaticTop);
	xml_init.InitStatic			(uiXml, "top_background", 0, m_pUIStaticTop);


	m_pUIStaticBottom			= new CUIStatic(); m_pUIStaticBottom->SetAutoDelete(true);
	AttachChild					(m_pUIStaticBottom);
	xml_init.InitStatic			(uiXml, "bottom_background", 0, m_pUIStaticBottom);

	m_pUIOurIcon				= new CUIStatic(); m_pUIOurIcon->SetAutoDelete(true);
	AttachChild					(m_pUIOurIcon);
	xml_init.InitStatic			(uiXml, "static_icon", 0, m_pUIOurIcon);

	m_pUIOthersIcon				= new CUIStatic(); m_pUIOthersIcon->SetAutoDelete(true);
	AttachChild					(m_pUIOthersIcon);
	xml_init.InitStatic			(uiXml, "static_icon", 1, m_pUIOthersIcon);


	m_pUICharacterInfoLeft		= new CUICharacterInfo(); m_pUICharacterInfoLeft->SetAutoDelete(true);
	m_pUIOurIcon->AttachChild	(m_pUICharacterInfoLeft);
	m_pUICharacterInfoLeft->InitCharacterInfo(Fvector2().set(0,0), Fvector2().set(m_pUIOurIcon->GetWidth(), m_pUIOurIcon->GetHeight()), "trade_character.xml");


	m_pUICharacterInfoRight			= new CUICharacterInfo(); m_pUICharacterInfoRight->SetAutoDelete(true);
	m_pUIOthersIcon->AttachChild	(m_pUICharacterInfoRight);
	m_pUICharacterInfoRight->InitCharacterInfo	(Fvector2().set(0,0), Fvector2().set(m_pUIOthersIcon->GetWidth(), m_pUIOthersIcon->GetHeight()), "trade_character.xml");

	m_pUIOurBagWnd					= new CUIStatic(); m_pUIOurBagWnd->SetAutoDelete(true);
	AttachChild						(m_pUIOurBagWnd);
	xml_init.InitStatic				(uiXml, "our_bag_static", 0, m_pUIOurBagWnd);


	m_pUIOthersBagWnd				= new CUIStatic(); m_pUIOthersBagWnd->SetAutoDelete(true);
	AttachChild						(m_pUIOthersBagWnd);
	xml_init.InitStatic				(uiXml, "others_bag_static", 0, m_pUIOthersBagWnd);

	m_pUIOurBagList					= new CUIDragDropListEx(); m_pUIOurBagList->SetAutoDelete(true);
	m_pUIOurBagWnd->AttachChild		(m_pUIOurBagList);	
	CUIXmlInitGame::InitDragDropListEx		(uiXml, "dragdrop_list_our", 0, m_pUIOurBagList);

	m_pUIOthersBagList				= new CUIDragDropListEx(); m_pUIOthersBagList->SetAutoDelete(true);
	m_pUIOthersBagWnd->AttachChild	(m_pUIOthersBagList);	
	CUIXmlInitGame::InitDragDropListEx		(uiXml, "dragdrop_list_other", 0, m_pUIOthersBagList);


	//информация о предмете
	m_pUIDescWnd					= new CUIFrameWindow(); m_pUIDescWnd->SetAutoDelete(true);
	AttachChild						(m_pUIDescWnd);
	xml_init.InitFrameWindow		(uiXml, "frame_window", 0, m_pUIDescWnd);

	m_pUIStaticDesc					= new CUIStatic(); m_pUIStaticDesc->SetAutoDelete(true);
	m_pUIDescWnd->AttachChild		(m_pUIStaticDesc);
	xml_init.InitStatic				(uiXml, "descr_static", 0, m_pUIStaticDesc);

	inherited::InitGamepadSelectors	();

	m_ItemInfo						= new CUIItemInfo(); 
	m_ItemInfo->SetAutoDelete		(true);
	m_pUIDescWnd->AttachChild		(m_ItemInfo);
	m_ItemInfo->InitItemInfo		(Fvector2().set(0,0), Fvector2().set(m_pUIDescWnd->GetWidth(), m_pUIDescWnd->GetHeight()), CARBODY_ITEM_XML);
	
	xml_init.InitAutoStaticGroup	(uiXml, "", 0, this);

	m_UIPropertiesBox				= new CUIPropertiesBox(); 
	m_UIPropertiesBox->SetAutoDelete(true);
	AttachChild						(m_UIPropertiesBox);
	m_UIPropertiesBox->InitPropertiesBox(Fvector2().set(0,0),Fvector2().set(300,300));
	m_UIPropertiesBox->Hide		();

	SetCurrentItem					(nullptr);
	m_pUIStaticDesc->SetText		(nullptr);

	m_pUITakeAll					= new CUI3tButton(); m_pUITakeAll->SetAutoDelete(true);
	AttachChild						(m_pUITakeAll);
	xml_init.Init3tButton				(uiXml, "take_all_btn", 0, m_pUITakeAll);
	
	if (uiXml.NavigateToNode("put_all_btn"))
	{
		m_pUIPutAll = new CUI3tButton(); m_pUIPutAll->SetAutoDelete(true);
		AttachChild(m_pUIPutAll);
		xml_init.Init3tButton(uiXml, "put_all_btn", 0, m_pUIPutAll);
	}
	
	CUIXml uiDropAmountXml;
	if (uiDropAmountXml.Load(CONFIG_PATH, UI_PATH, "custom_drop_amount.xml"))
	{
		m_pItemDropAmountWnd = new CUIItemDropAmountWnd();
		m_pItemDropAmountWnd->SetAutoDelete(true);
		m_pItemDropAmountWnd->InitDropAmount(uiDropAmountXml);
	}
	
	m_gamepad_legend = UIHelper::CreateGamepadLegend(uiXml, "gamepad_legend", this, false);

	BindDragDropListEvents			(m_pUIOurBagList);
	BindDragDropListEvents			(m_pUIOthersBagList);

	m_highlight_clear = true;
	clear_highlight_lists();

	const char* pSelectorTextureName = "ui_inv_item_selector_sec";
	m_pUIOurBagList->InitSelector(pSelectorTextureName);
	m_pUIOthersBagList->InitSelector(pSelectorTextureName);

	// Controller mode
	xr_map<xr_string, CUIWindow*> wndPointers;
	wndPointers["OurBagList"]				= m_pUIOurBagList;
	wndPointers["OthersBagList"]		= m_pUIOthersBagList;

	ReadWndSelectorsInfo(uiXml, "ui_c_navi_carbody",	m_ui_navigation_lists[mmDeadBodySearch], wndPointers);
}

void CUICarBodyWnd::InitCarBody(CInventoryOwner* pOur, CInventoryBox* pInvBox)
{
	m_pOurObject									= pOur;
	m_pOthersObject									= nullptr;
	m_pInventoryBox									= pInvBox;
	m_pInventoryBox->set_in_use						(true);

	m_pUICharacterInfoLeft->InitCharacter			(m_pOurObject);
	m_pUIOthersIcon->Show							(false);
	m_pUICharacterInfoRight->ClearInfo				();
	m_UIPropertiesBox->Hide							();
	EnableAll										();
	UpdateLists										();
	SetAreaSelectionTo								(m_pUIOthersBagList);
}

void CUICarBodyWnd::InitCarBody(CInventoryOwner* pOur, CInventoryOwner* pOthers)
{
	m_pOurObject									= pOur;
	m_pOthersObject									= pOthers;
	m_pInventoryBox									= nullptr;
	
	m_pUICharacterInfoLeft->InitCharacter			(m_pOurObject);
	m_pUIOthersIcon->Show							(true);
	
	CBaseMonster *monster = nullptr;
	if(m_pOthersObject) {
		monster										= m_pOthersObject->cast_base_monster();
		if (monster || m_pOthersObject->use_simplified_visual() ) 
		{
			m_pUICharacterInfoRight->ClearInfo		();
			if(monster)
			{
				const char* monster_tex_name = pSettings->read_if_exists<str_c>(monster->cNameSect(), "icon", "npc_icon_unknown_data");
				m_pUICharacterInfoRight->InitCharacter("", monster_tex_name);
			}
		}else 
		{
			m_pUICharacterInfoRight->InitCharacter	(m_pOthersObject);
		}
	}

	m_UIPropertiesBox->Hide							();
	EnableAll										();
	UpdateLists										();

	if(!monster){
		CInfoPortionWrapper	*known_info_registry = new CInfoPortionWrapper();
		known_info_registry->registry().init(m_pOthersObject->object_id());
		auto& known_info = known_info_registry->registry().objects();
		for (auto& info : known_info.Data)
		{
			NET_Packet		P;
			CGameObject::u_EventGen		(P,GE_INFO_TRANSFER, m_pOurObject->object_id());
			P.w_u16						(0);//not used
			P.w_stringZ					(info.info_id);			//сообщение
			P.w_u8						(1);						//добавление сообщения
			CGameObject::u_EventSend	(P);
		}
		known_info.Data.clear();
		xr_delete(known_info_registry);
	}
	SetAreaSelectionTo							(m_pUIOthersBagList);
}

void CUICarBodyWnd::UpdateLists()
{
	InitInventoryContents(GetActorList());
	UpdateDeadBodyBagList();

	InventoryUtilities::UpdateWeight				(*m_pUIOurBagWnd);
	UpdateConditionProgressBars();
}

void CUICarBodyWnd::SendMessage(CUIWindow *pWnd, s16 msg, void *pData)
{
	if (BUTTON_CLICKED == msg)
	{
		if (m_pUITakeAll == pWnd)
		{
			TakeAllFromPartner(this, nullptr);
		}
		else if (m_pUIPutAll == pWnd)
		{
			PutAllToPartner(this, nullptr);
		}
	}
	else if(pWnd == m_UIPropertiesBox &&	msg == PROPERTY_CLICKED)
	{
		ProcessPropertiesBoxClicked(this, nullptr);
	}

	inherited::SendMessage			(pWnd, msg, pData);
}

void CUICarBodyWnd::Draw()
{
	inherited::Draw	();
}


void CUICarBodyWnd::Update()
{
	if(	m_pOurObject->inventory().ModifyFrame()==Device.dwFrame || 
		(m_pOthersObject&&m_pOthersObject->inventory().ModifyFrame()==Device.dwFrame))
		InventoryUtilities::UpdateWeight(*m_pUIOurBagWnd);
	
	if(m_pOthersObject && m_pOurObject->cast_game_object()->Position().distance_to(m_pOthersObject->cast_game_object()->Position()) > 3.0f)
	{
		HideDialog();
	}

	m_pUITakeAll->SetVisible(!pInput->GetControllerMode());
	if (m_pUIPutAll)
	{
		m_pUIPutAll->SetVisible(!pInput->GetControllerMode());
	}

	inherited::Update();
}


void CUICarBodyWnd::Show(bool status) 
{ 
	inherited::Show(status);
	if (status)
	{
		InventoryUtilities::SendInfoToActor("ui_car_body");
		SetCurrentItem(nullptr);
		InventoryUtilities::UpdateWeight(*m_pUIOurBagWnd);
		PlaySnd(eSndOpen);
	}
	else
	{
		InventoryUtilities::SendInfoToActor("ui_car_body_hide");
		m_pUIOurBagList->ClearAll(true);
		m_pUIOthersBagList->ClearAll(true);
		PlaySnd(eSndClose);
		if (m_pInventoryBox)
			m_pInventoryBox->set_in_use(false);
	}
}

void CUICarBodyWnd::DisableAll()
{
	m_pUIOurBagWnd->Enable			(false);
	m_pUIOthersBagWnd->Enable		(false);
}

void CUICarBodyWnd::EnableAll()
{
	m_pUIOurBagWnd->Enable			(true);
	m_pUIOthersBagWnd->Enable		(true);
}

void CUICarBodyWnd::SetCurrentItem(CUICellItem* itm)
{
	m_pCurrentCellItem		= itm;
	m_ItemInfo->InitItem(CurrentItem(), nullptr, CurrentIItem() ? CurrentIItem()->Cost() : u32(-1), nullptr, true);
	TryHidePropertiesBox();
}
