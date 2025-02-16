#include "pch_script.h"
#include "UIUpgradeWnd.h"
#include "../xrCore/object_broker.h"
#include "xrUIXmlParser.h"
#include "UIXmlInit.h"

#include "../Entity.h"
#include "../HUDManager.h"
#include "../WeaponAmmo.h"
#include "../Actor.h"
#include "../Weapon.h"
#include "../UIGameSP.h"
#include "UIInventoryUtilities.h"
#include "../inventoryowner.h"
#include "../eatable_item.h"
#include "../inventory.h"
#include "../level.h"
#include "../../xrEngine/string_table.h"
#include "../character_info.h"
#include "UIMultiTextStatic.h"
#include "UI3tButton.h"
#include "UIItemInfo.h"
#include "UIMessageBoxEx.h"
#include "UICharacterInfo.h"
#include "UIDragDropListEx.h"
#include "UICellItem.h"
#include "UICellItemFactory.h"

#include "UIInventoryUpgradeWnd.h"

#include "../script_process.h"
#include "../ai_space.h"
#include "../alife_simulator.h"
#include "../script_engine.h"
#include "../script_game_object.h"
#include "../inventory_upgrade_manager.h"
#include "../inventory_upgrade.h"
#include "../inventory_upgrade_property.h"

#include "UIInvUpgradeInfo.h"

struct CUIUpgradeInternal
{
	CUIStatic			UIStaticTop;
	CUIStatic			UIStaticBottom;

	CUIStatic			UIOurBagWnd;
	CUIStatic			UIOurMoneyStatic;
	CUIDragDropListEx	UIOurBagList;

	//кнопки
	CUI3tButton			UIPerformRepairButton;
	CUI3tButton			UIToTalkButton;

	//информация о персонажах 
	CUIStatic			UIOurIcon;
	CUIStatic			UIOthersIcon;
	CUICharacterInfo	UICharacterInfoLeft;
	CUICharacterInfo	UICharacterInfoRight;

	//информация о перетаскиваемом предмете
	CUIStatic			UIDescWnd;
	CUIItemInfo			UIItemInfo;
};

CUIUpgradeWnd::CUIUpgradeWnd()
	: bStarted(false),
	bRepairMode(false),
	m_upgrade_selected(NULL),
	m_upgrade_info(NULL)
{
	m_uidata = new CUIUpgradeInternal();
	m_pUpgradeWnd = new CUIInventoryUpgradeWnd();
	Init();
}

CUIUpgradeWnd::~CUIUpgradeWnd()
{
	m_uidata->UIOurBagList.ClearAll(true);
	xr_delete(m_uidata);
}

void CUIUpgradeWnd::Init()
{
	CUIXml								uiXml;

	const u32 default_hud_style = 3;

	LPCSTR		TRADE_XML = "new_upgrade_wnd.xml";

	string128		TRADE_CHARACTER_XML;
	xr_sprintf(TRADE_CHARACTER_XML, "trade_character_%d.xml", default_hud_style);

	string128		TRADE_ITEM_XML;
	xr_sprintf(TRADE_ITEM_XML, "trade_item_%d.xml", default_hud_style);

	uiXml.Load(CONFIG_PATH, UI_PATH, TRADE_XML);
	CUIXmlInit							xml_init;

	xml_init.InitWindow(uiXml, "main", 0, this);

	//статические элементы интерфейса
	AttachChild(&m_uidata->UIStaticTop);
	xml_init.InitStatic(uiXml, "top_background", 0, &m_uidata->UIStaticTop);
	AttachChild(&m_uidata->UIStaticBottom);
	xml_init.InitStatic(uiXml, "bottom_background", 0, &m_uidata->UIStaticBottom);

	//иконки с изображение нас и партнера по торговле
	AttachChild(&m_uidata->UIOurIcon);
	xml_init.InitStatic(uiXml, "static_icon", 0, &m_uidata->UIOurIcon);
	AttachChild(&m_uidata->UIOthersIcon);
	xml_init.InitStatic(uiXml, "static_icon", 1, &m_uidata->UIOthersIcon);
	m_uidata->UIOurIcon.AttachChild(&m_uidata->UICharacterInfoLeft);
	m_uidata->UICharacterInfoLeft.Init(0, 0, m_uidata->UIOurIcon.GetWidth(), m_uidata->UIOurIcon.GetHeight(), TRADE_CHARACTER_XML);
	m_uidata->UIOthersIcon.AttachChild(&m_uidata->UICharacterInfoRight);
	m_uidata->UICharacterInfoRight.Init(0, 0, m_uidata->UIOthersIcon.GetWidth(), m_uidata->UIOthersIcon.GetHeight(), TRADE_CHARACTER_XML);


	//Списки торговли
	AttachChild(&m_uidata->UIOurBagWnd);
	xml_init.InitStatic(uiXml, "our_bag_static", 0, &m_uidata->UIOurBagWnd);

	m_uidata->UIOurBagWnd.AttachChild(&m_uidata->UIOurMoneyStatic);
	xml_init.InitStatic(uiXml, "our_money_static", 0, &m_uidata->UIOurMoneyStatic);

	//Списки Drag&Drop
	m_uidata->UIOurBagWnd.AttachChild(&m_uidata->UIOurBagList);
	xml_init.InitDragDropListEx(uiXml, "dragdrop_list", 0, &m_uidata->UIOurBagList);

	AttachChild(&m_uidata->UIDescWnd);
	xml_init.InitStatic(uiXml, "desc_static", 0, &m_uidata->UIDescWnd);
	m_uidata->UIDescWnd.AttachChild(&m_uidata->UIItemInfo);
	m_uidata->UIItemInfo.Init(TRADE_ITEM_XML);
	m_uidata->UIItemInfo.SetWndRect(Frect().set(0, 0, m_uidata->UIDescWnd.GetWidth(), m_uidata->UIDescWnd.GetHeight()));
	
	xml_init.InitAutoStatic(uiXml, "auto_static", this);


	AttachChild(&m_uidata->UIPerformRepairButton);
	xml_init.Init3tButton(uiXml, "button", 0, &m_uidata->UIPerformRepairButton);
	m_uidata->UIPerformRepairButton.Enable(false);

	AttachChild(&m_uidata->UIToTalkButton);
	xml_init.Init3tButton(uiXml, "button", 1, &m_uidata->UIToTalkButton);

	BindDragDropListEvents(&m_uidata->UIOurBagList);

	m_message_box_yes_no = new CUIMessageBoxEx();
	m_message_box_yes_no->InitMessageBox("message_box_repair");
	m_message_box_yes_no->SetAutoDelete(true);
	m_message_box_yes_no->SetText("");

	m_message_box_ok = new CUIMessageBoxEx();
	m_message_box_ok->InitMessageBox("message_box_ok");
	m_message_box_ok->SetAutoDelete(true);
	m_message_box_ok->SetText("");

	AttachChild(m_pUpgradeWnd);
	m_pUpgradeWnd->SetAutoDelete(true);
	m_pUpgradeWnd->Init();
	m_pUpgradeWnd->m_btn_repair = &m_uidata->UIPerformRepairButton;

	if (ai().get_alife())
	{
		m_upgrade_info = new UIInvUpgradeInfo();
		m_upgrade_info->SetAutoDelete(true);
		AttachChild(m_upgrade_info);
		m_upgrade_info->init_from_xml("actor_menu_item.xml");
	}

}

void CUIUpgradeWnd::InitUpgrade(CInventoryOwner* pOur, CInventoryOwner* pOthers)
{
	VERIFY(pOur);
	VERIFY(pOthers);

	m_pInvOwner = pOur;
	m_pOthersInvOwner = pOthers;

	m_uidata->UICharacterInfoLeft.InitCharacter(m_pInvOwner->object_id());
	m_uidata->UICharacterInfoRight.InitCharacter(m_pOthersInvOwner->object_id());

	m_pInv = &(pOur->inventory());
	
	EnableAll();

	UpdateLists(eBoth);
}

void CUIUpgradeWnd::SendMessage(CUIWindow *pWnd, s16 msg, void *pData)
{
	if (pWnd == &m_uidata->UIToTalkButton && msg == BUTTON_CLICKED)
	{
		SwitchToTalk();
	}
	else if (pWnd == &m_uidata->UIPerformRepairButton && msg == BUTTON_CLICKED)
	{
		TryRepairItem();
	}

	CUIWindow::SendMessage(pWnd, msg, pData);
}

extern void UpdateCameraDirection(CGameObject* pTo);
void CUIUpgradeWnd::Update()
{
	EListType et = eNone;
	if (m_pInv->ModifyFrame() == Device.dwFrame)
		et = e1st;
	
	if (et != eNone)
		UpdateLists(et);

	inherited::Update();
	UpdateCameraDirection(smart_cast<CGameObject*>(m_pOthersInvOwner));
}

#include "UIInventoryUtilities.h"
void CUIUpgradeWnd::Show()
{
	InventoryUtilities::SendInfoToActor("ui_trade");
	inherited::Show(true);
	inherited::Enable(true);

	ResetAll();
	m_pUpgradeWnd->Clear(); //tarinrafa: Added Clear function to avoid upgrading with folowing crash of game
}

void CUIUpgradeWnd::Hide()
{
	SetCurrentItem(NULL);
	InventoryUtilities::SendInfoToActor("ui_trade_hide");
	inherited::Show(false);
	inherited::Enable(false);
	if (bStarted)
		StopTrade();

	if (CurrentGameUI())
	{
		CurrentGameUI()->RemoveCustomStatic("not_enough_money_mine");
		CurrentGameUI()->RemoveCustomStatic("not_enough_money_other");
	}

	m_uidata->UIOurBagList.ClearAll(true);
}

void CUIUpgradeWnd::StartTrade()
{
	
}

void CUIUpgradeWnd::StopTrade()
{
	
}

void CUIUpgradeWnd::PerformRepair()
{
	PIItem item = CurrentIItem();
	CUICellItem* itm = CurrentItem();
	if (!item)
	{
		return;
	}
	LPCSTR item_name = item->m_section_id.c_str();

	luabind::functor<void>	funct;
	R_ASSERT(ai().script_engine().functor("inventory_upgrades.effect_repair_item", funct));
	funct(item_name, item->GetCondition(), smart_cast<CObject*>(m_pOthersInvOwner)->cName().c_str(), item->object_id());

	item->ResetCondition();
	m_uidata->UIItemInfo.UpdateCondition();
	SeparateUpgradeItem();
	UpdatePrices();

	m_uidata->UIPerformRepairButton.Enable(false);
}

void CUIUpgradeWnd::DisableAll()
{
	m_uidata->UIOurBagWnd.Enable(false);
}

void CUIUpgradeWnd::EnableAll()
{
	m_uidata->UIOurBagWnd.Enable(true);
}

struct SUpgradeSelector : public CInventory::SInventorySelectorPredicate
{
private:
	CUIUpgradeWnd* m_parent;
public:
	SUpgradeSelector(CUIUpgradeWnd* parent) : m_parent(parent) {}
	virtual bool operator() (PIItem item)
	{
		return m_parent->CanAddItem(item);
	}
};

void CUIUpgradeWnd::UpdateLists(EListType mode)
{
	m_uidata->UIOurBagList.ClearAll(true);

	UpdatePrices();

	ruck_list.clear();
	m_pInv->AddAvailableItems(ruck_list, (SUpgradeSelector&)SUpgradeSelector(this));
	std::sort(ruck_list.begin(), ruck_list.end(), InventoryUtilities::GreaterRoomInRuck);
	FillList(ruck_list, m_uidata->UIOurBagList, true);
}

void CUIUpgradeWnd::FillList(TIItemContainer& cont, CUIDragDropListEx& dragDropList, bool do_colorize)
{
	TIItemContainer::iterator it = cont.begin();
	TIItemContainer::iterator it_e = cont.end();

	for (; it != it_e; ++it)
	{
		CUICellItem* itm = create_cell_item(*it);
		if (do_colorize)
			ColorizeItem(itm);
		dragDropList.SetItem(itm);
	}
}

bool CUIUpgradeWnd::OnItemStartDrag(CUICellItem* itm)
{
	return false; //default behaviour
}

bool CUIUpgradeWnd::OnItemSelected(CUICellItem* itm)
{
	SetCurrentItem(itm);
	return				false;
}

bool CUIUpgradeWnd::OnItemRButtonClick(CUICellItem* itm)
{
	SetCurrentItem(itm);
	return						false;
}

bool CUIUpgradeWnd::OnItemDbClick(CUICellItem* itm)
{
	SetCurrentItem(itm);
	return true;
}


void CUIUpgradeWnd::SetCurrentItem(CUICellItem* itm)
{
	if (m_pCurrentCellItem == itm) return;
	m_pCurrentCellItem = itm;
	m_uidata->UIItemInfo.InitItem(CurrentIItem());
	SetupUpgradeItem();
}

bool CUIUpgradeWnd::CanAddItem(PIItem item)
{
	VERIFY(item && m_pOthersInvOwner);
	LPCSTR item_name = item->m_section_id.c_str();
	LPCSTR partner = smart_cast<CObject*>(m_pOthersInvOwner)->cName().c_str();

	luabind::functor<bool> funct;
	R_ASSERT2(
		ai().script_engine().functor("inventory_upgrades.can_add_item", funct),
		make_string<const char*>("Failed to get functor <inventory_upgrades.can_add_item>, item = %s, mechanic = %s", item_name, partner)
		);

	return funct(item_name, partner, item->object().lua_game_object());
}

bool CUIUpgradeWnd::CanUpgradeItem(PIItem item)
{
	VERIFY(item && m_pOthersInvOwner);
	LPCSTR item_name = item->m_section_id.c_str();
	LPCSTR partner = smart_cast<CObject*>(m_pOthersInvOwner)->cName().c_str();

	luabind::functor<bool> funct;
	R_ASSERT2(
		ai().script_engine().functor("inventory_upgrades.can_upgrade_item", funct),
		make_string<const char*>("Failed to get functor <inventory_upgrades.can_upgrade_item>, item = %s, mechanic = %s", item_name, partner)
		);

	return funct(item_name, partner, item->object().lua_game_object());
}

void CUIUpgradeWnd::SetupUpgradeItem()
{
	if (m_upgrade_selected)
	{
		m_upgrade_selected->Mark(false);
	}

	bool can_upgrade = false;
	PIItem item = CurrentIItem();
	if (item)
	{
		m_upgrade_selected = CurrentItem();
		m_upgrade_selected->Mark(true);
		can_upgrade = CanUpgradeItem(item);
	}
	m_pUpgradeWnd->InitInventory(item, can_upgrade);
	if (m_upgrade_info)
	{
		m_upgrade_info->Show(false);
	}
}

CUICellItem* CUIUpgradeWnd::CurrentItem()
{
	return m_pCurrentCellItem;
}

PIItem CUIUpgradeWnd::CurrentIItem()
{
	return	(m_pCurrentCellItem) ? (PIItem)m_pCurrentCellItem->m_pData : NULL;
}

void CUIUpgradeWnd::SwitchToTalk()
{
	GetMessageTarget()->SendMessage(this, TRADE_WND_CLOSED);
}

void CUIUpgradeWnd::BindDragDropListEvents(CUIDragDropListEx* lst)
{
	lst->m_f_item_start_drag = CUIDragDropListEx::DRAG_CELL_EVENT(this, &CUIUpgradeWnd::OnItemStartDrag);
	lst->m_f_item_db_click = CUIDragDropListEx::DRAG_CELL_EVENT(this, &CUIUpgradeWnd::OnItemDbClick);
	lst->m_f_item_selected = CUIDragDropListEx::DRAG_CELL_EVENT(this, &CUIUpgradeWnd::OnItemSelected);
}

void CUIUpgradeWnd::ColorizeItem(CUICellItem* itm)
{
	//lost alpha starts
	PIItem iitem = (PIItem)itm->m_pData;
	if (!CanUpgradeItem(iitem))
		itm->SetTextureColor(color_rgba(255, 100, 100, 255));
	else if (iitem->m_eItemPlace == eItemPlaceSlot || iitem->m_eItemPlace == eItemPlaceBelt)
		itm->SetTextureColor(color_rgba(100, 255, 100, 255));
}


void CUIUpgradeWnd::UpdatePrices()
{
	string128 buf;
	xr_sprintf(buf, "%d RU", m_pInvOwner->get_money());
	m_uidata->UIOurMoneyStatic.SetText(buf);

}

void CUIUpgradeWnd::UpdateActor()
{
	if (IsGameTypeSingle())
	{
		PIItem outfit = CurrentIItem();
		luabind::functor<void>	funct;
		R_ASSERT(ai().script_engine().functor("inventory_upgrades.move_outfit_to_inv", funct));
		funct(outfit->object_id());

		string64 buf;
		xr_sprintf(buf, "%d RU", m_pInvOwner->get_money());
		m_uidata->UIOurMoneyStatic.SetText(buf);
	}

	CActor* actor = smart_cast<CActor*>(m_pInvOwner);
	if (actor)
	{
		CWeapon* wp = smart_cast<CWeapon*>(actor->inventory().ActiveItem());
		if (wp)
			wp->ForceUpdateAmmo();
	}//actor
}

void CUIUpgradeWnd::CallMessageBoxYesNo(LPCSTR text)
{
	m_message_box_yes_no->SetText(text);
	m_message_box_yes_no->func_on_ok = CUIWndCallback::void_function(this, &CUIUpgradeWnd::OnMesBoxYes);
	m_message_box_yes_no->func_on_no = CUIWndCallback::void_function(this, &CUIUpgradeWnd::OnMesBoxNo);
	m_message_box_yes_no->ShowDialog(false);
}

void CUIUpgradeWnd::CallMessageBoxOK(LPCSTR text)
{
	m_message_box_ok->SetText(text);
	m_message_box_ok->ShowDialog(false);
}

void CUIUpgradeWnd::OnMesBoxYes(CUIWindow*, void*)
{
	if (!bRepairMode)
		m_pUpgradeWnd->OnMesBoxYes();
	else
		PerformRepair();
	//UpdateItemsPlace();
	bRepairMode = false;
}

void CUIUpgradeWnd::OnMesBoxNo(CUIWindow*, void*)
{
	UpdateItemsPlace();
	bRepairMode = false;
}

void CUIUpgradeWnd::UpdateItemsPlace(bool setupItem)
{
	if (setupItem)
	{
		SetupUpgradeItem();
	}
	UpdateActor();
	PIItem curr = CurrentIItem();
	if (curr)
		m_uidata->UIItemInfo.InitItem(curr);
}

void CUIUpgradeWnd::SeparateUpgradeItem()
{
	VERIFY(m_upgrade_selected);
	if (!m_upgrade_selected || !m_upgrade_selected->m_pData)
	{
		return;
	}
	CUIDragDropListEx* list_owner = m_upgrade_selected->OwnerList();

	m_upgrade_selected->Mark(false);
	CUICellItem* ci = list_owner->RemoveItem(m_upgrade_selected, false);
	list_owner->SetItem(ci);
}

bool CUIUpgradeWnd::SetInfoCurUpgrade(Upgrade_type* upgrade_type, CInventoryItem* inv_item)
{
	if (!m_upgrade_info) return false;
	bool res = m_upgrade_info->init_upgrade(upgrade_type, inv_item);

	if (!upgrade_type)
	{
		return false;
	}

	fit_in_rect(m_upgrade_info, Frect().set(0.0f, 0.0f, UI_BASE_WIDTH, UI_BASE_HEIGHT), 0.0f, GetWndRect().left);
	return res;
}

void CUIUpgradeWnd::TryRepairItem()
{
	PIItem item = (m_upgrade_selected) ? (PIItem)m_upgrade_selected->m_pData : NULL;
	if (!item)
	{
		return;
	}
	if (item->GetCondition() > 0.99f)
	{
		return;
	}
	LPCSTR item_name = item->m_section_id.c_str(); 
	LPCSTR partner = smart_cast<CObject*>(m_pOthersInvOwner)->cName().c_str();

	luabind::functor<bool> funct;
	R_ASSERT2(
		ai().script_engine().functor("inventory_upgrades.can_repair_item", funct),
		make_string<const char*>("Failed to get functor <inventory_upgrades.can_repair_item>, item = %s", item_name)
		);
	bool can_repair = funct(item_name, item->GetCondition(), partner);

	luabind::functor<LPCSTR> funct2;
	R_ASSERT2(
		ai().script_engine().functor("inventory_upgrades.question_repair_item", funct2),
		make_string<const char*>("Failed to get functor <inventory_upgrades.question_repair_item>, item = %s", item_name)
		);
	LPCSTR question = funct2(item_name, item->GetCondition(), can_repair, partner);

	if (can_repair)
	{
		bRepairMode = true;
		CallMessageBoxYesNo(question);
	}
	else
		CallMessageBoxOK(question);
}