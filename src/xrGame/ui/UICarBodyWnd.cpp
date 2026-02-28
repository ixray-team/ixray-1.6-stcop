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

#include "../WeaponBinoculars.h"
#include "../WeaponKnife.h"
#include "../WeaponMagazinedWGrenade.h"
#include "../PDA.h"
#include "ui_drop_amount.h"
#include "../game_sv_single.h"
#include "ai_object_location.h"

#define				CAR_BODY_XML		"carbody_new.xml"
#define				CARBODY_ITEM_XML	"carbody_item.xml"

void move_item_from_to(u16 from_id, u16 to_id, u16 what_id);
bool RemoveItemFromList(CUIDragDropListEx* lst, PIItem pItem);

CUICarBodyWnd::CUICarBodyWnd()
{
	m_pInventoryBox		= nullptr;
	m_pUIPutAll			= nullptr;
	Init				();
	Show				(false);
	m_b_need_update		= false;
	LoadCallbackGlobals(m_isCanTake, m_onCanTake, "OnCanTake");
	LoadCallbackGlobals(m_isCanMoveToPartner, m_onCanMoveToPartner, "OnCanMoveToPartner");
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

	m_pUIItemInfo					= new CUIItemInfo(); m_pUIItemInfo->SetAutoDelete(true);
	m_pUIDescWnd->AttachChild		(m_pUIItemInfo);
	m_pUIItemInfo->InitItemInfo		(Fvector2().set(0,0), Fvector2().set(m_pUIDescWnd->GetWidth(), m_pUIDescWnd->GetHeight()), CARBODY_ITEM_XML);
	
	xml_init.InitAutoStaticGroup	(uiXml, "", 0, this);


	m_UIPropertiesBox				= new CUIPropertiesBox(); m_UIPropertiesBox->SetAutoDelete(true);
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
	
	XML_NODE* stored_root							= uiXml.GetLocalRoot	();
	uiXml.SetLocalRoot					(uiXml.NavigateToNode	("action_sounds",0));
	::Sound->create						(sounds[eSndOpen],		uiXml.Read("snd_open",			0,	"interface\\inv_open"), st_Effect, sg_SourceType);
	::Sound->create						(sounds[eSndClose],		uiXml.Read("snd_close",			0,	"interface\\inv_close"), st_Effect, sg_SourceType);
	::Sound->create						(sounds[eItemToSlot],	uiXml.Read("snd_item_to_slot",	0,	"interface\\inv_slot"), st_Effect, sg_SourceType);
	::Sound->create						(sounds[eItemToBelt],	uiXml.Read("snd_item_to_belt",	0,	"interface\\inv_belt"), st_Effect, sg_SourceType);
	::Sound->create						(sounds[eItemToRuck],	uiXml.Read("snd_item_to_ruck",	0,	"interface\\inv_ruck"), st_Effect, sg_SourceType);
	::Sound->create						(sounds[eProperties],	uiXml.Read("snd_properties",	0,	"interface\\inv_properties"), st_Effect, sg_SourceType);
	::Sound->create						(sounds[eDropItem],		uiXml.Read("snd_drop_item",		0,	"interface\\inv_drop"), st_Effect, sg_SourceType);
	::Sound->create						(sounds[eAttachAddon],	uiXml.Read("snd_attach_addon",	0,	"interface\\inv_attach_addon"), st_Effect, sg_SourceType);
	::Sound->create						(sounds[eDetachAddon],	uiXml.Read("snd_detach_addon",	0,	"interface\\inv_detach_addon"), st_Effect, sg_SourceType);
	::Sound->create						(sounds[eItemUse],		uiXml.Read("snd_item_use",		0,	"interface\\inv_slot"), st_Effect, sg_SourceType);
	uiXml.SetLocalRoot					(stored_root);

	BindDragDropListEnents			(m_pUIOurBagList);
	BindDragDropListEnents			(m_pUIOthersBagList);

	m_highlight_clear = true;
	clear_highlight_lists();
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

}

CInventory* CUICarBodyWnd::GetInventory()
{
	return &m_pOurObject->inventory();
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
				LPCSTR monster_tex_name = READ_IF_EXISTS(pSettings, r_string, monster->cNameSect(), "icon", "npc_icon_unknown_data");
				m_pUICharacterInfoRight->InitCharacter("", monster_tex_name);
			}
		}else 
		{
			m_pUICharacterInfoRight->InitCharacter	(m_pOthersObject);
		}
	}

	m_UIPropertiesBox->Hide						();
	EnableAll										();
	UpdateLists										();

	if(!monster){
		CInfoPortionWrapper	*known_info_registry	= new CInfoPortionWrapper();
		known_info_registry->registry().init		(m_pOthersObject->object_id());
		KNOWN_INFO_VECTOR& known_info				= known_info_registry->registry().objects();

		KNOWN_INFO_VECTOR::iterator it = known_info.begin();
		for(int i=0;it!=known_info.end();++it,++i){
			(*it).info_id;	
			NET_Packet		P;
			CGameObject::u_EventGen		(P,GE_INFO_TRANSFER, m_pOurObject->object_id());
			P.w_u16						(0);//not used
			P.w_stringZ					((*it).info_id);			//сообщение
			P.w_u8						(1);						//добавление сообщения
			CGameObject::u_EventSend	(P);
		}
		known_info.clear	();
		xr_delete			(known_info_registry);
	}
}  

void CUICarBodyWnd::UpdateLists_delayed()
{
		m_b_need_update = true;
}

#include "UIInventoryUtilities.h"

void CUICarBodyWnd::UpdateLists()
{
	TIItemContainer								ruck_list;
	m_pUIOurBagList->ClearAll					(true);
	m_pUIOthersBagList->ClearAll				(true);

	ruck_list.clear								();
	m_pOurObject->inventory().AddAvailableItems	(ruck_list, true);
	std::sort									(ruck_list.begin(),ruck_list.end(),InventoryUtilities::GreaterRoomInRuck);

	//Наш рюкзак
	TIItemContainer::iterator it;
	for(it =  ruck_list.begin(); ruck_list.end() != it; ++it) 
	{
		CUICellItem* itm				= create_cell_item(*it);
		ColorizeItem					(itm);
		m_pUIOurBagList->SetItem		(itm);
	}


	ruck_list.clear									();
	if(m_pOthersObject)
		m_pOthersObject->inventory().AddAvailableItems	(ruck_list, false);
	else
		m_pInventoryBox->AddAvailableItems			(ruck_list);

	std::sort										(ruck_list.begin(),ruck_list.end(),InventoryUtilities::GreaterRoomInRuck);

	//Чужой рюкзак
	for(it =  ruck_list.begin(); ruck_list.end() != it; ++it) 
	{
		CUICellItem* itm							= create_cell_item(*it);
		m_pUIOthersBagList->SetItem					(itm);
	}

	InventoryUtilities::UpdateWeight				(*m_pUIOurBagWnd);
	m_b_need_update									= false;
}

void CUICarBodyWnd::SendMessage(CUIWindow *pWnd, s16 msg, void *pData)
{
	if (BUTTON_CLICKED == msg)
	{
		if (m_pUITakeAll == pWnd)
		{
			TakeAll();
		}
		else if (m_pUIPutAll == pWnd)
		{
			PutAll();
		}
	}
	else if(pWnd == m_UIPropertiesBox &&	msg == PROPERTY_CLICKED)
	{
		ProcessPropertiesBoxClicked();
	}

	inherited::SendMessage			(pWnd, msg, pData);
}

void CUICarBodyWnd::Draw()
{
	inherited::Draw	();
}


void CUICarBodyWnd::Update()
{
	if(	m_b_need_update||
		m_pOurObject->inventory().ModifyFrame()==Device.dwFrame || 
		(m_pOthersObject&&m_pOthersObject->inventory().ModifyFrame()==Device.dwFrame))

		UpdateLists		();

	
	if(m_pOthersObject && m_pOurObject->cast_game_object()->Position().distance_to(m_pOthersObject->cast_game_object()->Position()) > 3.0f)
	{
		GetHolder()->StartStopMenu(this,true);
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

CUICellItem* CUICarBodyWnd::CurrentItem()
{
	return m_pCurrentCellItem;
}

PIItem CUICarBodyWnd::CurrentIItem()
{
	return	(m_pCurrentCellItem)?(PIItem)m_pCurrentCellItem->m_pData : nullptr;
}

void CUICarBodyWnd::SetCurrentItem(CUICellItem* itm)
{
	m_pCurrentCellItem		= itm;
	m_pUIItemInfo->InitItem(CurrentItem(), nullptr, CurrentIItem() ? CurrentIItem()->Cost() : u32(-1), nullptr, true);
	TryHidePropertiesBox();
}

void CUICarBodyWnd::TakeAll()
{
	u32 cnt				= m_pUIOthersBagList->ItemsCount();
	u16 tmp_id = 0;
	if(m_pInventoryBox)
	{
		tmp_id	= m_pOurObject->cast_game_object()->ID();
	}

	for(u32 i=0; i<cnt; ++i)
	{
		CUICellItem*	ci = m_pUIOthersBagList->GetItemIdx(i);
		for(u32 j=0; j<ci->ChildsCount(); ++j)
		{
			PIItem _itm		= (PIItem)(ci->Child(j)->m_pData);
			if(m_pOthersObject)
				TransferItem	(_itm, m_pOthersObject, m_pOurObject, false);
			else{
				move_item_from_to		(m_pInventoryBox->ID(), tmp_id, _itm->object().ID());
//.				Actor()->callback(GameObject::eInvBoxItemTake)( m_pInventoryBox->lua_game_object(), _itm->object().lua_game_object() );
			}
		
		}
		PIItem itm		= (PIItem)(ci->m_pData);
		if(m_pOthersObject)
			TransferItem	(itm, m_pOthersObject, m_pOurObject, false);
		else{
			move_item_from_to		(m_pInventoryBox->ID(), tmp_id, itm->object().ID());
//.			Actor()->callback(GameObject::eInvBoxItemTake)(m_pInventoryBox->lua_game_object(), itm->object().lua_game_object() );
		}

	}
}

void CUICarBodyWnd::PutAll()
{
	u32 Iter = 0;
	while (Iter < m_pUIOurBagList->ItemsCount())
	{
		CUICellItem* ci = m_pUIOurBagList->GetItemIdx(Iter);
		if (!ToDeadBodyBag(ci, false))
		{
			++Iter;
		}
	}
}

bool CUICarBodyWnd::ToDeadBodyBag(CUICellItem* itm, bool b_use_cursor_pos)
{
	PIItem quest_item = (PIItem)itm->m_pData;
	if (quest_item->IsQuestItem())
		return false;

	if (m_pOthersObject)
	{
		if ( !m_pOthersObject->deadbody_can_take_status() )
		{
			return false;
		}
		if (m_isCanMoveToPartner && m_pOthersObject->is_alive())
		{

			luabind::functor<bool> funct;
			R_ASSERT2(ai().script_engine().functor(m_onCanMoveToPartner, funct), "failed to get OnCanMoveToPartner functor");
			float itmWeight = quest_item->Weight();
			float partner_inv_weight = m_pOthersObject->inventory().CalcTotalWeight();
			float partner_max_weight = m_pOthersObject->MaxCarryWeight();

			if (funct(m_pOthersObject->cast_game_object()->lua_game_object(), quest_item->object().lua_game_object(), 0, 0, itmWeight, partner_inv_weight, partner_max_weight) == false)
				return false;
			
		}
	}
	else // box
	{
		if (!m_pInventoryBox->can_take())
		{
			return false;
		}

		if (m_isCanTake)
		{
			luabind::functor<bool> funct;
			R_ASSERT2(ai().script_engine().functor(m_onCanTake, funct), "failed to get OnCanTake functor");

			if (funct(m_pInventoryBox->cast_game_object()->lua_game_object(), quest_item->cast_game_object()->lua_game_object()) == false)
			{
				return false;
			}
		}
	}

	CUIDragDropListEx* old_owner = itm->OwnerList();
	CUIDragDropListEx* new_owner = nullptr;

	if(b_use_cursor_pos)
	{
		new_owner						= CUIDragDropListEx::m_drag_item->BackList();
		VERIFY							(new_owner==m_pUIOthersBagList);
	}else
		new_owner						= m_pUIOthersBagList;
	
	CUICellItem* i						= old_owner->RemoveItem(itm, (old_owner==new_owner) );

	if(b_use_cursor_pos)
		new_owner->SetItem				(i,old_owner->GetDragItemPosition());
	else
		new_owner->SetItem				(i);

	PIItem iitem						= (PIItem)i->m_pData;

	if (m_pOthersObject)
	{
		move_item_from_to				(m_pOurObject->object_id(), m_pOthersObject->object_id(), iitem->object_id());
	}
	else // box
	{
		move_item_from_to				(m_pOurObject->object_id(), m_pInventoryBox->ID(), iitem->object_id());
	}
	
	return true;
}

#include "../../xrEngine/xr_level_controller.h"

bool CUICarBodyWnd::OnKeyboardAction(int dik, EUIMessages keyboard_action)
{
	if( inherited::OnKeyboardAction(dik,keyboard_action) )return true;

	if(keyboard_action==WINDOW_KEY_PRESSED && (is_binded(kUSE, dik) || is_binded(kQUIT, dik))) 
	{
		GetHolder()->StartStopMenu(this,true);
		return true;
	}
	if(keyboard_action==WINDOW_KEY_PRESSED && is_binded(kSPRINT_TOGGLE, dik))
	{
		TakeAll();
		return true;
	}
	return false;
}

#include "../medkit.h"
#include "../antirad.h"

void CUICarBodyWnd::ActivatePropertiesBox()
{
	TryHidePropertiesBox();
	
	PIItem item = CurrentIItem();
	if(!item) 
	{
		return;
	}

	CUICellItem* cell_item = CurrentItem();
	m_UIPropertiesBox->RemoveAll();
	bool b_show = false;

	PropertiesBoxForWeapon(cell_item, item, b_show);
	PropertiesBoxForAddon(item, b_show);
	PropertiesBoxForUsing(item, b_show);
	PropertiesBoxForPlaying(item, b_show);
	PropertiesBoxForDrop(cell_item, item, b_show);
	PropertiesBoxForParse(item, b_show);

	if(b_show)
	{
		m_UIPropertiesBox->AutoUpdateSize	();

		Fvector2						cursor_pos;
		Frect							vis_rect;

		GetAbsoluteRect					(vis_rect);
		cursor_pos						= GetUICursor().GetCursorPosition();
		cursor_pos.sub					(vis_rect.lt);
		m_UIPropertiesBox->Show		(vis_rect, cursor_pos);
		PlaySnd(eProperties);
	}
}

void CUICarBodyWnd::EatItem()
{
	CObject* current_entity = Level().CurrentEntity();
	CActor *pActor				= current_entity != nullptr ? current_entity->cast_actor() : nullptr;
	if(!pActor)					return;

	CUIDragDropListEx* owner_list		= CurrentItem()->OwnerList();
	if (owner_list == m_pUIOthersBagList)
	{
		u16 owner_id = (m_pInventoryBox) ? m_pInventoryBox->ID() : m_pOthersObject->cast_game_object()->ID();

		move_item_from_to(	owner_id, //from
					Actor()->ID(), //to
					CurrentIItem()->object().ID());
	}

	NET_Packet					P;
	CGameObject::u_EventGen		(P, GEG_PLAYER_ITEM_EAT, Actor()->ID());
	P.w_u16						(CurrentIItem()->object().ID());
	CGameObject::u_EventSend	(P);
	clear_highlight_lists		();
}


bool CUICarBodyWnd::OnItemDrop(CUICellItem* itm)
{
	CUIDragDropListEx*	old_owner		= itm->OwnerList();
	CUIDragDropListEx*	new_owner		= CUIDragDropListEx::m_drag_item->BackList();
	
	if(old_owner==new_owner || !old_owner || !new_owner || (false&&new_owner==m_pUIOthersBagList&&m_pInventoryBox))
					return true;

	if(m_pOthersObject)
	{
		if( TransferItem		(	CurrentIItem(),
								(old_owner==m_pUIOthersBagList)?m_pOthersObject:m_pOurObject, 
								(old_owner==m_pUIOurBagList)?m_pOthersObject:m_pOurObject, 
								(old_owner==m_pUIOurBagList)
							)
			)
		{
			CUICellItem* ci					= old_owner->RemoveItem(CurrentItem(), false);
			new_owner->SetItem				(ci);
		}
	}else
	{
		u16 tmp_id	= m_pOurObject->cast_game_object()->ID();

		bool bMoveDirection		= (old_owner==m_pUIOthersBagList);

		move_item_from_to		(
								bMoveDirection?m_pInventoryBox->ID():tmp_id,
								bMoveDirection?tmp_id:m_pInventoryBox->ID(),
								CurrentIItem()->object().ID());


//		Actor()->callback		(GameObject::eInvBoxItemTake)(m_pInventoryBox->lua_game_object(), CurrentIItem()->object().lua_game_object() );

		CUICellItem* ci			= old_owner->RemoveItem(CurrentItem(), false);
		new_owner->SetItem		(ci);
	}
	SetCurrentItem					(nullptr);

	return				true;
}

bool CUICarBodyWnd::OnItemStartDrag(CUICellItem* itm)
{
	return				false; //default behaviour
}

bool CUICarBodyWnd::OnItemDbClick(CUICellItem* itm)
{
	CUIDragDropListEx*	old_owner		= itm->OwnerList();
	CUIDragDropListEx*	new_owner		= (old_owner==m_pUIOthersBagList)?m_pUIOurBagList:m_pUIOthersBagList;

	if (old_owner == m_pUIOurBagList)
		ToDeadBodyBag(CurrentItem(), false);
	else
		ToBag(CurrentItem(), false);

	SetCurrentItem				(nullptr);

	return						true;
}

bool CUICarBodyWnd::OnItemSelected(CUICellItem* itm)
{
	SetCurrentItem		(itm);
	return				false;
}

bool CUICarBodyWnd::OnItemRButtonClick(CUICellItem* itm)
{
	SetCurrentItem				(itm);
	ActivatePropertiesBox		();
	return						false;
}

bool CUICarBodyWnd::TransferItem(PIItem itm, CInventoryOwner* owner_from, CInventoryOwner* owner_to, bool b_check)
{
	VERIFY									(nullptr==m_pInventoryBox);
	CGameObject* go_from					= owner_from->cast_game_object();
	CGameObject* go_to						= owner_to->cast_game_object();

	if(go_to->cast_base_monster() != nullptr)	return false;
	if(b_check)
	{
		float invWeight						= owner_to->inventory().CalcTotalWeight();
		float maxWeight						= owner_to->inventory().GetMaxWeight();
		float itmWeight						= itm->Weight();
		if(invWeight+itmWeight >=maxWeight)	return false;
	}

	move_item_from_to(go_from->ID(), go_to->ID(), itm->object().ID());

	return true;
}

void CUICarBodyWnd::BindDragDropListEnents(CUIDragDropListEx* lst)
{
	lst->m_f_item_drop				= CUIDragDropListEx::DRAG_CELL_EVENT(this,&CUICarBodyWnd::OnItemDrop);
	lst->m_f_item_start_drag		= CUIDragDropListEx::DRAG_CELL_EVENT(this,&CUICarBodyWnd::OnItemStartDrag);
	lst->m_f_item_db_click			= CUIDragDropListEx::DRAG_CELL_EVENT(this,&CUICarBodyWnd::OnItemDbClick);
	lst->m_f_item_selected			= CUIDragDropListEx::DRAG_CELL_EVENT(this,&CUICarBodyWnd::OnItemSelected);
	lst->m_f_item_rbutton_click		= CUIDragDropListEx::DRAG_CELL_EVENT(this,&CUICarBodyWnd::OnItemRButtonClick);
	lst->m_f_item_focus_received	= CUIDragDropListEx::DRAG_CELL_EVENT(this,&CUICarBodyWnd::OnItemFocusReceive);
	lst->m_f_item_focus_lost		= CUIDragDropListEx::DRAG_CELL_EVENT(this,&CUICarBodyWnd::OnItemFocusLost);
	lst->m_f_item_focused_update	= CUIDragDropListEx::DRAG_CELL_EVENT(this,&CUICarBodyWnd::OnItemFocusedUpdate);
}

bool CUICarBodyWnd::OnItemFocusReceive(CUICellItem* itm)
{
	itm->m_selected = true;
	set_highlight_item(itm);

	return true;
}

bool CUICarBodyWnd::OnItemFocusLost(CUICellItem* itm)
{
	if (itm)
	{
		itm->m_selected = false;
	}
	clear_highlight_lists();

	return true;
}

bool CUICarBodyWnd::OnItemFocusedUpdate(CUICellItem* itm)
{
	if (itm)
	{
		itm->m_selected = true;
		if (m_highlight_clear)
		{
			set_highlight_item(itm);
		}
	}

	return true;
}

void CUICarBodyWnd::set_highlight_item(CUICellItem* cell_item)
{
	PIItem item = (PIItem)cell_item->m_pData;
	if (!item)
	{
		return;
	}

	highlight_armament(item, m_pUIOurBagList);
	highlight_armament(item, m_pUIOthersBagList);
	m_highlight_clear = false;
}


void CUICarBodyWnd::clear_highlight_lists()
{

	m_pUIOurBagList->clear_select_armament();
	m_pUIOthersBagList->clear_select_armament();

	m_highlight_clear = true;
}

void CUICarBodyWnd::highlight_armament(PIItem item, CUIDragDropListEx* ddlist)
{
	ddlist->clear_select_armament();
	highlight_ammo_for_weapon(item, ddlist);
	highlight_weapons_for_ammo(item, ddlist);
	highlight_weapons_for_addon(item, ddlist);
}

void CUICarBodyWnd::highlight_ammo_for_weapon( PIItem weapon_item, CUIDragDropListEx* ddlist )
{
	VERIFY( weapon_item );
	VERIFY( ddlist );
	static xr_vector<shared_str>	ammo_types;
	ammo_types.resize(0);

	CWeapon* weapon = weapon_item->cast_weapon();
	CWeaponBinoculars* binoc = weapon_item->cast_weapon_binoculars();
	CWeaponKnife* knife = weapon_item->cast_weapon_knife();
	if ( !weapon || binoc || knife)
	{
		return;
	}
	ammo_types.assign( weapon->m_ammoTypes.begin(), weapon->m_ammoTypes.end() );

	CWeaponMagazinedWGrenade* wg = weapon_item->cast_weapon_magazined_w_grenade();
	if ( wg )
	{
		if ( wg->IsGrenadeLauncherAttached() && wg->m_ammoTypes2.size() )
		{
			ammo_types.insert( ammo_types.end(), wg->m_ammoTypes2.begin(), wg->m_ammoTypes2.end() );
		}
	}
	
	if ( ammo_types.size() == 0 )
	{
		return;
	}
	xr_vector<shared_str>::iterator ite = ammo_types.end();
	
	u32 const cnt = ddlist->ItemsCount();
	for ( u32 i = 0; i < cnt; ++i )
	{
		CUICellItem* ci = ddlist->GetItemIdx(i);
		PIItem item = (PIItem)ci->m_pData;
		if ( !item )
		{
			continue;
		}
		CWeaponAmmo* ammo = item->cast_weapon_ammo();
		if ( !ammo )
		{
			highlight_addons_for_weapon( weapon_item, ci );
			continue; // for i
		}
		shared_str const& ammo_name = item->object().cNameSect();

		xr_vector<shared_str>::iterator itb = ammo_types.begin();
		for ( ; itb != ite; ++itb )
		{
			if ( ammo_name._get() == (*itb)._get() )
			{
				ci->m_select_armament = true;
				break; // itb
			}
		}
	}//for i

}

void CUICarBodyWnd::highlight_weapons_for_ammo( PIItem ammo_item, CUIDragDropListEx* ddlist )
{
	VERIFY( ammo_item );
	VERIFY( ddlist );
	CWeaponAmmo* ammo = ammo_item->cast_weapon_ammo();
	CWeaponBinoculars* binoc = ammo_item->cast_weapon_binoculars();
	CWeaponKnife* knife = ammo_item->cast_weapon_knife();
	if ( !ammo  )
	{
		return;
	}
	
	shared_str const& ammo_name = ammo_item->object().cNameSect();

	u32 const cnt = ddlist->ItemsCount();
	for ( u32 i = 0; i < cnt; ++i )
	{
		CUICellItem* ci = ddlist->GetItemIdx(i);
		PIItem item = (PIItem)ci->m_pData;
		if ( !item )
		{
			continue;
		}
		CWeapon* weapon = item->cast_weapon();
		if (!weapon || binoc || knife)
		{
			continue;
		}

		xr_vector<shared_str>::iterator itb = weapon->m_ammoTypes.begin();
		xr_vector<shared_str>::iterator ite = weapon->m_ammoTypes.end();
		for ( ; itb != ite; ++itb )
		{
			if ( ammo_name._get() == (*itb)._get() )
			{
				ci->m_select_armament = true;
				break; // for itb
			}
		}
		
		CWeaponMagazinedWGrenade* wg = item->cast_weapon_magazined_w_grenade();
		if ( !wg || !wg->IsGrenadeLauncherAttached() || !wg->m_ammoTypes2.size() )
		{
			continue; // for i
		}
		itb = wg->m_ammoTypes2.begin();
		ite = wg->m_ammoTypes2.end();
		for ( ; itb != ite; ++itb )
		{
			if ( ammo_name._get() == (*itb)._get() )
			{
				ci->m_select_armament = true;
				break; // for itb
			}
		}
	}//for i

}

bool CUICarBodyWnd::highlight_addons_for_weapon( PIItem weapon_item, CUICellItem* ci )
{
	PIItem item = (PIItem)ci->m_pData;
	if ( !item )
	{
		return false;
	}

	CScope* pScope = item->cast_addon_scope();
	if (pScope && weapon_item->CanAttach(item))
	{
		ci->m_select_armament = true;
		return true;
	}

	CSilencer* pSilencer = item->cast_addon_silencer();
	if ( pSilencer && weapon_item->CanAttach(pSilencer) )
	{
		ci->m_select_armament = true;
		return true;
	}

	CGrenadeLauncher* pGrenadeLauncher = item->cast_addon_grenade_launcher();
	if ( pGrenadeLauncher && weapon_item->CanAttach(pGrenadeLauncher) )
	{
		ci->m_select_armament = true;
		return true;
	}
	return false;
}

void CUICarBodyWnd::highlight_weapons_for_addon( PIItem addon_item, CUIDragDropListEx* ddlist )
{
	VERIFY( addon_item );
	VERIFY( ddlist );

	CScope*				pScope				= addon_item->cast_addon_scope();
	CSilencer*			pSilencer			= addon_item->cast_addon_silencer();
	CGrenadeLauncher*	pGrenadeLauncher	= addon_item->cast_addon_grenade_launcher();

	if ( !pScope && !pSilencer && !pGrenadeLauncher )
	{
		return;
	}
	
	u32 const cnt = ddlist->ItemsCount();
	for ( u32 i = 0; i < cnt; ++i )
	{
		CUICellItem* ci = ddlist->GetItemIdx(i);
		PIItem item = (PIItem)ci->m_pData;
		if ( !item )
		{
			continue;
		}
		CWeapon* weapon = item->cast_weapon();
		if ( !weapon )
		{
			continue;
		}

		if (pScope && weapon->ScopeAttachable() && weapon->ScopeFit(pScope))
		{
			ci->m_select_armament = true;
			continue;
		}
		if ( pSilencer && weapon->CanAttach(pSilencer) )
		{
			ci->m_select_armament = true;
			continue;
		}
		if ( pGrenadeLauncher && weapon->CanAttach(pGrenadeLauncher) )
		{
			ci->m_select_armament = true;
			continue;
		}

	}//for i
}

void CUICarBodyWnd::PropertiesBoxForDrop(CUICellItem* cell_item, PIItem item, bool& b_show)
{
	if (!item->IsQuestItem())
	{
		if (item->parent_id() == m_pOurObject->object_id())
		{
			m_UIPropertiesBox->AddItem("st_move_to", nullptr, INVENTORY_DROP_ACTION);
			b_show = true;

			if (cell_item->ChildsCount())
			{
				m_UIPropertiesBox->AddItem("st_move_amount", (void*)77, INVENTORY_DROP_ACTION);
				m_UIPropertiesBox->AddItem("st_move_all", (void*)33, INVENTORY_DROP_ACTION);
			}
		}
	}
	if (item->parent_id() != m_pOurObject->object_id())
	{
		m_UIPropertiesBox->AddItem("st_take_to", nullptr, INVENTORY_DROP_ACTION);
		b_show = true;

		if (cell_item->ChildsCount())
		{
			m_UIPropertiesBox->AddItem("st_take_amount", (void*)77, INVENTORY_DROP_ACTION);
			m_UIPropertiesBox->AddItem("st_take_all", (void*)33, INVENTORY_DROP_ACTION);
		}
	}
}

void CUICarBodyWnd::ProcessPropertiesBoxClicked()
{
	PIItem			item		= CurrentIItem();
	CUICellItem*	cell_item	= CurrentItem();
	if ( !m_UIPropertiesBox->GetClickedItem() || !item || !cell_item || !cell_item->OwnerList() )
	{
		return;
	}
	CWeapon* weapon = item->cast_weapon();

	switch (m_UIPropertiesBox->GetClickedItem()->GetTAG() )
	{
	case INVENTORY_EAT_ACTION:
		TryUseItem( cell_item );
		break;	
	case INVENTORY_EAT2_ACTION:
	{
		CGameObject* GO = item->cast_game_object();
		LPCSTR functor_name = READ_IF_EXISTS(pSettings, r_string, GO->cNameSect(), "use1_functor", 0);
		if (functor_name)
		{
			luabind::functor<bool>	funct1;
			if (ai().script_engine().functor(functor_name, funct1))
			{
				if (funct1(GO->lua_game_object()))
					TryUseItem(cell_item);
			}
		}
		break;
	}
	case INVENTORY_EAT3_ACTION:
	{
		CGameObject* GO = item->cast_game_object();
		LPCSTR functor_name = READ_IF_EXISTS(pSettings, r_string, GO->cNameSect(), "use2_functor", 0);
		if (functor_name)
		{
			luabind::functor<bool>	funct2;
			if (ai().script_engine().functor(functor_name, funct2))
			{
				if (funct2(GO->lua_game_object()))
					TryUseItem(cell_item);
			}
		}
		break;
	}
	case INVENTORY_EAT4_ACTION:
	{
		CGameObject* GO = item->cast_game_object();
		LPCSTR functor_name = READ_IF_EXISTS(pSettings, r_string, GO->cNameSect(), "use3_functor", 0);
		if (functor_name)
		{
			luabind::functor<bool>	funct3;
			if (ai().script_engine().functor(functor_name, funct3))
			{
				if (funct3(GO->lua_game_object()))
					TryUseItem(cell_item);
			}
		}
		break;
	}
	case INVENTORY_EAT5_ACTION:
	{
		CGameObject* GO = item->cast_game_object();
		LPCSTR functor_name = READ_IF_EXISTS(pSettings, r_string, GO->cNameSect(), "use4_functor", 0);
		if (functor_name)
		{
			luabind::functor<bool>	funct4;
			if (ai().script_engine().functor(functor_name, funct4))
			{
				if (funct4(GO->lua_game_object()))
					TryUseItem(cell_item);
			}
		}
		break;
	}
	case INVENTORY_DROP_ACTION:
		{
			void* d_ = m_UIPropertiesBox->GetClickedItem()->GetData();
			if(item->parent_id() == m_pOurObject->object_id()) 
			{
				auto ownerID = m_pOthersObject ? m_pOthersObject->object_id() : m_pInventoryBox->ID();

				if(d_ == (void*)33) 
				{
					MoveAllCurrentItem(cell_item->ChildsCount() + 1);
				}
				else if (m_pItemDropAmountWnd != nullptr && d_ == (void*)77)
				{
					m_pItemDropAmountWnd->ShowDropAmount(cell_item->ChildsCount(), CUIItemDropAmountWnd::eModeMove, item);
				}
				else
				{
					move_item_from_to(item->parent_id(), ownerID, item->object_id());
				}
			}
			else 
			{
				if(d_ == (void*)33) 
				{
					TakeAllCurrentItem(cell_item->ChildsCount() + 1);
				}
				else if (m_pItemDropAmountWnd != nullptr && d_ == (void*)77)
				{
					m_pItemDropAmountWnd->ShowDropAmount(cell_item->ChildsCount(), CUIItemDropAmountWnd::eModeTake, item);
				}
				else
				{
					ToBag(cell_item, false);
				}
			}
			break;
		}
	case INVENTORY_DETACH_SCOPE_ADDON:
		if ( weapon )
		{
			DetachAddon( weapon->GetScopeName().c_str() );
			for ( u32 i = 0; i < cell_item->ChildsCount(); ++i )
			{
				CUICellItem*	child_itm	= cell_item->Child(i);
				PIItem			child_iitm	= (PIItem)(child_itm->m_pData);
				CWeapon* wpn = child_iitm != nullptr ? child_iitm->cast_weapon() : nullptr;
				if ( child_iitm && wpn )
				{
					DetachAddon(wpn->GetScopeName().c_str(), child_iitm);
				}
			}
		}
		break;
	case INVENTORY_DETACH_SILENCER_ADDON:
		if ( weapon )
		{
			DetachAddon( weapon->GetSilencerName().c_str() );
			for ( u32 i = 0; i < cell_item->ChildsCount(); ++i )
			{
				CUICellItem*	child_itm	= cell_item->Child(i);
				PIItem			child_iitm	= (PIItem)(child_itm->m_pData);
				CWeapon* wpn = child_iitm != nullptr ? child_iitm->cast_weapon() : nullptr;
				if ( child_iitm && wpn )
				{
					DetachAddon(wpn->GetSilencerName().c_str(), child_iitm);
				}
			}
		}
		break;
	case INVENTORY_DETACH_GRENADE_LAUNCHER_ADDON:
		if ( weapon )
		{
			DetachAddon( weapon->GetGrenadeLauncherName().c_str() );
			for ( u32 i = 0; i < cell_item->ChildsCount(); ++i )
			{
				CUICellItem*	child_itm	= cell_item->Child(i);
				PIItem			child_iitm	= (PIItem)(child_itm->m_pData);
				CWeapon* wpn = child_iitm != nullptr ? child_iitm->cast_weapon() : nullptr;
				if ( child_iitm && wpn )
				{
					DetachAddon(wpn->GetGrenadeLauncherName().c_str(), child_iitm);
				}
			}
		}
		break;
	case INVENTORY_RELOAD_MAGAZINE:
		if ( weapon )
		{
			weapon->Action( kWPN_RELOAD, CMD_START );
		}
		break;
	case INVENTORY_UNLOAD_MAGAZINE:
		{
			CWeapon* weapon = (CWeapon*)cell_item->m_pData;
			CWeaponMagazined* weap_mag = weapon != nullptr ? weapon->cast_weapon_magazined() : nullptr;
			if (weap_mag == nullptr)
			{
				break;
			}

			UnloadWeapon(weap_mag);
			weap_mag->UnloadChamber();
			for (u32 i = 0; i < cell_item->ChildsCount(); ++i)
			{
				CUICellItem* child_itm = cell_item->Child(i);
				weapon = (CWeapon*)child_itm->m_pData;
				CWeaponMagazined* child_weap_mag = weapon != nullptr ? weapon->cast_weapon_magazined() : nullptr;
				if (child_weap_mag != nullptr)
				{
					UnloadWeapon(child_weap_mag);
					child_weap_mag->UnloadChamber();
				}
			}
			break;
		}
	case INVENTORY_PLAY_ACTION:
		{
			CPda* pPda = item->cast_pda();
			if(!pPda)
				break;
			pPda->PlayScriptFunction();
			break;
		}
	case INVENTORY_PARSE_ITEM:
	{
		game_sv_Single* tpGame = Level().Server->game != nullptr ? Level().Server->game->cast_game_sv_single() : nullptr;
		if (tpGame == nullptr) {
			break;
		}

		CActor* actor = Level().CurrentEntity() ? Level().CurrentEntity()->cast_actor() : nullptr;
		if (actor == nullptr) {
			break;
		}

		extern CSE_Abstract* CALifeSimulator__spawn_item2(CALifeSimulator * self_, LPCSTR section, const Fvector & position, u32 level_vertex_id, GameGraph::_GRAPH_ID game_vertex_id, ALife::_OBJECT_ID id_parent);

		int Count = item->m_parse_params.m_items.size();
		int Count2 = item->m_parse_params.m_chances.size();

		for (int i = 0; i < Count; ++i)
		{
			float chance = 0.0f;

			if (i >= Count2)
			{
				chance = item->m_parse_params.m_chances.back();
			}
			else
			{
				chance = item->m_parse_params.m_chances[i];
			}

			if (chance >= ::Random.randF(0.0f, 1.0f))
			{
				CALifeSimulator__spawn_item2(&tpGame->alife(), *item->m_parse_params.m_items[i], actor->Position(), actor->ai_location().level_vertex_id(), actor->ai_location().game_vertex_id(), actor->ID());
			}
		}
		item->object().DestroyObject();
	}break;
	}//switch

//	SetCurrentItem( nullptr );

//	UpdateConditionProgressBars();
}//ProcessPropertiesBoxClicked

void CUICarBodyWnd::DetachAddon(LPCSTR addon_name, PIItem itm)
{
	PlaySnd										(eDetachAddon);
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

void CUICarBodyWnd::UnloadWeapon(CWeaponMagazined* pWpn)
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

void CUICarBodyWnd::TakeAllCurrentItem(u32 item_amount)
{
	u32 const childCount = CurrentItem()->ChildsCount();
	u32 const totalCount = 1 + childCount;
	u32 const toTake = (item_amount > totalCount) ? totalCount : item_amount;
	u32 const childrenToTake = (toTake < childCount) ? toTake : childCount;

	for (u32 i = 0; i < childrenToTake; ++i)
	{
		CUICellItem* child_itm = CurrentItem()->PopChild(nullptr);
		PIItem child_iitm = (PIItem)child_itm->m_pData;
		move_item_from_to(child_iitm->parent_id(), m_pOurObject->object_id(), child_iitm->object_id());
		m_pUIOurBagList->SetItem(child_itm);
	}

	if (toTake > childCount)
	{
		CUICellItem* parent_itm = CurrentItem();
		PIItem parent_iitm = CurrentIItem();
		move_item_from_to(parent_iitm->parent_id(), m_pOurObject->object_id(), parent_iitm->object_id());
		parent_itm = m_pUIOthersBagList->RemoveItem(parent_itm, true);
		if (parent_itm)
			m_pUIOurBagList->SetItem(parent_itm);
	}
}

void CUICarBodyWnd::MoveAllCurrentItem(u32 item_amount)
{
	auto ownerID = m_pOthersObject ? m_pOthersObject->object_id() : m_pInventoryBox->ID();
	u32 const childCount = CurrentItem()->ChildsCount();
	u32 const totalCount = 1 + childCount;
	u32 const toMove = (item_amount > totalCount) ? totalCount : item_amount;
	u32 const childrenToMove = (toMove < childCount) ? toMove : childCount;
	for (u32 i = 0; i < childrenToMove; ++i)
	{
		CUICellItem* child_itm = CurrentItem()->Child(i);
		PIItem child_iitm = (PIItem)child_itm->m_pData;
		move_item_from_to(CurrentIItem()->parent_id(), ownerID, child_iitm->object_id());
	}
	if (toMove > childCount)
		move_item_from_to(CurrentIItem()->parent_id(), ownerID, CurrentIItem()->object_id());
}

bool CUICarBodyWnd::TryUseItem( CUICellItem* cell_itm )
{
	if ( !cell_itm )
	{
		return false;
	}
	PIItem item	= (PIItem)cell_itm->m_pData;

	CBottleItem*	pBottleItem		= smart_cast<CBottleItem*>	(item);
	CMedkit*		pMedkit			= smart_cast<CMedkit*>		(item);
	CAntirad*		pAntirad		= smart_cast<CAntirad*>		(item);
	CEatableItem*	pEatableItem	= smart_cast<CEatableItem*>	(item);

	if ( !(pMedkit || pAntirad || pEatableItem || pBottleItem) )
	{
		return false;
	}
	if ( !item->Useful() )
	{
		return false;
	}

	u16 recipient = m_pOurObject->object_id();
	if ( item->parent_id() != recipient )
	{
		//move_item_from_to	(itm->parent_id(), recipient, itm->object_id());
		cell_itm->OwnerList()->RemoveItem( cell_itm, false );
	}

	SendEvent_Item_Eat		( item, recipient );
	PlaySnd					( eItemUse );
//	SetCurrentItem			( nullptr );
	return true;
}

void CUICarBodyWnd::SendEvent_Item_Eat(PIItem pItem, u16 recipient)
{
	if(pItem->parent_id()!=recipient)
		move_item_from_to			(pItem->parent_id(), recipient, pItem->object_id());

	NET_Packet						P;
	CGameObject::u_EventGen			(P, GEG_PLAYER_ITEM_EAT, recipient);
	P.w_u16							(pItem->object().ID());
	CGameObject::u_EventSend		(P);
	clear_highlight_lists			();
};

bool CUICarBodyWnd::ToBag(CUICellItem* itm, bool b_use_cursor_pos)
{
	PIItem	iitem						= (PIItem)itm->m_pData;

	bool b_own_item						= (iitem->parent_id()==m_pOurObject->object_id());

	bool b_already						= m_pOurObject->inventory().InRuck(iitem);

	CUIDragDropListEx*	old_owner		= itm->OwnerList();
	CUIDragDropListEx*	new_owner		= nullptr;
	if(b_use_cursor_pos)
	{
			new_owner					= CUIDragDropListEx::m_drag_item->BackList();
			VERIFY						(new_owner==m_pUIOurBagList);
	}else
			new_owner					= m_pUIOurBagList;

	if(m_pOurObject->inventory().CanPutInRuck(iitem) || (b_already && (new_owner!=old_owner)) )
	{
		// Pavel: если предмет в iActorTrade, то он уже должен находиться в рюкзаке
		// Проверка нужна для того, чтобы не сбрасывалась граната в МП,
		// при перекладывании из iActorTrade
		bool result = b_already || (!b_own_item || m_pOurObject->inventory().Ruck(iitem));
		R_ASSERT(result);

		CUICellItem* i						= old_owner->RemoveItem(itm, (old_owner==new_owner) );
		if(!i)
			return false;

		if(b_use_cursor_pos)
			new_owner->SetItem				(i,old_owner->GetDragItemPosition());
		else
			new_owner->SetItem				(i);

		if(!b_already || !b_own_item)
			SendEvent_Item2Ruck					(iitem, m_pOurObject->object_id());

		return true;
	}
	return false;
}

void CUICarBodyWnd::SendEvent_Item2Ruck(PIItem pItem, u16 recipient)
{
	if(pItem->parent_id()!=recipient)
		move_item_from_to			(pItem->parent_id(), recipient, pItem->object_id());

	NET_Packet						P;
	CGameObject::u_EventGen			(P, GEG_PLAYER_ITEM2RUCK, pItem->object().H_Parent()->ID());
	P.w_u16							(pItem->object().ID());
	CGameObject::u_EventSend		(P);
	clear_highlight_lists			();

	PlaySnd							(eItemToRuck);
};

void CUICarBodyWnd::PlaySnd(eCarBodySndAction a)
{
	if (sounds[a].handle())
		sounds[a].play					(nullptr, sm_2D);
}

void CUICarBodyWnd::ColorizeItem(CUICellItem* itm)
{
	PIItem IItm = (PIItem)itm->m_pData;
	if (IItm->CurrSlot() && IItm->CurrPlace() == eItemPlaceSlot && IItm->BaseSlot() != GRENADE_SLOT && !itm->ChildsCount())
	{
		itm->SetTextureColor(color_rgba(100, 255, 100, 255));
	}
	else
	{
		itm->SetTextureColor(color_rgba(255, 255, 255, 255));
	}
}