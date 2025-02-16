#include "pch_script.h"
#include "UIInventoryWnd.h"

#include "xrUIXmlParser.h"
#include "UIXmlInit.h"
#include "../../xrEngine/string_table.h"

#include "../actor.h"
#include "../uigamesp.h"
#include "../hudmanager.h"

#include "../CustomOutfit.h"

#include "../weapon.h"

#include "../script_process.h"

#include "../eatable_item.h"
#include "../inventory.h"

#include "UIInventoryUtilities.h"
using namespace InventoryUtilities;


#include "../InfoPortion.h"
#include "../level.h"
#include "../game_base_space.h"
#include "../entitycondition.h"

#include "../game_cl_base.h"
#include "../ActorCondition.h"
#include "UIDragDropListEx.h"
#include "UIOutfitSlot.h"
#include "UI3tButton.h"


CUIInventoryWnd*	g_pInvWnd = NULL;

CUIInventoryWnd::CUIInventoryWnd()
{
	m_iCurrentActiveSlot				= NO_ACTIVE_SLOT;
	UIRank								= NULL;
	Init								();
	SetCurrentItem						(NULL);

	g_pInvWnd							= this;	
	m_b_need_reinit						= false;	
}

void CUIInventoryWnd::Init()
{
	CUIXml								uiXml;

	if (!ui_hud_type)
		ui_hud_type = 1;

	string128		INVENTORY_ITEM_XML;
	xr_sprintf		(INVENTORY_ITEM_XML, "inventory_item_%d.xml", ui_hud_type);

	string128		INVENTORY_XML;
	xr_sprintf		(INVENTORY_XML, "inventory_new_%d.xml", ui_hud_type);

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
	UIItemInfo.InitItemInfo			(Fvector2().set(0,0),Fvector2().set(UIDescrWnd.GetWidth(), UIDescrWnd.GetHeight()), INVENTORY_ITEM_XML);

	AttachChild							(&UIPersonalWnd);
	xml_init.InitFrameWindow			(uiXml, "character_frame_window", 0, &UIPersonalWnd);

	AttachChild							(&UIProgressBack);
	xml_init.InitStatic					(uiXml, "progress_background", 0, &UIProgressBack);

	if (GameID() != GAME_SINGLE)
	{
		AttachChild						(&UIProgressBack_rank);
		xml_init.InitStatic				(uiXml, "progress_back_rank", 0, &UIProgressBack_rank);

		UIProgressBack_rank.AttachChild	(&UIProgressBarRank);
		xml_init.InitProgressBar		(uiXml, "progress_bar_rank", 0, &UIProgressBarRank);
		UIProgressBarRank.SetProgressPos(100);
	}

	UIProgressBack.AttachChild (&UIProgressBarHealth);
	xml_init.InitProgressBar (uiXml, "progress_bar_health", 0, &UIProgressBarHealth);

	UIProgressBack.AttachChild	(&UIProgressBarStamina);
	xml_init.InitProgressBar (uiXml, "progress_bar_stamina", 0, &UIProgressBarStamina);
	
	UIProgressBack.AttachChild	(&UIProgressBarArmor);
	xml_init.InitProgressBar (uiXml, "progress_bar_armor", 0, &UIProgressBarArmor);

	UIProgressBack.AttachChild	(&UIProgressBarRadiation);
	xml_init.InitProgressBar (uiXml, "progress_bar_radiation", 0, &UIProgressBarRadiation);

	UIProgressBack.AttachChild    (&UIProgressBarHunger);
	xml_init.InitProgressBar (uiXml, "progress_bar_hunger", 0, &UIProgressBarHunger);

	UIProgressBack.AttachChild	(&UIProgressBarMozg);
	xml_init.InitProgressBar (uiXml, "progress_bar_mozg", 0, &UIProgressBarMozg);

	UIProgressBack.AttachChild(&UIProgressBarThirst);
	xml_init.InitProgressBar(uiXml, "progress_bar_thirst", 0, &UIProgressBarThirst);

	UIPersonalWnd.AttachChild			(&UIStaticPersonal);
	xml_init.InitStatic					(uiXml, "static_personal",0, &UIStaticPersonal);

	AttachChild							(&UIOutfitInfo);
	UIOutfitInfo.InitFromXml			(uiXml);

	//Элементы автоматического добавления
	xml_init.InitAutoStatic				(uiXml, "auto_static", this);

	if (GameID() != GAME_SINGLE){
		UIRankFrame = new CUIStatic(); UIRankFrame->SetAutoDelete(true);
		UIRank = new CUIStatic (); UIRank->SetAutoDelete(true);

		CUIXmlInit::InitStatic(uiXml, "rank", 0, UIRankFrame);
		CUIXmlInit::InitStatic(uiXml, "rank:pic", 0, UIRank);
		AttachChild(UIRankFrame);
		UIRankFrame->AttachChild(UIRank);		
	}

	m_pUIBagList						= InitDragDropList(uiXml, "dragdrop_bag", 0, &UIBagWnd);
	m_pUIBeltList						= InitDragDropList(uiXml, "dragdrop_belt", 0);

	shared_str nodevalue				= uiXml.Read("build_style_outfit_slot", 0, "false");
	bool buildStyleOutfitSlot			= nodevalue == "true" ? 1 : 0;
	m_pUIOutfitList						= InitDragDropList(uiXml, "dragdrop_outfit", 0, this, buildStyleOutfitSlot ? new CUIOutfitDragDropList() : nullptr);

	m_pUIPistolList						= InitDragDropList(uiXml, "dragdrop_pistol", 0);
	m_pUIAutomaticList					= InitDragDropList(uiXml, "dragdrop_automatic", 0);

	// load second rifle slot if it is defined in XML (removing it will effectively disable the second rifle slot feature)
	int numRifleSLots = uiXml.GetNodesNum(uiXml.GetRoot(), "dragdrop_automatic");
	if (numRifleSLots > 1)
	{
		m_pUIAutomatic2List				= InitDragDropList(uiXml, "dragdrop_automatic", 1);
	}
	else
		m_pUIAutomatic2List = nullptr;

	m_pUIKnifeList						= InitDragDropList(uiXml, "dragdrop_knife", 0);
	m_pUIBinocularList					= InitDragDropList(uiXml, "dragdrop_binocular", 0);
	m_pUITorchList						= InitDragDropList(uiXml, "dragdrop_torch", 0);
	m_pUIDetectorList					= InitDragDropList(uiXml, "dragdrop_detecor", 0);
	m_pUIHelmetList						= InitDragDropList(uiXml, "dragdrop_helmets", 0);
	m_pUIPNVList						= InitDragDropList(uiXml, "dragdrop_pnv", 0);
	m_pUIAnomDetectorList				= InitDragDropList(uiXml, "dragdrop_anomaly_detector", 0);

	//pop-up menu
	AttachChild							(&UIPropertiesBox);
	UIPropertiesBox.InitPropertiesBox				(Fvector2().set(0,0),Fvector2().set(300,300));
	UIPropertiesBox.Hide				();

	AttachChild							(&UIStaticTime);
	xml_init.InitStatic					(uiXml, "time_static", 0, &UIStaticTime);

	UIStaticTime.AttachChild			(&UIStaticTimeString);
	xml_init.InitTextWnd					(uiXml, "time_static_str", 0, &UIStaticTimeString);

	UIExitButton						= new CUI3tButton();UIExitButton->SetAutoDelete(true);
	AttachChild							(UIExitButton);
	xml_init.Init3tButton				(uiXml, "exit_button", 0, UIExitButton);

	//Load sounds
	XML_NODE* stored_root				= uiXml.GetLocalRoot		();
	uiXml.SetLocalRoot					(uiXml.NavigateToNode		("action_sounds",0));
	::Sound->create						(sounds[eInvSndOpen],		uiXml.Read("snd_open",			0,	NULL),st_Effect,sg_SourceType);
	::Sound->create						(sounds[eInvSndClose],		uiXml.Read("snd_close",			0,	NULL),st_Effect,sg_SourceType);
	::Sound->create						(sounds[eInvItemToSlot],	uiXml.Read("snd_item_to_slot",	0,	NULL),st_Effect,sg_SourceType);
	::Sound->create						(sounds[eInvItemToBelt],	uiXml.Read("snd_item_to_belt",	0,	NULL),st_Effect,sg_SourceType);
	::Sound->create						(sounds[eInvItemToRuck],	uiXml.Read("snd_item_to_ruck",	0,	NULL),st_Effect,sg_SourceType);
	::Sound->create						(sounds[eInvProperties],	uiXml.Read("snd_properties",	0,	NULL),st_Effect,sg_SourceType);
	::Sound->create						(sounds[eInvDropItem],		uiXml.Read("snd_drop_item",		0,	NULL),st_Effect,sg_SourceType);
	::Sound->create						(sounds[eInvAttachAddon],	uiXml.Read("snd_attach_addon",	0,	NULL),st_Effect,sg_SourceType);
	::Sound->create						(sounds[eInvDetachAddon],	uiXml.Read("snd_detach_addon",	0,	NULL),st_Effect,sg_SourceType);
	::Sound->create						(sounds[eInvItemUse],		uiXml.Read("snd_item_use",		0,	NULL),st_Effect,sg_SourceType);

	uiXml.SetLocalRoot					(stored_root);
}

CUIDragDropListEx* CUIInventoryWnd::InitDragDropList(CUIXml& uiXml, LPCSTR name, int index, CUIWindow* parent, CUIDragDropListEx* list)
{
	if (!list) list = new CUIDragDropListEx();
	if (!parent) parent = this;
	parent->AttachChild(list);
	list->SetAutoDelete(true);
	CUIXmlInit::InitDragDropListEx		(uiXml, name, index, list);
	BindDragDropListEvents				(list);
	return list;
}

TSlotId CUIInventoryWnd::GetSlot(CUIDragDropListEx* l, CUICellItem* itm)
{
	if (l == m_pUIAutomatic2List)			return RIFLE_2_SLOT;
	auto iitm = (PIItem)itm->m_pData;
	return iitm->BaseSlot();
}

bool CUIInventoryWnd::SlotIsCompatible(TSlotId desiredSlot, CUICellItem* itm)
{
	auto iitm = (PIItem)itm->m_pData;
	auto baseSlot = iitm->BaseSlot();
	return baseSlot == desiredSlot
		|| (baseSlot == RIFLE_SLOT && desiredSlot == RIFLE_2_SLOT)
		|| (baseSlot == RIFLE_2_SLOT && desiredSlot == RIFLE_SLOT);
}

bool CUIInventoryWnd::CanPutInSlot(TSlotId desiredSlot, CUICellItem* itm)
{
	auto iitem = (PIItem)itm->m_pData;
	return !m_pInv->m_slots[desiredSlot].m_bPersistent && m_pInv->CanPutInSlot(iitem, desiredSlot);
}

EListType CUIInventoryWnd::GetType(CUIDragDropListEx* l)
{
	if (l == m_pUIBagList)					return iwBag;
	if (l == m_pUIBeltList)					return iwBelt;

	if (l == m_pUIAutomaticList)			return iwSlot;
	if (l == m_pUIAutomatic2List)			return iwSlot;
	if (l == m_pUIPistolList)				return iwSlot;
	if (l == m_pUIOutfitList)				return iwSlot;
	if (l == m_pUIKnifeList)				return iwSlot;
	if (l == m_pUIBinocularList)			return iwSlot;
	if (l == m_pUITorchList)				return iwSlot;
	if (l == m_pUIDetectorList)				return iwSlot;
	if (l == m_pUIHelmetList)				return iwSlot;
	if (l == m_pUIPNVList)					return iwSlot;
	if (l == m_pUIAnomDetectorList)			return iwSlot;

	NODEFAULT;
#ifdef DEBUG
	return iwSlot;
#endif // DEBUG
}

void CUIInventoryWnd::PlaySnd(eInventorySndAction a)
{
	if (sounds[a]._handle())
        sounds[a].play					(NULL, sm_2D);
}

CUIInventoryWnd::~CUIInventoryWnd()
{
//.	ClearDragDrop(m_vDragDropItems);
	ClearAllLists						();
}

bool CUIInventoryWnd::OnMouseAction(float x, float y, EUIMessages mouse_action)
{
	if(m_b_need_reinit)
		return true;

	//вызов дополнительного меню по правой кнопке
	if(mouse_action == WINDOW_RBUTTON_DOWN)
	{
		if(UIPropertiesBox.IsShown())
		{
			UIPropertiesBox.Hide		();
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


	CEntityAlive *pEntityAlive			= smart_cast<CEntityAlive*>(Level().CurrentEntity());

	if(pEntityAlive) 
	{
		float v = pEntityAlive->conditions().BleedingSpeed()*100.0f;
		UIProgressBarHealth.SetProgressPos		(v);

		v = pEntityAlive->conditions().GetRadiation()*100.0f;
		UIProgressBarRadiation.SetProgressPos	(v);

		v = pEntityAlive->conditions().GetPower()*100.0f;
		UIProgressBarStamina.SetProgressPos		(v);

		CActor *pActor = smart_cast<CActor*>(Level().CurrentEntity());

		v = pActor->conditions().GetSatiety()*100.0f;
		UIProgressBarHunger.SetProgressPos(v);

		v = pActor->conditions().GetThirsty()*100.0f;
		UIProgressBarThirst.SetProgressPos(v);

		v = pEntityAlive->conditions().GetPsyHealth()*100.0f;
		UIProgressBarMozg.SetProgressPos(v);

		// Armor progress bar stuff
		PIItem	pItem = pActor->inventory().ItemFromSlot(OUTFIT_SLOT);
		if (pItem)
		{
			v = pItem->GetCondition()*100;
			UIProgressBarArmor.SetProgressPos	(v);
		} else
			UIProgressBarArmor.SetProgressPos	(0.f);


		CInventoryOwner* pOurInvOwner	= smart_cast<CInventoryOwner*>(pEntityAlive);
		u32 _money						= 0;

		if (GameID() != GAME_SINGLE)
		{
			game_PlayerState* ps = Game().GetPlayerByGameID(pEntityAlive->ID());
			if (ps)
			{
				UIProgressBarRank.SetProgressPos(ps->experience_D*100);
				_money							= ps->money_for_round;
			}
			UIProgressBarHunger.SetProgressPos	(100.f);
			UIProgressBarThirst.SetProgressPos	(100.f);
		} else
			_money							= pOurInvOwner->get_money();

		// update money
		string64						sMoney;
		xr_sprintf							(sMoney,"%d RU", _money);
		UIMoneyWnd.SetText				(sMoney);

		// update outfit parameters
		CCustomOutfit* outfit			= smart_cast<CCustomOutfit*>(pOurInvOwner->inventory().m_slots[OUTFIT_SLOT].m_pIItem);	
		CHelmet* helmet					= (CHelmet*)pOurInvOwner->inventory().m_slots[HELMET_SLOT].m_pIItem;
		UIOutfitInfo.Update				(outfit, helmet);	

		//обновление веса
		InventoryUtilities::UpdateWeight(UIBagWnd, true);
	}



	if (use_sounds.size()>0)
	{
		for (auto it = use_sounds.begin(); it != use_sounds.end(); ++it)
		{
			ref_sound* sound = *it;

			if (sound && !sound->_feedback())

			{
				sound->destroy();

				use_sounds.erase(it);
				xr_delete(*it);

				it--;
			}
		}
	}


	UIStaticTimeString.SetText(*InventoryUtilities::GetGameTimeAsString(InventoryUtilities::etpTimeToMinutes));

	CUIWindow::Update					();
}

void CUIInventoryWnd::ShowDialog(bool bDoHideIndicators) 
{ 
	InitInventory			();
	inherited::ShowDialog			(bDoHideIndicators);

	if (!IsGameTypeSingle())
	{
		CActor *pActor = smart_cast<CActor*>(Level().CurrentEntity());
		if(!pActor) return;

		pActor->SetWeaponHideState(INV_STATE_INV_WND, true);

		//rank icon		
		int team = Game().local_player->team;
		int rank = Game().local_player->rank;
		string256 _path;		

		xr_sprintf(_path, "ui_hud_status_green_0%d", rank+1);

		UIRank->InitTexture(_path);
	}

	SendInfoToActor						("ui_inventory");

	Update								();
	PlaySnd								(eInvSndOpen);
}

void CUIInventoryWnd::HideDialog()
{
	PlaySnd								(eInvSndClose);
	inherited::HideDialog						();

	SendInfoToActor						("ui_inventory_hide");
	ClearAllLists						();

	//достать вещь в активный слот
	CActor *pActor = smart_cast<CActor*>(Level().CurrentEntity());
	if(pActor && m_iCurrentActiveSlot != NO_ACTIVE_SLOT && 
		pActor->inventory().m_slots[m_iCurrentActiveSlot].m_pIItem)
	{
		pActor->inventory().Activate(m_iCurrentActiveSlot);
		m_iCurrentActiveSlot = NO_ACTIVE_SLOT;
	}

	if (!IsGameTypeSingle())
	{
		CActor *pActor		= smart_cast<CActor*>(Level().CurrentEntity());
		if(!pActor)			return;

		pActor->SetWeaponHideState(INV_STATE_INV_WND, false);
	}
}

void CUIInventoryWnd::DeleteFromInventory(PIItem pIItem)
{
	NET_Packet					P;
	pIItem->object().u_EventGen	(P, GE_OWNERSHIP_REJECT, pIItem->object().H_Parent()->ID());
	P.w_u16						(u16(pIItem->object().ID()));
	P.w_u8						(1); // send just_before_destroy flag, so physical shell does not activates and disrupts nearby objects
	pIItem->object().u_EventSend(P);
	pIItem->object().u_EventGen(P, GE_DESTROY, u16(pIItem->object().ID()));
	pIItem->object().u_EventSend(P);
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
	CActor *pActor								= smart_cast<CActor*>(Level().CurrentEntity());
	if(pActor && item_to_upgrade == pActor->inventory().ActiveItem())
	{
			m_iCurrentActiveSlot				= pActor->inventory().GetActiveSlot();
			pActor->inventory().Activate		(NO_ACTIVE_SLOT);
	}
	SetCurrentItem								(NULL);
}

void CUIInventoryWnd::DetachAddon(const char* addon_name)
{
	PlaySnd										(eInvDetachAddon);
	if (OnClient())
	{
		NET_Packet								P;
		CurrentIItem()->object().u_EventGen		(P, GE_ADDON_DETACH, CurrentIItem()->object().ID());
		P.w_stringZ								(addon_name);
		CurrentIItem()->object().u_EventSend	(P);
	};
	CurrentIItem()->Detach						(addon_name, true);

	//спрятать вещь из активного слота в инвентарь на время вызова менюшки
	CActor *pActor								= smart_cast<CActor*>(Level().CurrentEntity());
	if(pActor && CurrentIItem() == pActor->inventory().ActiveItem())
	{
			m_iCurrentActiveSlot				= pActor->inventory().GetActiveSlot();
			pActor->inventory().Activate		(NO_ACTIVE_SLOT);
	}
}


void	CUIInventoryWnd::SendEvent_ActivateSlot	(PIItem	pItem, TSlotId slot_id)
{
	NET_Packet						P;
	pItem->object().u_EventGen		(P, GEG_PLAYER_ACTIVATE_SLOT, pItem->object().H_Parent()->ID());
	P.w_u16							((u16)slot_id);
	pItem->object().u_EventSend		(P);
}

void	CUIInventoryWnd::SendEvent_Item2Slot			(PIItem	pItem, TSlotId slot_id)
{
	NET_Packet						P;
	pItem->object().u_EventGen		(P, GEG_PLAYER_ITEM2SLOT, pItem->object().H_Parent()->ID());
	P.w_u16							(pItem->object().ID());
	P.w_u16							((u16)slot_id);
	pItem->object().u_EventSend		(P);
	g_pInvWnd->PlaySnd				(eInvItemToSlot);
};

void	CUIInventoryWnd::SendEvent_Item2Belt			(PIItem	pItem)
{
	NET_Packet						P;
	pItem->object().u_EventGen		(P, GEG_PLAYER_ITEM2BELT, pItem->object().H_Parent()->ID());
	P.w_u16							(pItem->object().ID());
	pItem->object().u_EventSend		(P);
	g_pInvWnd->PlaySnd				(eInvItemToBelt);
};

void	CUIInventoryWnd::SendEvent_Item2Ruck			(PIItem	pItem)
{
	NET_Packet						P;
	pItem->object().u_EventGen		(P, GEG_PLAYER_ITEM2RUCK, pItem->object().H_Parent()->ID());
	P.w_u16							(pItem->object().ID());
	pItem->object().u_EventSend		(P);

	g_pInvWnd->PlaySnd				(eInvItemToRuck);
};

void	CUIInventoryWnd::SendEvent_Item_Drop(PIItem	pItem)
{
	pItem->SetDropManual			(TRUE);

	if( OnClient() )
	{
		NET_Packet					P;
		pItem->object().u_EventGen	(P, GE_OWNERSHIP_REJECT, pItem->object().H_Parent()->ID());
		P.w_u16						(pItem->object().ID());
		pItem->object().u_EventSend(P);
	}
	g_pInvWnd->PlaySnd				(eInvDropItem);
};

void	CUIInventoryWnd::SendEvent_Item_Eat			(PIItem	pItem)
{
	R_ASSERT						(pItem->m_pCurrentInventory==m_pInv);
	NET_Packet						P;
	pItem->object().u_EventGen		(P, GEG_PLAYER_ITEM_EAT, pItem->object().H_Parent()->ID());
	P.w_u16							(pItem->object().ID());
	pItem->object().u_EventSend		(P);
};


void CUIInventoryWnd::BindDragDropListEvents(CUIDragDropListEx* lst)
{
	lst->m_f_item_drop				= CUIDragDropListEx::DRAG_CELL_EVENT(this,&CUIInventoryWnd::OnItemDrop);
	lst->m_f_item_start_drag		= CUIDragDropListEx::DRAG_CELL_EVENT(this,&CUIInventoryWnd::OnItemStartDrag);
	lst->m_f_item_db_click			= CUIDragDropListEx::DRAG_CELL_EVENT(this,&CUIInventoryWnd::OnItemDbClick);
	lst->m_f_item_selected			= CUIDragDropListEx::DRAG_CELL_EVENT(this,&CUIInventoryWnd::OnItemSelected);
	lst->m_f_item_rbutton_click		= CUIDragDropListEx::DRAG_CELL_EVENT(this,&CUIInventoryWnd::OnItemRButtonClick);
}


#include "../../xrEngine/xr_level_controller.h"
#include <dinput.h>

bool CUIInventoryWnd::OnKeyboardAction(int dik, EUIMessages keyboard_action)
{
	if(m_b_need_reinit)
		return true;

	if (UIPropertiesBox.GetVisible())
		UIPropertiesBox.OnKeyboardAction(dik, keyboard_action);

	if ( is_binded(kDROP, dik) )
	{
		if(WINDOW_KEY_PRESSED==keyboard_action)
			DropCurrentItem(false);
		return true;
	}

	if (WINDOW_KEY_PRESSED == keyboard_action)
	{
#ifdef DEBUG
		if(DIK_NUMPAD7 == dik && CurrentIItem())
		{
			CurrentIItem()->ChangeCondition(-0.05f);
			UIItemInfo.InitItem(CurrentIItem());
		}
		else if(DIK_NUMPAD8 == dik && CurrentIItem())
		{
			CurrentIItem()->ChangeCondition(0.05f);
			UIItemInfo.InitItem(CurrentIItem());
		}
#endif
	}
	if( inherited::OnKeyboardAction(dik,keyboard_action) )return true;

	return false;
}


void CUIInventoryWnd::PlayUseSound(LPCSTR sound_path)
{
	ref_sound* sound = new ref_sound;

	sound->create(sound_path, st_Effect, sg_SourceType);
	sound->play(NULL, sm_2D);

	use_sounds.push_back(sound);
}