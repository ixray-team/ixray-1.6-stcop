#include "StdAfx.h"
#include "UIPdaContactsWnd.h"
#include "../PDA.h"
#include "../../xrUI/UIXmlInit.h"
#include "../Actor.h"
#include "../Level.h"
#include "../../xrUI/Widgets/UIFrameWindow.h"
#include "../../xrUI/Widgets/UIFrameLineWnd.h"
#include "../../xrUI/Widgets/UIAnimatedStatic.h"
#include "../../xrUI/Widgets/UIScrollView.h"
#include "../../xrUI/Widgets/UIHint.h"
#include "../../xrEngine/string_table.h"
#include "../../xrUI/UIHelper.h"
#include "../../xrServerEntities/xrServer_Objects_ALife_Monsters.h"
#include "../../xrUI/UICursor.h"
#include "../../xrEngine/xr_input.h"
#include "../pda_communication.h"
#include "UICharacterInfo.h"
#include "UIGameCustom.h"
#include "UITalkWnd.h"
#include "PdaConstants.h"

extern CSE_ALifeTraderAbstract* ch_info_get_from_id(ALife::_OBJECT_ID id);

namespace
{
// Resolves a contact owner safely from the stable owner id, avoiding dereference of stale m_data pointers
// after the underlying NPC object was destroyed (death, alife unload) while the PDA window stays open.
CInventoryOwner* ResolveContactOwnerById(ALife::_OBJECT_ID ownerId)
{
	if (ownerId == ALife::INVALID_OBJECT_ID)
	{
		return nullptr;
	}

	CObject* object = Level().Objects.net_Find(ownerId);
	if (object == nullptr || object->getDestroy())
	{
		return nullptr;
	}

	return object->cast_inventory_owner();
}

// Ends embedded phrase UI and PDA talk session when the highlighted contact no longer matches the active NPC.
void StopEmbeddedPhraseUiIfSessionNpcDiffers(CInventoryOwner* highlightedOwner)
{
	if (!highlightedOwner)
	{
		return;
	}

	CPdaCommunication& comm = PdaCommunication();
	if (!comm.IsSessionActive())
	{
		return;
	}

	CInventoryOwner* sessionNpc = comm.GetSessionNpc();
	if (!sessionNpc || sessionNpc == highlightedOwner)
	{
		return;
	}

	CUIGameCustom* gameUi = CurrentGameUI();
	if (gameUi && gameUi->TalkMenu)
	{
		gameUi->TalkMenu->StopPdaDialog();
	}
	else
	{
		comm.Stop();
	}
}

bool TryLaunchEmbeddedPdaPhraseUi(CUIPdaContactsWnd* contactsWnd)
{
    CUIGameCustom* gameUi = CurrentGameUI();
    if (!gameUi || !gameUi->TalkMenu)
    {
        return false;
    }

    CUITalkWnd* talkWnd = gameUi->TalkMenu;
    talkWnd->SetPdaMode(true);
    if (!talkWnd->IsEmbeddedInPda() && contactsWnd)
    {
        talkWnd->BeginPdaEmbed(contactsWnd);
    }

    if (!talkWnd->IsEmbeddedInPda())
    {
        talkWnd->StopPdaDialog();
        return false;
    }

    const bool isInitialized = talkWnd->InitializeDialogForPda();
    if (!isInitialized)
    {
        talkWnd->StopPdaDialog();
    }

    return isInitialized;
}
} // namespace

#define PDA_CONTACT_HEIGHT 70

CUIPdaContactsWnd::CUIPdaContactsWnd()
{
	m_flags.zero();
	m_hint_wnd = nullptr;
	UIRightFrame = nullptr;
	UIRightFrameHeader = nullptr;
	UIDetailsWnd = nullptr;
	ActionRepeaters()->Register(this, kUI_DOWN);
	ActionRepeaters()->Register(this, kUI_UP);
}

CUIPdaContactsWnd::~CUIPdaContactsWnd()
{
	xr_delete(_layoutXml);
	ActionRepeaters()->UnregisterOwner(this);
}	

void CUIPdaContactsWnd::Show(bool status)
{
	inherited::Show(status);
	if (status)
	{
		Reload();
	}

}

void CUIPdaContactsWnd::Init()
{
	xr_delete(_layoutXml);
	_layoutXml = new CUIXml();
	_hasValidDialogLayout = false;

	// CUIXml::Load(CONFIG_PATH, UI_PATH, ...) maps names via UI().get_xml_name() (e.g. widescreen -> *_16.xml).
	if (!_layoutXml->Load(CONFIG_PATH, UI_PATH, PdaXml::ContactsNew))
	{
		Msg("! CUIPdaContactsWnd: failed to load [%s] from configs/ui (check addon merge order)", PdaXml::ContactsNew);
		xr_delete(_layoutXml);
		return;
	}

	const SPdaContactsLayoutInfo layoutInfo = InspectPdaContactsLayout(*_layoutXml);
	LogPdaContactsLayoutIssues(layoutInfo, _layoutXml->m_xml_file_name);
	_hasValidDialogLayout = IsPdaContactsLayoutValid(layoutInfo);

	CUIXmlInit	xml_init;

	xml_init.InitWindow					(*_layoutXml, "main_wnd", 0, this);

	CUIWindow* frameParent = this;
	m_background = UIHelper::CreateFrameWindow(*_layoutXml, PdaXml::ContactsBackground, this, false);
	if (m_background)
	{
		frameParent = m_background;
	}

	UIFrameContacts						= UIHelper::CreateFrameWindow(*_layoutXml, PdaXml::ContactsLeftFrame, frameParent);

	UIContactsHeader					= UIHelper::CreateFrameLine(*_layoutXml, "left_frame_line", UIFrameContacts);

	UIRightFrame						= UIHelper::CreateFrameWindow(*_layoutXml, PdaXml::ContactsRightFrame, frameParent, false);

	UIRightFrameHeader					= UIHelper::CreateFrameLine(*_layoutXml, "right_frame_line", UIRightFrame, false);

	UIAnimation							= new CUIAnimatedStatic();UIAnimation->SetAutoDelete(true);
	UIContactsHeader->AttachChild		(UIAnimation);
	xml_init.InitAnimatedStatic			(*_layoutXml, "a_static", 0, UIAnimation);

	UIListWnd							= new CUIScrollView();UIListWnd->SetAutoDelete(true);
	UIFrameContacts->AttachChild		(UIListWnd);
	xml_init.InitScrollView				(*_layoutXml, "list", 0, UIListWnd);

	UIDetailsWnd						= UIHelper::CreateScrollView(*_layoutXml, PdaXml::ContactsDetailList, UIRightFrame, false);

	if (_layoutXml->NavigateToNode("hint_wnd"))
	{
		m_hint_wnd = UIHelper::CreateHint(*_layoutXml, "hint_wnd");
	}
	
	int leftStaticCount					= _layoutXml->GetNodesNum(_layoutXml->GetRoot(), "left_auto_static");
	for (int i = 0; i < leftStaticCount; ++i)
	{
		CUIStatic* leftStatic = new CUIStatic();
		leftStatic->SetAutoDelete(true);
		UIFrameContacts->AttachChild(leftStatic);
		xml_init.InitStatic(*_layoutXml, "left_auto_static", i, leftStatic);
	}
	
	int rightStaticCount					= _layoutXml->GetNodesNum(_layoutXml->GetRoot(), "right_auto_static");
	for (int i = 0; i < rightStaticCount; ++i)
	{
		CUIStatic* rightStatic = new CUIStatic();
		rightStatic->SetAutoDelete(true);
		UIRightFrame->AttachChild(rightStatic);
		xml_init.InitStatic(*_layoutXml, "right_auto_static", i, rightStatic);
	}
	m_gamepad_legend = UIHelper::CreateGamepadLegend(*_layoutXml, "gamepad_legend", this, false);
}

void CUIPdaContactsWnd::Draw()
{
	inherited::Draw();
}

void CUIPdaContactsWnd::DrawHint()
{
	if (m_hint_wnd)
		m_hint_wnd->Draw();
}

void CUIPdaContactsWnd::Update()
{
	if (true == m_flags.test(flNeedUpdate))
	{
		UpdateInfo();
	}

	inherited::Update();
}

void CUIPdaContactsWnd::UpdateInfo()
{
	RemoveAll();

	if (m_hint_wnd)
		m_hint_wnd->set_text("");

	CPda* pPda = Actor()->GetPDA();
	if (!pPda)			return;

	pPda->ActivePDAContacts(m_pda_list);

	xr_vector<CInventoryOwner*>::iterator it = m_pda_list.begin();

	bool needSelect = true;
	for (; it != m_pda_list.end(); ++it) 
	{
		AddContact(*it);
		if (needSelect)
		{
			UIListWnd->SetSelected(UIListWnd->GetItem(0));
			CUIPdaContactItem* itm = smart_cast<CUIPdaContactItem*>(UIListWnd->GetSelected());
			if (pInput->GetControllerMode() && itm && itm->m_frame_selected)
			{
				itm->m_frame_selected->Show(true);
			}
			needSelect = false;
		}
	}
	m_flags.set(flNeedUpdate, false);
}

void CUIPdaContactsWnd::AddContact(CInventoryOwner* owner)
{
	VERIFY(owner);


	CUIPdaContactItem* pItem		= nullptr;
	pItem							= new CUIPdaContactItem(this);
	UIListWnd->AddWindow			(pItem, true);
	pItem->Init						(0,0,UIListWnd->GetWidth(),85);
	pItem->InitCharacter			(owner);
	pItem->m_data					= (void*)owner;
	pItem->set_hint_wnd				(m_hint_wnd);
	pItem->set_hint_delay			(0);
}

//удалить все контакты из списка
void CUIPdaContactsWnd::RemoveAll()
{
	UIListWnd->Clear		();
	if (UIDetailsWnd)
		UIDetailsWnd->Clear		();
}

void CUIPdaContactsWnd::Reload()
{
	m_flags.set(flNeedUpdate, true);
}

void CUIPdaContactsWnd::Reset()
{
	inherited::Reset			();
	Reload						();
}

bool CUIPdaContactsWnd::OnGamepadKeyAction(int id, EUIMessages gamepad_action)
{
	if (gamepad_action == WINDOW_KEY_PRESSED)
	{
		if (is_binded(kUI_UP, id))
		{
			ActionRepeaters()->SetActionStarted(this, kUI_UP);
			CUIPdaContactItem* itm = smart_cast<CUIPdaContactItem*>(UIListWnd->GetSelected());
			if (itm && itm->m_frame_selected)
			{
				itm->m_frame_selected->Show(false);
			}
			UIListWnd->MoveSelectionUp(true);
			itm = smart_cast<CUIPdaContactItem*>(UIListWnd->GetSelected());
			if (itm && itm->m_frame_selected)
			{
				itm->m_frame_selected->Show(true);
			}
			if (UIListWnd->GetSelected())
			{
				const float barHeight = UIListWnd->ScrollBar() ? UIListWnd->ScrollBar()->GetHeight() : 0.0f;
				UIListWnd->ScrollToItem(UIListWnd->GetSelected(), iFloor(-barHeight / 2.0f + UIListWnd->GetSelected()->GetWndRect().height() / 2.0f));
			}
			return true;
		}
		else if (is_binded(kUI_DOWN, id))
		{
			ActionRepeaters()->SetActionStarted(this, kUI_DOWN);
			CUIPdaContactItem* itm = smart_cast<CUIPdaContactItem*>(UIListWnd->GetSelected());
			if (itm && itm->m_frame_selected)
			{
				itm->m_frame_selected->Show(false);
			}
			UIListWnd->MoveSelectionDown(true);
			itm = smart_cast<CUIPdaContactItem*>(UIListWnd->GetSelected());
			if (itm && itm->m_frame_selected)
			{
				itm->m_frame_selected->Show(true);
			}
			if (UIListWnd->GetSelected())
			{
				const float barHeight = UIListWnd->ScrollBar() ? UIListWnd->ScrollBar()->GetHeight() : 0.0f;
				UIListWnd->ScrollToItem(UIListWnd->GetSelected(), iFloor(-barHeight / 2.0f + UIListWnd->GetSelected()->GetWndRect().height() / 2.0f));
			}
			return true;
		}
	}

	return inherited::OnGamepadKeyAction(id, gamepad_action);
}

bool CUIPdaContactsWnd::OnGamepadKeyHold(int id)
{
	if (is_binded(kUI_UP, id))
	{
		if (ActionRepeaters()->CanRepeatActionNow(this, kUI_UP))
		{
			CUIPdaContactItem* itm = smart_cast<CUIPdaContactItem*>(UIListWnd->GetSelected());
			if (itm && itm->m_frame_selected)
			{
				itm->m_frame_selected->Show(false);
			}
			UIListWnd->MoveSelectionUp(false);
			itm = smart_cast<CUIPdaContactItem*>(UIListWnd->GetSelected());
			if (itm && itm->m_frame_selected)
			{
				itm->m_frame_selected->Show(true);
			}
			if (UIListWnd->GetSelected())
			{
				const float barHeight = UIListWnd->ScrollBar() ? UIListWnd->ScrollBar()->GetHeight() : 0.0f;
				UIListWnd->ScrollToItem(UIListWnd->GetSelected(), iFloor(-barHeight / 2.0f + UIListWnd->GetSelected()->GetWndRect().height() / 2.0f));
			}
		}
		return true;
	}
	else if (is_binded(kUI_DOWN, id))
	{
		if (ActionRepeaters()->CanRepeatActionNow(this, kUI_DOWN))
		{
			CUIPdaContactItem* itm = smart_cast<CUIPdaContactItem*>(UIListWnd->GetSelected());
			if (itm && itm->m_frame_selected)
			{
				itm->m_frame_selected->Show(false);
			}
			UIListWnd->MoveSelectionDown(false);
			itm = smart_cast<CUIPdaContactItem*>(UIListWnd->GetSelected());
			if (itm && itm->m_frame_selected)
			{
				itm->m_frame_selected->Show(true);
			}
			if (UIListWnd->GetSelected())
			{
				const float barHeight = UIListWnd->ScrollBar() ? UIListWnd->ScrollBar()->GetHeight() : 0.0f;
				UIListWnd->ScrollToItem(UIListWnd->GetSelected(), iFloor(-barHeight / 2.0f + UIListWnd->GetSelected()->GetWndRect().height() / 2.0f));
			}
		}
		return true;
	}

	return inherited::OnGamepadKeyHold(id);
}


void CUIPdaContactItem::SetSelected	(bool b)
{
	CUISelectable::SetSelected(b);

	if (!b || !m_cw->UIDetailsWnd)
	{
		return;
	}

	CInventoryOwner* owner = ResolveContactOwnerById(UIInfo->OwnerID());
	StopEmbeddedPhraseUiIfSessionNpcDiffers(owner);

	m_cw->UIDetailsWnd->Clear		();
	CCharacterInfo				chInfo;
	CSE_ALifeTraderAbstract*	T = ch_info_get_from_id(UIInfo->OwnerID());
	chInfo.Init					(T);

	ADD_TEXT_TO_VIEW2( g_pStringTable->translate(chInfo.Bio()).c_str(), m_cw->UIDetailsWnd);
}

bool CUIPdaContactItem::OnMouseDown(int mouse_btn)
{
	if (mouse_btn != MOUSE_1)
	{
		return false;
	}

	// Selection/focus alone must not start a phrase session; LMB activates dialog branches like face-to-face talk.
	m_cw->UIListWnd->SetSelected(this);

	CInventoryOwner* owner = ResolveContactOwnerById(UIInfo->OwnerID());
	if (!owner)
	{
		return true;
	}

	if (!PdaCommunication().IsEnabled())
	{
		return true;
	}

	if (!m_cw->HasValidPdaDialogLayout())
	{
		Msg("! [PDA] contacts: invalid <%s> layout; see earlier [PDA] messages", PdaXml::ContactsDialog);
		return true;
	}

	if (PdaCommunication().OpenDialog(owner))
	{
		TryLaunchEmbeddedPdaPhraseUi(m_cw);
	}

	return true;
}

void CUIPdaContactItem::OnFocusReceive()
{
	inherited::OnFocusReceive();

	if (get_hint_wnd())
	{
		SetHintText();
	}
}

void CUIPdaContactItem::SetHintText()
{
	CSE_ALifeTraderAbstract* T = ch_info_get_from_id(UIInfo->OwnerID());
	if (!T)
	{
		set_hint_text("");
		return;
	}

	CInventoryOwner* owner = ResolveContactOwnerById(UIInfo->OwnerID());
	CActor* actor = Actor();

	EPdaCommunicationStatus status = EPdaCommunicationStatus::DisabledByConfig;
	if (PdaCommunication().IsEnabled() && owner && actor)
	{
		status = PdaCommunication().CanStart(owner, actor->cast_inventory_owner());
	}
	else if (PdaCommunication().IsEnabled() && !owner)
	{
		status = EPdaCommunicationStatus::NpcOffline;
	}

	const char* stalkersKilled = "0";
	const char* mutantsKilled = "0";
	const char* artsFound = "0";
	const char* itemsSold = "0";

	bool cocFunctorsExist = false;
	luabind::functor<const char*> functorGetStalkersKilled;
	if (ai().script_engine().functor("pda.coc_contacts_get_stalkers_killed", functorGetStalkersKilled))
	{
		stalkersKilled = functorGetStalkersKilled(UIInfo->OwnerID());
		cocFunctorsExist = true;
	}

	luabind::functor<const char*> functorGetMutantsKilled;
	if (ai().script_engine().functor("pda.coc_contacts_get_mutants_killed", functorGetMutantsKilled))
	{
		mutantsKilled = functorGetMutantsKilled(UIInfo->OwnerID());
		cocFunctorsExist = true;
	}

	luabind::functor<const char*> functorGetArtsFound;
	if (ai().script_engine().functor("pda.coc_contacts_get_arts_found", functorGetArtsFound))
	{
		artsFound = functorGetArtsFound(UIInfo->OwnerID());
		cocFunctorsExist = true;
	}

	luabind::functor<const char*> functorGetItemsSold;
	if (ai().script_engine().functor("pda.coc_contacts_get_items_sold", functorGetItemsSold))
	{
		itemsSold = functorGetItemsSold(UIInfo->OwnerID());
		cocFunctorsExist = true;
	}

	xr_string str = "%c[255, 255, 160, 255] %c[default]";
	str += T->m_character_name.c_str();
	str += "\\n \\n %c[255, 215, 215, 215]";
	if (!cocFunctorsExist)
	{
		str += g_pStringTable->translate("st_pda_talk_status_label").c_str();
		str += ": %c[default] ";
		str += g_pStringTable->translate(CPdaCommunication::StatusStringId(status)).c_str();
	}
	else
	{
		str += g_pStringTable->translate("st_mm_pda_statistics").c_str();
		str += ": %c[default] \\n%c[255, 160, 160, 160]";
		str += g_pStringTable->translate("st_mm_pda_stalkers_killed").c_str();
		str += ": %c[default] ";
		str += stalkersKilled;
		str += "\\n%c[255, 160, 160, 160]";
		str += g_pStringTable->translate("st_mm_pda_mutants_killed").c_str();
		str += ": %c[default] ";
		str += mutantsKilled;
		str += "\\n%c[255, 160, 160, 160]";
		str += g_pStringTable->translate("st_mm_pda_artes_found").c_str();
		str += ": %c[default] ";
		str += artsFound;
		str += "\\n%c[255, 160, 160, 160]";
		str += g_pStringTable->translate("st_mm_pda_items_sold").c_str();
		str += ": %c[default] ";
		str += itemsSold;
	}

	set_hint_text(str.c_str());
}
