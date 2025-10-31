#include "stdafx.h"
#include "UIPdaContactsWnd.h"
#include "../PDA.h"
#include "../../xrUI/UIXmlInit.h"
#include "../Actor.h"
#include "../../xrUI/Widgets/UIFrameWindow.h"
#include "../../xrUI/Widgets/UIFrameLineWnd.h"
#include "../../xrUI/Widgets/UIAnimatedStatic.h"
#include "../../xrUI/Widgets/UIScrollView.h"
#include "../../xrUI/Widgets/UIHint.h"
#include "../../xrEngine/string_table.h"
#include "../../xrUI/UIHelper.h"
#include "../../xrServerEntities/xrServer_Objects_ALife_Monsters.h"
#include "../../xrUI/UICursor.h"

#define PDA_CONTACT_HEIGHT 70

#define		PDA_CONTACTS_XML			"pda_contacts_new.xml"

CUIPdaContactsWnd::CUIPdaContactsWnd()
{
	m_flags.zero();
	m_hint_wnd = nullptr;
	UIRightFrame = nullptr;
	UIRightFrameHeader = nullptr;
	UIDetailsWnd = nullptr;
}

CUIPdaContactsWnd::~CUIPdaContactsWnd()
{
}

void CUIPdaContactsWnd::Show(bool status)
{
	inherited::Show(status);
	if (status)
	{
		if (UIDetailsWnd)
			UIDetailsWnd->Clear();
		Reload();
	}

}

void CUIPdaContactsWnd::Init()
{
	CUIXml		uiXml;
	uiXml.Load(CONFIG_PATH, UI_PATH, PDA_CONTACTS_XML);

	CUIXmlInit	xml_init;

	xml_init.InitWindow					(uiXml, "main_wnd", 0, this);

	CUIWindow* frameParent = this;
	if (uiXml.NavigateToNode("background"))
	{
		m_background					= UIHelper::CreateFrameWindow(uiXml, "background", this);
		frameParent						= m_background;
	}

	UIFrameContacts						= UIHelper::CreateFrameWindow(uiXml, "left_frame_window", frameParent);

	UIContactsHeader					= UIHelper::CreateFrameLine(uiXml, "left_frame_line", UIFrameContacts);

	UIRightFrame						= UIHelper::CreateFrameWindow(uiXml, "right_frame_window", frameParent, false);

	UIRightFrameHeader					= UIHelper::CreateFrameLine(uiXml, "right_frame_line", UIRightFrame, false);

	UIAnimation							= new CUIAnimatedStatic();UIAnimation->SetAutoDelete(true);
	UIContactsHeader->AttachChild		(UIAnimation);
	xml_init.InitAnimatedStatic			(uiXml, "a_static", 0, UIAnimation);

	UIListWnd							= new CUIScrollView();UIListWnd->SetAutoDelete(true);
	UIFrameContacts->AttachChild		(UIListWnd);
	xml_init.InitScrollView				(uiXml, "list", 0, UIListWnd);

	UIDetailsWnd						= UIHelper::CreateScrollView(uiXml, "detail_list", UIRightFrame, false);

	if (uiXml.NavigateToNode("hint_wnd"))
	{
		m_hint_wnd = UIHelper::CreateHint(uiXml, "hint_wnd");
	}
	
	int leftStaticCount					= uiXml.GetNodesNum(uiXml.GetRoot(), "left_auto_static");
	for (int i = 0; i < leftStaticCount; ++i)
	{
		CUIStatic* leftStatic = new CUIStatic();
		leftStatic->SetAutoDelete(true);
		UIFrameContacts->AttachChild(leftStatic);
		xml_init.InitStatic(uiXml, "left_auto_static", i, leftStatic);
	}
	
	int rightStaticCount					= uiXml.GetNodesNum(uiXml.GetRoot(), "right_auto_static");
	for (int i = 0; i < rightStaticCount; ++i)
	{
		CUIStatic* rightStatic = new CUIStatic();
		rightStatic->SetAutoDelete(true);
		UIRightFrame->AttachChild(rightStatic);
		xml_init.InitStatic(uiXml, "right_auto_static", i, rightStatic);
	}
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
	if (TRUE == m_flags.test(flNeedUpdate))
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

	for (; it != m_pda_list.end(); ++it) {
		AddContact(*it);
	}
	m_flags.set(flNeedUpdate, FALSE);
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
	m_flags.set(flNeedUpdate, TRUE);
}

void CUIPdaContactsWnd::Reset()
{
	inherited::Reset			();
	Reload						();
}

CUIPdaContactItem::~CUIPdaContactItem()
{
}

extern CSE_ALifeTraderAbstract* ch_info_get_from_id (u16 id);

#include "UICharacterInfo.h"

void CUIPdaContactItem::SetSelected	(bool b)
{
	CUISelectable::SetSelected(b);

	if (!m_cw->UIDetailsWnd)
		return;

	if(b)
	{
		m_cw->UIDetailsWnd->Clear		();
		CCharacterInfo				chInfo;
		CSE_ALifeTraderAbstract*	T = ch_info_get_from_id(UIInfo->OwnerID());
		chInfo.Init					(T);

		ADD_TEXT_TO_VIEW2( *(chInfo.Bio()), m_cw->UIDetailsWnd);
	}
}

bool CUIPdaContactItem::OnMouseDown(int mouse_btn)
{
	if(mouse_btn==MOUSE_1){
		m_cw->UIListWnd->SetSelected(this);
		return true;
	}
	return false;
}

void CUIPdaContactItem::OnFocusReceive()
{
	CUIWindow::OnFocusReceive();

	if (!m_cw->m_hint_wnd)
		return;

	Frect rect;
	m_cw->UIListWnd->GetAbsoluteRect(rect);
	Fvector2 pos = UI().GetUICursor().GetCursorPosition();

	if (!m_bCursorOverWindow || !rect.in(pos))
	{
		m_cw->m_hint_wnd->set_text("");
		return;
	}
	SetHintText();
}

void CUIPdaContactItem::SetHintText()
{
	CSE_ALifeTraderAbstract* T = ch_info_get_from_id(UIInfo->OwnerID());

	const char* stalkersKilled = "0";
	const char* mutantsKilled = "0";
	const char* artsFound = "0";
	const char* itemsSold = "0";

	luabind::functor<const char*> functorGetStalkersKilled;
	if (ai().script_engine().functor("pda.coc_contacts_get_stalkers_killed", functorGetStalkersKilled))
		stalkersKilled = functorGetStalkersKilled(UIInfo->OwnerID());

	luabind::functor<const char*> functorGetMutantsKilled;
	if (ai().script_engine().functor("pda.coc_contacts_get_mutants_killed", functorGetMutantsKilled))
		mutantsKilled = functorGetMutantsKilled(UIInfo->OwnerID());

	luabind::functor<const char*> functorGetArtsFound;
	if (ai().script_engine().functor("pda.coc_contacts_get_arts_found", functorGetArtsFound))
		artsFound = functorGetArtsFound(UIInfo->OwnerID());

	luabind::functor<const char*> functorGetItemsSold;
	if (ai().script_engine().functor("pda.coc_contacts_get_items_sold", functorGetItemsSold))
		itemsSold = functorGetItemsSold(UIInfo->OwnerID());

	xr_string str;
	str = "%c[255, 255, 160, 255] %c[default]";
	str += T->m_character_name.c_str();
	str += "\\n \\n %c[255, 215, 215, 215]";
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

	m_cw->m_hint_wnd->set_text(str.c_str());
}

void CUIPdaContactItem::OnFocusLost()
{
	CUIWindow::OnFocusLost();
	if (m_cw->m_hint_wnd)
		m_cw->m_hint_wnd->set_text("");
}
