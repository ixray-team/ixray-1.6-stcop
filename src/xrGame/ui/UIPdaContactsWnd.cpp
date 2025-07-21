#include "stdafx.h"
#include "UIPdaContactsWnd.h"
#include "../Pda.h"
#include "../../xrUI/UIXmlInit.h"
#include "../actor.h"
#include "../../xrUI/Widgets/UIFrameWindow.h"
#include "../../xrUI/Widgets/UIFrameLineWnd.h"
#include "../../xrUI/Widgets/UIAnimatedStatic.h"
#include "../../xrUI/Widgets/UIScrollView.h"
#include "../actor.h"
#include "../../xrEngine/string_table.h"
#include "../../xrUI/UIHelper.h"

#define PDA_CONTACT_HEIGHT 70

#define		PDA_CONTACTS_XML			"pda_contacts_new.xml"

CUIPdaContactsWnd::CUIPdaContactsWnd()
{
	m_flags.zero();
}

CUIPdaContactsWnd::~CUIPdaContactsWnd()
{
}

void CUIPdaContactsWnd::Show(bool status)
{
	inherited::Show(status);
	if (status)
	{
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

	UIRightFrame						= UIHelper::CreateFrameWindow(uiXml, "right_frame_window", frameParent);

	UIRightFrameHeader					= UIHelper::CreateFrameLine(uiXml, "right_frame_line", UIRightFrame);

	UIAnimation							= new CUIAnimatedStatic();UIAnimation->SetAutoDelete(true);
	UIContactsHeader->AttachChild		(UIAnimation);
	xml_init.InitAnimatedStatic			(uiXml, "a_static", 0, UIAnimation);

	UIListWnd							= new CUIScrollView();UIListWnd->SetAutoDelete(true);
	UIFrameContacts->AttachChild		(UIListWnd);
	xml_init.InitScrollView				(uiXml, "list", 0, UIListWnd);

	UIDetailsWnd						= new CUIScrollView();UIDetailsWnd->SetAutoDelete(true);
	UIRightFrame->AttachChild			(UIDetailsWnd);
	xml_init.InitScrollView				(uiXml, "detail_list", 0, UIDetailsWnd);
	

	xml_init.InitAutoStaticGroup		(uiXml, "left_auto_static", 0, UIFrameContacts);
	xml_init.InitAutoStaticGroup		(uiXml, "right_auto_static", 0, UIRightFrame);
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
	if(b){
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
