#include "StdAfx.h"
#include "UIPdaWnd.h"
#include "../PDA.h"
#include "UIPdaAux.h"
#include "../../xrUI/xrUIXmlParser.h"
#include "../../xrUI/UIXmlInit.h"
#include "UIInventoryUtilities.h"
#include "../../xrEngine/xr_input.h"
#include "../Level.h"
#include "UIGameCustom.h"
#include "UIStalkersRankingWnd.h"
#include "../../xrUI/Widgets/UIStatic.h"
#include "../../xrUI/Widgets/UIFrameWindow.h"
#include "../../xrUI/Widgets/UITabControl.h"
#include "UIMapWnd.h"
#include "../../xrUI/Widgets/UIFrameLineWnd.h"
#include "object_broker.h"
#include "UIMessagesWindow.h"
#include "UIMainIngameWnd.h"
#include "../../xrUI/Widgets/UITabButton.h"
#include "../../xrUI/Widgets/UIAnimatedStatic.h"
#include "UIEventsWnd.h"
#include "../../xrUI/UIHelper.h"
#include "../../xrUI/Widgets/UIHint.h"
#include "../../xrUI/Widgets/UIBtnHint.h"
#include "UITaskWnd.h"
#include "UIRankingWnd.h"
#include "UILogsWnd.h"
#include "UIFactionWarWnd.h"
#include "UIScriptWnd.h"
#include "UIPdaContactsWnd.h"
#include "UIEncyclopediaWnd.h"
#include "UIActorInfo.h"
#include "UIDiaryWnd.h"

#define PDA_XML		"pda.xml"

u32 g_pda_info_state = 0;

void RearrangeTabButtons(CUITabControl* pTab);
void RearrangeTabButtonsLegacy(CUITabControl* pTab, xr_vector<Fvector2>& vec_sign_places);

CUIPdaWnd::CUIPdaWnd()
{
	LoadCallbackGlobals(m_isSetActiveSubdialog, m_onSetActiveSubdialog, "OnSetActiveSubdialog");

	pUITaskWnd       = nullptr;
	pUIFactionWarWnd = nullptr;
	pUIRankingWnd    = nullptr;
	pUILogsWnd       = nullptr;
	UIPdaContactsWnd = nullptr;
	pUIEventsWnd       = nullptr;
	pUIStalkersRankingWnd = nullptr;
	pUIEncyclopediaWnd = nullptr;
	pUIActorInfoWnd	 = nullptr;
	pUIDiaryWnd		 = nullptr;
	pUIMapWnd		 = nullptr;

	m_hint_wnd       = nullptr;
	m_caption		 = nullptr;
	m_caption_const	 = "";
	m_clock			 = nullptr;
	UIMainButtonsBackground = nullptr;
	UITimerBackground = nullptr;
	UINoice			 = nullptr;
	m_btn_close		 = nullptr;
	m_updatedSectionImage = nullptr;
	m_oldSectionImage = nullptr;
	m_sign_places_main.clear();

	LoadCallbackGlobals(m_isSetActiveSubdialog, m_onSetActiveSubdialog, "OnSetActiveSubdialog");
	Init();
	ActionRepeaters()->Register(this, kUI_TAB_LEFT);
	ActionRepeaters()->Register(this, kUI_TAB_RIGHT);
	ActionRepeaters()->Register(this, kUI_TAB_SECONDARY_LEFT);
	ActionRepeaters()->Register(this, kUI_TAB_SECONDARY_RIGHT);
}

CUIPdaWnd::~CUIPdaWnd()
{
	ActionRepeaters()->UnregisterOwner(this);
	delete_data( pUITaskWnd );
	delete_data( pUIFactionWarWnd );
	delete_data( UIPdaContactsWnd );
	delete_data( pUIRankingWnd );
	delete_data( pUILogsWnd );
	delete_data( pUIEventsWnd );
	delete_data( pUIStalkersRankingWnd );
	delete_data( pUIEncyclopediaWnd );
	delete_data( pUIActorInfoWnd );
	delete_data( pUIDiaryWnd );
	delete_data( pUIMapWnd );

	delete_data( m_hint_wnd );
	delete_data( UINoice );
	delete_data( m_updatedSectionImage );
	delete_data( m_oldSectionImage );
}

void CUIPdaWnd::Init()
{
	CUIXml					uiXml;
	uiXml.Load				(CONFIG_PATH, UI_PATH, PDA_XML);

	m_pActiveDialog			= nullptr;
	m_sActiveSection		= "";

	CUIXmlInit::InitWindow	(uiXml, "main", 0, this);

	UIMainPdaFrame			= UIHelper::CreateStatic	( uiXml, "background_static", this );
	if (uiXml.NavigateToNode("caption_static"))
	{
		m_caption				= UIHelper::CreateStatic	( uiXml, "caption_static", this );
		m_caption_const			= ( m_caption->TextItemControl()->GetText() );
	}

	if (uiXml.NavigateToNode("clock_wnd"))
		m_clock					= UIHelper::CreateStatic	( uiXml, "clock_wnd", this );

	CUIWindow* tabControlParent = this;
	if (uiXml.NavigateToNode("mbbackground_frame_line"))
	{
		UIMainButtonsBackground = UIHelper::CreateFrameLine	( uiXml, "mbbackground_frame_line", UIMainPdaFrame);
		tabControlParent = UIMainPdaFrame;
	}
	
	if (uiXml.NavigateToNode("timer_frame_line"))
		UITimerBackground = UIHelper::CreateFrameLine(uiXml, "timer_frame_line", UIMainPdaFrame);

	if (uiXml.NavigateToNode("anim_static"))
	{
		m_anim_static = new CUIAnimatedStatic();
		AttachChild(m_anim_static);
		m_anim_static->SetAutoDelete(true);
		CUIXmlInit::InitAnimatedStatic(uiXml, "anim_static", 0, m_anim_static);
	}
	if (uiXml.NavigateToNode("close_button"))
		m_btn_close				= UIHelper::Create3tButton( uiXml, "close_button", this );

	if (uiXml.NavigateToNode("hint_wnd"))
		m_hint_wnd				= UIHelper::CreateHint( uiXml, "hint_wnd" );

	UITabControl					= new CUITabControl();
	UITabControl->SetAutoDelete		(true);
	tabControlParent->AttachChild	(UITabControl);
	CUIXmlInit::InitTabControl		(uiXml, "tab", 0, UITabControl);
	UITabControl->SetMessageTarget	(this);

	std::tuple<LPCSTR,LPCSTR> 
		tabLegacyList[] = 
	{ 
		{"0", "eptQuests"},
		{"1", "eptMap"},
		{"2", "eptDiary"},
		{"3", "eptContacts"},
		{"4", "eptRankingGlobal"},
		{"5", "eptActorStatistic"},
		{"6", "eptEncyclopedia"},
	};
	for (u32 i = 0; i < UITabControl->GetTabsCount(); i++)
	{
		CUITabButton* btn = UITabControl->GetButtonByIndex(i);
		if (!btn || !btn->IsIdDefaultAssigned())
			continue;

		for (const auto& [id, replace] : tabLegacyList)
		{
			if (btn->m_btn_id == id)
			{
				btn->m_btn_id = replace;
				break;
			}
		}
	}

	if (UITabControl->GetButtonById("eptTasks"))
	{
		pUITaskWnd					= new CUITaskWnd();
		pUITaskWnd->hint_wnd		= m_hint_wnd;
		pUITaskWnd->Init			();
	}

	if (UITabControl->GetButtonById("eptQuests"))
	{
		pUIEventsWnd = new CUIEventsWnd();
		pUIEventsWnd->Init();
	}

	if (UITabControl->GetButtonById("eptFractionWar"))
	{
		pUIFactionWarWnd = new CUIFactionWarWnd();
		pUIFactionWarWnd->hint_wnd = m_hint_wnd;
		pUIFactionWarWnd->Init();
	}

	if (UITabControl->GetButtonById("eptContacts"))
	{
		UIPdaContactsWnd = new CUIPdaContactsWnd();
		UIPdaContactsWnd->Init();
	}
	if (UITabControl->GetButtonById("eptRanking"))
	{
		pUIRankingWnd					= new CUIRankingWnd();
		pUIRankingWnd->Init				();
	}
	
	if (UITabControl->GetButtonById("eptRankingGlobal"))
	{
		pUIStalkersRankingWnd = new CUIStalkersRankingWnd();
		pUIStalkersRankingWnd->Init();
	}

	if (UITabControl->GetButtonById("eptLogs"))
	{
		pUILogsWnd						= new CUILogsWnd();
		pUILogsWnd->Init				();
	}

	if (UITabControl->GetButtonById("eptEncyclopedia"))
	{
		pUIEncyclopediaWnd = new CUIEncyclopediaWnd();
		pUIEncyclopediaWnd->Init();
	}

	if (UITabControl->GetButtonById("eptActorStatistic"))
	{
		pUIActorInfoWnd = new CUIActorInfoWnd();
		pUIActorInfoWnd->Init();
	}

	if (UITabControl->GetButtonById("eptDiary"))
	{
		pUIDiaryWnd = new CUIDiaryWnd();
		pUIDiaryWnd->Init();
	}
	
	if (UITabControl->GetButtonById("eptMap"))
	{
		pUIMapWnd = new CUIMapWnd();
		pUIMapWnd->Init("pda_map.xml", "map_wnd");
	}


	if (uiXml.NavigateToNode("noice_static"))
	{
		UINoice					= new CUIStatic();
		UINoice->SetAutoDelete	( true );
		CUIXmlInit::InitStatic	( uiXml, "noice_static", 0, UINoice );
	}

	if (uiXml.NavigateToNode("updated_section_static"))
	{
		m_updatedSectionImage = new CUIStatic();
		CUIXmlInit::InitStatic(uiXml, "updated_section_static", 0, m_updatedSectionImage);
	}

	if (uiXml.NavigateToNode("old_section_static"))
	{
		m_oldSectionImage = new CUIStatic();
		CUIXmlInit::InitStatic(uiXml, "old_section_static", 0, m_oldSectionImage);
	}

	const static bool rearrangeButtons = EngineExternal()[EEngineExternalUI::PdaRearrangeTabButtons];
	if (rearrangeButtons)
	{
		if (m_updatedSectionImage && m_oldSectionImage)
			RearrangeTabButtonsLegacy(UITabControl, m_sign_places_main);
		else
			RearrangeTabButtons		(UITabControl);
	}
}

void CUIPdaWnd::SendMessage(CUIWindow* pWnd, s16 msg, void* pData)
{
	switch ( msg )
	{
	case TAB_CHANGED:
		{
			if ( pWnd == UITabControl )
			{
				SetActiveSubdialog			(UITabControl->GetActiveId());
			}
			break;
		}
	case BUTTON_CLICKED:
		{
			if (m_btn_close && pWnd == m_btn_close )
			{
				HideDialog();
			}
			break;
		}
	default:
		{
		if (m_pActiveDialog)
			m_pActiveDialog->SendMessage	(pWnd, msg, pData);
		}
	};
}

void CUIPdaWnd::Show(bool status)
{
	inherited::Show						(status);
	if(status)
	{
		InventoryUtilities::SendInfoToActor	("ui_pda");
		
		if (m_sActiveSection == nullptr || strcmp(m_sActiveSection.c_str(), "") == 0)
		{
			if (UITabControl->GetButtonById("eptTasks"))
			{
				SetActiveSubdialog("eptTasks");
				UITabControl->SetActiveTab("eptTasks");
			}
			else
			{
				SetActiveSubdialog("eptQuests");
				UITabControl->SetActiveTab("eptQuests");
			}
		}
		else
			SetActiveSubdialog(m_sActiveSection);
	}else
	{
		InventoryUtilities::SendInfoToActor	("ui_pda_hide");
		CurrentGameUI()->UIMainIngameWnd->SetFlashIconState_(CUIMainIngameWnd::efiPdaTask, false);
		if (m_pActiveDialog)
		{
			m_pActiveDialog->Show				(false);
			if (pUITaskWnd)
				m_pActiveDialog = pUITaskWnd; //hack for script window
			else
				m_pActiveDialog = pUIEventsWnd;
		}
		g_btnHint->Discard					();
		g_statHint->Discard					();
	}
}

void CUIPdaWnd::UpdateDateTime()
{
	if (!UITimerBackground)
		return;

	static shared_str prevStrTime = " ";
	xr_string strTime = *InventoryUtilities::GetGameTimeAsString(InventoryUtilities::etpTimeToMinutes);
				strTime += " ";
				strTime += *InventoryUtilities::GetDateAsStringLegacy(Level().GetGameTime(), InventoryUtilities::edpDateToDay);

	if (xr_strcmp(strTime.c_str(), prevStrTime))
	{
		UITimerBackground->UITitleText.SetText(strTime.c_str());
		prevStrTime = strTime.c_str();
	}
}

void CUIPdaWnd::Update()
{
	inherited::Update();
	if (m_pActiveDialog)
		m_pActiveDialog->Update();
	if (m_clock)
		m_clock->SetText(InventoryUtilities::GetGameTimeAsString(InventoryUtilities::etpTimeToMinutes).c_str());
	UpdateDateTime();

	if (pUILogsWnd)
		Device.seqParallel.push_back(xr_make_delegate(pUILogsWnd, &CUILogsWnd::PerformWork));
}

void CUIPdaWnd::SetActiveSubdialog(const shared_str& section)
{
	if ( m_pActiveDialog )
	{
		if (UIMainPdaFrame->IsChild(m_pActiveDialog))
			UIMainPdaFrame->DetachChild( m_pActiveDialog );
		m_pActiveDialog->Show( false );
	}

	if ( section == "eptTasks" )
	{
		m_pActiveDialog = pUITaskWnd;
		g_pda_info_state &= ~pda_section::quests;
	}
	else if (section == "eptQuests")
	{
		m_pActiveDialog = pUIEventsWnd;
		g_pda_info_state &= ~pda_section::quests;
	}
	else if ( section == "eptFractionWar" )
	{
		m_pActiveDialog = pUIFactionWarWnd;
	}
	else if (section == "eptContacts")
	{
		if (UIPdaContactsWnd) // safety check for contacts keybind
			m_pActiveDialog = UIPdaContactsWnd;
		else
			m_pActiveDialog = pUITaskWnd;
		g_pda_info_state &= ~pda_section::contacts;
	}
	else if (section == "eptRanking")
	{
		if (IsGameTypeSingle()) 
		{
			m_pActiveDialog = pUIRankingWnd;
		}
		g_pda_info_state &= ~pda_section::ranking;
	}
	else if (section == "eptRankingGlobal")
	{
		m_pActiveDialog = pUIStalkersRankingWnd;
		g_pda_info_state &= ~pda_section::ranking;
	}
	else if ( section == "eptLogs" )
	{
		m_pActiveDialog = pUILogsWnd;
		g_pda_info_state &= ~pda_section::news;
	}
	else if (section == "eptEncyclopedia")
	{
		m_pActiveDialog = pUIEncyclopediaWnd;
		g_pda_info_state &= ~pda_section::encyclopedia;
	}
	else if (section == "eptActorStatistic")
	{
		m_pActiveDialog = pUIActorInfoWnd;
		g_pda_info_state &= ~pda_section::statistics;
	}
	else if (section == "eptDiary")
	{
		m_pActiveDialog = pUIDiaryWnd;
		g_pda_info_state &= ~pda_section::diary;
	}
	else if (section == "eptMap")
	{
		if (pUIMapWnd) // safety check for map keybind
			m_pActiveDialog = pUIMapWnd;
		else
			m_pActiveDialog = pUITaskWnd;
		g_pda_info_state &= ~pda_section::map;
	}
	if (m_isSetActiveSubdialog)
	{
		luabind::functor<CUIDialogWndEx*> funct;
		R_ASSERT2(ai().script_engine().functor(m_onSetActiveSubdialog, funct), "failed to get OnSetActiveSubdialog functor");

		CUIDialogWndEx* ret = funct((LPCSTR)section.c_str());
		CUIWindow* pScriptWnd = ret ? smart_cast<CUIWindow*>(ret) : (0);
		if (pScriptWnd)
			m_pActiveDialog = pScriptWnd;
		
			if (m_pActiveDialog)
			{
				if (!UIMainPdaFrame->IsChild(m_pActiveDialog))
					UIMainPdaFrame->AttachChild(m_pActiveDialog);
				m_pActiveDialog->Show(true);
				m_sActiveSection = section;
				SetActiveCaption();
			}
			else {
				m_sActiveSection = "";
			}
	}
	else
	{
		if (!UIMainPdaFrame->IsChild(m_pActiveDialog))
			UIMainPdaFrame->AttachChild(m_pActiveDialog);
		m_pActiveDialog->Show(true);

		if (UITabControl->GetActiveId() != section)
		{
			UITabControl->SetActiveTab(section);
		}
		m_sActiveSection = section;
		SetActiveCaption();
	}
}

void CUIPdaWnd::SetActiveCaption()
{
	TABS_VECTOR*	btn_vec		= UITabControl->GetButtonsVector();
	TABS_VECTOR::iterator it_b	= btn_vec->begin();
	TABS_VECTOR::iterator it_e	= btn_vec->end();
	for ( ; it_b != it_e; ++it_b )
	{
		if ( (*it_b)->m_btn_id == m_sActiveSection )
		{
			LPCSTR cur = (*it_b)->TextItemControl()->GetText();
			string256 buf;
			xr_strconcat(buf, m_caption_const.c_str(), cur );
			SetCaption( buf );
			return;
		}
	}
}

void CUIPdaWnd::Show_SecondTaskWnd( bool status )
{
	if (!pUITaskWnd)
		return;

	if ( status )
	{
		SetActiveSubdialog( "eptTasks" );
	}
	pUITaskWnd->Show_TaskListWnd( status );
}

void CUIPdaWnd::Show_MapLegendWnd( bool status )
{
	if (!pUITaskWnd)
		return;

	if ( status )
	{
		SetActiveSubdialog( "eptTasks" );
	}
	pUITaskWnd->ShowMapLegend( status );
}

static u32 pda_render_frame = 0;

void CUIPdaWnd::Draw()
{
	if (pda_render_frame == Device.dwFrame)
	{
		return;
	}

	pda_render_frame = Device.dwFrame;

	inherited::Draw();
	DrawUpdatedSections();
	DrawHint();
	if (UINoice)
		UINoice->Draw(); // over all
}

void CUIPdaWnd::DrawHint()
{
	if (m_sActiveSection == "eptTasks")
	{
		pUITaskWnd->DrawHint();
	}
	if (m_sActiveSection == "eptQuests")
	{
		pUIEventsWnd->DrawHint();
	}
	else if (m_sActiveSection == "eptMap")
	{
		pUIMapWnd->DrawHint();
	}
	else if (m_sActiveSection == "eptFractionWar")
	{
		//m_hint_wnd->Draw();
	}
	else if (m_sActiveSection == "eptRanking")
	{
			pUIRankingWnd->DrawHint();
	}
	else if (m_sActiveSection == "eptLogs")
	{

	}
	else if (m_sActiveSection == "eptContacts")
	{
		UIPdaContactsWnd->DrawHint();
	}
	else if (m_sActiveSection == "eptRankingGlobal")
	{
		pUIStalkersRankingWnd->DrawHint();
	}
	if (m_hint_wnd)
		m_hint_wnd->Draw();
}

void CUIPdaWnd::UpdatePda()
{
	if (pUILogsWnd)
		pUILogsWnd->UpdateNews();

	if (m_sActiveSection == "eptTasks")
	{
		pUITaskWnd->ReloadTaskInfo();
	}
}

void CUIPdaWnd::UpdateRankingWnd()
{
	if (pUIRankingWnd)
		pUIRankingWnd->Update();
}

void CUIPdaWnd::PdaContentsChanged	(pda_section::part type)
{
	bool b = true;

	if (type == pda_section::encyclopedia && pUIEncyclopediaWnd)
	{
		pUIEncyclopediaWnd->ReloadArticles	();
	}
	else if (type == pda_section::news && pUIDiaryWnd)
	{
		pUIDiaryWnd->AddNews();
		pUIDiaryWnd->MarkNewsAsRead(pUIDiaryWnd->IsShown());
	}
	else if (type == pda_section::quests && pUIEventsWnd)
	{
		pUIEventsWnd->Reload				();
	}
	else if (type == pda_section::contacts)
	{
		if (UIPdaContactsWnd)
			UIPdaContactsWnd->Reload			();
		b = false;
	}
	else
	{
		b = false;
	}

	if(b)
	{
		g_pda_info_state |= type;
		CurrentGameUI()->UIMainIngameWnd->SetFlashIconState_(CUIMainIngameWnd::efiPdaTask, true);
	}

}
void draw_sign		(CUIStatic* s, Fvector2& pos)
{
	s->SetWndPos		(pos);
	s->Draw				();
}

void CUIPdaWnd::DrawUpdatedSections				()
{
	if (!m_updatedSectionImage || !m_oldSectionImage)
		return;

	m_updatedSectionImage->Update				();
	m_oldSectionImage->Update					();
	
	Fvector2									tab_pos;
	UITabControl->GetAbsolutePos				(tab_pos);

	Fvector2 pos;

	pos = m_sign_places_main[0];
	pos.add(tab_pos);
	if (g_pda_info_state & pda_section::quests)
		draw_sign								(m_updatedSectionImage, pos);
	else
		draw_sign								(m_oldSectionImage, pos);

	pos = m_sign_places_main[1];
	pos.add(tab_pos);
	if (g_pda_info_state & pda_section::map)
		draw_sign								(m_updatedSectionImage, pos);
	else
		draw_sign								(m_oldSectionImage, pos);

	pos = m_sign_places_main[2];
	pos.add(tab_pos);
	if (g_pda_info_state & pda_section::diary)
		draw_sign								(m_updatedSectionImage, pos);
	else
		draw_sign								(m_oldSectionImage, pos);

	pos = m_sign_places_main[3];
	pos.add(tab_pos);
	if (g_pda_info_state & pda_section::contacts)
		draw_sign								(m_updatedSectionImage, pos);
	else
		draw_sign								(m_oldSectionImage, pos);

	pos = m_sign_places_main[4];
	pos.add(tab_pos);
	if (g_pda_info_state & pda_section::ranking)
		draw_sign								(m_updatedSectionImage, pos);
	else
		draw_sign								(m_oldSectionImage, pos);

	pos = m_sign_places_main[5];
	pos.add(tab_pos);
	if (g_pda_info_state & pda_section::statistics)
		draw_sign								(m_updatedSectionImage, pos);
	else
		draw_sign								(m_oldSectionImage, pos);

	pos = m_sign_places_main[6];
	pos.add(tab_pos);
	if (g_pda_info_state & pda_section::encyclopedia)
		draw_sign								(m_updatedSectionImage, pos);
	else
		draw_sign								(m_oldSectionImage, pos);
	
}

void CUIPdaWnd::Reset()
{
	inherited::ResetAll		();

	if ( pUIEventsWnd )		
		pUIEventsWnd->ResetAll();

	if ( pUITaskWnd )		
		pUITaskWnd->ResetAll();

	if ( pUIFactionWarWnd )	
		pUIFactionWarWnd->ResetAll();

	if ( UIPdaContactsWnd )	
		UIPdaContactsWnd->ResetAll();

	if ( pUIRankingWnd )	
		pUIRankingWnd->ResetAll();

	if ( pUIStalkersRankingWnd )	
		pUIStalkersRankingWnd->ResetAll();

	if ( pUILogsWnd )		
		pUILogsWnd->ResetAll();

	if ( pUIEncyclopediaWnd )		
		pUIEncyclopediaWnd->ResetAll();

	if ( pUIActorInfoWnd )	
		pUIActorInfoWnd->ResetAll();

	if ( pUIDiaryWnd )	
		pUIDiaryWnd->ResetAll();
	
	if ( pUIMapWnd )	
		pUIMapWnd->ResetAll();
}

void CUIPdaWnd::SetCaption( LPCSTR text )
{
	if (m_caption)
		m_caption->TextItemControl()->SetText( text );
}

void RearrangeTabButtons(CUITabControl* pTab)
{
	TABS_VECTOR *	btn_vec		= pTab->GetButtonsVector();
	TABS_VECTOR::iterator it	= btn_vec->begin();
	TABS_VECTOR::iterator it_e	= btn_vec->end();

	Fvector2					pos;
	pos.set						((*it)->GetWndPos());
	float						size_x;

	for ( ; it != it_e; ++it )
	{
		(*it)->SetWndPos		(pos);
		(*it)->AdjustWidthToText();
		size_x					= (*it)->GetWndSize().x + 30.0f;
		(*it)->SetWidth			(size_x);
		pos.x					+= size_x - 6.0f;
	}
	
	pTab->SetWidth( pos.x + 5.0f );
	pos.x = pTab->GetWndPos().x - pos.x;
	pos.y = pTab->GetWndPos().y;
	pTab->SetWndPos( pos );
}

bool CUIPdaWnd::OnKeyboardAction(int dik, EUIMessages keyboard_action)
{
	if (is_binded(kACTIVE_JOBS, dik))
	{
		if (WINDOW_KEY_PRESSED == keyboard_action)
		{
			HideDialog();
		}

		return true;
	}

	return inherited::OnKeyboardAction(dik, keyboard_action);
}

bool CUIPdaWnd::OnGamepadKeyAction(int key, EUIMessages gamepad_action)
{
	if (WINDOW_KEY_PRESSED == gamepad_action)
	{
		switch (get_binded_action(key))
		{
			case kACTIVE_JOBS:
			{
				HideDialog();
				break;
			}
			return true;
		}
		switch (get_binded_action(key, agUIGeneral))
		{
			case kUI_BACK:
			{
				HideDialog();
				break;
			}
			case kUI_TAB_LEFT:
			{
				ActionRepeaters()->SetActionStarted(this, kUI_TAB_LEFT);
				UITabControl->PrevTab(true);
				break;
			}
			case kUI_TAB_RIGHT:
			{
				ActionRepeaters()->SetActionStarted(this, kUI_TAB_RIGHT);
				UITabControl->NextTab(true);
				break;
			}
			case kUI_TAB_SECONDARY_LEFT:
			{
				if (m_pActiveDialog == pUIDiaryWnd)
				{
					ActionRepeaters()->SetActionStarted(this, kUI_TAB_SECONDARY_LEFT);
					pUIDiaryWnd->m_FilterTab->PrevTab(true);
				}
				break;
			}
			case kUI_TAB_SECONDARY_RIGHT:
			{
				if (m_pActiveDialog == pUIDiaryWnd)
				{
					ActionRepeaters()->SetActionStarted(this, kUI_TAB_SECONDARY_RIGHT);
					pUIDiaryWnd->m_FilterTab->NextTab(true);
				}
				break;
			}
			return true;
		}
		switch (get_binded_action(key, agUITaskMenu))
		{
			case kPDA_TASKS_MAP_SHOW_ME:
			{
				if (m_pActiveDialog == pUIMapWnd)
				{
					pUIMapWnd->ViewActor();
					return true;
				}
				break;
			}
		}
	}

	return inherited::OnGamepadKeyAction(key, gamepad_action);
}

bool CUIPdaWnd::OnGamepadKeyHold(int key)
{
	switch (get_binded_action(key, agUIGeneral))
	{
		case kUI_TAB_LEFT:
		{
			if (ActionRepeaters()->CanRepeatActionNow(this, kUI_TAB_LEFT) && !any_binded_key_for_action_pressed_c(kUI_TAB_RIGHT))
			{
				UITabControl->PrevTab();
				return true;
			}
			break;
		}
		case kUI_TAB_RIGHT:
		{
			if (ActionRepeaters()->CanRepeatActionNow(this, kUI_TAB_RIGHT) && !any_binded_key_for_action_pressed_c(kUI_TAB_LEFT))
			{
				UITabControl->NextTab();
				return true;
			}
			break;
		}
		case kUI_TAB_SECONDARY_LEFT:
		{
			if (m_pActiveDialog == pUIDiaryWnd && ActionRepeaters()->CanRepeatActionNow(this, kUI_TAB_SECONDARY_LEFT) && !any_binded_key_for_action_pressed_c(kUI_TAB_SECONDARY_RIGHT))
			{
				pUIDiaryWnd->m_FilterTab->PrevTab();
				return true;
			}
			break;
		}
		case kUI_TAB_SECONDARY_RIGHT:
		{
			if (m_pActiveDialog == pUIDiaryWnd && ActionRepeaters()->CanRepeatActionNow(this, kUI_TAB_SECONDARY_LEFT) && !any_binded_key_for_action_pressed_c(kUI_TAB_SECONDARY_RIGHT))
			{
				pUIDiaryWnd->m_FilterTab->NextTab();
				return true;
			}
			break;
		}
	}

	return inherited::OnGamepadKeyHold(key);
}

void CUIPdaWnd::HideDialog()
{
	if (!IsShown())
	{
		return;
	}

	CObject* current_entity = Level().CurrentControlEntity();

	CHudPdaAnimator* pda_animator = current_entity != nullptr ? current_entity->cast_actor()->HudAnimator()->PdaAnimator() : nullptr;

	if (pda_animator != nullptr
		&& pda_animator->GetState() != CHudStateAnimator::EAnimatorStates::eHidden
		&& pda_animator->GetState() != CHudStateAnimator::EAnimatorStates::eHiding)
	{
		pda_animator->SetState(CHudStateAnimator::EAnimatorStates::eHiding);
	}

	GetHolder()->StopDialog(this);
}

void RearrangeTabButtonsLegacy(CUITabControl* pTab, xr_vector<Fvector2>& vec_sign_places)
{
	TABS_VECTOR *	btn_vec		= pTab->GetButtonsVector();
	TABS_VECTOR::iterator it	= btn_vec->begin();
	TABS_VECTOR::iterator it_e	= btn_vec->end();
	vec_sign_places.clear		();
	vec_sign_places.resize		(btn_vec->size());

	Fvector2					pos;
	pos.set						((*it)->GetWndPos());
	Fvector2					sign_sz;
	sign_sz.set					(9.0f+3.0f, 11.0f);
	u32 idx						= 0;
	float	btn_text_len		= 0.0f;
	CUIStatic* st				= nullptr;

	for(;it!=it_e;++it,++idx)
	{
		if(idx!=0)
		{
			st = new CUIStatic(); st->SetAutoDelete(true);pTab->AttachChild(st);
			st->SetFont((*it)->GetFont());
			st->SetTextColor	(color_rgba(90,90,90,255));
			st->SetText("//");
			st->SetWndSize		((*it)->GetWndSize());
			st->AdjustWidthToText();
			st->SetWndPos		(pos);
			pos.x				+= st->GetWndSize().x;
		}

		vec_sign_places[idx].set(pos);
		vec_sign_places[idx].y	+= iFloor(((*it)->GetWndSize().y - sign_sz.y)/2.0f);
		vec_sign_places[idx].y	= (float)iFloor(vec_sign_places[idx].y);
		pos.x					+= sign_sz.x * UI().get_current_kx();

		(*it)->SetWndPos		(pos);
		(*it)->AdjustWidthToText();
		btn_text_len			= (*it)->GetWndSize().x;
		pos.x					+= btn_text_len+3.0f;
	}

}

bool CUIPdaWnd::StopAnyMove() 
{ 
	return pInput->GetControllerMode(); 
}
