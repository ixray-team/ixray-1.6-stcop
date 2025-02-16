#include "stdafx.h"
#include "../pch_script.h"
#include "UIPdaWnd.h"
#include "../Pda.h"

#include "xrUIXmlParser.h"
#include "UIXmlInit.h"
#include "UIInventoryUtilities.h"

#include "../UIGameCustom.h"
#include "../level.h"
#include "../game_cl_base.h"

#include "UIStatic.h"
#include "UIFrameWindow.h"
#include "UITabControl.h"
//#include "UIPdaCommunication.h"
#include "UIPdaContactsWnd.h"
#include "UIMapWnd.h"
#include "UIDiaryWnd.h"
#include "UIFrameLineWnd.h"
#include "UIEncyclopediaWnd.h"
#include "UIStalkersRankingWnd.h"
#include "UIActorInfo.h"
#include "UIEventsWnd.h"
#include "../xrCore/object_broker.h"
#include "UIMessagesWindow.h"
#include "UIMainIngameWnd.h"
#include "uidialogwnd.h"
#include "uiscriptwnd.h"
#include "UITabButton.h"
#include "../script_engine.h"
#include "../ai_space.h"

u32			g_pda_info_state		= 0;

CUIPdaWnd::CUIPdaWnd()
{
	UIMapWnd				= NULL;
	UIPdaContactsWnd		= NULL;
	UIEncyclopediaWnd		= NULL;
	UIDiaryWnd				= NULL;
	UIActorInfo				= NULL;
	UIStalkersRanking		= NULL;
	UIEventsWnd				= NULL;
	UIChatWnd				= NULL;
	UISkillsWnd				= NULL;
	UIDownloadsWnd			= NULL;
	UIGamesWnd				= NULL;
	UIMPlayerWnd			= NULL;
	m_pActiveDialog			= NULL;
	bUpgraded				= false;
	m_initialized			= false;
}

CUIPdaWnd::~CUIPdaWnd()
{
	delete_data		(UIMapWnd);
	delete_data		(UIPdaContactsWnd);
	delete_data		(UIEncyclopediaWnd);
	delete_data		(UIDiaryWnd);
	delete_data		(UIActorInfo);
	delete_data		(UIStalkersRanking);
	delete_data		(UIEventsWnd);
	m_pActiveDialog	= NULL;
	//delete_data		(UISkillsWnd);
	//delete_data		(UIDownloadsWnd);
	//delete_data		(UIGamesWnd);
	//delete_data		(UIMPlayerWnd);
}

//////////////////////////////////////////////////////////////////////////

void CUIPdaWnd::Init()
{
	if (m_initialized) return;

	CUIXml uiXml;

	string128			PDA_XML;
	bUpgraded = IsGameTypeSingle() && InventoryUtilities::HasActorInfo("pda_upgraded");

	if (bUpgraded)
		xr_strcpy				(PDA_XML, "pda_upgraded.xml");
	else
		xr_strcpy				(PDA_XML, "pda.xml");

	bool xml_result			= uiXml.Load(CONFIG_PATH, UI_PATH,PDA_XML);
	R_ASSERT3				(xml_result, "xml file not found", PDA_XML);
	uiXml.Load(CONFIG_PATH, UI_PATH,PDA_XML);

	CUIXmlInit xml_init;
	
	m_pActiveDialog			= NULL;

	xml_init.InitWindow		(uiXml, "main", 0, this);

	UIMainPdaFrame			= new CUIStatic(); UIMainPdaFrame->SetAutoDelete(true);
	AttachChild				(UIMainPdaFrame);
	xml_init.InitStatic		(uiXml, "background_static", 0, UIMainPdaFrame);

	//Элементы автоматического добавления
	xml_init.InitAutoStatic	(uiXml, "auto_static", this);

	// Main buttons background
	UIMainButtonsBackground = new CUIFrameLineWnd(); UIMainButtonsBackground->SetAutoDelete(true);
	UIMainPdaFrame->AttachChild(UIMainButtonsBackground);
	xml_init.InitFrameLine	(uiXml, "mbbackground_frame_line", 0, UIMainButtonsBackground);

	// Timer background
	UITimerBackground		= new CUIFrameLineWnd(); UITimerBackground->SetAutoDelete(true);
	UIMainPdaFrame->AttachChild(UITimerBackground);
	xml_init.InitFrameLine	(uiXml, "timer_frame_line", 0, UITimerBackground);

	// Oкно карты
	UIMapWnd				= new CUIMapWnd();
	UIMapWnd->Init			("pda_map.xml","map_wnd");

	if( IsGameTypeSingle() )
	{
		// Oкно коммуникaции
		UIPdaContactsWnd		= new CUIPdaContactsWnd();
		UIPdaContactsWnd->Init	();


		// Oкно новостей
		UIDiaryWnd				= new CUIDiaryWnd();
		UIDiaryWnd->Init		();

		// Окно энциклопедии
		UIEncyclopediaWnd		= new CUIEncyclopediaWnd();
		UIEncyclopediaWnd->Init	();

		// Окно статистики о актере
		UIActorInfo				= new CUIActorInfoWnd();
		UIActorInfo->Init		();

		// Окно рейтинга сталкеров
		UIStalkersRanking		= new CUIStalkersRankingWnd();
		UIStalkersRanking->Init	();

		UIEventsWnd				= new CUIEventsWnd();
		UIEventsWnd->Init		();

		// gr1ph starts

		luabind::functor<CUIDialogWndEx*>	lua_pda_wnd_factory;
		string256							fn;
		xr_strcpy							(fn, pSettings->r_string("lost_alpha_cfg", "get_script_pda_window"));
		R_ASSERT2							(ai().script_engine().functor<CUIDialogWndEx*>(fn, lua_pda_wnd_factory), 
																			make_string("Can't find function '%s'", fn));
		UIChatWnd				= lua_pda_wnd_factory("comm");
		UISkillsWnd				= lua_pda_wnd_factory("skills");
		UIDownloadsWnd			= lua_pda_wnd_factory("downs");
		if (bUpgraded)
		{
			UIGamesWnd			= lua_pda_wnd_factory("games");
			UIMPlayerWnd			= lua_pda_wnd_factory("mplayer");
		}
	}
	// Tab control
	UITabControl				= new CUITabControl(); UITabControl->SetAutoDelete(true);
	UIMainPdaFrame->AttachChild	(UITabControl);

	xml_init.InitTabControl		(uiXml, "tab", 0, UITabControl);

	UITabControl->SetMessageTarget(this);

	if(GameID()!=GAME_SINGLE){
		for (u16 i = 0; i <= 8; i++){
			UITabControl->GetButtonsVector()->at(i)->Enable(false);
		}
	}

	m_pActiveSection				= eptNoActiveTab;

	//On\off button
	m_pUIClose					= new CUI3tButton(); m_pUIClose->SetAutoDelete(true);
	AttachChild						(m_pUIClose);
	xml_init.Init3tButton				(uiXml, "btn_close", 0, m_pUIClose);

	if( IsGameTypeSingle() )
	{
		bool enable_pda_skills = InventoryUtilities::HasActorInfo("pda_skills_enabled");
		bool enable_pda_downloads = InventoryUtilities::HasActorInfo("pda_downloads_enabled");
		
		UITabControl->GetButtonsVector()->at(eptSkills)->Enable(enable_pda_skills);	
		UITabControl->GetButtonsVector()->at(eptDownloads)->Enable(enable_pda_downloads);
	}

	m_initialized = true;
}

void CUIPdaWnd::SendMessage(CUIWindow* pWnd, s16 msg, void* pData)
{
	if (BUTTON_CLICKED == msg && m_pUIClose == pWnd)
		HideDialog();

	if(pWnd == UITabControl){
		if (TAB_CHANGED == msg){
			SetActiveSubdialog	((EPdaTabs)UITabControl->GetActiveIndex());
		}
	} else {
		R_ASSERT(m_pActiveDialog);
		m_pActiveDialog->SendMessage(pWnd, msg, pData);
	}
}

void CUIPdaWnd::ShowDialog(bool bDoHideIndicators)
{
	if (!m_initialized)		
		Init();
	InventoryUtilities::SendInfoToActor("ui_pda");
	inherited::ShowDialog(bDoHideIndicators);

	//BringToTop(UITabControl);
}

void CUIPdaWnd::ShowDialog(bool bDoHideIndicators, EPdaTabs section)
{
	if (!m_initialized)
		Init								();
	InventoryUtilities::SendInfoToActor		("ui_pda");
	SetActiveSubdialog						(section);
	inherited::ShowDialog					(bDoHideIndicators);
	//BringToTop(UITabControl);
}

void CUIPdaWnd::HideDialog()
{
	inherited::HideDialog();

	UITabControl->SetNewActiveTab	(eptMap); //skyloader: hack for contacts wnd

	InventoryUtilities::SendInfoToActor("ui_pda_hide");
	CurrentGameUI()->UIMainIngameWnd->SetFlashIconState_(CUIMainIngameWnd::efiPdaTask, false);
	CurrentGameUI()->UIMainIngameWnd->SetFlashIconState_(CUIMainIngameWnd::efiEncyclopedia, false);	
}

void CUIPdaWnd::UpdateDateTime()
{
	static shared_str prevStrTime = " ";
	xr_string strTime = *InventoryUtilities::GetGameTimeAsString(InventoryUtilities::etpTimeToMinutes);
				strTime += " ";
				strTime += *InventoryUtilities::GetGameDateAsString(InventoryUtilities::edpDateToDay);

	if (xr_strcmp(strTime.c_str(), prevStrTime))
	{
		UITimerBackground->UITitleText.SetText(strTime.c_str());
		prevStrTime = strTime.c_str();
	}
}

void CUIPdaWnd::Update()
{
	inherited::Update		();
	UpdateDateTime			();
}


void CUIPdaWnd::SetActiveSubdialog(EPdaTabs section)
{
	if (!m_initialized)		
		Init();
	if(	m_pActiveSection == section) return;
	
	CUIDialogWnd* dlg = 0;

	if (m_pActiveDialog)
	{
		dlg = smart_cast<CUIDialogWnd*>(m_pActiveDialog);
		if (dlg)
			dlg->Dispatch(0x30011991, 0);
		UIMainPdaFrame->DetachChild(m_pActiveDialog);
		m_pActiveDialog->Show(false);
	}

	m_pActiveDialog				= NULL;

	switch (section) 
	{
	case eptDiary:
		m_pActiveDialog			= smart_cast<CUIWindow*>(UIDiaryWnd);
		InventoryUtilities::SendInfoToActor("ui_pda_events");
		g_pda_info_state		&= ~pda_section::diary;
		break;
	case eptContacts:
		m_pActiveDialog			= smart_cast<CUIWindow*>(UIChatWnd);
		InventoryUtilities::SendInfoToActor("ui_pda_contacts");
		g_pda_info_state		&= ~pda_section::contacts;
		break;
	case eptMap:
		m_pActiveDialog			= smart_cast<CUIWindow*>(UIMapWnd);
		g_pda_info_state		&= ~pda_section::map;
		InventoryUtilities::SendInfoToActor("ui_pda_map");
		break;
	case eptEncyclopedia:
		m_pActiveDialog			= smart_cast<CUIWindow*>(UIEncyclopediaWnd);
		InventoryUtilities::SendInfoToActor("ui_pda_encyclopedia");
		g_pda_info_state		&= ~pda_section::encyclopedia;
		break;
	case eptActorStatistic:
		m_pActiveDialog			= smart_cast<CUIWindow*>(UIActorInfo);
		InventoryUtilities::SendInfoToActor("ui_pda_actor_info");
		g_pda_info_state		&= ~pda_section::statistics;
		break;
	case eptRanking:
		m_pActiveDialog			= smart_cast<CUIWindow*>(UIStalkersRanking);
		g_pda_info_state		&= ~pda_section::ranking;
		InventoryUtilities::SendInfoToActor("ui_pda_ranking");
		break;
	case eptQuests:
		m_pActiveDialog			= smart_cast<CUIWindow*>(UIEventsWnd);
		g_pda_info_state		&= ~pda_section::quests;
		InventoryUtilities::SendInfoToActor("ui_pda_quests");
		break;
	case eptSkills:
		m_pActiveDialog			= smart_cast<CUIWindow*>(UISkillsWnd);
		g_pda_info_state		&= ~pda_section::skills;
		InventoryUtilities::SendInfoToActor("ui_pda_skills");
		break;
	case eptDownloads:
		m_pActiveDialog			= smart_cast<CUIWindow*>(UIDownloadsWnd);
		g_pda_info_state		&= ~pda_section::downloads;
		InventoryUtilities::SendInfoToActor("ui_pda_downloads");
		break;
	case eptGames:
		m_pActiveDialog			= smart_cast<CUIWindow*>(UIGamesWnd);
		g_pda_info_state		&= ~pda_section::games;
		InventoryUtilities::SendInfoToActor("ui_pda_games");
		break;
	case eptMPlayer:
		m_pActiveDialog			= smart_cast<CUIWindow*>(UIMPlayerWnd);
		g_pda_info_state		&= ~pda_section::mplayer;
		InventoryUtilities::SendInfoToActor("ui_pda_mplayer");
		break;
	default:
		Msg("not registered button identifier [%d]",UITabControl->GetActiveIndex());
		m_pActiveDialog = NULL;
		break;
	}

	R_ASSERT(m_pActiveDialog && m_pActiveDialog->GetWidth());

	if (UIMainPdaFrame->IsChild(m_pActiveDialog)) {
		UIMainPdaFrame->DetachChild(m_pActiveDialog);
		m_pActiveDialog->SetParent(NULL);
	}
	
	UIMainPdaFrame->AttachChild		(m_pActiveDialog);
	m_pActiveDialog->Show			(true);

	if(UITabControl->GetActiveIndex()!=section)
		UITabControl->SetNewActiveTab	(section);

	m_pActiveSection = section;

	dlg = smart_cast<CUIDialogWnd*>(m_pActiveDialog);
	if (dlg)
		dlg->Dispatch(0x30011991, 1);
}

void CUIPdaWnd::PdaContentsChanged	(pda_section::part type)
{
	bool bTask = true;
	bool bEncyclopedia = false;

	if (!m_initialized)		
		Init();

	if (type==pda_section::encyclopedia)
	{
		UIEncyclopediaWnd->ReloadArticles();
		bEncyclopedia = true;
		bTask = false;

	} else if(type==pda_section::news) {
		UIDiaryWnd->AddNews();
		UIDiaryWnd->MarkNewsAsRead(UIDiaryWnd->IsShown());

	} else if(type==pda_section::quests)
		UIEventsWnd->Reload();

	else if(type==pda_section::contacts) {
		UIPdaContactsWnd->Reload();
		bTask = false;
	}

	if(bTask)
	{
		g_pda_info_state |= type;
		CurrentGameUI()->UIMainIngameWnd->SetFlashIconState_(CUIMainIngameWnd::efiPdaTask, true);
	}


	if(bEncyclopedia)
	{
		g_pda_info_state |= type;
		CurrentGameUI()->UIMainIngameWnd->SetFlashIconState_(CUIMainIngameWnd::efiEncyclopedia, true);
	}

}

void CUIPdaWnd::Reset()
{
	inherited::ResetAll();
	if (!m_initialized)		return;
	if (UIMapWnd)			UIMapWnd->ResetAll();
	if (UIPdaContactsWnd)	UIPdaContactsWnd->ResetAll();
	if (UIEncyclopediaWnd)	UIEncyclopediaWnd->ResetAll();
	if (UIDiaryWnd)			UIDiaryWnd->ResetAll();
	if (UIActorInfo)		UIActorInfo->ResetAll();
	if (UIStalkersRanking)	UIStalkersRanking->ResetAll();
	if (UIEventsWnd)		UIEventsWnd->ResetAll();
	//if (UISkillsWnd)			UISkillsWnd->ResetAll();
	//if (UIDownloadsWnd)		UIDownloadsWnd->ResetAll();
	//if (UIGamesWnd)			UIGamesWnd->ResetAll();
	//if (UIMPlayerWnd)		UIMPlayerWnd->ResetAll();
}

void CUIPdaWnd::EnableSkills(bool val)
{
	InventoryUtilities::SendInfoToActor(val ? "pda_skills_enabled" : "pda_skills_disabled");
	if (m_initialized)
	{
		UITabControl->GetButtonsVector()->at(eptSkills)->Enable(val);
	}
}

void CUIPdaWnd::EnableDownloads(bool val)
{
	InventoryUtilities::SendInfoToActor(val ? "pda_downloads_enabled" : "pda_downloads_disabled");
	if (m_initialized)
	{
		UITabControl->GetButtonsVector()->at(eptDownloads)->Enable(val);
	}
}
