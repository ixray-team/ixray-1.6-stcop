#include "StdAfx.h"

#include "UIGameDM.h"

#include "ui/UISkinSelector.h"
#include "ui/UIPdaWnd.h"
#include "ui/UIMapDesc.h"
#include "ui/KillMessageStruct.h"
#include "Level.h"
#include "game_cl_base.h"
#include "Spectator.h"
#include "Inventory.h"
#include "InventoryOwner.h"
#include "xrServer_Objects_ALife_Items.h"
#include "../../xrUI/xrUIXmlParser.h"
#include "game_cl_deathmatch.h"
#include "ui/UIMoneyIndicator.h"
#include "ui/UIRankIndicator.h"
#include "ui/UIVoteStatusWnd.h"
#include "ui/UIActorMenu.h"
#include "UIHelperGame.h"
#include "UITeamPanels.h"
#include "object_broker.h"
#include "../../xrUI/UIFontDefines.h"
#include "UIFrags.h"
#include "UIDMStatisticWnd.h"

#define MSGS_OFFS 510

#define TIME_MSG_COLOR			0xffff0000
#define SPECTRMODE_MSG_COLOR	0xffff0000
#define NORMAL_MSG_COLOR		0xffffffff
#define ROUND_RESULT_COLOR		0xfff0fff0
#define VOTE0_MSG_COLOR			0xffff0000
#define VOTE1_MSG_COLOR			0xff00ff00
#define DEMOPLAY_COLOR			0xff00ff00
#define WARM_UP_COLOR			0xff00ff00


#define DI2PX(x) float(iFloor((x+1)*float(UI_BASE_WIDTH)*0.5f))
#define DI2PY(y) float(iFloor((y+1)*float(UI_BASE_HEIGHT)*0.5f))
#define SZ(x) x*UI_BASE_WIDTH

//--------------------------------------------------------------------
#define TEAM_PANELS_DM_XML_NAME "ui_team_panels_dm.xml"

//--------------------------------------------------------------------
CUIGameDM::CUIGameDM()
{
	m_game								= nullptr; 

	m_pFragLists						= nullptr;
	m_pPlayerLists						= nullptr;
	m_pStatisticWnds					= nullptr;
	m_pTeamPanels						= nullptr;

	m_time_caption						= nullptr;
	m_spectrmode_caption				= nullptr;
	m_spectator_caption					= nullptr;
	m_pressjump_caption					= nullptr;
	m_pressbuy_caption					= nullptr;
	m_round_result_caption				= nullptr;
	m_force_respawn_time_caption		= nullptr;
	m_demo_play_caption					= nullptr;
	m_warm_up_caption					= nullptr;

	m_voteStatusWnd						= nullptr;
	m_pMapDesc							= nullptr;
}

//--------------------------------------------------------------------
void CUIGameDM::SetClGame (game_cl_GameState* g)
{
	inherited::SetClGame(g);
	m_game = g->cast_game_cl_deathmatch();
	R_ASSERT(m_game);
	
	if (m_pMapDesc && m_pMapDesc->IsShown())
	{
		m_pMapDesc->ShowDialog(true);
	}
	delete_data(m_pMapDesc);
	m_pMapDesc			= new CUIMapDesc		();
	UpdateTeamPanels	();
}

void	CUIGameDM::Init(int stage)
{
	if(stage==0)
	{ // shared
		CUIXml xml_test;
		if (xml_test.Load(CONFIG_PATH, UI_PATH, TEAM_PANELS_DM_XML_NAME))
		{
			m_pTeamPanels = new UITeamPanels();
			m_pTeamPanels->Init(TEAM_PANELS_DM_XML_NAME, "team_panels_wnd");
		}
		else
		{
			xml_test.Load(CONFIG_PATH, UI_PATH, "stats.xml");

			CUIFrags* pFragList = new CUIFrags();
			CUIFrags* pPlayerList = new CUIFrags();
			CUIDMStatisticWnd* pStatisticWnd = new CUIDMStatisticWnd();
			pFragList->SetAutoDelete(true);
			pPlayerList->SetAutoDelete(true);
			pStatisticWnd->SetAutoDelete(true);


			float ScreenW = UI_BASE_WIDTH;
			float ScreenH = UI_BASE_HEIGHT;
			//-----------------------------------------------------------
			pFragList->Init(xml_test, "stats_wnd", "frag_wnd_dm");
			pPlayerList->Init(xml_test, "players_wnd", "frag_wnd_dm");

			Frect FrameRect = pFragList->GetWndRect();
			float FrameW = FrameRect.right - FrameRect.left;
			float FrameH = FrameRect.bottom - FrameRect.top;
			pFragList->SetWndPos(Fvector2().set((ScreenW - FrameW) / 2.0f, (ScreenH - FrameH) / 2.0f));

			m_pFragLists = new CUIWindow();
			m_pFragLists->AttachChild(pFragList);
			//-----------------------------------------------------------
			FrameRect = pPlayerList->GetWndRect();
			FrameW = FrameRect.right - FrameRect.left;
			FrameH = FrameRect.bottom - FrameRect.top;
			pPlayerList->SetWndPos(Fvector2().set((ScreenW - FrameW) / 2.0f, (ScreenH - FrameH) / 2.0f));

			m_pPlayerLists = new CUIWindow();
			m_pPlayerLists->AttachChild(pPlayerList);
			//-----------------------------------------------------------
			FrameRect = pStatisticWnd->GetFrameRect();
			FrameW = FrameRect.right - FrameRect.left;
			FrameH = FrameRect.bottom - FrameRect.top;
			pStatisticWnd->SetWndRect(Frect().set((ScreenW - FrameW) / 2.0f, (ScreenH - FrameH) / 2.0f, FrameW, FrameH));

			m_pStatisticWnds = new CUIWindow();
			m_pStatisticWnds->AttachChild(pStatisticWnd);
		}
		m_pMoneyIndicator				= new CUIMoneyIndicator();
		m_pMoneyIndicator->SetAutoDelete(true);
		m_pRankIndicator				= new CUIRankIndicator();
		m_pRankIndicator->SetAutoDelete	(true);
		m_pFragLimitIndicator			= new CUIStatic();
		m_pFragLimitIndicator->SetAutoDelete(true);
		
		inherited::Init					(stage);
		if (m_msgs_xml->NavigateToNode("mp_timelimit"))
			m_time_caption				= UIHelper::CreateTextWnd(*m_msgs_xml, "mp_timelimit", m_window);
		else
		{
			GameCaptions()->addCustomMessage(m_time_caption_legacy, DI2PX(0.0f), DI2PY(-0.8f), SZ(0.03f), UI().Font().GetFont(GRAFFITI19_FONT_NAME), CGameFont::alCenter, TIME_MSG_COLOR, "");
		}
		if (m_msgs_xml->NavigateToNode("mp_spetatormode"))
			m_spectrmode_caption		= UIHelper::CreateTextWnd(*m_msgs_xml, "mp_spetatormode", m_window);
		else
		{
			GameCaptions()->addCustomMessage(m_spectrmode_caption_legacy, DI2PX(0.0f), DI2PY(-0.7f), SZ(0.03f), UI().Font().GetFont(GRAFFITI19_FONT_NAME), CGameFont::alCenter, SPECTRMODE_MSG_COLOR, "");
		}
		if (m_msgs_xml->NavigateToNode("mp_spectator"))
			m_spectator_caption			= UIHelper::CreateTextWnd(*m_msgs_xml, "mp_spectator", m_window);
		else
		{
			GameCaptions()->addCustomMessage(m_spectator_caption_legacy, DI2PX(0.0f), DI2PY(0.0f), SZ(0.03f), UI().Font().GetFont(GRAFFITI19_FONT_NAME), CGameFont::alCenter, NORMAL_MSG_COLOR, "");
		}
		if (m_msgs_xml->NavigateToNode("mp_pressjump"))
			m_pressjump_caption			= UIHelper::CreateTextWnd(*m_msgs_xml, "mp_pressjump", m_window);
		else
		{
			GameCaptions()->addCustomMessage(m_pressjump_caption_legacy, DI2PX(0.0f), DI2PY(0.9f), SZ(0.02f), UI().Font().GetFont(GRAFFITI19_FONT_NAME), CGameFont::alCenter, NORMAL_MSG_COLOR, "");
		}
		if (m_msgs_xml->NavigateToNode("mp_pressbuy"))
			m_pressbuy_caption			= UIHelper::CreateTextWnd(*m_msgs_xml, "mp_pressbuy", m_window);
		else
		{
			GameCaptions()->addCustomMessage(m_pressbuy_caption_legacy, DI2PX(0.0f), DI2PY(0.95f), SZ(0.02f), UI().Font().GetFont(GRAFFITI19_FONT_NAME), CGameFont::alCenter, NORMAL_MSG_COLOR, "");
		}
		if (m_msgs_xml->NavigateToNode("mp_round_result"))
			m_round_result_caption		= UIHelper::CreateTextWnd(*m_msgs_xml, "mp_round_result", m_window);
		else
		{
			GameCaptions()->addCustomMessage(m_round_result_caption_legacy, DI2PX(0.0f), DI2PY(-0.1f), SZ(0.03f), UI().Font().GetFont(GRAFFITI19_FONT_NAME), CGameFont::alCenter, ROUND_RESULT_COLOR, "");
		}
		if (m_msgs_xml->NavigateToNode("mp_force_respawn_time"))
			m_force_respawn_time_caption	= UIHelper::CreateTextWnd(*m_msgs_xml, "mp_force_respawn_time", m_window);
		else
		{
			GameCaptions()->addCustomMessage(m_force_respawn_time_caption_legacy, DI2PX(0.0f), DI2PY(-0.9f), SZ(0.02f), UI().Font().GetFont(GRAFFITI19_FONT_NAME), CGameFont::alCenter, NORMAL_MSG_COLOR, "");
		}
		if (m_msgs_xml->NavigateToNode("mp_demo_play"))
			m_demo_play_caption			= UIHelper::CreateTextWnd(*m_msgs_xml, "mp_demo_play", m_window);
		else
		{
			GameCaptions()->addCustomMessage(m_demo_play_caption_legacy, DI2PX(-1.0f), DI2PY(-0.95f), SZ(0.05f), UI().Font().GetFont(GRAFFITI19_FONT_NAME), CGameFont::alLeft, DEMOPLAY_COLOR, "");
		}
		if (m_msgs_xml->NavigateToNode("mp_warm_up"))
			m_warm_up_caption			= UIHelper::CreateTextWnd(*m_msgs_xml, "mp_warm_up", m_window);
		else
		{
			GameCaptions()->addCustomMessage(m_warm_up_caption_legacy, DI2PX(0.0f), DI2PY(-0.75f), SZ(0.05f), UI().Font().GetFont(GRAFFITI19_FONT_NAME), CGameFont::alCenter, WARM_UP_COLOR, "");
		}
	}
	if(stage==1)
	{ //unique
		CUIXml							uiXml;
		uiXml.Load						(CONFIG_PATH, UI_PATH, "ui_game_dm.xml");
		if (uiXml.NavigateToNode("global"))
			CUIXmlInit::InitWindow			(uiXml,"global", 0, m_window);
		else
		{
			m_window->SetWndPos(Fvector2().set(0, 0));
			m_window->SetWndSize(Fvector2().set(UI_BASE_WIDTH, UI_BASE_HEIGHT));
		}
		m_pMoneyIndicator->InitFromXML	(uiXml);
		m_pRankIndicator->InitFromXml	(uiXml);
		CUIXmlInit::InitStatic			(uiXml,"fraglimit",0, m_pFragLimitIndicator);
	}
	if(stage==2)
	{ //after
		inherited::Init					(stage);
		m_window->AttachChild			(m_pMoneyIndicator);
		m_window->AttachChild			(m_pRankIndicator);
		m_window->AttachChild			(m_pFragLimitIndicator);

	}
};

void CUIGameDM::UnLoad()
{
	inherited::UnLoad		();
	xr_delete				(m_pStatisticWnds);
	xr_delete				(m_pFragLists);
	xr_delete				(m_pPlayerLists);
	xr_delete				(m_pTeamPanels);
	xr_delete				(m_voteStatusWnd);
	delete_data				(m_pMapDesc);	
}

CUIGameDM::~CUIGameDM()
{
}


void CUIGameDM::SetTimeMsgCaption(LPCSTR str)
{
	if (m_time_caption)
		m_time_caption->SetTextST(str);
	else
		GameCaptions()->setCaption(m_time_caption_legacy, str, TIME_MSG_COLOR, true);
}

void CUIGameDM::ShowFragList(bool bShow)
{
	if (bShow)
	{
		if (m_pTeamPanels)
			AddDialogToRender(m_pTeamPanels);
		else if (m_pFragLists)
			AddDialogToRender(m_pFragLists);
	}
	else
	{
		if (m_pTeamPanels)
			RemoveDialogToRender(m_pTeamPanels);
		else if (m_pFragLists)
			RemoveDialogToRender(m_pFragLists);
	}
}

void CUIGameDM::ShowPlayersList(bool bShow)
{
	if (bShow)
	{
		if (m_pTeamPanels)
			AddDialogToRender(m_pTeamPanels);
		else if (m_pPlayerLists)
			AddDialogToRender(m_pPlayerLists);
	}
	else
	{
		if (m_pTeamPanels)
			RemoveDialogToRender(m_pTeamPanels);
		else if (m_pPlayerLists)
			RemoveDialogToRender(m_pPlayerLists);
	}
}


void CUIGameDM::SetSpectrModeMsgCaption(LPCSTR str)
{
	if (m_spectrmode_caption)
		m_spectrmode_caption->SetTextST(str);
	else
		GameCaptions()->setCaption(m_spectrmode_caption_legacy, str, SPECTRMODE_MSG_COLOR, true);
}

void CUIGameDM::SetSpectatorMsgCaption(LPCSTR str)
{
	if (m_spectator_caption)
		m_spectator_caption->SetTextST(str);
	else if (GameCaptions()) 
		GameCaptions()->setCaption(m_spectator_caption_legacy, str, NORMAL_MSG_COLOR, true);
}

void CUIGameDM::SetPressJumpMsgCaption(LPCSTR str)
{
	if (m_pressjump_caption)
		m_pressjump_caption->SetTextST(str);
	else
		GameCaptions()->setCaption(m_pressjump_caption_legacy, str, NORMAL_MSG_COLOR, true);
}

void CUIGameDM::SetPressBuyMsgCaption(LPCSTR str)
{
	if (m_pressbuy_caption)
		m_pressbuy_caption->SetTextST(str);
	else
		GameCaptions()->setCaption(m_pressbuy_caption_legacy, str, NORMAL_MSG_COLOR, true);
}


void CUIGameDM::SetRoundResultCaption(LPCSTR str)
{
	if (m_round_result_caption)
		m_round_result_caption->SetTextST(str);
	else
		GameCaptions()->setCaption(m_round_result_caption_legacy, str, ROUND_RESULT_COLOR, true);
}

void CUIGameDM::SetForceRespawnTimeCaption(LPCSTR str)
{
	if (m_force_respawn_time_caption)
		m_force_respawn_time_caption->SetTextST(str);
	else
		GameCaptions()->setCaption(m_force_respawn_time_caption_legacy, str, NORMAL_MSG_COLOR, true);
}

void CUIGameDM::SetDemoPlayCaption(LPCSTR str)
{
	if (m_demo_play_caption)
		m_demo_play_caption->SetTextST(str);
	else
		GameCaptions()->setCaption(m_demo_play_caption_legacy, str, DEMOPLAY_COLOR, true);
}

void CUIGameDM::SetWarmUpCaption(LPCSTR str)
{
	if (m_warm_up_caption)
		m_warm_up_caption->SetTextST(str);
	else
		GameCaptions()->setCaption(m_warm_up_caption_legacy, str, WARM_UP_COLOR, true);
}

void CUIGameDM::SetVoteMessage					(LPCSTR str)
{
	if(!str)
		xr_delete(m_voteStatusWnd);
	else{
		if(!m_voteStatusWnd)
		{
			CUIXml							uiXml;
			uiXml.Load						(CONFIG_PATH, UI_PATH, "ui_game_dm.xml");
			m_voteStatusWnd					= new UIVoteStatusWnd();
			m_voteStatusWnd->InitFromXML	(uiXml);
		}
		m_voteStatusWnd->Show				(true);
		m_voteStatusWnd->SetVoteMsg			(str);
	}
};

void CUIGameDM::SetVoteTimeResultMsg			(LPCSTR str)
{
	if(m_voteStatusWnd)
		m_voteStatusWnd->SetVoteTimeResultMsg(str);
}

void CUIGameDM::OnFrame()
{
	inherited::OnFrame				();
	if(m_voteStatusWnd && m_voteStatusWnd->IsShown()) 
		m_voteStatusWnd->Update		();
}

void CUIGameDM::Render()
{
	inherited::Render				();
	if(m_voteStatusWnd && m_voteStatusWnd->IsShown()) 
		m_voteStatusWnd->Draw		();
}

void CUIGameDM::DisplayMoneyChange(LPCSTR deltaMoney)
{
	m_pMoneyIndicator->SetMoneyChange(deltaMoney);
}

void CUIGameDM::DisplayMoneyBonus(KillMessageStruct* bonus){
	m_pMoneyIndicator->AddBonusMoney(*bonus);
}

void CUIGameDM::ChangeTotalMoneyIndicator(LPCSTR newMoneyString)
{
	m_pMoneyIndicator->SetMoneyAmount(newMoneyString);
}

void	CUIGameDM::SetRank(s16 team, u8 rank)
{
	m_pRankIndicator->SetRank(u8(m_game->ModifyTeam(team)), rank);
};

void CUIGameDM::SetFraglimit(int local_frags, int fraglimit)
{
	string64 str;
	if(fraglimit)
		xr_sprintf(str,"%d/%d", local_frags, fraglimit);
	else
		xr_sprintf(str,"%d", local_frags);

	m_pFragLimitIndicator->SetText(str);
}

void CUIGameDM::UpdateTeamPanels()
{
	if (m_pTeamPanels)
	{
		m_pTeamPanels->NeedUpdatePanels();
		m_pTeamPanels->NeedUpdatePlayers();
	}
}
