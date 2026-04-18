#include "StdAfx.h"
#include "UIGameAHunt.h"

#include "team_base_zone.h"
#include "Level.h"
#include "game_cl_artefacthunt.h"
#include "../../xrUI/Widgets/UIStatic.h"
#include "../../xrUI/Widgets/UIProgressShape.h"
#include "../../xrUI/UIXmlInit.h"
#include "ui/UIMessageBoxEx.h"
#include "ui/UIMoneyIndicator.h"
#include "ui/UIRankIndicator.h"
#include "UIHelperGame.h"
#include "UITeamPanels.h"
#include "object_broker.h"
#include "../../xrUI/Widgets/UIMultiTextStatic.h"
#include "../../xrUI/UIFontDefines.h"

#define MSGS_OFFS 510
#define TEAM_PANELS_TDM_XML_NAME "ui_team_panels_tdm.xml"

#define BUY_MSG_COLOR		0xffffff00
#define SCORE_MSG_COLOR		0xffffffff
#define REINFORCEMENT_MSG_COLOR		0xff8080ff
#define TODO_MSG_COLOR		0xff00ff00

#define DI2PX(x) float(iFloor((x+1)*float(UI_BASE_WIDTH)*0.5f))
#define DI2PY(y) float(iFloor((y+1)*float(UI_BASE_HEIGHT)*0.5f))
#define SZ(x) x*UI_BASE_WIDTH

#define TEAM_PANELS_AHUNT_XML_NAME "ui_team_panels_ahunt.xml"

CUIGameAHunt::CUIGameAHunt()
:m_game(nullptr),m_pBuySpawnMsgBox(nullptr)
{
	m_buy_msg_caption = nullptr;
}

void CUIGameAHunt::Init	(int stage)
{
	if(stage==0)
	{ // shared
		inherited::Init					(stage);
		if (m_msgs_xml->NavigateToNode("mp_ah_buy"))
			m_buy_msg_caption			= UIHelper::CreateStatic(*m_msgs_xml, "mp_ah_buy", m_window);
		else
		{
			GameCaptions()->addCustomMessage(m_buy_msg_caption_legacy, DI2PX(0.0f), DI2PY(0.9f), SZ(0.02f), UI().Font().GetFont(GRAFFITI19_FONT_NAME), CGameFont::alCenter, BUY_MSG_COLOR, "");
		}
	}
	if(stage==1)
	{ //unique
		CUIXml							uiXml;
		if (uiXml.Load(CONFIG_PATH, UI_PATH, TEAM_PANELS_AHUNT_XML_NAME))
		{
			m_pTeamPanels = new UITeamPanels();
			m_pTeamPanels->Init(TEAM_PANELS_AHUNT_XML_NAME, "team_panels_wnd");
		}

		uiXml.Load						(CONFIG_PATH, UI_PATH, "ui_game_ahunt.xml");

		CUIXmlInit::InitWindow			(uiXml, "global", 0,		m_window);
		CUIXmlInit::InitStatic			(uiXml, "fraglimit",0,		m_pFragLimitIndicator);

		if (uiXml.NavigateToNode("reinforcement:front"))
		{
			m_pReinforcementInidcator_old = new CUIProgressShape();
			CUIXmlInit::InitProgressShape(uiXml, "reinforcement", 0, m_pReinforcementInidcator_old);
		}
		else
		{
			m_pReinforcementInidcator = new CUIStatic();
			m_pReinforcementInidcator->SetAutoDelete(true);
			CUIXmlInit::InitStatic(uiXml, "reinforcement", 0, m_pReinforcementInidcator);
		}
		CUIXmlInit::InitStatic			(uiXml, "team1_icon", 0,	m_team1_icon);
		CUIXmlInit::InitStatic			(uiXml, "team2_icon", 0,	m_team2_icon);
		CUIXmlInit::InitStatic			(uiXml, "team1_score", 0,	m_team1_score);
		CUIXmlInit::InitStatic			(uiXml, "team2_score", 0,	m_team2_score);

		m_pMoneyIndicator->InitFromXML	(uiXml);
		m_pRankIndicator->InitFromXml	(uiXml);
	}
	if(stage==2)
	{ //after
		inherited::Init					(stage);
		if (m_pReinforcementInidcator)
			m_window->AttachChild			(m_pReinforcementInidcator);
	}
};

void CUIGameAHunt::UnLoad()
{
	inherited::UnLoad	();
}

CUIGameAHunt::~CUIGameAHunt()
{
	if (m_pReinforcementInidcator_old)
		xr_delete(m_pReinforcementInidcator_old);
	delete_data			(m_pBuySpawnMsgBox);
}

void CUIGameAHunt::SetClGame (game_cl_GameState* g)
{
	inherited::SetClGame(g);
	m_game = g->cast_game_cl_artefacthunt();
	R_ASSERT(m_game);
	//-----------------------------------------------------------------------
	delete_data							(m_pBuySpawnMsgBox);
	m_pBuySpawnMsgBox					= new CUIMessageBoxEx();	
	m_pBuySpawnMsgBox->InitMessageBox	("message_box_buy_spawn");
	m_pBuySpawnMsgBox->SetText			("");

	game_cl_mp* clmp_game = g->cast_game_cl_mp();
	//m_pBuySpawnMsgBox->AddCallback("msg_box", MESSAGE_BOX_YES_CLICKED, CUIWndCallback::void_function(clmp_game, &game_cl_mp::OnBuySpawn));
	m_pBuySpawnMsgBox->func_on_ok = CUIWndCallback::void_function(clmp_game, &game_cl_mp::OnBuySpawn);
}

void CUIGameAHunt::SetBuyMsgCaption(const char* str)
{
	if (m_buy_msg_caption)
		m_buy_msg_caption->SetTextST(str);
	else
		GameCaptions()->setCaption(m_buy_msg_caption_legacy, str, BUY_MSG_COLOR, true);
}

void CUIGameAHunt::Render()
{
	if (m_pReinforcementInidcator_old)
		m_pReinforcementInidcator_old->Draw();
	inherited::Render();
}

void CUIGameAHunt::OnFrame()
{
	inherited::OnFrame();
	if (m_pReinforcementInidcator_old)
		m_pReinforcementInidcator_old->Update();
}
