/*
sv_listplayers - выводит список игроков
sv_kick_id <id> - кикает чувака
sv_banplayer <id> <time> - банит чувака
make_screenshot <id> - делает скриншот
make_config_dump <id> - делает дамп конфигов
config_dump_all - дамп всех клиентов
screenshot_all - скриншот всех клиентов
sv_max_ping_limit - лимит пинга
g_restart_fast - быстрый рестарт
sv_setenvtime <time> - поставить время
*/
#include "stdafx.h"
#include "UIMPAdminMenu.h"
#include "UIXmlInit.h"
#include "object_broker.h"
#include "UITabControl.h"
#include "UIListBox.h"
#include "UIListBoxItem.h"
#include "UIStatic.h"
#include "UI3tButton.h"
#include "UITrackBar.h"

#include "../Level.h"
#include "../xrServer.h"
#include "../../xrEngine/xr_ioconsole.h"
CUIMpAdminMenu::CUIMpAdminMenu()
{
	xml_doc	= NULL;
	m_pActiveDialog = NULL;
	m_sActiveSection = "";

	m_pBack = xr_new<CUIStatic>();
	m_pBack->SetAutoDelete(true);
	AttachChild(m_pBack);

	m_pTabControl = xr_new<CUITabControl>();
	m_pTabControl->SetAutoDelete(true);
	AttachChild(m_pTabControl);

	m_pPlayersAdm = xr_new<CUIMpPlayersAdm>();
	m_pPlayersAdm->SetAutoDelete(false);

	m_pServerAdm = xr_new<CUIMpServerAdm>();
	m_pServerAdm->SetAutoDelete(false);

	m_pClose = xr_new<CUI3tButton>();
	m_pClose->SetAutoDelete(true);
	AttachChild(m_pClose);

	Init();
}

CUIMpAdminMenu::~CUIMpAdminMenu()
{
	xr_delete(xml_doc);
	delete_data(m_pPlayersAdm);
	delete_data(m_pServerAdm);
}

void CUIMpAdminMenu::Init()
{
	if (!xml_doc)
		xml_doc = xr_new<CUIXml>();

	xml_doc->Load(CONFIG_PATH, UI_PATH, "ui_mp_admin_menu.xml");

	CUIXmlInit::InitWindow(*xml_doc, "admin_menu", 0, this);
	CUIXmlInit::InitStatic(*xml_doc, "admin_menu:background", 0, m_pBack);
	CUIXmlInit::InitTabControl(*xml_doc, "admin_menu:tab_control", 0, m_pTabControl);
	m_pPlayersAdm->Init(*xml_doc);
	m_pServerAdm->Init(*xml_doc);
	m_pTabControl->SetActiveTab("players");
	SetActiveSubdialog("players");
	CUIXmlInit::Init3tButton(*xml_doc, "admin_menu:close_button", 0, m_pClose);
}

void CUIMpAdminMenu::SendMessage(CUIWindow* pWnd, s16 msg, void* pData)
{
	switch(msg)
	{
	case WINDOW_KEY_PRESSED:
		{
			break;
		}
	case TAB_CHANGED:
		{
			if(pWnd==m_pTabControl)
				SetActiveSubdialog(m_pTabControl->GetActiveId());
			break;
		}
	case BUTTON_CLICKED:
		{
			if(pWnd==m_pClose)
				HideDialog();
			break;
		}
	default:
		{
			R_ASSERT(m_pActiveDialog);
			m_pActiveDialog->SendMessage(pWnd, msg, pData);
		}
	};
}

void CUIMpAdminMenu::SetActiveSubdialog(const shared_str& section)
{
	if(m_sActiveSection==section) 
		return;

	if(m_pActiveDialog)
	{
		DetachChild(m_pActiveDialog);
		m_pActiveDialog->Show(false);
	}

	if(section=="players")
		m_pActiveDialog = m_pPlayersAdm;
	else if(section=="server")
		m_pActiveDialog = m_pServerAdm;

	R_ASSERT(m_pActiveDialog);
	AttachChild(m_pActiveDialog);
	m_pActiveDialog->Show(true);
	m_sActiveSection = section;
}

//---------------------------------------------------------------------------------------------------
CUIMpPlayersAdm::CUIMpPlayersAdm()
{
	m_pPlayersList = xr_new<CUIListBox>();
	m_pPlayersList->SetAutoDelete(true);
	AttachChild(m_pPlayersList);

	m_pRefreshBtn = xr_new<CUI3tButton>();
	m_pRefreshBtn->SetAutoDelete(true);
	AttachChild(m_pRefreshBtn);

	m_pScreenAllBtn = xr_new<CUI3tButton>();
	m_pScreenAllBtn->SetAutoDelete(true);
	AttachChild(m_pScreenAllBtn);

	m_pConfigAllBtn = xr_new<CUI3tButton>();
	m_pConfigAllBtn->SetAutoDelete(true);
	AttachChild(m_pConfigAllBtn);
	
	m_pPingLimitBtn = xr_new<CUI3tButton>();
	m_pPingLimitBtn->SetAutoDelete(true);
	AttachChild(m_pPingLimitBtn);

	m_pPingLimitTrack = xr_new<CUITrackBar>();
	m_pPingLimitTrack->SetAutoDelete(true);
	AttachChild(m_pPingLimitTrack);

	m_pPingLimitText = xr_new<CUITextWnd>();
	m_pPingLimitText->SetAutoDelete(true);
	AttachChild(m_pPingLimitText);

	m_pScreenPlayerBtn = xr_new<CUI3tButton>();
	m_pScreenPlayerBtn->SetAutoDelete(true);
	AttachChild(m_pScreenPlayerBtn);

	m_pConfigPlayerBtn = xr_new<CUI3tButton>();
	m_pConfigPlayerBtn->SetAutoDelete(true);
	AttachChild(m_pConfigPlayerBtn);

	m_pKickPlayerBtn = xr_new<CUI3tButton>();
	m_pKickPlayerBtn->SetAutoDelete(true);
	AttachChild(m_pKickPlayerBtn);

	m_pBanPlayerBtn = xr_new<CUI3tButton>();
	m_pBanPlayerBtn->SetAutoDelete(true);
	AttachChild(m_pBanPlayerBtn);

	m_pBanTimeTrack = xr_new<CUITrackBar>();
	m_pBanTimeTrack->SetAutoDelete(true);
	AttachChild(m_pBanTimeTrack);

	m_pBanTimeText = xr_new<CUITextWnd>();
	m_pBanTimeText->SetAutoDelete(true);
	AttachChild(m_pBanTimeText);
}

CUIMpPlayersAdm::~CUIMpPlayersAdm()
{
}

void CUIMpPlayersAdm::Init(CUIXml& xml_doc)
{
	CUIXmlInit::InitWindow(xml_doc, "admin_menu:players_adm", 0, this);
	CUIXmlInit::InitListBox(xml_doc, "admin_menu:players_adm:players_list", 0, m_pPlayersList);
	CUIXmlInit::Init3tButton(xml_doc, "admin_menu:players_adm:refresh_button", 0, m_pRefreshBtn);
	CUIXmlInit::Init3tButton(xml_doc, "admin_menu:players_adm:screen_all_button", 0, m_pScreenAllBtn);
	CUIXmlInit::Init3tButton(xml_doc, "admin_menu:players_adm:config_all_button", 0, m_pConfigAllBtn);
	CUIXmlInit::Init3tButton(xml_doc, "admin_menu:players_adm:max_ping_limit_button", 0, m_pPingLimitBtn);
	CUIXmlInit::InitTrackBar(xml_doc, "admin_menu:players_adm:max_ping_limit_track", 0, m_pPingLimitTrack);
	CUIXmlInit::InitTextWnd(xml_doc, "admin_menu:players_adm:max_ping_limit_text", 0, m_pPingLimitText);
	CUIXmlInit::Init3tButton(xml_doc, "admin_menu:players_adm:screen_player_button", 0, m_pScreenPlayerBtn);
	CUIXmlInit::Init3tButton(xml_doc, "admin_menu:players_adm:config_player_button", 0, m_pConfigPlayerBtn);
	CUIXmlInit::Init3tButton(xml_doc, "admin_menu:players_adm:kick_player_button", 0, m_pKickPlayerBtn);
	CUIXmlInit::Init3tButton(xml_doc, "admin_menu:players_adm:ban_player_button", 0, m_pBanPlayerBtn);
	CUIXmlInit::InitTrackBar(xml_doc, "admin_menu:players_adm:ban_time_track", 0, m_pBanTimeTrack);
	CUIXmlInit::InitTextWnd(xml_doc, "admin_menu:players_adm:ban_time_text", 0, m_pBanTimeText);
	RefreshPlayersList();
	m_pPingLimitTrack->SetCurrentOptValue();
	m_pBanTimeTrack->SetCurrentOptValue();
}

void CUIMpPlayersAdm::RefreshPlayersList()
{
	if (!g_pGameLevel || !Level().Server || !Level().Server->game) 
		return;

	m_pPlayersList->Clear();

	struct PlayersEnumerator
	{
		CUIListBox* list_box;
		PlayersEnumerator() {list_box = NULL;};
		void operator()(IClient* client)
		{
			xrClientData *l_pC	= (xrClientData*)client;
			if (!l_pC)
				return;
			if (!l_pC->ps)
				return;
			ip_address Address;
			DWORD dwPort = 0;
			Level().Server->GetClientAddress(client->ID, Address, &dwPort);
			string512 tmp_string;
			xr_sprintf(tmp_string, "%s, id:%u, ip:%s, ping:%u",
				l_pC->ps->getName(),
				client->ID.value(),
				Address.to_string().c_str(),
				l_pC->ps->ping);

			CUIListBoxItem* itm = list_box->AddTextItem(tmp_string);
			itm->SetTAG(client->ID.value());
		}
	};
	PlayersEnumerator tmp_functor;
	tmp_functor.list_box = m_pPlayersList;
	Level().Server->ForEachClientDo(tmp_functor);
}

void CUIMpPlayersAdm::SendMessage(CUIWindow* pWnd, s16 msg, void* pData)
{
	switch(msg)
	{
	case BUTTON_CLICKED:
		{
			if(pWnd==m_pRefreshBtn)
				RefreshPlayersList();
			else if(pWnd==m_pScreenAllBtn)
				Console->Execute("screenshot_all");
			else if(pWnd==m_pConfigAllBtn)
				Console->Execute("config_dump_all");
			else if(pWnd==m_pPingLimitBtn)
				SetMaxPingLimit();
			else if(pWnd==m_pScreenPlayerBtn)
				GetSelPlayerScreenshot();
			else if(pWnd==m_pConfigPlayerBtn)
				GetSelPlayerConfig();
			else if(pWnd==m_pKickPlayerBtn)
				KickSelPlayer();
			else if(pWnd==m_pBanPlayerBtn)
				BanSelPlayer();
			break;
		}
	};
}
void CUIMpPlayersAdm::SetMaxPingLimit()
{
	int ping_limit = m_pPingLimitTrack->GetIValue();
	string512 tmp_string;
	xr_sprintf(tmp_string, "sv_max_ping_limit %d", ping_limit*10);
	Console->Execute(tmp_string);
}
void CUIMpPlayersAdm::GetSelPlayerScreenshot()
{
	CUIListBoxItem* itm = m_pPlayersList->GetSelectedItem();
	if(!itm)
		return;

	u32 client_id = itm->GetTAG();
	string512 tmp_string;
	xr_sprintf(tmp_string, "make_screenshot %u", client_id);
	Console->Execute(tmp_string);
}
void CUIMpPlayersAdm::GetSelPlayerConfig()
{
	CUIListBoxItem* itm = m_pPlayersList->GetSelectedItem();
	if(!itm)
		return;

	u32 client_id = itm->GetTAG();
	string512 tmp_string;
	xr_sprintf(tmp_string, "make_config_dump %u", client_id);
	Console->Execute(tmp_string);
}
void CUIMpPlayersAdm::KickSelPlayer()
{
	CUIListBoxItem* itm = m_pPlayersList->GetSelectedItem();
	if(!itm)
		return;

	u32 client_id = itm->GetTAG();
	string512 tmp_string;
	xr_sprintf(tmp_string, "sv_kick_id %u", client_id);
	Console->Execute(tmp_string);
}
void CUIMpPlayersAdm::BanSelPlayer()
{
	CUIListBoxItem* itm = m_pPlayersList->GetSelectedItem();
	if(!itm)
		return;

	u32 client_id = itm->GetTAG();
	int ban_time = m_pBanTimeTrack->GetIValue();
	string512 tmp_string;
	xr_sprintf(tmp_string, "sv_banplayer %u %d", client_id, ban_time*60);
	Console->Execute(tmp_string);

}

//---------------------------------------------------------------------------------------------------
CUIMpServerAdm::CUIMpServerAdm()
{
}

CUIMpServerAdm::~CUIMpServerAdm()
{
}

void CUIMpServerAdm::Init(CUIXml& xml_doc)
{
	CUIXmlInit::InitWindow(xml_doc, "admin_menu:server_adm", 0, this);
}