#include "StdAfx.h"
#include "GameSpy_Full.h"

#include "GameSpy_Available.h"
#include "GameSpy_HTTP.h"
#include "GameSpy_Browser.h"
#include "GameSpy_GP.h"
#include "GameSpy_SAKE.h"
#include "GameSpy_ATLAS.h"
#include "../MainMenu.h"
#include "../xrCore/object_broker.h"


CGameSpy_Full::CGameSpy_Full()	
{
	m_pGSA	= nullptr;
	m_pGS_HTTP = nullptr;
	m_pGS_SB = nullptr;
	m_pGS_GP = nullptr;
	m_pGS_SAKE = nullptr;
	m_pGS_ATLAS = nullptr;

	m_bServicesAlreadyChecked	= false;

	if (Engine.External.hGameSpy == 0)
		return;

	LoadGameSpy();
	//---------------------------------------
	m_pGSA = new CGameSpy_Available();
	//-----------------------------------------------------
	shared_str resultstr;
	m_bServicesAlreadyChecked = m_pGSA->CheckAvailableServices(resultstr);
	//-----------------------------------------------------
	if (Engine.External.hGameSpy != 0)
	{
		CoreInitialize();
		m_pGS_HTTP = new CGameSpy_HTTP();
		m_pGS_SB = new CGameSpy_Browser();
		m_pGS_GP = new CGameSpy_GP();
		m_pGS_SAKE = new CGameSpy_SAKE();
		m_pGS_ATLAS = new CGameSpy_ATLAS();
	}
}

CGameSpy_Full::~CGameSpy_Full()
{
	if (Engine.External.hGameSpy != 0)
	{
		delete_data(m_pGSA);
		delete_data(m_pGS_HTTP);
		delete_data(m_pGS_SB);
		delete_data(m_pGS_GP);
		delete_data(m_pGS_SAKE);
		delete_data(m_pGS_ATLAS);

		CoreShutdown();
	}
}

void CGameSpy_Full::LoadGameSpy()
{
	GAMESPY_LOAD_FN			(xrGS_GetGameVersion);
	GAMESPY_LOAD_FN			(xrGS_gsCoreInitialize);
	GAMESPY_LOAD_FN			(xrGS_gsCoreThink);
	GAMESPY_LOAD_FN			(xrGS_gsCoreShutdown);
}

void CGameSpy_Full::Update()
{
	if (Engine.External.hGameSpy == 0)
		return;

	if (!m_bServicesAlreadyChecked)
	{
		m_bServicesAlreadyChecked = true;
		MainMenu()->SetErrorDialog(CMainMenu::ErrGSServiceFailed);
	}
	m_pGS_HTTP->Think	();
	m_pGS_SB->Update	();
	m_pGS_GP->Think		();
	CoreThink			(15);
	m_pGS_ATLAS->Think	();
};

const char* CGameSpy_Full::GetGameVersion()
{
	return xrGS_GetGameVersion();
};