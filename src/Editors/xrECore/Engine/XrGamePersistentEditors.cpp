#include "stdafx.h"

#include "../xrEngine/IGame_Persistent.h"
//#include "..\XrAPI\xrGameManager.h"
#include "XrGamePersistentEditors.h"

XrGamePersistentEditors::XrGamePersistentEditors():IGame_Persistent ()
{
	g_dedicated_server = false;
	m_pMainMenu						= NULL;
	//pEnvironment = nullptr;
	/*switch (xrGameManager::GetGame())
	{
	case EGame::SHOC:
		pEnvironment = xr_new<CEnvironmentSOC>();
		break;
	default:
		pEnvironment = xr_new<CEnvironment>();
		break;

	}*/
}

XrGamePersistentEditors::~XrGamePersistentEditors	()
{
}

void XrGamePersistentEditors::OnAppActivate		()
{
}

void XrGamePersistentEditors::OnAppDeactivate		()
{
}

void XrGamePersistentEditors::OnAppStart	()
{
}

void XrGamePersistentEditors::OnAppEnd		()
{
	OnGameEnd						();

}


void XrGamePersistentEditors::PreStart		(LPCSTR op)
{
	string256						prev_type;
	params							new_game_params;
	xr_strcpy							(prev_type,m_game_params.m_game_type);
	new_game_params.parse_cmd_line	(op);

	// change game type
	if (0!=xr_strcmp(prev_type,new_game_params.m_game_type)){
		OnGameEnd					();
	}
}
void XrGamePersistentEditors::Start		(LPCSTR op)
{
	string256						prev_type;
	xr_strcpy							(prev_type,m_game_params.m_game_type);
	m_game_params.parse_cmd_line	(op);
	// change game type
	if ((0!=xr_strcmp(prev_type,m_game_params.m_game_type))) 
	{
		if (*m_game_params.m_game_type)
			OnGameStart					();
	}
	else UpdateGameType();

	VERIFY							(ps_destroy.empty());
}

void XrGamePersistentEditors::Disconnect	()
{
}

void XrGamePersistentEditors::OnGameStart()
{
}

void XrGamePersistentEditors::OnGameEnd	()
{
}

void XrGamePersistentEditors::OnFrame		()
{

}



void XrGamePersistentEditors::OnAssetsChanged()
{
}

void XrGamePersistentEditors::RegisterModel(IRenderVisual* V)
{
}

float XrGamePersistentEditors::MtlTransparent(u32 mtl_idx)
{
	return 0.0f;
}

void XrGamePersistentEditors::Statistics(CGameFont* F)
{
}
