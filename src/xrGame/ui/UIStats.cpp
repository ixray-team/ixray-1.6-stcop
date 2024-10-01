#include "StdAfx.h"

#include "UIStats.h"
#include "../../xrUI/UIXmlInit.h"
#include "UIStatsPlayerList.h"
#include "../../xrUI/Widgets/UIStatic.h"
#include "../../xrUI/Widgets/UIFrameWindow.h"
#include "../level.h"
#include "game_base_space.h"


CUIStats::CUIStats()
{}

CUIStats::~CUIStats()
{}

CUIWindow* CUIStats::InitStats(CUIXml& xml_doc, LPCSTR path,  int team)
{
	string256						_path;
	CUIXmlInit::InitScrollView		(xml_doc, path, 0, this);
	this->SetFixedScrollBar			(false);
	CUIWindow* pWnd					= nullptr;
	CUIWindow* pTinfo				= nullptr;

    // players
	CUIStatsPlayerList* pPList		= new CUIStatsPlayerList();
	pPList->SetTeam					(team);
	pPList->Init					(xml_doc, xr_strconcat(_path, path, ":player_list"));
	pPList->SetMessageTarget		(this);
	pWnd							= pPList->GetHeader();
	pTinfo							= pPList->GetTeamHeader();
	AddWindow						(pWnd, true);
	AddWindow						(pPList, true);

	if (xml_doc.NavigateToNode(xr_strconcat(_path, path, ":spectator_list"),0))
	{
		// spectators
		pPList						= new CUIStatsPlayerList();
		pPList->SetTeam				(team);
		pPList->Init				(xml_doc, _path);
		pPList->SetMessageTarget	(this);
		pWnd						= pPList->GetHeader();
		AddWindow					(pWnd, true);
		AddWindow					(pPList, true);
	}

	return pTinfo;
}
