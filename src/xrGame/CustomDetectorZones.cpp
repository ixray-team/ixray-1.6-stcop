#include "StdAfx.h"
#include "CustomDetectorZones.h"

////////////////////////////////////////////////////////////////////////
// CAfList

BOOL CAfList::feel_touch_contact(CObject* O)
{
	TypesMapIt it = m_TypesMap.find(O->cNameSect());

	bool res = (it != m_TypesMap.end());
	if (res)
	{
		CArtefact* pAf = O&&O->cast_game_object() ? O->cast_game_object()->cast_artefact() : NULL;

		if (pAf && pAf->GetAfRank() > m_af_rank)
			res = false;
	}
	return						res;
}

////////////////////////////////////////////////////////////////////////
// CZoneList

BOOL CZoneList::feel_touch_contact(CObject* O)
{
	TypesMapIt it = m_TypesMap.find(O->cNameSect());
	bool res = (it != m_TypesMap.end());

	CCustomZone* pZone = O&&O->cast_game_object() ? O->cast_game_object()->cast_custom_zone() : NULL;
	if (pZone && !pZone->IsEnabled())
	{
		res = false;
	}
	return res;
}

CZoneList::~CZoneList()
{
	clear();
	destroy();
}