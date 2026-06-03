#include "stdafx.h"
#include "CustomDetectorZones.h"

////////////////////////////////////////////////////////////////////////
// CAfList

bool CAfList::feel_touch_contact(CObject* O)
{
	TypesMapIt it = m_TypesMap.find(O->cNameSect());

	bool res = (it != m_TypesMap.end());
	if (res)
	{
		CArtefact* pAf = O && O->cast_game_object() ? O->cast_game_object()->cast_artefact() : nullptr;

		if (pAf != nullptr && pAf->GetAfRank() > m_af_rank)
		{
			res = false;
		}
	}

	return res;
}

////////////////////////////////////////////////////////////////////////
// CZoneList

bool CZoneList::feel_touch_contact(CObject* O)
{
	TypesMapIt it = m_TypesMap.find(O->cNameSect());
	bool res = (it != m_TypesMap.end());

	CAnomalyZone* pZone = O && O->cast_game_object() ? O->cast_game_object()->cast_anomaly_zone() : nullptr;
	if (pZone != nullptr && !pZone->IsEnabled())
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