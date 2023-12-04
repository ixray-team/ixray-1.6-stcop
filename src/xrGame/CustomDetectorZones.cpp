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
		CArtefact* pAf = smart_cast<CArtefact*>(O);

		if (pAf->GetAfRank() > m_af_rank)
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

	CCustomZone* pZone = smart_cast<CCustomZone*>(O);
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