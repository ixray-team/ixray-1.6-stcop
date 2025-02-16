#include "stdafx.h"
#include "zonegalantine.h"

CZoneGalantine::CZoneGalantine(void) 
{
	m_fActorBlowoutRadiusPercent=0.5f;
}

CZoneGalantine::~CZoneGalantine(void) 
{
}

BOOL CZoneGalantine::net_Spawn(CSE_Abstract* DC)
{
	return inherited::net_Spawn(DC);
}