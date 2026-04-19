#include "StdAfx.h"

#include "Entity.h"
#include "WeaponCustomPistol.h"

void CWeaponCustomPistol::switch2_Fire	()
{
	m_bFireSingleShot			= true;
	bWorking					= false;
	m_iShotNum					= 0;
	m_bStopedAfterQueueFired	= false;
}



void CWeaponCustomPistol::FireEnd() 
{
	if(fShotTimeCounter<=0) 
	{
		SetPending			(false);
		inherited::FireEnd	();
	}
}