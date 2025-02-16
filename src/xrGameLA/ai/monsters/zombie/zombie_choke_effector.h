#pragma once

#include "../../../../xrEngine/effectorPP.h"
#include "../../../CameraEffector.h"
#include "../../../../xrEngine/cameramanager.h"

//////////////////////////////////////////////////////////////////////////
// Choke PPE Effector by SkyLoader (based on vampire effector)
//////////////////////////////////////////////////////////////////////////
class CChokePPEffector : public CEffectorPP {
	typedef CEffectorPP inherited;	

	SPPInfo state;			//current state
	float	m_total;		// total PP time

public:
					CChokePPEffector		(const SPPInfo &ppi, float life_time);
	virtual	BOOL	Process					(SPPInfo& pp);
};