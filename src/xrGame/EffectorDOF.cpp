#include "stdafx.h"
#include "EffectorDOF.h"
#include "CameraEffector.h"
#include "GamePersistent.h"

CEffectorDOF::CEffectorDOF(const Fvector4& dof, float life_time) : CEffectorCam(eCEDOF, life_time)
{
	GamePersistent().SetEffectorDOF(Fvector().set(dof.x, dof.y, dof.z));
	m_fPhase = Device.fTimeGlobal + dof.w;
}

bool CEffectorDOF::ProcessCam(SCamEffectorInfo& info)
{
	if (m_fPhase < Device.fTimeGlobal)
	{
		GamePersistent().RestoreEffectorDOF();
		fLifeTime = -1;
	}

	return true;
}
