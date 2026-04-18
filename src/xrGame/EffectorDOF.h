#pragma once

#include "../xrEngine/Effector.h"

class CEffectorDOF final : public CEffectorCam
{
	float m_fPhase = 0.0f;

public:
	CEffectorDOF(const Fvector4& dof, float life_time = 100000.0f);
	virtual ~CEffectorDOF() = default;
	virtual bool ProcessCam(SCamEffectorInfo& info) override;
};
