#pragma once
#include "../../../CameraEffector.h"

class CPseudoGiantBaseStepEffector : public CEffectorCam 
{
protected:
	using inherited = CEffectorCam;

	float total;
	float max_amp;
	float period_number;
	float power;

public:
	CPseudoGiantBaseStepEffector(float time, float amp, float periods, float power);
	virtual ~CPseudoGiantBaseStepEffector() override;

	virtual BOOL	ProcessCam					(SCamEffectorInfo& info) override;
};
