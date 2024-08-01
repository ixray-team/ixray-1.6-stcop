#pragma once
#include "../../pseudogigant/pseudogigant_step_effector.h"
#include "pseudogigant_jumper_step_effector.h"
#include "../../../../CameraEffector.h"

class CPseudogigantJumperStepEffector final : 
    public CPseudogigantStepEffector 
{
    using inherited = CPseudogigantStepEffector;

public:
    CPseudogigantJumperStepEffector(float time, float amp, float periods, float power);
    virtual ~CPseudogigantJumperStepEffector();

    virtual BOOL ProcessCam(SCamEffectorInfo& info) override;
};
