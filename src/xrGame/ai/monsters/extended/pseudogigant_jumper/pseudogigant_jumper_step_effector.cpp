#include "stdafx.h"
#include "pseudogigant_jumper_step_effector.h" 

CPseudogigantJumperStepEffector::CPseudogigantJumperStepEffector(float time, float amp, float periods, float power)
    : CPseudogigantStepEffector(time, amp, periods, power) 
{

}

CPseudogigantJumperStepEffector::~CPseudogigantJumperStepEffector() 
{
}

BOOL CPseudogigantJumperStepEffector::ProcessCam(SCamEffectorInfo& info) 
{
    return inherited::ProcessCam(info);
}
