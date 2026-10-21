#pragma once
#include "Include/xrRender/KinematicsAnimated.h"

namespace ExtendedEntityesFlightAnimation
{
// Override one-shot asset flags only for the continuous flight instance.
void PlayLoop(IKinematicsAnimated& Skeleton, MotionID Motion);
} // namespace ExtendedEntityesFlightAnimation
