#include "stdafx.h"
#include "CELFlyingAnimationCycles.h"

void ExtendedEntityesFlightAnimation::PlayLoop(IKinematicsAnimated& Skeleton, MotionID Motion)
{
	if (!Motion.valid())
	{
		return;
	}
	CMotionDef* Definition = Skeleton.LL_GetMotionDef(Motion);
	if (Definition)
	{
		Skeleton.LL_PlayCycle(Definition->bone_or_part, Motion, true, Definition->Accrue(), Definition->Falloff(), Definition->Speed(), false, nullptr, nullptr);
	}
}
