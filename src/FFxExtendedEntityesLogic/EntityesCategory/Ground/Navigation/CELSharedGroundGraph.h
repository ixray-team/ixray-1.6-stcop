#pragma once
#include "FFxExtendedEntityesLogic/Interfaces/IELNavigation.h"

enum class EELGraphCaptureState : u8
{
	Capturing,
	Submitted,
	Failed
};

class CELSharedGroundGraph final
{
public:
	static bool Submit(SELNavigationRequest Request, u32 GoalVertex);
	static bool Retarget(const SELNavigationIdentity& Identity, const Fvector& GoalPosition, u32 GoalVertex);
	static EELGraphCaptureState TakeState(const SELNavigationIdentity& Identity);
	static void Cancel(const SELNavigationIdentity& Identity);
	static void BeginFrame();
	static void Clear();
	static u32 OutstandingRequests();
};
