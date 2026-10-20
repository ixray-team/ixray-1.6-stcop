#pragma once
#include "../../../Interfaces/IELNavigation.h"

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
	static EELGraphCaptureState TakeState(const SELNavigationIdentity& Identity);
	static void Cancel(const SELNavigationIdentity& Identity);
	static void BeginFrame();
	static void Clear();
	static u32 OutstandingRequests();
};
