#include "stdafx.h"
#include <algorithm>
#include "ai/ExtendedEntityesLogic/Navigation/CELSharedNavigationBudget.h"
#include "ai/ExtendedEntityesLogic/CELSharedSettings.h"

// Восстанавливает кадровые квоты расчёта, захвата узлов и геометрических лучей.
void CELSharedNavigationBudget::BeginFrame(u32 FrameIdentifier)
{
	if (LastFrame != FrameIdentifier)
	{
		LastFrame = FrameIdentifier;
		RemainingOperations = CELSharedSettings::Work().NavigationSteps;
		RemainingCaptureNodes = CELSharedSettings::Work().NavigationCaptureNodes;
		RemainingGeometryRays = CELSharedSettings::Work().FlightGeometryRays;
		RemainingEmergencyRays = 96;
	}
}

// Резервирует операции из оставшейся порционной квоты.
bool CELSharedNavigationBudget::TryConsume(u32 OperationCount)
{
	if (OperationCount > RemainingOperations)
	{
		return false;
	}
	RemainingOperations -= OperationCount;
	return true;
}

// Резервирует квоту захвата одного узла штатного графа.
bool CELSharedNavigationBudget::TryCaptureNode()
{
	if (!RemainingCaptureNodes)
	{
		return false;
	}
	--RemainingCaptureNodes;
	return true;
}

// Атомарно резервирует необходимое число лучей проверки геометрии.
bool CELSharedNavigationBudget::TryGeometryRays(u32 RayCount)
{
	if (RayCount > RemainingGeometryRays)
	{
		return false;
	}
	RemainingGeometryRays -= RayCount;
	return true;
}

// One shared reserve for continuity, never a separate allowance per creature.
bool CELSharedNavigationBudget::TryEmergencyGeometryRays(u32 RayCount)
{
	const u32 Ordinary = std::min(RayCount, RemainingGeometryRays);
	const u32 Extra = RayCount - Ordinary;
	if (Extra > RemainingEmergencyRays)
	{
		return false;
	}
	RemainingGeometryRays -= Ordinary;
	RemainingEmergencyRays -= Extra;
	return true;
}
