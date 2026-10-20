#include "stdafx.h"
#include "ai/ExtendedEntityesLogic/CELSharedPerceptionBudget.h"
#include "ai/ExtendedEntityesLogic/CELSharedSettings.h"
#include "ai/ExtendedEntityesLogic/EntryPoint.h"

// Восстанавливает общие кадровые квоты восприятия, пространственных запросов и обмена.
void CELSharedPerceptionBudget::BeginFrame(u32 FrameIdentifier)
{
	if (LastFrame != FrameIdentifier)
	{
		LastFrame = FrameIdentifier;
		RemainingOperations = CELSharedSettings::Work().PerceptionOperations;
		RemainingRays = CELSharedSettings::Work().PerceptionRays;
		RemainingSpatial = CELSharedSettings::Work().SpatialQueries;
		RemainingSessions = CELSharedSettings::Work().SharingSessions;
		RemainingTransfers = CELSharedSettings::Work().SharingTransfers;
	}
}

// Резервирует операции из оставшейся порционной квоты.
bool CELSharedPerceptionBudget::TryConsume(u32 OperationCount)
{
	if (OperationCount > RemainingOperations)
	{
		return false;
	}
	RemainingOperations -= OperationCount;
	return true;
}

// Резервирует луч из общего бюджета восприятия.
bool CELSharedPerceptionBudget::TryRay()
{
	if (!RemainingRays)
	{
		return false;
	}
	--RemainingRays;
	return true;
}

// Резервирует один сеанс обмена сведениями.
bool CELSharedPerceptionBudget::TrySharing()
{
	if (!RemainingSessions)
	{
		return false;
	}
	--RemainingSessions;
	return true;
}

// Резервирует доставку одного сообщения об угрозе.
bool CELSharedPerceptionBudget::TryTransfer()
{
	if (!RemainingTransfers)
	{
		return false;
	}
	--RemainingTransfers;
	return true;
}

// Сбрасывает накопленные данные и незавершённую работу подсистемы.
void CELSharedPerceptionBudget::Clear()
{
	SpatialWaiters.clear();
	LastFrame = u32(-1);
	RemainingOperations = RemainingRays = RemainingSpatial = RemainingSessions = RemainingTransfers = 0;
}

// Резервирует пространственный запрос в текущем кадре.
bool CELSharedPerceptionBudget::TrySpatial()
{
	if (!RemainingSpatial)
	{
		return false;
	}
	--RemainingSpatial;
	return true;
}

// Справедливо обслуживает ожидающих летунов: исчерпание квоты не отдаёт каждый следующий кадр тем же ID.
bool CELSharedPerceptionBudget::TrySpatial(u16 OwnerIdentifier)
{
	const u32 Generation = ExtendedEntityes::OwnerGeneration(OwnerIdentifier);
	const u32 Now = Device.dwTimeGlobal;
	if (!Generation)
	{
		return false;
	}
	bool Found = false;
	for (SSpatialWaiter& Waiter : SpatialWaiters)
	{
		if (Waiter.Identifier == OwnerIdentifier)
		{
			Waiter = {OwnerIdentifier, Generation, Now};
			Found = true;
			break;
		}
	}
	if (!Found && SpatialWaiters.size() < 1024)
	{
		SpatialWaiters.push_back({OwnerIdentifier, Generation, Now});
	}
	while (!SpatialWaiters.empty())
	{
		const SSpatialWaiter& First = SpatialWaiters.front();
		if (ExtendedEntityes::OwnerGeneration(First.Identifier) != First.Generation || Now - First.RequestedAt > 250)
		{
			SpatialWaiters.erase(SpatialWaiters.begin());
			continue;
		}
		if (First.Identifier != OwnerIdentifier || !TrySpatial())
		{
			return false;
		}
		SpatialWaiters.erase(SpatialWaiters.begin());
		return true;
	}
	return false;
}
