#include "stdafx.h"
#include "entity_alive.h"
#include "ai/ExtendedEntityesLogic/EntityesCategory/Flying/Navigation/CELSharedFlyingObstacles.h"
#include "ai/ExtendedEntityesLogic/EntityesCategory/Flying/Navigation/CELFlyingGeometry.h"
#include "ai/ExtendedEntityesLogic/EntryPoint.h"
#include "ai/ExtendedEntityesLogic/CELSharedSettings.h"
#include "ai/ExtendedEntityesLogic/Debug/Debug.h"
#include "ai/ExtendedEntityesLogic/Navigation/CELSharedNavigationScheduler.h"

namespace ExtendedEntityesFlightObstacles
{
struct SCorridor
{
	SELFlyingCorridor Request;
	SELNavigationObstacle Obstacle;
	bool IsBlocked = false;
	float ClearDistance = 0.f;
	Fvector CheckedStart = {0.f, 0.f, 0.f};
};
} // namespace ExtendedEntityesFlightObstacles
struct CELSharedFlyingObstacles::SState
{
	xr_vector<ExtendedEntityesFlightObstacles::SCorridor> Corridors;
	u32 NextCorridor = 0;
};
// Возвращает общее хранилище состояния очереди.
CELSharedFlyingObstacles::SState& CELSharedFlyingObstacles::State()
{
	static SState Runtime;
	return Runtime;
}

// Добавляет или обновляет запись в общей очереди подсистемы.
void CELSharedFlyingObstacles::Publish(const SELFlyingCorridor& Corridor)
{
	using namespace ExtendedEntityesFlightObstacles;
	xr_vector<SCorridor>::iterator CorridorIterator;
	for (CorridorIterator = State().Corridors.begin(); CorridorIterator != State().Corridors.end(); ++CorridorIterator)
	{
		if (CorridorIterator->Request.Identity == Corridor.Identity)
		{
			CorridorIterator->Request = Corridor;
			return;
		}
	}
	if (State().Corridors.size() >= CELSharedSettings::Work().NavigationTaskLimit)
	{
		return;
	}
	SCorridor Entry;
	Entry.Request = Corridor;
	State().Corridors.push_back(Entry);
}
// Возвращает обнаруженное препятствие и оставшийся запас свободного пути.
bool CELSharedFlyingObstacles::IsBlocked(const SELNavigationIdentity& Identity, SELNavigationObstacle& Obstacle, float& ClearDistance)
{
	using namespace ExtendedEntityesFlightObstacles;
	xr_vector<SCorridor>::const_iterator CorridorIterator;
	CEntityAlive* Owner;
	for (CorridorIterator = State().Corridors.begin(); CorridorIterator != State().Corridors.end(); ++CorridorIterator)
	{
		if (CorridorIterator->Request.Identity == Identity && CorridorIterator->IsBlocked)
		{
			Obstacle = CorridorIterator->Obstacle;
			Owner = ExtendedEntityes::FindEntity(Identity.OwnerIdentifier);
			ClearDistance = Owner ? CorridorIterator->ClearDistance - Owner->Position().distance_to(CorridorIterator->CheckedStart) : 0.f;
			return true;
		}
	}
	return false;
}
// Отменяет принадлежащую владельцу заявку и очищает промежуточный результат.
void CELSharedFlyingObstacles::Cancel(const SELNavigationIdentity& Identity)
{
	using namespace ExtendedEntityesFlightObstacles;
	xr_vector<SCorridor>::iterator CorridorIterator = State().Corridors.begin();
	while (CorridorIterator != State().Corridors.end())
	{
		if (CorridorIterator->Request.Identity == Identity)
		{
			CorridorIterator = State().Corridors.erase(CorridorIterator);
		}
		else
		{
			++CorridorIterator;
		}
	}
}
// Перепроверяет активные коридоры по круговой очереди в пределах общего бюджета.
void CELSharedFlyingObstacles::BeginFrame()
{
	CELSharedDebugScope ProfileScope(EELDebugGroup::Flying, EELDebugStage::Geometry);
	using namespace ExtendedEntityesFlightObstacles;
	u32 Remaining = u32(State().Corridors.size());
	u32 Checks = 0;
	SCorridor* Corridor;
	EELGeometryResult ResultState;
	CEntityAlive* Owner;
	while (Remaining > 0 && !State().Corridors.empty() && Checks < CELSharedSettings::Work().FlightDynamicChecks)
	{
		--Remaining;
		State().NextCorridor %= u32(State().Corridors.size());
		Corridor = &State().Corridors[State().NextCorridor++];
		Owner = ExtendedEntityes::FindEntity(Corridor->Request.Identity.OwnerIdentifier);
		if (!Owner || Corridor->Request.Identity.WorldGeneration != ExtendedEntityes::Navigation().GetGeneration() || Corridor->Request.Identity.OwnerGeneration != ExtendedEntityes::OwnerGeneration(Corridor->Request.Identity.OwnerIdentifier))
		{
			State().Corridors.erase(State().Corridors.begin() + (State().NextCorridor - 1));
			--State().NextCorridor;
			continue;
		}
		ResultState = CELFlyingGeometry::QueryDynamicCorridor(Corridor->Request, Corridor->Obstacle, Corridor->ClearDistance);
		if (ResultState == EELGeometryResult::Pending)
		{
			--State().NextCorridor;
			break;
		}
		Corridor->CheckedStart = Corridor->Request.Start;
		Corridor->IsBlocked = ResultState == EELGeometryResult::Blocked;
		++Checks;
	}
}
// Сбрасывает накопленные данные и незавершённую работу подсистемы.
void CELSharedFlyingObstacles::Clear()
{
	State().Corridors.clear();
	State().NextCorridor = 0;
}
// Возвращает число заявок, ожидающих обработки или получения результата.
u32 CELSharedFlyingObstacles::OutstandingRequests()
{
	return u32(State().Corridors.size());
}
