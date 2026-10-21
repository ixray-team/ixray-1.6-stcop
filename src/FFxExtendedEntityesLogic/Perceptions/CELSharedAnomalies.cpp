#include "FFxExtendedEntityesLogic/stdafx.h"
#include "FFxExtendedEntityesLogic/Debug/Debug.h"
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedAnomalies.h"
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedPerceptionProfiles.h"
#include "FFxExtendedEntityesLogic/CELSharedPerceptionBudget.h"
#include "FFxExtendedEntityesLogic/EntryPoint.h"
#include "xrGame/ai/monsters/basemonster/base_monster.h"
#include "xrGame/AnomalyZone.h"

namespace ExtendedEntityesAnomalies
{
struct SLocalFields
{
	xr_vector<SELNavigationObstacle> Fields;
	u32 OwnerGeneration = 0;
	u32 NextScan = 0;
};
xr_map<u16, SLocalFields> Observations;

// Ищет сохранённую запись по владельцу или идентификатору цели.
const SLocalFields* Find(u16 OwnerIdentifier)
{
	xr_map<u16, SLocalFields>::const_iterator Found = Observations.find(OwnerIdentifier);
	return Found != Observations.end() && Found->second.OwnerGeneration == ExtendedEntityes::OwnerGeneration(OwnerIdentifier) ? &Found->second : nullptr;
}
} // namespace ExtendedEntityesAnomalies

// Обновляет личные наблюдения владельца с учётом доступного бюджета.
void CELSharedAnomalies::Observe(CBaseMonster& OwnerMonster, const SELPerceptionProfile& Profile)
{
	if (!Profile.CanAvoidAnomalies)
	{
		return;
	}
	ExtendedEntityesAnomalies::SLocalFields& Local = ExtendedEntityesAnomalies::Observations[OwnerMonster.ID()];
	if (s32(Device.dwTimeGlobal - Local.NextScan) < 0 || !ExtendedEntityes::PerceptionBudget().TrySpatial())
	{
		return;
	}
	Local.NextScan = Device.dwTimeGlobal + 1000 + OwnerMonster.ID() % 201;
	Local.OwnerGeneration = ExtendedEntityes::OwnerGeneration(OwnerMonster.ID());
	Local.Fields.clear();
	xr_vector<ISpatialShared> Candidates;
	g_SpatialSpace->q_sphere(Candidates, 0, ESPATIAL_TYPE::ANOMALY_ZONE, OwnerMonster.Position(), Profile.AnomalyScanRadius);
	xr_vector<ISpatialShared>::const_iterator CandidateIterator;
	CObject* CandidateObject;
	CGameObject* CandidateGameObject;
	CAnomalyZone* Zone;
	SELNavigationObstacle Field;
	SELDebugEvent Event;
	for (CandidateIterator = Candidates.begin(); CandidateIterator != Candidates.end() && Local.Fields.size() < 32; ++CandidateIterator)
	{
		CandidateObject = *CandidateIterator ? (*CandidateIterator)->dcast_CObject() : nullptr;
		CandidateGameObject = CandidateObject ? CandidateObject->cast_game_object() : nullptr;
		Zone = CandidateGameObject ? CandidateGameObject->cast_anomaly_zone() : nullptr;
		if (!Zone || Zone->getDestroy() || !Zone->IsEnabled())
		{
			continue;
		}
		Zone->Center(Field.Position);
		Field.Radius = Zone->Radius() + Profile.AnomalyMargin;
		Local.Fields.push_back(Field);
		if (CELSharedDebug::Instance().IsDrawEnabled())
		{
			Event = SELDebugEvent();
			Event.Kind = EELDebugEvent::Anomaly;
			Event.OwnerIdentifier = OwnerMonster.ID();
			Event.TargetIdentifier = Zone->ID();
			Event.Start = OwnerMonster.Position();
			Event.Finish = Field.Position;
			Event.Radius = Field.Radius;
			Event.Label = "Известная аномалия: обход";
			CELSharedDebug::Instance().PublishEvent(std::move(Event));
		}
	}
}

// Проверяет, находится ли точка внутри известной опасной области.
bool CELSharedAnomalies::Contains(u16 OwnerIdentifier, const Fvector& Position)
{
	const ExtendedEntityesAnomalies::SLocalFields* Local = ExtendedEntityesAnomalies::Find(OwnerIdentifier);
	if (!Local)
	{
		return false;
	}
	xr_vector<SELNavigationObstacle>::const_iterator FieldIterator;
	for (FieldIterator = Local->Fields.begin(); FieldIterator != Local->Fields.end(); ++FieldIterator)
	{
		if (FieldIterator->Position.distance_to_sqr(Position) < FieldIterator->Radius * FieldIterator->Radius)
		{
			return true;
		}
	}
	return false;
}

// Проверяет пересечение отрезка с известной опасной областью.
bool CELSharedAnomalies::Crosses(u16 OwnerIdentifier, const Fvector& StartPosition, const Fvector& EndPosition)
{
	const ExtendedEntityesAnomalies::SLocalFields* Local = ExtendedEntityesAnomalies::Find(OwnerIdentifier);
	if (!Local)
	{
		return false;
	}
	return Crosses(Local->Fields, StartPosition, EndPosition);
}

// Проверяет пересечение отрезка с известной опасной областью.
bool CELSharedAnomalies::Crosses(const xr_vector<SELNavigationObstacle>& Fields, const Fvector& StartPosition, const Fvector& EndPosition)
{
	Fvector Segment;
	Segment.sub(EndPosition, StartPosition);
	const float LengthSquared = Segment.square_magnitude();
	Fvector Offset;
	Fvector Nearest;
	float Fraction;
	xr_vector<SELNavigationObstacle>::const_iterator FieldIterator;
	for (FieldIterator = Fields.begin(); FieldIterator != Fields.end(); ++FieldIterator)
	{
		Offset.sub(FieldIterator->Position, StartPosition);
		Fraction = LengthSquared > EPS_L ? Offset.dotproduct(Segment) / LengthSquared : 0.f;
		clamp(Fraction, 0.f, 1.f);
		Nearest.mad(StartPosition, Segment, Fraction);
		if (Nearest.distance_to_sqr(FieldIterator->Position) >= FieldIterator->Radius * FieldIterator->Radius)
		{
			continue;
		}
		// An entity already inside may leave along a monotonically outward step.
		if (StartPosition.distance_to_sqr(FieldIterator->Position) < FieldIterator->Radius * FieldIterator->Radius &&
			EndPosition.distance_to_sqr(FieldIterator->Position) > StartPosition.distance_to_sqr(FieldIterator->Position) && Offset.dotproduct(Segment) <= 0.f)
		{
			continue;
		}
		return true;
	}
	return false;
}

// Копирует известные препятствия в независимый снимок для фонового расчёта.
void CELSharedAnomalies::Snapshot(u16 OwnerIdentifier, xr_vector<SELNavigationObstacle>& Result)
{
	const ExtendedEntityesAnomalies::SLocalFields* Local = ExtendedEntityesAnomalies::Find(OwnerIdentifier);
	if (Local)
	{
		Result = Local->Fields;
	}
	else
	{
		Result.clear();
	}
}

// Удаляет запись, относящуюся к указанному объекту.
void CELSharedAnomalies::Remove(u16 OwnerIdentifier)
{
	ExtendedEntityesAnomalies::Observations.erase(OwnerIdentifier);
}

// Сбрасывает накопленные данные и незавершённую работу подсистемы.
void CELSharedAnomalies::Clear()
{
	ExtendedEntityesAnomalies::Observations.clear();
}
