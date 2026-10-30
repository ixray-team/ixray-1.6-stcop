#include "FFxExtendedEntityesLogic/stdafx.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Flying/Entityes/Aggressive/Behaviors/CELFlyingSearchBehavior.h"
#include "FFxExtendedEntityesLogic/Interfaces/IELAdapter.h"
#include "FFxExtendedEntityesLogic/EntryPoint.h"
#include "xrGame/ai/monsters/basemonster/base_monster.h"

CELFlyingSearchBehavior::CELFlyingSearchBehavior(IELAdapter& OwnerAdapter, const SELFlyingSettings& FlightSettings, CELFlyingMovement& FlightMovement, const SELPerceptionProfile& OwnerProfile)
	: CELFlyingBehaviorBase(OwnerAdapter, FlightSettings, FlightMovement), Profile(OwnerProfile) {}

void CELFlyingSearchBehavior::SetEvidence(const SELObservation& Evidence)
{
	if (Observation.Kind != Evidence.Kind || Observation.InterestType != Evidence.InterestType || Observation.TargetIdentifier != Evidence.TargetIdentifier || Evidence.ObservedAt > Observation.ObservedAt || Observation.Position.distance_to_sqr(Evidence.Position) > 1.f)
	{
		CancelRouteRequest();
		HasPreparedRoute = false;
		Phase = 0;
		Until = Device.dwTimeGlobal + Random.randI(Profile.SearchMin, Profile.SearchMax + 1);
	}
	SetObservation(&Evidence);
}

void CELFlyingSearchBehavior::Enter()
{
	CELFlyingBehaviorBase::Enter();
	Phase = 0;
	if (!Until)
	{
		Until = Device.dwTimeGlobal + Random.randI(Profile.SearchMin, Profile.SearchMax + 1);
	}
}

void CELFlyingSearchBehavior::Update(float DeltaTime)
{
	if (Expired())
	{
		CancelRouteRequest();
		Validation = {EELBehaviorState::Completed, "Воздушный поиск завершён"};
		return;
	}
	CELFlyingBehaviorBase::Update(DeltaTime);
}

void CELFlyingSearchBehavior::ChooseGoal()
{
	GoalPosition = Observation.Position;
	GoalDirection = Movement.Direction();
	if (Observation.Trail.size() >= 2)
	{
		GoalDirection.sub(Observation.Trail.back(), Observation.Trail.front()).normalize_safe();
	}
	if (Phase)
	{
		const float Heading = atan2f(GoalDirection.x, GoalDirection.z);
		const float Lane = (float(Adapter.Monster().ID() % 5) / 4.f - .5f);
		const float Angle = Heading + (Lane + float(s32(Phase % 5) - 2) * .2f) * deg2rad(Profile.SearchAngle);
		const float Radius = std::min(Profile.SearchRadius * (.4f + float(Phase / 5) * .3f), std::max(1.f, Profile.SharingRadius * Profile.CallSharingRatio * .8f));
		GoalPosition.x += sinf(Angle) * Radius;
		GoalPosition.z += cosf(Angle) * Radius;
	}
	GoalPosition.y += Settings.GroundClearance;
	++Phase;
}

bool CELFlyingSearchBehavior::Expired() const
{
	return s32(Device.dwTimeGlobal - Until) >= 0 || (Observation.Kind == EELObservationKind::Interest && s32(Device.dwTimeGlobal - Observation.ExpiresAt) >= 0);
}

void CELFlyingSearchBehavior::FrameUpdate(float DeltaTime)
{
	if (!Expired())
	{
		CELFlyingBehaviorBase::FrameUpdate(DeltaTime);
	}
}

void CELFlyingSearchBehavior::AfterMovement()
{
	if (!Expired())
	{
		CELFlyingBehaviorBase::AfterMovement();
	}
}

SELBehaviorValidation CELFlyingSearchBehavior::Validate() const
{
	return Expired() ? SELBehaviorValidation{EELBehaviorState::Completed, "Исчерпан личный срок воздушного поиска"} : CELFlyingBehaviorBase::Validate();
}
