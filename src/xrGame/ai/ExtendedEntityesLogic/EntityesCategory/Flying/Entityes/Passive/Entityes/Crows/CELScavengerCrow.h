#pragma once
#include "../../CELFlyingNonAttackingBase.h"
#include "CELCrowSettings.h"
#include "CELCrowPersistence.h"
#include "Perceptions/CELCrowPerception.h"
#include "Behaviors/CELCrowCruiseBehavior.h"
#include "Behaviors/CELCrowLandingBehavior.h"
#include "Behaviors/CELCrowRestBehavior.h"
#include "Behaviors/CELCrowFeedingBehavior.h"
class CELCrowEntity;

enum class EELCrowPhase : u8
{
	Flight,
	Landing,
	Rest
};

class CELScavengerCrow final : public CELFlyingNonAttackingBase
{
public:
	CELScavengerCrow(xr_unique_ptr<IELAdapter> OwnerAdapter, const SELPerceptionProfile& OwnerProfile, CELCrowEntity& OwnerEntity);
	~CELScavengerCrow() override;
	void Attach() override;
	void Detach() override;
	void Observe() override;
	EELControl Update(float DeltaTime) override;
	void FrameUpdate(float DeltaTime) override;
	void Hear(const SELSoundEvent& Event) override;
	void RemoveObject(u16 ObjectIdentifier) override;
	void Serialize(ISaveObject& Object) override;

protected:
	Fvector DebugGoal() const override;

private:
	void BeginFlight(EELCrowFlightMode Mode, float Duration);
	void BeginLanding(EELCrowRest Rest, const Fvector& Contact, const Fvector& Facing);
	void BeginRest(EELCrowRest Rest, bool Resume);
	void UpdateFlight(float DeltaTime);
	void UpdateLanding(float DeltaTime);
	void UpdateRest(float DeltaTime);
	bool TryFood();
	bool TryPerch(EELCrowRest Rest);
	u16 BirdIdentifier = u16(-1);
	bool IsRegistered = false;
	CELCrowEntity& Bird;
	const SELCrowSettings& Settings;
	SELCrowState State;
	CELCrowPersistence Persistence;
	CELCrowPerception Perception;
	CELCrowCruiseBehavior CrowCruiseBehavior;
	CELCrowLandingBehavior LandingBehavior;
	CELCrowRestBehavior RestBehavior;
	CELCrowFeedingBehavior FeedingBehavior;
	EELCrowPhase Phase = EELCrowPhase::Flight;
	EELCrowFlightMode FlightMode = EELCrowFlightMode::Roam;
	EELCrowRest DesiredRest = EELCrowRest::None;
	Fvector LandingContact = {0.f, 0.f, 0.f};
	Fvector FailedPerch = {0.f, 0.f, 0.f};
	u32 FailedPerchUntil = 0;
	u32 NextDecision = 0;
	u32 PerchAttempt = 0;
	u16 CircledFood = u16(-1);
	bool HasStarted = false;
	bool IsFeeding = false;
};
