#pragma once
#include "FFxExtendedEntityesLogic/Interfaces/IELLogic.h"
#include "FFxExtendedEntityesLogic/Interfaces/IELAdapter.h"
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedMemory.h"
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedHearing.h"
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedEnemySharing.h"

struct SELDebugRecord;

class CELComponentBase : public IELLogic
{
public:
	CELComponentBase(xr_unique_ptr<IELAdapter> OwnerAdapter, const SELPerceptionProfile& OwnerProfile);
	~CELComponentBase() override = default;
	void Attach() override;
	void Detach() override;
	bool IsAttached() const override { return HasAttached; }
	void Observe() override;
	void RefreshDecision() override;
	void Serialize(ISaveObject& Object) override {}
	u16 GetTargetIdentifier() const override { return TargetIdentifier; }
	void FrameUpdate(float DeltaTime) override;
	void TraceCombat(EELControl Control) override;
	void NoticeActorLight(const Fvector& Point) override;
	void Hear(const SELSoundEvent& SoundEvent) override;
	void ForgetTarget(u16 ObjectIdentifier) override;
	void RemoveObject(u16 ObjectIdentifier) override;
	bool CanEngage(const SELObservation& Observation) override;
	void RecordCombatHit(CEntityAlive& Opponent, float HealthFraction, bool Received) override;
	bool Accept(const SELObservation& Observation, CBaseMonster& SourceMonster) override;

protected:
	void FillDebugRecord(SELDebugRecord& Record) const;
	virtual void FillCombatDebug(SELDebugRecord&) const {}
	void UpdateFieldExposure();
	void UpdateCharacter();
	bool RegroupPoint(Fvector& Point, bool& Wait);
	xr_map<u16, u32> RallyMembers;
	u32 RallyUntil = 0;
	u32 NextRallyCapture = 0;
	bool HasEnemyPressure();
	bool ShouldFleeCharacter(const SELObservation* Observation);
	bool ShouldFleePredator(const SELObservation* Observation);
	virtual const SELObservation* SelectTarget();
	void RememberEntity(CEntityAlive& TargetEntity, EELObservationSource Source, u32 ObservedAt);
	void PublishNativeMemory();
	void ObserveCloseContact();
	virtual void OnVisualContact(CEntityAlive& Target, u32 ObservedAt) {}
	virtual void ObserveCloseScent() {}
	void ObservePeriphery();
	void UpdateAuditoryAttention();
	virtual void ObserveVision(CBaseMonster& OwnerMonster, u32 CurrentTime);
	void ObserveDamage(CBaseMonster& OwnerMonster, u32 CurrentTime);
	bool HasVisualAcquired(CEntityAlive& Candidate, u32 CurrentTime);

	xr_unique_ptr<IELAdapter> Adapter;
	const SELPerceptionProfile& Profile;
	CELSharedMemory Memory;
	CELSharedHearing Hearing;
	CELSharedEnemySharing Sharing;
	float FieldHeadache = 0.f;
	bool IsFieldRecovering = false;
	u32 LastFieldAt = 0;
	Fvector FieldEscapeSource = {0.f, 0.f, 0.f};
	struct SEncounter
	{
		u32 Generation = 0;
		u32 EnemyTier = 2;
		u32 UpdatedAt = 0;
		u32 LastContactAt = 0;
		u32 ProvokedUntil = 0;
		u32 AssistanceUntil = 0;
		u32 DecisionUntil = 0;
		u32 NextDecisionAt = 0;
		float Initiative = 0.f;
		float ResolveBias = 0.f;
		float Experience = 0.f;
		float EnemyHealth = 1.f;
		float EnemyScale = 1.f;
		bool IsActor = false;
		bool Initialized = false;
		bool Engaged = false;
		bool Fleeing = false;
		bool Withdrawn = false;
	};
	SEncounter& Encounter(CEntityAlive& Target);
	float CharacterConfidence(const SEncounter& State) const;
	float CharacterResolve(const SEncounter& State) const;
	xr_map<u16, SEncounter> Encounters;
	float CharacterShock = 0.f;
	float PackCourage = 0.f;
	u32 CharacterUpdatedAt = 0;
	u16 FearTarget = u16(-1);
	u32 FearDecisionUntil = 0;
	bool IsFleeingPredator = false;
	float Courage = 0.f;
	u32 LastCourageAt = 0;
	u16 TargetIdentifier = u16(-1);
	u16 SharingSightSender = u16(-1);
	u32 SharingSightGeneration = 0;
	u32 SharingSightAt = 0;
	bool SharingSightVisible = false;
	Fvector LastFearPosition = {0.f, 0.f, 0.f};
	u32 LastFearAt = 0;
	u32 ReceiveAfter = 0;
	u32 NextCloseProbe = 0;
	u32 NextPeripheralProbe = 0;
	u32 NextSoundLook = 0;
	u32 SoundLookAppliedUntil = 0;
	u32 PeripheralCursor = 0;
	u32 LastReceivedAt = 0;
	u16 LastSender = u16(-1);
	bool HasAttached = false;
	u16 ProvokedTarget = u16(-1);
	u32 ProvokedUntil = 0;
	u32 LastActorVisualAt = 0;
	bool HasActorVisualSample = false;

private:
	u32 NextCandidateTrace = 0;
	u32 NextCombatSample = 0;
	u32 LastCombatLog = 0;
	xr_string LastCombatState;
	mutable u32 NextAnimationSample = 0;
	mutable xr_vector<shared_str> AnimationLabels;

	struct SVisualAcquisition
	{
		u32 SampleTime = 0;
		u32 VisibleTime = 0;
	};
	xr_map<u16, SVisualAcquisition> VisualAcquisition;
};
