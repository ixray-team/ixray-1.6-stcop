#pragma once
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedPerceptionProfiles.h"
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedMemory.h"

enum class EELSoundKind : u8
{
	Native,
	Jump,
	Melee,
	Explosion,
	Light
};
enum class EELWeaponKind : u8
{
	Automatic,
	Sniper,
	Shotgun,
	Pistol,
	Launcher
};
enum class EELMovementNoise : u8
{
	None,
	Walk,
	Run,
	Sprint,
	HighCrouch,
	LowCrouch
};

struct SELSoundEvent
{
	u16 SourceIdentifier = u16(-1);
	u32 SoundType = 0;
	u32 OccurredAt = 0;
	Fvector Position = {0.f, 0.f, 0.f};
	float Power = 0.f;
	EELSoundKind Kind = EELSoundKind::Native;
	EELWeaponKind WeaponKind = EELWeaponKind::Automatic;
	EELMovementNoise Movement = EELMovementNoise::None;
	bool IsSuppressed = false;
	bool IsActorSound = false;
	Fvector ActorPosition = {0.f, 0.f, 0.f};
};

struct SELNoiseAttention
{
	u16 SourceIdentifier = u16(-1);
	u32 SoundType = 0;
	EELSoundKind Kind = EELSoundKind::Native;
	u32 LastEvent = 0;
	u32 LastDecay = 0;
	Fvector Position = {0.f, 0.f, 0.f};
	EELNoiseType NoiseType = EELNoiseType::Other;
	bool IsActorSound = false;
	float Interest = 0.f;
	float Fear = 0.f;
};

class CELSharedHearing
{
public:
	static EELNoiseType Classify(const SELSoundEvent& Event);
	static float HearingRadius(const SELSoundEvent& Event, const SELPerceptionProfile& Profile);
	bool Hear(const SELSoundEvent& Event, const Fvector& ListenerPosition, const SELPerceptionProfile& Profile, u16 ListenerIdentifier);
	void PublishDebug(const Fvector& ListenerPosition, const SELPerceptionProfile& Profile, u16 ListenerIdentifier) const;
	void Clear();
	bool GetLookPoint(Fvector& Position) const;
	bool GetCounterOpportunity(u16 Source, u32& OccurredAt) const;
	void FillDebug(struct SELDebugRecord& Record) const;
	void QueueInterest(const Fvector& Position, u16 Source, EELNoiseType Type, u32 Until, u32 Importance, bool IsActorSound, u32 ObservedAt = 0);
	bool RememberInterest(const SELObservation& Observation, const SELPerceptionProfile& Profile);
	void ExportInterests(CELSharedMemory::Records& Records) const;
	void ArchiveAttention();
	void ResumeInterest(const Fvector& Position);
	void CompleteAttention();
	u32 PendingCount() const { return u32(Pending.size()); }
	void Investigate(const Fvector& Position, u16 Source, u32 Duration, u32 Importance);
	static u32 Priority(EELNoiseType Type, const SELPerceptionProfile& Profile);
	static u32 PanicDuration(EELNoiseType Type, const SELPerceptionProfile& Profile, float Scale);
	void RemoveObject(u16 SourceIdentifier);
	bool IsReactingTo(u16 SourceIdentifier) const;
	void SetSource(u16 SourceIdentifier) { AttentionSource = SourceIdentifier; }
	Fvector AttentionPosition = {0.f, 0.f, 0.f};
	u32 InvestigateUntil = 0;
	u32 PanicUntil = 0;
	bool DamagePanic = false;

private:
	struct SPendingInterest
	{
		Fvector Position;
		u16 Source;
		u32 Until;
		u32 Priority;
		EELNoiseType Type;
		bool IsActorSound;
		u32 ObservedAt;
	};
	xr_vector<SPendingInterest> Pending;
	xr_vector<SPendingInterest> Finished;
	xr_vector<SELNoiseAttention> Attention;
	Fvector LookPosition = {0.f, 0.f, 0.f};
	u32 OpportunityUntil = 0;
	u32 OpportunityAt = 0;
	u16 OpportunitySource = u16(-1);
	u32 LookUntil = 0;
	u32 LookEventAt = 0;
	u32 LookPriority = 0;
	u16 LookSource = u16(-1);
	u32 NextReaction = 0;
	u32 AttentionPriority = 0;
	u32 AttentionStartedAt = 0;
	u32 LastAttentionEventAt = 0;
	u16 AttentionSource = u16(-1);
	EELNoiseType AttentionNoiseType = EELNoiseType::Other;
	bool AttentionIsActorSound = false;
};
