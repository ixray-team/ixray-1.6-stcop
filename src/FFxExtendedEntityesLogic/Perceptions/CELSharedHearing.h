#pragma once
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedPerceptionProfiles.h"

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
	void FillDebug(struct SELDebugRecord& Record) const;
	void Investigate(const Fvector& Position, u16 Source, u32 Duration, u32 Importance);
	static u32 Priority(EELNoiseType Type, const SELPerceptionProfile& Profile);
	void RemoveObject(u16 SourceIdentifier);
	bool IsReactingTo(u16 SourceIdentifier) const;
	void SetSource(u16 SourceIdentifier) { AttentionSource = SourceIdentifier; }
	Fvector AttentionPosition = {0.f, 0.f, 0.f};
	u32 InvestigateUntil = 0;
	u32 PanicUntil = 0;

private:
	xr_vector<SELNoiseAttention> Attention;
	u32 NextReaction = 0;
	u32 AttentionPriority = 0;
	u32 AttentionStartedAt = 0;
	u32 LastAttentionEventAt = 0;
	u16 AttentionSource = u16(-1);
	EELNoiseType AttentionNoiseType = EELNoiseType::Other;
	bool AttentionIsActorSound = false;
};
