#include "stdafx.h"
#include <algorithm>
#include "../Debug/Debug.h"
#include "ai/ExtendedEntityesLogic/Perceptions/CELSharedHearing.h"
#include "../xrSound/ai_sounds.h"

// Определяет тип звукового события по игровым флагам.
EELNoiseType CELSharedHearing::Classify(const SELSoundEvent& Event)
{
	if (Event.Kind == EELSoundKind::Light)
	{
		return EELNoiseType::Light;
	}
	if (Event.Kind == EELSoundKind::Explosion || (Event.SoundType & SOUND_TYPE_OBJECT_EXPLODING))
	{
		return EELNoiseType::Explosion;
	}
	if (Event.Kind == EELSoundKind::Jump)
	{
		return EELNoiseType::Jump;
	}
	if (Event.Kind == EELSoundKind::Melee)
	{
		return EELNoiseType::Melee;
	}
	if (Event.SoundType & SOUND_TYPE_SHOOTING)
	{
		return EELNoiseType::Shot;
	}
	if (Event.SoundType & SOUND_TYPE_STEP)
	{
		return Event.Movement != EELMovementNoise::None ? EELNoiseType::ActorSteps : EELNoiseType::MonsterSteps;
	}
	const u32 NativeTypes[] = {SOUND_TYPE_ATTACKING, SOUND_TYPE_INJURING, SOUND_TYPE_DYING, SOUND_TYPE_BULLET_HIT, SOUND_TYPE_OBJECT_BREAKING, SOUND_TYPE_OBJECT_COLLIDING, SOUND_TYPE_TALKING, SOUND_TYPE_EATING, SOUND_TYPE_IDLE, SOUND_TYPE_AMBIENT, SOUND_TYPE_PICKING_UP, SOUND_TYPE_DROPPING, SOUND_TYPE_HIDING, SOUND_TYPE_TAKING, SOUND_TYPE_USING, SOUND_TYPE_EMPTY_CLICKING, SOUND_TYPE_RECHARGING};
	static_assert(sizeof(NativeTypes) / sizeof(NativeTypes[0]) == u32(EELNoiseType::Other) - u32(EELNoiseType::Attack));
	u32 TypeIndex;
	for (TypeIndex = 0; TypeIndex < sizeof(NativeTypes) / sizeof(NativeTypes[0]); ++TypeIndex)
	{
		if (Event.SoundType & NativeTypes[TypeIndex])
		{
			return EELNoiseType(u32(EELNoiseType::Attack) + TypeIndex);
		}
	}
	return EELNoiseType::Other;
}

// Рассчитывает дальность слуха по типу звука и профилю получателя.
float CELSharedHearing::HearingRadius(const SELSoundEvent& Event, const SELPerceptionProfile& Profile)
{
	const EELNoiseType Type = Classify(Event);
	u32 RatioIndex = 3;
	switch (Type)
	{
		case EELNoiseType::Shot: RatioIndex = 0; break;
		case EELNoiseType::Explosion: RatioIndex = 1; break;
		case EELNoiseType::ActorSteps:
		case EELNoiseType::MonsterSteps:
		case EELNoiseType::Jump: RatioIndex = 2; break;
		case EELNoiseType::Injury: RatioIndex = 4; break;
		case EELNoiseType::Death: RatioIndex = 5; break;
		case EELNoiseType::Breaking: RatioIndex = 6; break;
		case EELNoiseType::Collision: RatioIndex = 7; break;
		case EELNoiseType::BulletHit: RatioIndex = 8; break;
		default: break;
	}
	const float HearingRatio = Profile.HearingRatios[RatioIndex];
	if (HearingRatio <= 0.f)
	{
		return 0.f;
	}
	const bool HasOwnRadius = Type == EELNoiseType::Jump || Type == EELNoiseType::Melee || Event.Movement != EELMovementNoise::None;
	float Radius = Type == EELNoiseType::Shot ? Profile.Shot.Radius : Type == EELNoiseType::Explosion ? Profile.Explosion.Radius
																									  : Profile.GeneralNoiseRadius;
	if (Type == EELNoiseType::Jump)
	{
		Radius = Profile.ActorJumpRadius;
	}
	else if (Type == EELNoiseType::Melee)
	{
		Radius = Profile.MeleeRadius * Profile.MeleeRatio;
	}
	else if (Event.Movement != EELMovementNoise::None)
	{
		Radius = Profile.ActorMovementRadii[u32(Event.Movement) - 1];
	}
	else if (Type == EELNoiseType::Shot)
	{
		Radius *= Profile.WeaponRatios[u32(Event.WeaponKind)];
	}
	if (Event.IsSuppressed)
	{
		Radius *= Profile.SuppressedRatio;
	}
	return Radius * (HasOwnRadius ? 1.f : HearingRatio) * Profile.HearingFactor;
}

u32 CELSharedHearing::Priority(EELNoiseType Type, const SELPerceptionProfile& Profile)
{
	if (Type == EELNoiseType::Explosion || Type == EELNoiseType::Injury || Type == EELNoiseType::Death)
	{
		return Profile.NoiseDangerPriority;
	}
	if (Type == EELNoiseType::Shot || Type == EELNoiseType::Attack || Type == EELNoiseType::Melee || Type == EELNoiseType::BulletHit)
	{
		return Profile.NoiseShotPriority;
	}
	return Profile.NoiseOtherPriority;
}

void CELSharedHearing::Investigate(const Fvector& Position, u16 Source, u32 Duration, u32 Importance)
{
	AttentionPosition = Position;
	AttentionSource = Source;
	AttentionStartedAt = Device.dwTimeGlobal;
	AttentionNoiseType = EELNoiseType::Death;
	AttentionPriority = Importance;
	PanicUntil = 0;
	InvestigateUntil = AttentionStartedAt + Duration;
}

void CELSharedHearing::FillDebug(SELDebugRecord& Record) const
{
	const u32 Now = Device.dwTimeGlobal;
	const u32 Until = s32(PanicUntil - Now) > 0 ? PanicUntil : InvestigateUntil;
	if (s32(Until - Now) <= 0)
	{
		return;
	}
	string256 Label;
	if (AttentionNoiseType == EELNoiseType::Light)
	{
		xr_sprintf(Label, "СВЕТ: интерес к точке | приоритет %u | осталось %.1f с", AttentionPriority, float(Until - Now) * .001f);
	}
	else
	{
		xr_sprintf(Label, "СЛУХ: %s от #%u | приоритет %u | прошло %.1f с / осталось %.1f с", CELSharedPerceptionProfiles::NoiseName(AttentionNoiseType), AttentionSource, AttentionPriority, float(Now - AttentionStartedAt) * .001f, float(Until - Now) * .001f);
	}
	Record.Senses.push_back(Label);
}

// Обрабатывает принятое звуковое событие по настроенным правилам реакции.
bool CELSharedHearing::Hear(const SELSoundEvent& Event, const Fvector& ListenerPosition, const SELPerceptionProfile& Profile, u16 ListenerIdentifier)
{
	if (Event.Power < Profile.NoiseMinPower || !Event.SoundType || Event.SoundType == u32(-1) || !_valid(Event.Position))
	{
		return false;
	}
	const EELNoiseType Type = Classify(Event);
	const u32 TypeMask = ELNoiseTypeMask(Type);
	const bool IsLight = Type == EELNoiseType::Light;
	const bool CanFrighten = !IsLight && (Profile.FearNoiseTypes & TypeMask) != 0;
	const bool CanAttract = IsLight || (!CanFrighten && (Profile.InterestNoiseTypes & TypeMask) != 0);
	if (!CanAttract && !CanFrighten)
	{
		return false;
	}
	const SELNoiseProfile* Noise = Type == EELNoiseType::Shot ? &Profile.Shot : Type == EELNoiseType::Explosion ? &Profile.Explosion
																												: nullptr;
	const float InterestStep = Noise ? Noise->InterestStep : Profile.NoiseInterestStep;
	const float InterestThreshold = Noise ? Noise->InterestThreshold : Profile.NoiseInterestThreshold;
	const float FearStep = Noise ? Noise->FearStep : Profile.NoiseFearStep;
	const float FearThreshold = Noise ? Noise->FearThreshold : Profile.NoiseFearThreshold;
	const float PanicChance = Noise ? Noise->PanicChance : Profile.NoisePanicChance;
	const float Radius = HearingRadius(Event, Profile);
	// Light has already passed visibility checks and is independent of hearing range.
	if (!IsLight && (Radius <= 0.f || ListenerPosition.distance_to_sqr(Event.Position) > Radius * Radius))
	{
		return false;
	}
	const s32 DebugAge = s32(Device.dwTimeGlobal - Event.OccurredAt);
	if (Event.IsActorSound && DebugAge >= 0 && DebugAge < 250 && CELSharedDebug::Instance().IsDrawEnabled())
	{
		SELDebugEvent DebugEvent;
		DebugEvent.Kind = EELDebugEvent::SoundReception;
		DebugEvent.OwnerIdentifier = ListenerIdentifier;
		DebugEvent.TargetIdentifier = Event.SourceIdentifier;
		DebugEvent.Start = ListenerPosition;
		DebugEvent.Finish = Event.ActorPosition;
		DebugEvent.Finish.y += .3f;
		DebugEvent.Label = CELSharedPerceptionProfiles::NoiseName(Type);
		CELSharedDebug::Instance().PublishEvent(std::move(DebugEvent), 250u - u32(DebugAge));
	}
	SELNoiseAttention* SourceAttention = nullptr;
	xr_vector<SELNoiseAttention>::iterator AttentionIterator;
	for (AttentionIterator = Attention.begin(); AttentionIterator != Attention.end(); ++AttentionIterator)
	{
		if (AttentionIterator->SourceIdentifier == Event.SourceIdentifier && AttentionIterator->SoundType == Event.SoundType && AttentionIterator->Kind == Event.Kind)
		{
			SourceAttention = &*AttentionIterator;
			break;
		}
	}
	if (!SourceAttention)
	{
		if (Attention.size() >= 8)
		{
			Attention.erase(Attention.begin());
		}
		Attention.emplace_back();
		SourceAttention = &Attention.back();
		SourceAttention->SourceIdentifier = Event.SourceIdentifier;
		SourceAttention->SoundType = Event.SoundType;
		SourceAttention->Kind = Event.Kind;
		SourceAttention->LastDecay = Event.OccurredAt;
	}
	if (SourceAttention->LastEvent && s32(Event.OccurredAt - SourceAttention->LastEvent) < s32(Profile.NoiseInterestInterval))
	{
		return true;
	}
	SourceAttention->Position = Event.Position;
	SourceAttention->NoiseType = Type;
	const float Decay = float(Event.OccurredAt - SourceAttention->LastDecay) * .001f * Profile.NoiseDecay;
	SourceAttention->Interest = std::max(0.f, SourceAttention->Interest - Decay);
	SourceAttention->Fear = std::max(0.f, SourceAttention->Fear - Decay);
	SourceAttention->LastEvent = SourceAttention->LastDecay = Event.OccurredAt;
	if (CanFrighten)
	{
		SourceAttention->Fear += FearStep;
	}
	if (CanAttract)
	{
		SourceAttention->Interest += InterestStep;
	}
	const u32 IncomingPriority = Priority(Type, Profile);
	const bool HasActiveAttention = s32(InvestigateUntil - Event.OccurredAt) > 0 || s32(PanicUntil - Event.OccurredAt) > 0;
	// Equal and weaker events accumulate separately, without moving the active search anchor.
	if (HasActiveAttention && IncomingPriority <= AttentionPriority)
	{
		return true;
	}
	if (s32(Event.OccurredAt - NextReaction) < 0 && (!HasActiveAttention || IncomingPriority <= AttentionPriority))
	{
		return true;
	}
	if (CanFrighten && (SourceAttention->Fear >= FearThreshold || Random.randF() < PanicChance))
	{
		AttentionPosition = Event.Position;
		AttentionSource = Event.SourceIdentifier;
		AttentionNoiseType = Type;
		AttentionPriority = IncomingPriority;
		AttentionStartedAt = Event.OccurredAt;
		PanicUntil = Event.OccurredAt + Profile.NoisePanicDuration;
		InvestigateUntil = 0;
		NextReaction = Event.OccurredAt + Profile.NoiseCooldown;
		SourceAttention->Fear = 0.f;
	}
	else if (CanAttract && s32(Event.OccurredAt - PanicUntil) >= 0 && (SourceAttention->Interest >= InterestThreshold || Random.randF() < Profile.NoiseImmediateChance))
	{
		AttentionPosition = Event.Position;
		AttentionSource = Event.SourceIdentifier;
		AttentionNoiseType = Type;
		AttentionPriority = IncomingPriority;
		AttentionStartedAt = Event.OccurredAt;
		InvestigateUntil = Event.OccurredAt + Profile.NoiseSearchTime;
		NextReaction = Event.OccurredAt + Profile.NoiseCooldown;
		SourceAttention->Interest = 0.f;
	}
	PublishDebug(ListenerPosition, Profile, ListenerIdentifier);
	return true;
}

// Продлевает отметки реально услышанных источников на время накопления интереса и текущей реакции.
void CELSharedHearing::PublishDebug(const Fvector& ListenerPosition, const SELPerceptionProfile& Profile, u16 ListenerIdentifier) const
{
	if (!CELSharedDebug::Instance().IsDrawEnabled())
	{
		return;
	}
	const u32 CurrentTime = Device.dwTimeGlobal;
	const s32 ReactionRemaining = std::max(s32(InvestigateUntil - CurrentTime), s32(PanicUntil - CurrentTime));
	SELDebugEvent DebugEvent;
	DebugEvent.OwnerIdentifier = ListenerIdentifier;
	DebugEvent.Start = ListenerPosition;
	DebugEvent.Radius = .25f;
	xr_vector<SELNoiseAttention>::const_iterator AttentionIterator;
	float Decay;
	float RemainingAttention;
	u32 Lifetime;
	string256 Label;
	for (AttentionIterator = Attention.begin(); AttentionIterator != Attention.end(); ++AttentionIterator)
	{
		if (ReactionRemaining > 0 && Priority(AttentionIterator->NoiseType, Profile) < AttentionPriority)
		{
			continue;
		}
		Decay = float(CurrentTime - AttentionIterator->LastDecay) * .001f * Profile.NoiseDecay;
		RemainingAttention = std::max(AttentionIterator->Interest, AttentionIterator->Fear) - Decay;
		if (RemainingAttention <= 0.f)
		{
			continue;
		}
		Lifetime = Profile.NoiseDecay > 0.f ? u32(std::min(1500.f, RemainingAttention / Profile.NoiseDecay * 1000.f)) : 1500u;
		DebugEvent.Kind = AttentionIterator->NoiseType == EELNoiseType::Light ? EELDebugEvent::Light : EELDebugEvent::Sound;
		DebugEvent.TargetIdentifier = AttentionIterator->SourceIdentifier;
		DebugEvent.Finish = AttentionIterator->Position;
		if (AttentionIterator->NoiseType == EELNoiseType::Light)
		{
			xr_sprintf(Label, "СВЕТ: интерес %.2f / %.2f | точка без привязки к врагу", RemainingAttention, Profile.NoiseInterestThreshold);
		}
		else
		{
			xr_sprintf(Label, "Услышан: %s (источник #%u)", CELSharedPerceptionProfiles::NoiseName(AttentionIterator->NoiseType), AttentionIterator->SourceIdentifier);
		}
		DebugEvent.Label = Label;
		CELSharedDebug::Instance().PublishEvent(DebugEvent, Lifetime);
	}
	if (ReactionRemaining <= 0)
	{
		return;
	}
	DebugEvent.TargetIdentifier = AttentionSource;
	DebugEvent.Finish = AttentionPosition;
	DebugEvent.Kind = AttentionNoiseType == EELNoiseType::Light ? EELDebugEvent::Light : EELDebugEvent::Sound;
	xr_sprintf(Label, AttentionNoiseType == EELNoiseType::Light ? "СВЕТ: замечен источник" : "Услышан: %s", CELSharedPerceptionProfiles::NoiseName(AttentionNoiseType));
	DebugEvent.Label = Label;
	CELSharedDebug::Instance().PublishEvent(DebugEvent, std::min(1500u, u32(ReactionRemaining)));
	DebugEvent.Kind = s32(PanicUntil - CurrentTime) > 0 ? EELDebugEvent::Fear : EELDebugEvent::Interest;
	DebugEvent.Label = AttentionNoiseType == EELNoiseType::Light ? "СВЕТ: интерес, исследование точки" : DebugEvent.Kind == EELDebugEvent::Fear ? "Испуг: уход от звука"
																																				: "Интерес: исследование звука";
	CELSharedDebug::Instance().PublishEvent(std::move(DebugEvent), std::min(1500u, u32(ReactionRemaining)));
}

// Сбрасывает накопленные данные и незавершённую работу подсистемы.
void CELSharedHearing::Clear()
{
	Attention.clear();
	AttentionSource = u16(-1);
	InvestigateUntil = PanicUntil = NextReaction = AttentionPriority = AttentionStartedAt = 0;
}

// Удаляет накопленный шум и активную реакцию исчезнувшего источника.
void CELSharedHearing::RemoveObject(u16 SourceIdentifier)
{
	Attention.erase(std::remove_if(Attention.begin(), Attention.end(), [SourceIdentifier](const SELNoiseAttention& Record)
								   { return Record.SourceIdentifier == SourceIdentifier; }),
					Attention.end());
	if (AttentionSource == SourceIdentifier)
	{
		AttentionSource = u16(-1);
		InvestigateUntil = PanicUntil = NextReaction = AttentionPriority = AttentionStartedAt = 0;
	}
}

// Определяет, удерживает ли источник текущий интерес или испуг.
bool CELSharedHearing::IsReactingTo(u16 SourceIdentifier) const
{
	return AttentionSource == SourceIdentifier && (s32(Device.dwTimeGlobal - InvestigateUntil) < 0 || s32(Device.dwTimeGlobal - PanicUntil) < 0);
}
