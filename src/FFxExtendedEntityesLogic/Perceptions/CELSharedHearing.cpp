#include "FFxExtendedEntityesLogic/stdafx.h"
#include <algorithm>
#include "FFxExtendedEntityesLogic/Debug/Debug.h"
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedHearing.h"
#include "xrSound/ai_sounds.h"
#include "FFxExtendedEntityesLogic/EntryPoint.h"
#include "xrGame/entity_alive.h"
#include <cmath>

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

// Derive bounded fear duration from the sound category and current equivalent body mass.
u32 CELSharedHearing::PanicDuration(EELNoiseType Type, const SELPerceptionProfile& Profile, float Scale)
{
	const float Minimum = Type == EELNoiseType::Shot ? 10000.f : Type == EELNoiseType::Explosion ? 20000.f : 15000.f;
	const float Maximum = Type == EELNoiseType::Shot ? 30000.f : Type == EELNoiseType::Explosion ? 60000.f : 40000.f;
	const u32 Base = Type == EELNoiseType::Shot ? Profile.ShotPanicDuration : Type == EELNoiseType::Explosion ? Profile.ExplosionPanicDuration : Profile.NoisePanicDuration;
	// Scale changes equivalent body mass cubically; limits remain independent of size.
	const float Fearfulness = clampr((float(Base) - Minimum) / (Maximum - Minimum) - 3.f * logf(std::max(.01f, Scale)) / logf(40.f), 0.f, 1.f);
	return u32(Minimum + (Maximum - Minimum) * Fearfulness);
}

void CELSharedHearing::QueueInterest(const Fvector& Position, u16 Source, EELNoiseType Type, u32 Until, u32 Importance, bool IsActorSound, u32 ObservedAt)
{
	if (s32(Until - Device.dwTimeGlobal) <= 0 || !_valid(Position))
	{
		return;
	}
	Finished.erase(std::remove_if(Finished.begin(), Finished.end(), [](const SPendingInterest& Task) { return s32(Device.dwTimeGlobal - Task.Until) >= 0; }), Finished.end());
	for (const auto& Task : Finished)
	{
		if (Task.Source == Source && Task.Type == Type && Task.Position.distance_to_sqr(Position) < 1.f && s32(Task.Until - Until) >= 0)
		{
			return;
		}
	}
	for (auto& Task : Pending)
	{
		if (Task.Source == Source && Task.Type == Type && Task.Position.distance_to_sqr(Position) < 1.f)
		{
			// A shared replay does not refresh the original deadline.
			return;
		}
	}
	if (Pending.size() >= 8)
	{
		Pending.erase(std::min_element(Pending.begin(), Pending.end(), [](const SPendingInterest& A, const SPendingInterest& B) { return A.Until < B.Until; }));
	}
	Pending.push_back({Position, Source, Until, Importance, Type, IsActorSound, ObservedAt ? ObservedAt : Device.dwTimeGlobal});
}

void CELSharedHearing::ArchiveAttention()
{
	QueueInterest(AttentionPosition, AttentionSource, AttentionNoiseType, InvestigateUntil, AttentionPriority, AttentionIsActorSound, AttentionStartedAt);
}

bool CELSharedHearing::RememberInterest(const SELObservation& Observation, const SELPerceptionProfile& Profile)
{
	if (Observation.Kind != EELObservationKind::Interest || Observation.InterestType >= u32(EELNoiseType::Count) || s32(Observation.ExpiresAt - Device.dwTimeGlobal) <= 0)
	{
		return false;
	}
	const EELNoiseType Type = EELNoiseType(Observation.InterestType);
	if (Type != EELNoiseType::Light && !(Profile.InterestNoiseTypes & ELNoiseTypeMask(Type)))
	{
		return false;
	}
	if (s32(InvestigateUntil - Device.dwTimeGlobal) > 0 && AttentionSource == Observation.TargetIdentifier && AttentionNoiseType == Type && AttentionPosition.distance_to_sqr(Observation.Position) < 1.f)
	{
		return false;
	}
	QueueInterest(Observation.Position, Observation.TargetIdentifier, Type, Observation.ExpiresAt, Priority(Type, Profile), false, Observation.ObservedAt);
	return true;
}

void CELSharedHearing::ExportInterests(CELSharedMemory::Records& Records) const
{
	const auto Append = [&Records](const Fvector& Position, u16 Source, EELNoiseType Type, u32 Until, u32 ObservedAt)
	{
		if (s32(Until - Device.dwTimeGlobal) <= 0)
		{
			return;
		}
		SELObservation Observation;
		Observation.Kind = EELObservationKind::Interest;
		Observation.Position = Position;
		Observation.TargetIdentifier = Source;
		Observation.InterestType = u8(Type);
		Observation.ExpiresAt = Until;
		Observation.ObservedAt = ObservedAt;
		Records.push_back(Observation);
	};
	Append(AttentionPosition, AttentionSource, AttentionNoiseType, InvestigateUntil, AttentionStartedAt);
	for (const auto& Task : Pending)
	{
		Append(Task.Position, Task.Source, Task.Type, Task.Until, Task.ObservedAt);
	}
}

void CELSharedHearing::ResumeInterest(const Fvector& Position)
{
	Pending.erase(std::remove_if(Pending.begin(), Pending.end(), [](const SPendingInterest& Task) { return s32(Device.dwTimeGlobal - Task.Until) >= 0; }), Pending.end());
	if (s32(PanicUntil - Device.dwTimeGlobal) > 0 || s32(InvestigateUntil - Device.dwTimeGlobal) > 0 || Pending.empty())
	{
		return;
	}
	const auto Best = std::max_element(Pending.begin(), Pending.end(), [&Position](const SPendingInterest& A, const SPendingInterest& B)
	{
		return float(A.Priority) / (1.f + Position.distance_to(A.Position)) < float(B.Priority) / (1.f + Position.distance_to(B.Position));
	});
	AttentionPosition = Best->Position;
	AttentionSource = Best->Source;
	AttentionPriority = Best->Priority;
	AttentionNoiseType = Best->Type;
	AttentionIsActorSound = Best->IsActorSound;
	AttentionStartedAt = LastAttentionEventAt = Best->ObservedAt;
	InvestigateUntil = Best->Until;
	Pending.erase(Best);
}

void CELSharedHearing::CompleteAttention()
{
	if (s32(InvestigateUntil - Device.dwTimeGlobal) > 0)
	{
		if (Finished.size() >= 8)
		{
			Finished.erase(Finished.begin());
		}
		Finished.push_back({AttentionPosition, AttentionSource, InvestigateUntil, AttentionPriority, AttentionNoiseType, AttentionIsActorSound, AttentionStartedAt});
	}
	InvestigateUntil = PanicUntil = Device.dwTimeGlobal;
	DamagePanic = false;
}

void CELSharedHearing::Investigate(const Fvector& Position, u16 Source, u32 Duration, u32 Importance)
{
	ArchiveAttention();
	AttentionPosition = Position;
	AttentionSource = Source;
	AttentionStartedAt = Device.dwTimeGlobal;
	LastAttentionEventAt = AttentionStartedAt;
	AttentionNoiseType = EELNoiseType::Death;
	AttentionIsActorSound = false;
	AttentionPriority = Importance;
	PanicUntil = 0;
	DamagePanic = false;
	InvestigateUntil = AttentionStartedAt + Duration;
}

bool CELSharedHearing::GetCounterOpportunity(u16 Source, u32& OccurredAt) const
{
	if (Source != OpportunitySource || s32(OpportunityUntil - Device.dwTimeGlobal) <= 0)
	{
		return false;
	}
	OccurredAt = OpportunityAt;
	return true;
}

bool CELSharedHearing::GetLookPoint(Fvector& Position) const
{
	if (s32(LookUntil - Device.dwTimeGlobal) <= 0)
	{
		return false;
	}
	Position = LookPosition;
	return true;
}

void CELSharedHearing::FillDebug(SELDebugRecord& Record) const
{
	const u32 Now = Device.dwTimeGlobal;
	if (s32(LookUntil - Now) > 0)
	{
		Record.Senses.push_back("СЛУХ: свежий источник для осмотра");
	}
	if (!Pending.empty())
	{
		string64 PendingLabel;
		xr_sprintf(PendingLabel, "ПАМЯТЬ: незавершённых интересов %u", u32(Pending.size()));
		Record.Senses.push_back(PendingLabel);
	}
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
	const bool CounterEvent = Profile.CanCounterAttack &&
		((Type == EELNoiseType::Reload && Profile.CounterOnReload && Profile.CanHearWorld) ||
		 (Type == EELNoiseType::EmptyClick && Profile.CounterOnEmptyClick && Profile.CanHearWorld) ||
		 (Type == EELNoiseType::BulletHit && Profile.CounterOnNearMiss && Profile.CanHearCombat && ListenerPosition.distance_to_sqr(Event.Position) <= _sqr(Profile.CounterNearMissRadius)));
	if (!CanAttract && !CanFrighten && !CounterEvent)
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
	if (CounterEvent && Event.SourceIdentifier != u16(-1) && s32(Device.dwTimeGlobal - Event.OccurredAt) >= 0 &&
		Device.dwTimeGlobal - Event.OccurredAt < Profile.CounterOpportunityTime && s32(Event.OccurredAt - OpportunityAt) >= 0)
	{
		OpportunitySource = Event.SourceIdentifier;
		OpportunityAt = Event.OccurredAt;
		OpportunityUntil = Event.OccurredAt + Profile.CounterOpportunityTime;
	}
	if (!CanAttract && !CanFrighten)
	{
		return true;
	}
	// An orienting reflex must not wait for the threshold that starts a full investigation or panic.
	if (!IsLight)
	{
		const u32 ReflexPriority = Type == EELNoiseType::Shot || Type == EELNoiseType::Explosion ? 3u : Type == EELNoiseType::Attack || Type == EELNoiseType::BulletHit || Type == EELNoiseType::Melee ? 2u : 1u;
		const u32 Duration = ReflexPriority == 3u ? Profile.SoundDangerLookTime : Profile.SoundLookTime;
		const s32 Age = s32(Device.dwTimeGlobal - Event.OccurredAt);
		if (Age >= 0 && u32(Age) < Duration && s32(Event.OccurredAt - LookEventAt) >= 0 &&
			(s32(LookUntil - Device.dwTimeGlobal) <= 0 || ReflexPriority >= LookPriority))
		{
			LookPosition = Event.Position;
			LookUntil = Event.OccurredAt + Duration;
			LookEventAt = Event.OccurredAt;
			LookPriority = ReflexPriority;
			LookSource = Event.SourceIdentifier;
		}
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
	// Repeated evidence from the investigated object refreshes only its last heard point.
	// This bypasses accumulation throttling, without reading the live object's position.
	if (CanAttract && Event.SourceIdentifier != u16(-1) && Event.SourceIdentifier == AttentionSource &&
		s32(InvestigateUntil - Device.dwTimeGlobal) > 0 && s32(PanicUntil - Device.dwTimeGlobal) <= 0 &&
		s32(Event.OccurredAt - LastAttentionEventAt) > 0)
	{
		AttentionPosition = Event.Position;
		AttentionNoiseType = Type;
		AttentionIsActorSound = Event.IsActorSound;
		AttentionPriority = std::max(AttentionPriority, Priority(Type, Profile));
		LastAttentionEventAt = Event.OccurredAt;
		InvestigateUntil = Event.OccurredAt + Profile.NoiseSearchTime;
	}
	SourceAttention->Position = Event.Position;
	SourceAttention->NoiseType = Type;
	SourceAttention->IsActorSound = Event.IsActorSound;
	if (SourceAttention->LastEvent && s32(Event.OccurredAt - SourceAttention->LastEvent) < s32(Profile.NoiseInterestInterval))
	{
		PublishDebug(ListenerPosition, Profile, ListenerIdentifier);
		return true;
	}
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
	// Other equal and weaker sources accumulate without stealing the current investigation.
	if (HasActiveAttention && IncomingPriority <= AttentionPriority)
	{
		if (CanAttract && SourceAttention->Interest >= InterestThreshold)
		{
			QueueInterest(Event.Position, Event.SourceIdentifier, Type, Event.OccurredAt + Profile.NoiseSearchTime, IncomingPriority, Event.IsActorSound, Event.OccurredAt);
		}
		PublishDebug(ListenerPosition, Profile, ListenerIdentifier);
		return true;
	}
	if (s32(Event.OccurredAt - NextReaction) < 0 && (!HasActiveAttention || IncomingPriority <= AttentionPriority))
	{
		PublishDebug(ListenerPosition, Profile, ListenerIdentifier);
		return true;
	}
	if (CanFrighten && (SourceAttention->Fear >= FearThreshold || Random.randF() < PanicChance))
	{
		ArchiveAttention();
		AttentionPosition = Event.Position;
		AttentionSource = Event.SourceIdentifier;
		AttentionNoiseType = Type;
		AttentionIsActorSound = Event.IsActorSound;
		AttentionPriority = IncomingPriority;
		AttentionStartedAt = Event.OccurredAt;
		LastAttentionEventAt = Event.OccurredAt;
		CEntityAlive* Listener = ExtendedEntityes::FindEntity(ListenerIdentifier);
		PanicUntil = Event.OccurredAt + PanicDuration(Type, Profile, Listener ? Listener->GetVisualScale() : 1.f);
		DamagePanic = false;
		InvestigateUntil = 0;
		NextReaction = Event.OccurredAt + Profile.NoiseCooldown;
		SourceAttention->Fear = 0.f;
	}
	else if (CanAttract && s32(Event.OccurredAt - PanicUntil) >= 0 && (SourceAttention->Interest >= InterestThreshold || Random.randF() < Profile.NoiseImmediateChance))
	{
		ArchiveAttention();
		AttentionPosition = Event.Position;
		AttentionSource = Event.SourceIdentifier;
		AttentionNoiseType = Type;
		AttentionIsActorSound = Event.IsActorSound;
		AttentionPriority = IncomingPriority;
		AttentionStartedAt = Event.OccurredAt;
		LastAttentionEventAt = Event.OccurredAt;
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
		Decay = float(CurrentTime - AttentionIterator->LastDecay) * .001f * Profile.NoiseDecay;
		RemainingAttention = std::max(AttentionIterator->Interest, AttentionIterator->Fear) - Decay;
		if (RemainingAttention <= 0.f)
		{
			continue;
		}
		Lifetime = Profile.NoiseDecay > 0.f ? u32(std::min(1500.f, RemainingAttention / Profile.NoiseDecay * 1000.f)) : 1500u;
		DebugEvent.Kind = AttentionIterator->NoiseType == EELNoiseType::Light ? EELDebugEvent::Light : EELDebugEvent::Sound;
		DebugEvent.TargetIdentifier = AttentionIterator->SourceIdentifier;
		DebugEvent.StimulusType = u32(AttentionIterator->NoiseType);
		DebugEvent.Finish = AttentionIterator->Position;
		if (AttentionIterator->NoiseType == EELNoiseType::Light)
		{
			xr_sprintf(Label, "СВЕТ: интерес %.2f / %.2f | точка без привязки к врагу", RemainingAttention, Profile.NoiseInterestThreshold);
		}
		else
		{
			xr_sprintf(Label, "%s%s | услышан, интерес %.2f", CELSharedPerceptionProfiles::NoiseName(AttentionIterator->NoiseType), AttentionIterator->IsActorSound ? " ГГ" : "", RemainingAttention);
		}
		DebugEvent.Label = Label;
		CELSharedDebug::Instance().PublishEvent(DebugEvent, Lifetime);
	}
	if (ReactionRemaining <= 0)
	{
		return;
	}
	DebugEvent.TargetIdentifier = AttentionSource;
	DebugEvent.StimulusType = u32(AttentionNoiseType);
	DebugEvent.Finish = AttentionPosition;
	DebugEvent.Kind = AttentionNoiseType == EELNoiseType::Light ? EELDebugEvent::Light : EELDebugEvent::Sound;
	xr_sprintf(Label, AttentionNoiseType == EELNoiseType::Light ? "СВЕТ: замечен источник" : "%s%s | исследование точки", CELSharedPerceptionProfiles::NoiseName(AttentionNoiseType), AttentionIsActorSound ? " ГГ" : "");
	DebugEvent.Label = Label;
	CELSharedDebug::Instance().PublishEvent(DebugEvent, std::min(1500u, u32(ReactionRemaining)));
	DebugEvent.Radius = 0.f;
	DebugEvent.Kind = s32(PanicUntil - CurrentTime) > 0 ? EELDebugEvent::Fear : EELDebugEvent::Interest;
	DebugEvent.Label = AttentionNoiseType == EELNoiseType::Light ? "СВЕТ: интерес, исследование точки" : DebugEvent.Kind == EELDebugEvent::Fear ? "Испуг: уход от звука"
																																				: "Интерес: исследование звука";
	CELSharedDebug::Instance().PublishEvent(std::move(DebugEvent), std::min(1500u, u32(ReactionRemaining)));
}

// Сбрасывает накопленные данные и незавершённую работу подсистемы.
void CELSharedHearing::Clear()
{
	DamagePanic = false;
	LookUntil = LookEventAt = LookPriority = OpportunityUntil = OpportunityAt = 0;
	OpportunitySource = u16(-1);
	LookSource = u16(-1);
	Pending.clear();
	Finished.clear();
	Attention.clear();
	AttentionIsActorSound = false;
	AttentionSource = u16(-1);
	InvestigateUntil = PanicUntil = NextReaction = AttentionPriority = AttentionStartedAt = LastAttentionEventAt = 0;
}

// Удаляет накопленный шум и активную реакцию исчезнувшего источника.
void CELSharedHearing::RemoveObject(u16 SourceIdentifier)
{
	if (OpportunitySource == SourceIdentifier)
	{
		OpportunityUntil = 0;
		OpportunitySource = u16(-1);
	}
	if (LookSource == SourceIdentifier)
	{
		LookUntil = 0;
		LookSource = u16(-1);
	}
	Pending.erase(std::remove_if(Pending.begin(), Pending.end(), [SourceIdentifier](const SPendingInterest& Task) { return Task.Source == SourceIdentifier; }), Pending.end());
	Attention.erase(std::remove_if(Attention.begin(), Attention.end(), [SourceIdentifier](const SELNoiseAttention& Record)
								   { return Record.SourceIdentifier == SourceIdentifier; }),
					Attention.end());
	if (AttentionSource == SourceIdentifier)
	{
		AttentionSource = u16(-1);
		AttentionIsActorSound = false;
		InvestigateUntil = PanicUntil = NextReaction = AttentionPriority = AttentionStartedAt = LastAttentionEventAt = 0;
	}
}

// Определяет, удерживает ли источник текущий интерес или испуг.
bool CELSharedHearing::IsReactingTo(u16 SourceIdentifier) const
{
	return AttentionSource == SourceIdentifier && (s32(Device.dwTimeGlobal - InvestigateUntil) < 0 || s32(Device.dwTimeGlobal - PanicUntil) < 0);
}
