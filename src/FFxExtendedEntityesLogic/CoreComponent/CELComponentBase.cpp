#include "FFxExtendedEntityesLogic/stdafx.h"
#include "xrGame/ai/monsters/monster_options.h"
#include <algorithm>
#include "FFxExtendedEntityesLogic/CoreComponent/CELComponentBase.h"
#include "xrGame/movement_manager.h"
#include "xrGame/ai_monster_space.h"
#include "xrGame/ai/monsters/control_animation_base.h"
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedTargets.h"
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedAnomalies.h"
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedDeaths.h"
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedSight.h"
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedEnvironment.h"
#include "xrGame/ai/monsters/control_direction_base.h"
#include "FFxExtendedEntityesLogic/EntryPoint.h"
#include "FFxExtendedEntityesLogic/CELSharedSettings.h"
#include "FFxExtendedEntityesLogic/CELSharedPerceptionBudget.h"
#include "xrGame/ai/monsters/basemonster/base_monster.h"
#include "xrGame/ai_object_location.h"
#include "xrGame/memory_manager.h"
#include "xrGame/enemy_manager.h"
#include "xrGame/visual_memory_manager.h"
#include "xrGame/Actor.h"
#include "FFxExtendedEntityesLogic/Debug/Debug.h"
#include "xrGame/Level.h"
#include "xrGame/ai_space.h"
#include "xrGame/level_graph.h"
#include "xrSound/ai_sounds.h"
#include "Include/xrRender/KinematicsAnimated.h"
#include "xrGame/ai/monsters/state_manager.h"
#include "xrGame/EntityCondition.h"
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedHuntGroups.h"

namespace
{
// Inspect actual skeletal tracks, including partial-body and fading animations.
class CAnimationDebugCollector final : public IterateBlendsCallback
{
public:
	explicit CAnimationDebugCollector(IKinematicsAnimated& Owner) : Skeleton(Owner) {}
	void operator()(CBlend& Blend) override
	{
		if (!Blend.motionID.valid() || Blend.blendAmount <= .01f)
		{
			return;
		}
		xr_string Name = "<unknown>";
		if (Blend.motionID.slot < Skeleton.LL_MotionsSlotCount())
		{
			shared_motions Motions = Skeleton.LL_MotionsSlot(Blend.motionID.slot);
			if (const auto* Map = Motions.motion_map())
			{
				for (const auto& Entry : *Map)
				{
					if (Entry.second == Blend.motionID.idx)
					{
						Name = Entry.first.c_str();
						break;
					}
				}
			}
		}
		string512 Label;
		xr_sprintf(Label, "ANIM: %s | part=%u ch=%u weight=%.2f time=%.2f/%.2f speed=%.2f", Name.c_str(), u32(Blend.bone_or_part), u32(Blend.channel), Blend.blendAmount, Blend.timeCurrent, Blend.timeTotal, Blend.speed);
		Labels.emplace_back(Label);

	}
	xr_vector<shared_str> Labels;
private:
	IKinematicsAnimated& Skeleton;
};
}


// Связывает общую память и восприятие владельца с необходимыми службами и состоянием владельца.
CELComponentBase::CELComponentBase(xr_unique_ptr<IELAdapter> OwnerAdapter, const SELPerceptionProfile& OwnerProfile) : Adapter(std::move(OwnerAdapter)), Profile(OwnerProfile) {}

// Подключает владельца к службам расширения и создаёт начальное состояние.
void CELComponentBase::Attach()
{
	if (HasAttached)
	{
		return;
	}
	Adapter->Monster().UsesExtendedCombat = Profile.HasCharacter && Profile.CanUseBehaviors && !Profile.IsPassive;
	Adapter->Monster().ExtendedMeleeHitTolerance = Profile.MeleeHitTolerance;
	Adapter->Monster().ExtendedMeleePredictionTime = Profile.MeleePredictionTime;
	HasAttached = true;
}

// Снимает управление расширения и освобождает регистрации владельца.
void CELComponentBase::Detach()
{
	if (!HasAttached)
	{
		return;
	}
	Adapter->Monster().UsesExtendedCombat = false;
	Adapter->ReleaseControl();
	Adapter->Monster().EnemyMemory.clear();
	Memory.Clear();
	Hearing.Clear();
	Sharing.Clear();
	VisualAcquisition.clear();
	Adapter->Monster().ProvokedEnemyIdentifier = u16(-1);
	Adapter->Monster().ProvokedEnemyUntil = 0;
	Encounters.clear();
	RallyMembers.clear();
	RallyUntil = NextRallyCapture = 0;
	SharingSightSender = u16(-1);
	SharingSightAt = LastFearAt = 0;
	CharacterShock = PackCourage = 0.f;
	CharacterUpdatedAt = CharacterCounterAttackUntil = 0;
	CharacterCounterTarget = u16(-1);
	IsFleeingPredator = false;
	TargetIdentifier = u16(-1);
	HasAttached = false;
	HasActorVisualSample = false;
	ProvokedUntil = 0;
	ProvokedTarget = u16(-1);
}

// Проверяет, истёк ли срок зрительного распознавания цели.
bool CELComponentBase::HasVisualAcquired(CEntityAlive& Candidate, u32 CurrentTime)
{
	if (!smart_cast<CActor*>(&Candidate) || !Profile.VisualAcquireTime)
	{
		return true;
	}
	SVisualAcquisition& Acquisition = VisualAcquisition[Candidate.ID()];
	if (Acquisition.SampleTime && CurrentTime - Acquisition.SampleTime > Profile.MemoryMax)
	{
		Acquisition.VisibleTime = 0;
	}
	Acquisition.VisibleTime += Acquisition.SampleTime ? std::min(100u, CurrentTime - Acquisition.SampleTime) : 0;
	Acquisition.SampleTime = CurrentTime;
	return float(Acquisition.VisibleTime) >= float(Profile.VisualAcquireTime) / CELSharedEnvironment::Acquisition(Profile);
}

// Записывает подтверждённые сведения о сущности в личную память.
void CELComponentBase::RememberEntity(CEntityAlive& TargetEntity, EELObservationSource Source, u32 ObservedAt)
{
	if (ExtendedEntityes::IsIgnored(TargetEntity))
	{
		return;
	}
	// Do not let the native close-contact bridge renew its own original sighting.
	if (Source == EELObservationSource::Vision && CELSharedSight::HasScentContact(Adapter->Monster(), TargetEntity) && !Adapter->Monster().memory().visual().visible_right_now(&TargetEntity))
	{
		Source = EELObservationSource::CloseScent;
	}
	if (Source == EELObservationSource::Vision && smart_cast<CActor*>(&TargetEntity))
	{
		LastActorVisualAt = ObservedAt;
		HasActorVisualSample = true;
	}
	if (Source == EELObservationSource::Vision)
	{
		if (!Memory.Find(TargetEntity.ID()))
		{
			Sharing.NotifyOwnContact(ObservedAt);
		}
		OnVisualContact(TargetEntity, ObservedAt);
	}
	if (Profile.HasCharacter)
	{
		SEncounter& State = Encounter(TargetEntity);
		State.LastContactAt = Device.dwTimeGlobal;
		State.EnemyHealth = std::clamp(TargetEntity.GetfHealth() / std::max(EPS_L, TargetEntity.conditions().GetMaxHealth()), 0.f, 1.f);
		State.EnemyScale = TargetEntity.GetVisualScale();
	}
	SELObservation Observation;
	Observation.TargetIdentifier = TargetEntity.ID();
	Observation.Position = TargetEntity.Position();
	Observation.VertexIdentifier = TargetEntity.ai_location().level_vertex_id();
	Observation.ObservedAt = ObservedAt;
	Observation.Source = Source;
	Memory.Remember(Observation, Device.dwTimeGlobal, Random.randI(Profile.MemoryMin, Profile.MemoryMax + 1), Profile.CanRecordTrail ? Profile.TrailLength : 0.f);
}

// Обновляет личные наблюдения владельца с учётом доступного бюджета.
void CELComponentBase::Observe()
{
	if (!HasAttached)
	{
		return;
	}
	CBaseMonster& OwnerMonster = Adapter->Monster();
	const u32 CurrentTime = Device.dwTimeGlobal;
	if (Actor() && ExtendedEntityes::IsIgnored(*Actor()))
	{
		ForgetTarget(Actor()->ID());
	}
	CELSharedSight::UpdateEvasionTarget(OwnerMonster);
	CEntityAlive* Tracked = ExtendedEntityes::FindEntity(TargetIdentifier);
	if (Tracked && Adapter->IsEnemy(*Tracked) && CELSharedSight::HasEvasionTarget(OwnerMonster, *Tracked))
	{
		RememberEntity(*Tracked, EELObservationSource::Vision, CurrentTime);
	}
	Hearing.PublishDebug(OwnerMonster.Position(), Profile, OwnerMonster.ID());
	ObserveVision(OwnerMonster, CurrentTime);
	ObserveCloseScent();
	ObserveDamage(OwnerMonster, CurrentTime);
	SELObservation Witnessed;
	if (CELSharedDeaths::Witness(OwnerMonster, Profile, Witnessed))
	{
		if (CELSharedDebug::Instance().CanLogCombat(OwnerMonster.Position()))
		{
			CELSharedDebug::Instance().LogCombat(OwnerMonster.ID(), "[monsters_debug] WITNESS t=%u id=%u killer=%u shock_before=%.1f", CurrentTime, OwnerMonster.ID(), Witnessed.TargetIdentifier, CharacterShock);
		}
		if (Profile.HasCharacter)
		{
			CharacterShock = std::min(40.f, CharacterShock + Profile.CharacterAllyDeathLoss);
		}
		if (!ShouldSuppressInterest(Witnessed.TargetIdentifier))
		{
			Hearing.Investigate(Witnessed.Position, Witnessed.TargetIdentifier, Profile.NoiseSearchTime, Profile.NoiseDangerPriority + 1);
		}
		if (Witnessed.TargetIdentifier != u16(-1))
		{
			Memory.Remember(Witnessed, CurrentTime, Profile.NoiseSearchTime, Profile.TrailLength);
			ProvokedTarget = Witnessed.TargetIdentifier;
			ProvokedUntil = CurrentTime + Profile.NoiseSearchTime;
		}
	}
	CELSharedDeaths::UpdateCombat(OwnerMonster, Profile, OwnerMonster.EnemyMan.see_enemy_now());
	CELSharedDeaths::Observe(OwnerMonster, Profile);
	CELSharedAnomalies::Observe(OwnerMonster, Profile);
	ObserveCloseContact();
	ObservePeriphery();
	Memory.Expire(CurrentTime);
	const SELObservation* SelectedObservation = SelectTarget();
	TargetIdentifier = SelectedObservation ? SelectedObservation->TargetIdentifier : u16(-1);
	PublishNativeMemory();
	Sharing.Update(OwnerMonster, Memory, Profile, Hearing, TargetIdentifier);
	if (SelectedObservation && CELSharedDebug::Instance().IsDrawEnabled())
	{
		SELDebugEvent Event;
		Event.Kind = EELDebugEvent::Trail;
		Event.OwnerIdentifier = OwnerMonster.ID();
		Event.TargetIdentifier = TargetIdentifier;
		Event.Start = OwnerMonster.Position();
		Event.Finish = SelectedObservation->Position;
		Event.Points = SelectedObservation->Trail;
		Event.Radius = .25f;
		Event.Label = "Последняя известная точка / след";
		CELSharedDebug::Instance().PublishEvent(std::move(Event));
	}
}

// Пересматривает известное назначение без повторного полного сбора окружения.
void CELComponentBase::RefreshDecision()
{
	if (!HasAttached)
	{
		return;
	}
	ObserveCloseContact();
	ObserveVision(Adapter->Monster(), Device.dwTimeGlobal);
	ObserveCloseScent();
	ObserveDamage(Adapter->Monster(), Device.dwTimeGlobal);
	Memory.Expire(Device.dwTimeGlobal);
	const SELObservation* SelectedObservation = SelectTarget();
	TargetIdentifier = SelectedObservation ? SelectedObservation->TargetIdentifier : u16(-1);
	PublishNativeMemory();
}

// Выбирает назначение до синхронизации штатной памяти и запуска схемы поведения.
const SELObservation* CELComponentBase::SelectTarget()
{
	UpdateCharacter();
	// An animation, jump or species ability already owned by the native FSM must finish.
	if (Profile.HasCharacter && !Profile.IsPassive && !Adapter->CanControl())
	{
		const CEntityAlive* Committed = Adapter->Monster().EnemyMan.get_enemy();
		const SELObservation* Known = Committed ? Memory.Find(Committed->ID()) : nullptr;
		if (Known && Adapter->IsEnemy(*Committed))
		{
			return Known;
		}
	}
	const SELObservation* Selected = Profile.IsPassive ? nullptr : CELSharedTargets::Select(Memory, *Adapter, TargetIdentifier, this);
	if (Selected && Profile.HasCharacter)
	{
		if (CEntityAlive* Target = ExtendedEntityes::FindEntity(Selected->TargetIdentifier))
		{
			SEncounter& State = Encounter(*Target);
			if (!State.Withdrawn && !State.Fleeing)
			{
				State.Engaged = true;
			}
		}
	}
	if (Selected && CELSharedTargets::ThreatPriority(*Selected, *Adapter))
	{
		return Selected;
	}
	if (!Profile.IsPassive && s32(ProvokedUntil - Device.dwTimeGlobal) > 0)
	{
		CEntityAlive* Target = ExtendedEntityes::FindEntity(ProvokedTarget);
		const SELObservation* Known = Memory.Find(ProvokedTarget);
		if (Target && Known && Adapter->IsEnemy(*Target) && Adapter->CanPursue(*Target, Known->Position) && CanEngage(*Known))
		{
			return Known;
		}
	}
	Hearing.ResumeInterest(Adapter->Monster().Position());
	return Selected;
}

// Передаёт актуальные подтверждённые знания в штатную память монстра.
void CELComponentBase::PublishNativeMemory()
{
	CBaseMonster& OwnerMonster = Adapter->Monster();
	if (Profile.HasCharacter && !Profile.IsPassive && !Adapter->CanControl() && OwnerMonster.EnemyMan.get_enemy())
	{
		return;
	}
	OwnerMonster.EnemyMemory.clear();
	if (Profile.IsPassive)
	{
		return;
	}
	CELSharedMemory::Records::const_iterator ObservationIterator;
	CEntityAlive* TargetEntity;
	Fvector KnownPosition;
	u32 KnownVertex;
	for (ObservationIterator = Memory.GetRecords().begin(); ObservationIterator != Memory.GetRecords().end(); ++ObservationIterator)
	{
		TargetEntity = ExtendedEntityes::FindEntity(ObservationIterator->TargetIdentifier);
		if (!TargetEntity || !Adapter->IsEnemy(*TargetEntity) || !CanEngage(*ObservationIterator))
		{
			continue;
		}
		const auto EncounterIterator = Encounters.find(TargetEntity->ID());
		if (Profile.HasCharacter && (TargetEntity->ID() != TargetIdentifier || (EncounterIterator != Encounters.end() && (EncounterIterator->second.Fleeing || EncounterIterator->second.Withdrawn))))
		{
			continue;
		}
		KnownPosition = ObservationIterator->Position;
		KnownVertex = ObservationIterator->VertexIdentifier;
		if (Profile.CanTrackLive)
		{
			KnownPosition = TargetEntity->Position();
			KnownVertex = TargetEntity->ai_location().level_vertex_id();
		}
		// Высотная недосягаемость не стирает знание, но запрещает штатной FSM возобновить ту же погоню.
		if (Adapter->CanPursue(*TargetEntity, KnownPosition))
		{
			OwnerMonster.EnemyMemory.add_enemy(TargetEntity, KnownPosition, KnownVertex, ObservationIterator->ObservedAt);
		}
	}
}

// Принимает сведения другого источника с проверкой правил передачи.
bool CELComponentBase::Accept(const SELObservation& Observation, CBaseMonster& SourceMonster)
{
	CBaseMonster& OwnerMonster = Adapter->Monster();
	if (!HasAttached || Profile.IsPassive || !CELSharedEnemySharing::CanReceive(OwnerMonster, SourceMonster))
	{
		return false;
	}
	const SELPerceptionProfile& SourceProfile = CELSharedSettings::Profile(*SourceMonster.cNameSect());
	CEntityAlive* TargetEntity = ExtendedEntityes::FindEntity(Observation.TargetIdentifier);
	// A nearby ally can identify its personal attacker through a valid communication channel below.
	const bool DefensiveAlert = Profile.HasCharacter && TargetEntity && SourceMonster.ProvokedEnemyIdentifier == TargetEntity->ID() && s32(SourceMonster.ProvokedEnemyUntil - Device.dwTimeGlobal) > 0;
	const float Distance = OwnerMonster.Position().distance_to(SourceMonster.Position());
	if (!Observation.HasParentalChannel)
	{
		const float AudibleRadius = SourceProfile.SharingRadius * SourceProfile.CallSharingRatio;
		const bool HeardCall = Observation.HasCall && Distance <= AudibleRadius;
		if (Distance > std::max(SourceProfile.SharingRadius, Observation.HasCall ? AudibleRadius : 0.f))
		{
			return false;
		}
		if (!SourceProfile.IsWireless)
		{
			const u32 Generation = ExtendedEntityes::OwnerGeneration(SourceMonster.ID());
			if (Distance <= SourceProfile.SharingRadius && (SharingSightSender != SourceMonster.ID() || SharingSightGeneration != Generation || Device.dwTimeGlobal - SharingSightAt >= 150u))
			{
				const EELSightResult Sight = CELSharedSight::Check(OwnerMonster, SourceMonster, 360.f, SourceProfile.SharingRadius, false);
				if (Sight == EELSightResult::WaitingForBudget) { return false; }
				SharingSightSender = SourceMonster.ID();
				SharingSightGeneration = Generation;
				SharingSightAt = Device.dwTimeGlobal;
				SharingSightVisible = Sight == EELSightResult::Visible;
			}
			const bool SeesSender = Distance <= SourceProfile.SharingRadius && SharingSightVisible;
			if (!SeesSender && !HeardCall) { return false; }
			if (HeardCall && (!SeesSender || (Observation.Kind == EELObservationKind::Enemy && (!TargetEntity || !Adapter->CanSee(*TargetEntity)))))
			{
				// Hearing a hidden ally reveals the caller, never the caller's enemy position.
				if (!ExtendedEntityes::PerceptionBudget().TryTransfer()) { return false; }
				const SELObservation* Committed = Memory.Find(TargetIdentifier);
				CEntityAlive* CurrentEnemy = Committed ? ExtendedEntityes::FindEntity(TargetIdentifier) : nullptr;
				const bool Busy = (CurrentEnemy && Adapter->IsEnemy(*CurrentEnemy)) || IsFieldRecovering || s32(Hearing.PanicUntil - Device.dwTimeGlobal) > 0;
				if (!Busy)
				{
					Hearing.ReceiveCall(SourceMonster.Position(), SourceMonster.ID(), Profile);
				}
				if (CELSharedDebug::Instance().CanLogCombat(OwnerMonster.Position()))
				{
					CELSharedDebug::Instance().LogCombat(OwnerMonster.ID(), "[monsters_debug] CALL_RECEIVED t=%u id=%u sender=%u action=%s sender_visible=%d", Device.dwTimeGlobal, OwnerMonster.ID(), SourceMonster.ID(), Busy ? "busy_keep_target" : "investigate_caller", int(SeesSender));
				}
				return true;
			}
		}
	}
	if (Observation.Kind == EELObservationKind::Enemy && (!TargetEntity || (!DefensiveAlert && !Adapter->IsEnemy(*TargetEntity))))
	{
		return false;
	}
	if (Observation.Kind != EELObservationKind::Enemy)
	{
		const bool IsInterest = Observation.Kind == EELObservationKind::Interest;
		if (IsInterest && ShouldSuppressInterest(Observation.TargetIdentifier))
		{
			Hearing.SuppressInterest(Observation.TargetIdentifier);
			return false;
		}
		if (!ExtendedEntityes::PerceptionBudget().TryTransfer() || !(IsInterest ? Hearing.RememberInterest(Observation, Profile) : CELSharedDeaths::Remember(OwnerMonster, Observation, Profile)))
		{
			return false;
		}
		return true;
	}
	if (DefensiveAlert)
	{
		OwnerMonster.ProvokedEnemyIdentifier = TargetEntity->ID();
		OwnerMonster.ProvokedEnemyUntil = Device.dwTimeGlobal + Profile.CharacterMemory;
	}
	const SELObservation* ExistingObservation = Memory.Find(Observation.TargetIdentifier);
	if (ExistingObservation && (s32(Observation.ObservedAt - ExistingObservation->ObservedAt) < 0 || (!Observation.HasLivePosition && Observation.ObservedAt - ExistingObservation->ObservedAt < 1000 && Observation.Position.distance_to_sqr(ExistingObservation->Position) < 16.f)))
	{
		return false;
	}
	SELObservation ReceivedObservation = Observation;
	ReceivedObservation.SenderIdentifier = SourceMonster.ID();
	ReceivedObservation.SenderGeneration = ExtendedEntityes::OwnerGeneration(SourceMonster.ID());
	ReceivedObservation.IsPsiShared = SourceProfile.IsWireless || SourceProfile.CanTrackLive || Observation.HasParentalChannel;
	if (!ExtendedEntityes::PerceptionBudget().TryTransfer() || !Memory.Remember(ReceivedObservation, Device.dwTimeGlobal, Random.randI(Profile.MemoryMin, Profile.MemoryMax + 1), Profile.CanRecordTrail ? Profile.TrailLength : 0.f))
	{
		return false;
	}
	if (Profile.HasCharacter && Profile.CharacterAggression > 0.f && TargetEntity && SourceMonster.EnemyMan.get_enemy() == TargetEntity)
	{
		Encounter(*TargetEntity).AssistanceUntil = Device.dwTimeGlobal + Profile.CharacterDecisionHold;
	}
	if (Observation.HasParentalChannel)
	{
		TargetIdentifier = Observation.TargetIdentifier;
		PublishNativeMemory();
		OwnerMonster.EnemyMan.update();
	}
	return true;
}

// Обрабатывает принятое звуковое событие по настроенным правилам реакции.
// Refusing or withdrawing from a known opponent must also suppress lures attributed to it.
bool CELComponentBase::ShouldSuppressInterest(u16 SourceIdentifier) const
{
	CEntityAlive* Source = ExtendedEntityes::FindEntity(SourceIdentifier);
	if (Source && ExtendedEntityes::IsIgnored(*Source))
	{
		return true;
	}
	const auto Found = Encounters.find(SourceIdentifier);
	if (!Profile.HasCharacter || !Source || !Adapter->IsEnemy(*Source) || Found == Encounters.end() ||
		Found->second.Generation != ExtendedEntityes::OwnerGeneration(SourceIdentifier))
	{
		return false;
	}
	const SEncounter& State = Found->second;
	if (State.Fleeing || State.Withdrawn)
	{
		return true;
	}
	const u32 Now = Device.dwTimeGlobal;
	if (State.Engaged || s32(State.ProvokedUntil - Now) > 0 || s32(State.AssistanceUntil - Now) > 0 ||
		(ProvokedTarget == SourceIdentifier && s32(ProvokedUntil - Now) > 0) ||
		s32(Adapter->Monster().CorneredBerserkUntil - Now) > 0)
	{
		return false;
	}
	return (Profile.CharacterAggression < 100.f && Profile.CharacterAggression <= State.Initiative) || CharacterResolve(State) < Profile.CharacterRetreatThreshold;
}

void CELComponentBase::Hear(const SELSoundEvent& SoundEvent)
{
	if (!HasAttached || SoundEvent.SourceIdentifier == Adapter->Monster().ID())
	{
		return;
	}
	CEntityAlive* SourceEntity = ExtendedEntityes::FindEntity(SoundEvent.SourceIdentifier);
	CBaseMonster* SoundMonster = SourceEntity ? smart_cast<CBaseMonster*>(SourceEntity) : nullptr;
	if (SoundMonster && CELSharedEnemySharing::IgnoresOrdinarySound(Adapter->Monster(), *SoundMonster))
	{
		return;
	}
	if (SourceEntity && ExtendedEntityes::IsIgnored(*SourceEntity))
	{
		Hearing.RemoveObject(SoundEvent.SourceIdentifier);
		return;
	}
	const EELNoiseType SoundKind = CELSharedHearing::Classify(SoundEvent);
	const bool RefusedSource = ShouldSuppressInterest(SoundEvent.SourceIdentifier);
	const bool ActiveEnemy = SoundEvent.SourceIdentifier == TargetIdentifier && Memory.Find(TargetIdentifier) && SourceEntity &&
		(Adapter->CanSee(*SourceEntity) || IsFleeingPredator);
	const bool AllowAttraction = !RefusedSource && !ActiveEnemy;
	if (!AllowAttraction)
	{
		Hearing.SuppressInterest(SoundEvent.SourceIdentifier);
		if (CELSharedDebug::Instance().CanLogCombat(Adapter->Monster().Position()) && s32(Device.dwTimeGlobal - NextHearingDecisionTrace) >= 0)
		{
			NextHearingDecisionTrace = Device.dwTimeGlobal + 2000u;
			CELSharedDebug::Instance().LogCombat(Adapter->Monster().ID(), "[monsters_debug] SOUND_DECISION t=%u id=%u source=%u type=%s attract=0 reason=%s", Device.dwTimeGlobal, Adapter->Monster().ID(), SoundEvent.SourceIdentifier, CELSharedPerceptionProfiles::NoiseName(SoundKind), RefusedSource ? "refused_or_retreating" : "already_in_combat");
		}
	}
	const bool IsDeath = SoundKind == EELNoiseType::Death;
	const bool WasHeard = Hearing.Hear(SoundEvent, Adapter->Monster().Position(), Profile, Adapter->Monster().ID(), AllowAttraction);

	CBaseMonster* SourceMonster = SourceEntity ? smart_cast<CBaseMonster*>(SourceEntity) : nullptr;
	if (WasHeard && IsDeath && Profile.CanHearDeaths && (Profile.InterestNoiseTypes & ELNoiseTypeMask(EELNoiseType::Death)) &&
		SourceMonster && !SourceMonster->g_Alive() && CELSharedEnemySharing::AreFriends(Adapter->Monster(), *SourceMonster) &&
		Adapter->Monster().Position().distance_to_sqr(SoundEvent.Position) <= Profile.DeathHearingRadius * Profile.DeathHearingRadius)
	{
		Hearing.SetSource(SoundEvent.SourceIdentifier);
		Hearing.AttentionPosition = SoundEvent.Position;
		Hearing.InvestigateUntil = SoundEvent.OccurredAt + Profile.SearchMax;
	}
}

// Rotate an idle ground observer toward a heard point; do not identify its unseen source as an enemy.
void CELComponentBase::UpdateAuditoryAttention()
{
	CBaseMonster& Owner = Adapter->Monster();
	if (!HasAttached || !Adapter->CanControl() || !Profile.CanUseBehaviors || s32(Device.dwTimeGlobal - NextSoundLook) < 0 ||
		Owner.movement().speed() > .1f || Owner.anim().m_tAction == ACT_ATTACK || s32(Hearing.PanicUntil - Device.dwTimeGlobal) > 0)
	{
		return;
	}
	const SELObservation* Current = Memory.Find(TargetIdentifier);
	if (Current && CELSharedTargets::ThreatPriority(*Current, *Adapter))
	{
		return;
	}
	Fvector Point;
	if (!Hearing.GetLookPoint(Point) || Owner.Position().distance_to_sqr(Point) <= EPS_L)
	{
		return;
	}
	Owner.dir().face_target(Point, 0);
	Owner.dir().set_heading_speed(std::max(Owner.dir().heading().speed_target, Profile.SoundLookTurnSpeed));
	NextSoundLook = Device.dwTimeGlobal + Profile.SoundLookInterval;
	SoundLookAppliedUntil = NextSoundLook + 50u;
}

// Удаляет знания и ссылки, связанные с исчезнувшим объектом.
void CELComponentBase::RemoveObject(u16 ObjectIdentifier)
{
	if (CharacterCounterTarget == ObjectIdentifier)
	{
		CharacterCounterAttackUntil = 0;
		CharacterCounterTarget = u16(-1);
	}
	Encounters.erase(ObjectIdentifier);
	ForgetTarget(ObjectIdentifier);
	Hearing.RemoveObject(ObjectIdentifier);
}

// Удаляет сведения о противнике, не затрагивая слуховое внимание к его звукам.
void CELComponentBase::ForgetTarget(u16 ObjectIdentifier)
{
	const bool HadObservation = Memory.Find(ObjectIdentifier) != nullptr;
	Memory.Remove(ObjectIdentifier);
	VisualAcquisition.erase(ObjectIdentifier);
	if (TargetIdentifier == ObjectIdentifier)
	{
		TargetIdentifier = u16(-1);
		CELSharedTargets::RemoveOwner(Adapter->Monster().ID());
	}
	if (HadObservation && HasAttached)
	{
		PublishNativeMemory();
	}
}

void CELComponentBase::UpdateFieldExposure()
{
	const u32 Now = Device.dwTimeGlobal;
	const float Elapsed = LastFieldAt ? std::min(1.f, float(Now - LastFieldAt) * .001f) : 0.f;
	LastFieldAt = Now;
	float Exposure = 0.f;
	Fvector Source;
	u32 Until;
	const bool IsInside = CBaseMonster::PanicZonePressure(Adapter->Monster().Position(), Source, Until, Adapter->Monster().EmmiPainProtection, &Exposure) > 0.f;
	if (IsInside)
	{
		FieldEscapeSource = Source;
		FieldHeadache = std::min(1.f, FieldHeadache + Elapsed * Profile.FieldHeadacheGain * Exposure);
		if (FieldHeadache >= 1.f)
		{
			IsFieldRecovering = true;
		}
	}
	else
	{
		FieldHeadache = std::max(0.f, FieldHeadache - Elapsed * Profile.FieldHeadacheRecovery);
		if (FieldHeadache <= Profile.FieldHeadacheResume)
		{
			IsFieldRecovering = false;
		}
	}
	Adapter->SetFieldEscapePermission(IsFieldRecovering && s32(Adapter->Monster().CorneredBerserkUntil - Now) <= 0);
}

bool CELComponentBase::ShouldFleePredator(const SELObservation* Observation)
{
	if (Profile.HasCharacter)
	{
		return ShouldFleeCharacter(Observation);
	}
	CBaseMonster& Monster = Adapter->Monster();
	CEntityAlive* Target = Observation ? ExtendedEntityes::FindEntity(Observation->TargetIdentifier) : nullptr;
	if (Target && !Adapter->IsEnemy(*Target))
	{
		Target = nullptr;
	}
	// Species fear is independent of damage and of the native combat strength estimator.
	const u16 CurrentTarget = Target ? Target->ID() : u16(-1);
	const u32 Members = Target && Profile.UsesPackCourage ? CELSharedHuntGroups::Observe(Monster, CurrentTarget, Profile.PackCourageRadius).Count : 1;
	const float DesiredCourage = Profile.UsesPackCourage ? std::min(.8f, float(Members > 0 ? Members - 1 : 0) * Profile.PackCouragePerAlly) : 0.f;
	const float Elapsed = LastCourageAt ? std::min(1.f, float(Device.dwTimeGlobal - LastCourageAt) * .001f) : 0.f;
	LastCourageAt = Device.dwTimeGlobal;
	const float Step = (DesiredCourage > Courage ? Profile.PackCourageGain : Profile.SoloCourageDecay) * Elapsed;
	Courage += std::clamp(DesiredCourage - Courage, -Step, Step);
	if (FearTarget != CurrentTarget)
	{
		FearTarget = CurrentTarget;
		FearDecisionUntil = 0;
		IsFleeingPredator = false;
	}
	bool IsPredator = false;
	if (Target && Profile.CanFearPredators && Profile.PredatorSpecies.size())
	{
		const char* Species = ExtendedEntityesOptions::Read<str_c>(pSettings, Target->cNameSect().c_str(), "species", "");
		string128 Item;
		for (u32 Index = 0; Index < _GetItemCount(Profile.PredatorSpecies.c_str()); ++Index)
		{
			_GetItem(Profile.PredatorSpecies.c_str(), Index, Item);
			if (xr_strcmp(Item, Species) == 0)
			{
				IsPredator = true;
				break;
			}
		}
	}
	if (IsPredator && s32(Device.dwTimeGlobal - FearDecisionUntil) >= 0)
	{
		const float RawThreshold = Monster.panic_threshold();
		const float Threshold = _valid(RawThreshold) ? std::clamp(RawThreshold, 0.f, 5.f) : 0.f;
		const float Health = clampr(Monster.conditions().GetHealth() / std::max(EPS_L, Monster.conditions().GetMaxHealth()), 0.f, 1.f);
		const float Chance = std::clamp(Profile.PredatorFearChance * (1.f + Threshold) * (1.f - Courage) + Profile.InjuryFearGain * (1.f - Health), .05f, .95f);
		IsFleeingPredator = Random.randF(0.f, 1.f) < Chance;
		FearDecisionUntil = Device.dwTimeGlobal + Random.randI(6000u, 10001u);
	}
	return s32(Monster.CorneredBerserkUntil - Device.dwTimeGlobal) <= 0 && IsPredator && IsFleeingPredator;
}

// Display the actual cached decisions, without rolling initiative or evaluating combat from the renderer.
void CELComponentBase::FillCharacterDecisions(SELDebugRecord& Record) const
{
	if (!Profile.HasCharacter || !CELSharedDebug::Instance().Detail(3))
	{
		return;
	}
	const u32 Now = Device.dwTimeGlobal;
	xr_string Feared;
	xr_string Refused;
	u32 FearedCount = 0;
	u32 RefusedCount = 0;
	u32 Details = 0;
	u32 Omitted = 0;
	const auto AppendIdentifier = [&Record](xr_string& List, u32& Count, const char* Prefix, u16 Identifier)
	{
		string32 Number;
		xr_sprintf(Number, "%u", Identifier);
		if (!List.empty())
		{
			List += ", ";
		}
		List += Number;
		if (++Count % 8u == 0)
		{
			string256 Label;
			xr_sprintf(Label, "%s ID (%s)", Prefix, List.c_str());
			Record.Senses.push_back(Label);
			List.clear();
		}
	};
	for (const auto& Item : Encounters)
	{
		const SEncounter& State = Item.second;
		CEntityAlive* Target = ExtendedEntityes::FindEntity(Item.first);
		if (!Target || !Target->g_Alive() || Target->getDestroy() || ExtendedEntityes::IsIgnored(*Target) ||
			State.Generation != ExtendedEntityes::OwnerGeneration(Item.first) || Now - State.LastContactAt > Profile.CharacterMemory)
		{
			continue;
		}
		const bool Afraid = State.Fleeing || State.Withdrawn;
		const bool Rejected = State.HasCombatDecision && !State.CombatAccepted && !State.Engaged && Now - State.CombatDecisionAt <= Profile.CharacterMemory;
		if (!Afraid && !Rejected)
		{
			continue;
		}
		AppendIdentifier(Afraid ? Feared : Refused, Afraid ? FearedCount : RefusedCount, Afraid ? "БОЮСЬ" : "НЕ АТАКУЮ", Item.first);
		if (Details >= 6)
		{
			++Omitted;
			continue;
		}
		++Details;
		const char* Reason = Afraid ? (State.Withdrawn ? "восстановление после отхода" : "испуг: отступаю") :
			!xr_strcmp(State.CombatReason, "initiative_roll") ? "не прошёл бросок агрессии" :
			!xr_strcmp(State.CombatReason, "risk_too_high") ? "не хватает смелости" :
			!xr_strcmp(State.CombatReason, "withdrawal_hold") ? "удержание решения об отходе" :
			!xr_strcmp(State.CombatReason, "confidence_not_recovered") ? "смелость ещё не восстановилась" : State.CombatReason;
		string512 Label;
		const float Hold = float(std::max(0, s32(State.DecisionUntil - Now))) * .001f;
		if (Afraid)
		{
			xr_sprintf(Label, "ОТКАЗ #%u%s: %s | решимость %.1f / возврат %.1f | удержание %.1f с", Item.first, State.IsActor ? " (ГГ)" : "", Reason, CharacterResolve(State), CharacterResumeResolve(State), Hold);
		}
		else
		{
			xr_sprintf(Label, "ОТКАЗ #%u%s: %s | агрессия %.1f / бросок %.1f | решимость %.1f / порог %.1f", Item.first, State.IsActor ? " (ГГ)" : "", Reason, Profile.CharacterAggression, State.Initiative, CharacterResolve(State), Profile.CharacterRetreatThreshold);
		}
		Record.Senses.push_back(Label);
		if (Afraid && State.ClosePressureSince && Profile.CharacterCornerPressureTime)
		{
			xr_sprintf(Label, "ОТКАЗ #%u: пойман вблизи %.1f / %.1f с до контратаки", Item.first, float(Now - State.ClosePressureSince) * .001f, float(Profile.CharacterCornerPressureTime) * .001f);
			Record.Senses.push_back(Label);
		}
	}
	const auto FlushIdentifiers = [&Record](const char* Prefix, const xr_string& List)
	{
		if (!List.empty())
		{
			string256 Label;
			xr_sprintf(Label, "%s ID (%s)", Prefix, List.c_str());
			Record.Senses.push_back(Label);
		}
	};
	FlushIdentifiers("БОЮСЬ", Feared);
	FlushIdentifiers("НЕ АТАКУЮ", Refused);
	if (Omitted)
	{
		string128 Label;
		xr_sprintf(Label, "ОТКАЗ: ещё %u причин; все ID перечислены выше", Omitted);
		Record.Senses.push_back(Label);
	}
}

// Use existing perception snapshots: debug drawing must not consume vision rays.
void CELComponentBase::FillDebugRecord(SELDebugRecord& Record) const
{
	CELSharedTargets::FillDebug(Record, Adapter->Monster().ID());
	Record.HasVitalBars = true;
	Record.HasCharacterBars = Profile.HasCharacter;
	Record.HealthPercent = std::clamp(Adapter->Monster().GetfHealth() / std::max(EPS_L, Adapter->Monster().conditions().GetMaxHealth()), 0.f, 1.f) * 100.f;
	Record.CouragePercent = std::clamp(Courage * 100.f, 0.f, 100.f);
	Record.AggressionPercent = Profile.CharacterAggression;
	Record.BaseCouragePercent = Profile.CharacterCourage;
	Record.BaseAggressionPercent = Profile.CharacterAggression;
	Fbox DebugBounds;
	DebugBounds.xform(Adapter->Monster().BoundingBox(), Adapter->Monster().XFORM());
	DebugBounds.getcenter(Record.BarsPosition);
	Record.BarsPosition.y = DebugBounds.max.y + .25f;
	FillCharacterDecisions(Record);
	Record.IsBerserk = s32(Adapter->Monster().CorneredBerserkUntil - Device.dwTimeGlobal) > 0;
	Record.IsCounterAttacking = !Record.IsBerserk && (Record.IsCounterAttacking || IsCharacterCounterAttacking());
	if (IsCharacterCounterAttacking())
	{
		string128 CounterLabel;
		xr_sprintf(CounterLabel, "КОНТРАТАКА: цель #%u | осталось %.1f с", CharacterCounterTarget, float(CharacterCounterAttackUntil - Device.dwTimeGlobal) * .001f);
		Record.Senses.push_back(CounterLabel);
	}
	CEntityAlive* EvasiveTarget = ExtendedEntityes::FindEntity(TargetIdentifier);
	if (EvasiveTarget && CELSharedSight::HasEvasionTarget(Adapter->Monster(), *EvasiveTarget))
	{
		Record.Senses.push_back("ЗРЕНИЕ: удержание подтвержденной цели при увороте");
	}
	Hearing.FillDebug(Record);
	if (s32(SoundLookAppliedUntil - Device.dwTimeGlobal) > 0)
	{
		Record.Senses.push_back("ВНИМАНИЕ: разворот к услышанной точке");
	}
	if (s32(Adapter->Monster().CorneredBerserkUntil - Device.dwTimeGlobal) > 0)
	{
		Record.Senses.push_back("БЕРСЕРК: загнан в угол, страх подавлен, урон усилен");
	}
	string256 EnvironmentLabel;
	xr_sprintf(EnvironmentLabel, "ВОСПРИЯТИЕ: зрение ×%.2f | дождь %.2f | туман %.2f | нюх %s", CELSharedEnvironment::Vision(Profile), CELSharedEnvironment::Rain(), CELSharedEnvironment::Fog(), CELSharedEnvironment::CanSmell() ? "разрешён" : "отключён дождём");
	Record.Senses.push_back(EnvironmentLabel);
	xr_sprintf(EnvironmentLabel, "ПОЛЕ: головная боль %.0f%% | %s", FieldHeadache * 100.f, IsFieldRecovering ? "уход/восстановление" : "может рискнуть войти");
	Record.Senses.push_back(EnvironmentLabel);
	if (Profile.HasCharacter)
	{
		xr_sprintf(EnvironmentLabel, "ХАРАКТЕР: агрессия %.0f | смелость %.0f / база %.0f | стая +%.1f | потрясение -%.1f | %s", Profile.CharacterAggression, Courage * 100.f, Profile.CharacterCourage, PackCourage * Profile.CharacterPackBonus, CharacterShock, IsFleeingPredator ? "отход" : "бой/наблюдение");
	}
	else
	{
		xr_sprintf(EnvironmentLabel, "СМЕЛОСТЬ: %.0f%% | видовой страх %s | решение: %s", Courage * 100.f, Profile.CanFearPredators ? "разрешён" : "отключён", IsFleeingPredator ? "отступление" : "бой/нет угрозы");
	}
	Record.Senses.push_back(EnvironmentLabel);
	const SELObservation* Guided = Memory.Find(TargetIdentifier);
	if (Guided && Guided->Source == EELObservationSource::Sharing && Guided->IsPsiShared && s32(Guided->ExpiresAt - Device.dwTimeGlobal) > 0 &&
		Guided->SenderGeneration == ExtendedEntityes::OwnerGeneration(Guided->SenderIdentifier))
	{
		CEntityAlive* Guide = ExtendedEntityes::FindEntity(Guided->SenderIdentifier);
		if (Guide && Guide->g_Alive() && !Guide->getDestroy())
		{
			Record.PsiGuideIdentifier = Guide->ID();
			Guide->Center(Record.PsiGuidePosition);
		}
	}
	CActor* Player = Actor();
	if (!Player)
	{
		return;
	}
	if (CELSharedDebug::Instance().Detail(0) && Player->Position().distance_to_sqr(Adapter->Monster().Position()) <= 60.f * 60.f)
	{
		if (s32(Device.dwTimeGlobal - NextAnimationSample) >= 0)
		{
			NextAnimationSample = Device.dwTimeGlobal + 100;
			AnimationLabels.clear();
			CBaseMonster& Owner = Adapter->Monster();
			CControlAnimationBase& Animation = Owner.anim();
			string512 Header;
			xr_sprintf(Header, "ANIM REQUEST: native=%u action=%u motion=%u override=%u selected=%s age=%u ms", Owner.StateMan ? u32(Owner.StateMan->get_state_type()) : u32(-1), u32(Animation.m_tAction), u32(Animation.GetCurAnim()), u32(Animation.get_override_animation()), Animation.cur_anim_info().name.size() ? Animation.cur_anim_info().name.c_str() : "<none>", Device.dwTimeGlobal - Animation.cur_anim_info().time_started);
			AnimationLabels.emplace_back(Header);
			if (Owner.Visual())
			{
				if (IKinematicsAnimated* Skeleton = Owner.Visual()->dcast_PKinematicsAnimated())
				{
					CAnimationDebugCollector Collector(*Skeleton);
					Skeleton->LL_IterateBlends(Collector);
					AnimationLabels.insert(AnimationLabels.end(), Collector.Labels.begin(), Collector.Labels.end());
				}
			}

		}
		Record.Senses.insert(Record.Senses.end(), AnimationLabels.begin(), AnimationLabels.end());
	}
	else
	{
		AnimationLabels.clear();
	}
	// Capture marker placement on the update thread; rendering consumes only this snapshot.
	if (!ExtendedEntityes::IsIgnored(*Player) && s32(Adapter->Monster().CorneredBerserkUntil - Device.dwTimeGlobal) <= 0)
	{
		const auto Encounter = Encounters.find(Player->ID());
		const bool CharacterFear = Encounter != Encounters.end() && Encounter->second.Generation == ExtendedEntityes::OwnerGeneration(Player->ID()) &&
			Device.dwTimeGlobal - Encounter->second.LastContactAt <= Profile.CharacterMemory && (Encounter->second.Fleeing || Encounter->second.Withdrawn);
		const bool SpeciesFear = !Profile.HasCharacter && IsFleeingPredator && FearTarget == Player->ID();
		const bool SoundFear = s32(Hearing.PanicUntil - Device.dwTimeGlobal) > 0 && Hearing.IsReactingTo(Player->ID());
		Record.IsAfraidOfActor = CharacterFear || SpeciesFear || SoundFear;
	}
	if (Record.IsAfraidOfActor || Record.IsCounterAttacking || Record.IsBerserk)
	{
		Record.StateMarkerPosition = Record.BarsPosition;
		Record.StateMarkerPosition.y += 1.25f;
	}
	string512 Label;
	const SELObservation* Known = Memory.Find(Player->ID());
	if (ExtendedEntityes::IsIgnored(*Player))
	{
		Record.Senses.push_back("АКТОР ИГНОРИРУЕТСЯ: включён запрет реакции");
	}
	else if (CELSharedSight::HasCloseContact(Adapter->Monster(), *Player))
	{
		Record.Senses.push_back(CELSharedSight::HasScentContact(Adapter->Monster(), *Player) ? "АКТОР ОБНАРУЖЕН: ближнее чутьё" : "АКТОР ОБНАРУЖЕН: ближний контакт");
	}
	else if (Adapter->CanSee(*Player))
	{
		Record.Senses.push_back("ВИЖУ АКТОРА: текущая видимость подтверждена");
	}
	else
	{
		Record.Senses.push_back("ЗРЕНИЕ: нет свежего подтверждения актора");
	}
	if (Known)
	{
		const char* Source = Known->Source == EELObservationSource::Vision ? "зрение" : Known->Source == EELObservationSource::Sharing	  ? "союзник"
																					: Known->Source == EELObservationSource::Damage		  ? "урон"
																					: Known->Source == EELObservationSource::CloseScent	  ? "ближнее чутьё"
																					: Known->Source == EELObservationSource::CloseContact ? "контакт"
																																		  : "слух";
		xr_sprintf(Label, "%s | %s | давность %.1f с / память ещё %.1f с", TargetIdentifier == Player->ID() ? "ПРЕСЛЕДУЮ АКТОРА ПО ИЗВЕСТНОЙ ТОЧКЕ" : "ПОМНЮ АКТОРА", Source, float(Device.dwTimeGlobal - Known->ObservedAt) * .001f, float(std::max(0, s32(Known->ExpiresAt - Device.dwTimeGlobal))) * .001f);
		Record.Senses.push_back(Label);
		if (Known->Source == EELObservationSource::Sharing)
		{
			xr_sprintf(Label, "Передал союзник #%u", Known->SenderIdentifier);
			Record.Senses.push_back(Label);
		}
	}
	if (Profile.CanTrackLive)
	{
		Record.Senses.push_back("ОСОБЫЙ ПРОФИЛЬ: слежение за живой позицией цели");
	}
}

// Выполняет покадровую часть активной схемы без повторного выбора поведения.
void CELComponentBase::FrameUpdate(float DeltaTime) {}

// Проверяет близкие угрозы, доступные контактному обнаружению.
void CELComponentBase::ObserveCloseContact()
{
	if (Profile.IsPassive || Profile.CloseDetectionRadius <= 0.f || s32(Device.dwTimeGlobal - NextCloseProbe) < 0)
	{
		return;
	}
	CBaseMonster& OwnerMonster = Adapter->Monster();
	CEntityAlive* CurrentEnemy = const_cast<CEntityAlive*>(OwnerMonster.EnemyMan.get_enemy());
	if (CurrentEnemy && Adapter->IsEnemy(*CurrentEnemy) && CELSharedSight::CheckClose(OwnerMonster, *CurrentEnemy) == EELSightResult::Visible)
	{
		RememberEntity(*CurrentEnemy, EELObservationSource::CloseContact, Device.dwTimeGlobal);
	}
	if (!ExtendedEntityes::PerceptionBudget().TrySpatial(OwnerMonster.ID()))
	{
		return;
	}
	NextCloseProbe = Device.dwTimeGlobal + 100;
	xr_vector<ISpatialShared> Candidates;
	g_SpatialSpace->q_sphere(Candidates, 0, ESPATIAL_TYPE::COLLIDEABLE, OwnerMonster.Position(), Profile.CloseDetectionRadius * CELSharedEnvironment::CloseFactor());
	xr_vector<ISpatialShared>::const_iterator CandidateIterator;
	CObject* CandidateObject;
	CEntityAlive* Candidate;
	for (CandidateIterator = Candidates.begin(); CandidateIterator != Candidates.end(); ++CandidateIterator)
	{
		CandidateObject = *CandidateIterator ? (*CandidateIterator)->dcast_CObject() : nullptr;
		Candidate = CandidateObject ? CandidateObject->cast_entity_alive() : nullptr;
		if (!Candidate || !Adapter->IsEnemy(*Candidate))
		{
			continue;
		}
		if (CELSharedSight::CheckClose(OwnerMonster, *Candidate) == EELSightResult::Visible)
		{
			RememberEntity(*Candidate, EELObservationSource::CloseContact, Device.dwTimeGlobal);
		}
	}
}

// Пополняет личную память целями, подтверждёнными зрением.
void CELComponentBase::ObserveVision(CBaseMonster& OwnerMonster, u32 CurrentTime)
{
	const CObjectManager<const CEntityAlive>::OBJECTS& Candidates = OwnerMonster.memory().enemy().objects();
	CObjectManager<const CEntityAlive>::OBJECTS::const_iterator CandidateIterator;
	CEntityAlive* Candidate;
	for (CandidateIterator = Candidates.begin(); CandidateIterator != Candidates.end(); ++CandidateIterator)
	{
		Candidate = const_cast<CEntityAlive*>(*CandidateIterator);
		if (Adapter->IsEnemy(*Candidate) && Adapter->CanSee(*Candidate) && HasVisualAcquired(*Candidate, CurrentTime))
		{
			RememberEntity(*Candidate, EELObservationSource::Vision, CurrentTime);
		}
	}
}

// Запоминает противника, установленного по полученному урону.
void CELComponentBase::ObserveDamage(CBaseMonster& OwnerMonster, u32 CurrentTime)
{
	CObject* Attacker = OwnerMonster.HitMemory.get_last_hit_object();
	CEntityAlive* Candidate = Attacker ? Attacker->cast_entity_alive() : nullptr;
	if (Candidate && Adapter->IsEnemy(*Candidate) && CurrentTime - OwnerMonster.HitMemory.get_last_hit_time() < 1000)
	{
		const bool HasVision = Adapter->CanSee(*Candidate);
		const float Distance = OwnerMonster.Position().distance_to(Candidate->Position());
		const float ContactRadius = std::max(Profile.CloseDetectionRadius * CELSharedEnvironment::CloseFactor(), OwnerMonster.MeleeChecker.get_max_distance() + Candidate->Radius());
		const bool CanLocate = HasVision || Distance <= ContactRadius || (Profile.CanAutoFocus && Distance <= Profile.AutoFocusRadius);
		if (CanLocate)
		{
			RememberEntity(*Candidate, EELObservationSource::Damage, OwnerMonster.HitMemory.get_last_hit_time());
		}
		if (!CanLocate && Profile.CanPanicOnDamage)
		{
			Hearing.SetSource(Candidate->ID());
			Hearing.AttentionPosition = OwnerMonster.HitMemory.get_last_hit_position();
			Hearing.PanicUntil = OwnerMonster.HitMemory.get_last_hit_time() + 10000;
			Hearing.DamagePanic = true;
		}
	}
}

// Проверяет периферийные стимулы и направляет осмотр без всеведения.
void CELComponentBase::ObservePeriphery()
{
	CBaseMonster& OwnerMonster = Adapter->Monster();
	if (!Profile.CanUsePeriphery || Profile.IsPassive || !Adapter->CanControl() || OwnerMonster.EnemyMan.see_enemy_now() || s32(Device.dwTimeGlobal - NextPeripheralProbe) < 0)
	{
		return;
	}
	if (!ExtendedEntityes::PerceptionBudget().TrySpatial())
	{
		return;
	}
	NextPeripheralProbe = Device.dwTimeGlobal + Profile.PeripheralInterval + OwnerMonster.ID() % 201;
	xr_vector<ISpatialShared> Candidates;
	g_SpatialSpace->q_sphere(Candidates, 0, ESPATIAL_TYPE::COLLIDEABLE, OwnerMonster.Position(), Profile.PeripheralRadius);
	if (Candidates.empty())
	{
		return;
	}
	u32 Checked;
	CObject* CandidateObject;
	CEntityAlive* Candidate;
	for (Checked = 0; Checked < Candidates.size() && Checked < 64; ++Checked)
	{
		PeripheralCursor %= u32(Candidates.size());
		CandidateObject = Candidates[PeripheralCursor] ? Candidates[PeripheralCursor]->dcast_CObject() : nullptr;
		++PeripheralCursor;
		Candidate = CandidateObject ? CandidateObject->cast_entity_alive() : nullptr;
		if (!Candidate || !Adapter->IsEnemy(*Candidate))
		{
			continue;
		}
		if (CELSharedSight::Visible(OwnerMonster, *Candidate, std::max(Profile.PeripheralAngle, OwnerMonster.eye_fov), std::min(Profile.PeripheralRadius, OwnerMonster.eye_range)))
		{
			// Attention rotates the native view; only native confirmed vision may add an enemy fact.
			OwnerMonster.dir().face_target(Candidate, 800);
			break;
		}
	}
}

void CELComponentBase::NoticeActorLight(const Fvector& Point)
{
	CBaseMonster& OwnerMonster = Adapter->Monster();
	if (!HasAttached || Profile.IsPassive || !Adapter->CanControl())
	{
		return;
	}
	// Light shares noise interest accumulation, without recording its owner as an enemy.
	SELSoundEvent Stimulus;
	Stimulus.SourceIdentifier = Actor() ? Actor()->ID() : u16(-1);
	if (ShouldSuppressInterest(Stimulus.SourceIdentifier))
	{
		Hearing.SuppressInterest(Stimulus.SourceIdentifier);
		return;
	}
	Stimulus.Kind = EELSoundKind::Light;
	Stimulus.SoundType = SOUND_TYPE_AMBIENT;
	Stimulus.Position = Point;
	Stimulus.Power = 1.f;
	Stimulus.OccurredAt = Device.dwTimeGlobal;
	Hearing.Hear(Stimulus, OwnerMonster.Position(), Profile, OwnerMonster.ID());
}

// Bounded encounter memory keeps decisions stable without a second species matrix.
CELComponentBase::SEncounter& CELComponentBase::Encounter(CEntityAlive& Target)
{
	const u32 Now = Device.dwTimeGlobal;
	auto Existing = Encounters.find(Target.ID());
	if (Existing == Encounters.end() && Encounters.size() >= 64)
	{
		auto Oldest = std::min_element(Encounters.begin(), Encounters.end(), [this](const auto& Left, const auto& Right)
		{
			if (Left.first == TargetIdentifier || Right.first == TargetIdentifier)
			{
				return Right.first == TargetIdentifier && Left.first != TargetIdentifier;
			}
			return Left.second.LastContactAt < Right.second.LastContactAt;
		});
		Encounters.erase(Oldest);
	}
	SEncounter& State = Encounters[Target.ID()];
	const u32 Generation = ExtendedEntityes::OwnerGeneration(Target.ID());
	if (!State.Initialized || State.Generation != Generation)
	{
		State = {};
		State.Initialized = true;
		State.Generation = Generation;
		State.IsActor = smart_cast<CActor*>(&Target) != nullptr;
		State.EnemyTier = std::clamp(ExtendedEntityesOptions::Read<u32>(pSettings, *Target.cNameSect(), "monster_threat_tier", 2u), 1u, 5u);
		State.UpdatedAt = Now;
		State.LastContactAt = Now;
		State.Initiative = Random.randF(0.f, 100.f);
		State.ResolveBias = Random.randF(-5.f, 5.f);
	}
	return State;
}

void CELComponentBase::UpdateCharacter()
{
	if (!Profile.HasCharacter)
	{
		return;
	}
	const u32 Now = Device.dwTimeGlobal;
	const float Elapsed = CharacterUpdatedAt ? std::min(1.f, float(Now - CharacterUpdatedAt) * .001f) : 0.f;
	CharacterUpdatedAt = Now;
	CBaseMonster& Monster = Adapter->Monster();
	const u32 Members = Profile.UsesPackCourage ? CELSharedHuntGroups::CountCompanions(Monster, Profile.PackCourageRadius) : 1;
	const float DesiredPack = Profile.UsesPackCourage ? std::min(1.f, float(Members > 0 ? Members - 1 : 0) * Profile.PackCouragePerAlly) : 0.f;
	const float PackStep = (DesiredPack > PackCourage ? Profile.PackCourageGain : Profile.SoloCourageDecay) * Elapsed;
	PackCourage += std::clamp(DesiredPack - PackCourage, -PackStep, PackStep);
	const bool Fighting = TargetIdentifier != u16(-1);
	if (Fighting && Profile.UsesPackCourage && Profile.PackRegroupTime)
	{
		RallyUntil = Now + Profile.PackRegroupTime;
		if (s32(Now - NextRallyCapture) >= 0)
		{
			NextRallyCapture = Now + 1000u;
			CELSharedHuntGroups::CaptureCompanions(Monster, Profile.PackCourageRadius, RallyMembers);
		}
	}
	if (!Fighting || IsFleeingPredator)
	{
		CharacterShock = std::max(0.f, CharacterShock - Profile.CharacterRecovery * Elapsed);
	}
	for (auto Iterator = Encounters.begin(); Iterator != Encounters.end();)
	{
		SEncounter& State = Iterator->second;
		if ((Iterator->first != TargetIdentifier && Now - State.LastContactAt > Profile.CharacterMemory) || !ExtendedEntityes::FindEntity(Iterator->first))
		{
			Iterator = Encounters.erase(Iterator);
			continue;
		}
		const float Recovery = Profile.CharacterRecovery * std::min(1.f, float(Now - State.UpdatedAt) * .001f);
		State.UpdatedAt = Now;
		if (Iterator->first != TargetIdentifier || State.Fleeing || State.Withdrawn)
		{
			State.Experience += std::clamp(-State.Experience, -Recovery, Recovery);
			State.Engaged = false;
		}
		++Iterator;
	}
	const auto Current = Encounters.find(TargetIdentifier);
	Courage = std::clamp(CharacterConfidence(Current != Encounters.end() ? Current->second : SEncounter()) * .01f, 0.f, 1.f);
	Monster.CharacterCourage = Courage;
}

float CELComponentBase::CharacterConfidence(const SEncounter& State) const
{
	CBaseMonster& Monster = Adapter->Monster();
	const float Health = std::clamp(Monster.GetfHealth() / std::max(EPS_L, Monster.conditions().GetMaxHealth()), 0.f, 1.f);
	const float SizeSpan = Monster.GetVisualScale() < 1.f ? 1.f - Profile.CharacterSizeScaleMin : Profile.CharacterSizeScaleMax - 1.f;
	const float Size = std::clamp((Monster.GetVisualScale() - 1.f) / SizeSpan, -1.f, 1.f) * Profile.CharacterSizeCourage;
	const float Bonus = std::max(0.f, Size) + PackCourage * Profile.CharacterPackBonus + std::max(0.f, State.Experience);
	// Positive support uses remaining headroom; injuries and resistance retain their full weight.
	return Profile.CharacterCourage + std::min(0.f, Size) + Bonus * (1.f - Profile.CharacterCourage * .01f) + std::min(0.f, State.Experience) - CharacterShock - (1.f - Health) * Profile.CharacterHealthPenalty;
}

float CELComponentBase::CharacterResolve(const SEncounter& State) const
{
	CBaseMonster& Monster = Adapter->Monster();
	const float Confidence = CharacterConfidence(State);
	const u32 EnemyTier = State.EnemyTier;
	const bool IsActor = State.IsActor;
	const float RankPressure = IsActor || !Profile.CanFearPredators ? 0.f : std::max(0.f, float(std::min(5u, EnemyTier)) - float(Monster.ThreatTier)) * Profile.CharacterTierPressure;
	const float EnemyWeakness = IsActor ? 0.f : (1.f - State.EnemyHealth) * Profile.CharacterHealthPenalty * .5f;
	const float RelativeSize = IsActor ? 0.f : std::clamp(State.EnemyScale - Monster.GetVisualScale(), -.6f, .6f) * Profile.CharacterSizeCourage;
	return Confidence - RankPressure - RelativeSize + EnemyWeakness + State.ResolveBias;
}

// The resume threshold must be attainable without receiving external support or growing in size.
float CELComponentBase::CharacterResumeResolve(const SEncounter& State) const
{
	const float RestedResolve = CharacterResolve(State) + CharacterShock - std::min(0.f, State.Experience);
	return std::min(Profile.CharacterResumeThreshold, std::max(Profile.CharacterRetreatThreshold, RestedResolve - .5f));
}

bool CELComponentBase::CanEngage(const SELObservation& Observation)
{
	if (!Profile.HasCharacter)
	{
		return true;
	}
	if (Profile.IsPassive)
	{
		return false;
	}
	CEntityAlive* Target = ExtendedEntityes::FindEntity(Observation.TargetIdentifier);
	if (!Target || !Adapter->IsEnemy(*Target))
	{
		return false;
	}
	const u32 Now = Device.dwTimeGlobal;
	SEncounter& State = Encounter(*Target);
	// Sampling takes place once per encounter, never once per sight update or candidate scan.
	const bool Provoked = s32(State.ProvokedUntil - Now) > 0 || (ProvokedTarget == Target->ID() && s32(ProvokedUntil - Now) > 0);
	const bool Berserk = s32(Adapter->Monster().CorneredBerserkUntil - Now) > 0;
	CBaseMonster* Other = smart_cast<CBaseMonster*>(Target);
	const bool UnderAttack = Other && Other->EnemyMan.get_enemy() == &Adapter->Monster() && Adapter->Monster().Position().distance_to_sqr(Observation.Position) < _sqr(Profile.CharacterSafeDistance);
	const bool RecentHit = CELSharedTargets::ThreatPriority(Observation, *Adapter) == 3;
	const auto Report = [&](bool Accepted, const char* Reason)
	{
		State.HasCombatDecision = true;
		State.CombatDecisionAt = Now;
		State.CombatAccepted = Accepted;
		State.CombatReason = Reason;
		if (!Accepted || State.Withdrawn || State.Fleeing)
		{
			Hearing.SuppressInterest(Target->ID());
		}
		if (CELSharedDebug::Instance().CanLogCombat(Adapter->Monster().Position()) && s32(Now - NextCandidateTrace) >= 0)
		{
			NextCandidateTrace = Now + 2000;
			CELSharedDebug::Instance().LogCombat(Adapter->Monster().ID(), "[monsters_debug] CANDIDATE t=%u id=%u candidate=%u accepted=%d reason=%s aggression=%.1f roll=%.1f resolve=%.1f experience=%.1f engaged=%d withdrawn=%d provoked=%d under_attack=%d resume_resolve=%.1f", Now, Adapter->Monster().ID(), Target->ID(), int(Accepted), Reason, Profile.CharacterAggression, State.Initiative, CharacterResolve(State), State.Experience, int(State.Engaged), int(State.Withdrawn), int(Provoked), int(UnderAttack), CharacterResumeResolve(State));
		}
		return Accepted;
	};
	if (State.Withdrawn && !RecentHit && !UnderAttack && !Berserk)
	{
		if (s32(Now - State.DecisionUntil) < 0 || CharacterResolve(State) < CharacterResumeResolve(State))
		{
			// Recognition remains active during withdrawal; a close visible threat outranks investigation.
			if (Adapter->CanSee(*Target))
			{
				return Report(true, "withdrawing_from_visible_enemy");
			}
			// Losing sight briefly does not turn a known dangerous opponent into an ordinary sound task.
			if (Now - State.LastContactAt <= Profile.SearchMax && s32(Observation.ExpiresAt - Now) > 0)
			{
				return Report(true, "following_last_known_threat");
			}
			return Report(false, s32(Now - State.DecisionUntil) < 0 ? "withdrawal_hold" : "confidence_not_recovered");
		}
		State.Withdrawn = false;
	}
	if (State.Engaged || Provoked || UnderAttack || Berserk || (Profile.CharacterAggression > 0.f && s32(State.AssistanceUntil - Now) > 0))
	{
		return Report(true, Berserk ? "berserk" : Provoked || UnderAttack ? "self_defence" : s32(State.AssistanceUntil - Now) > 0 ? "ally_in_combat" : "committed_target");
	}
	const float Resolve = CharacterResolve(State);
	if (Resolve < Profile.CharacterRetreatThreshold && Adapter->CanSee(*Target) &&
		Adapter->Monster().Position().distance_to_sqr(Target->Position()) < _sqr(Profile.CharacterSafeDistance))
	{
		// Refusing combat is not indifference: keep a perceived nearby enemy as an avoidance target.
		// Withdrawal prevents selection from committing an attack and suppresses attraction to this source.
		State.Withdrawn = true;
		State.DecisionUntil = Now + Profile.CharacterDecisionHold;
		State.NextDecisionAt = Now + Profile.CharacterReevaluate;
		return Report(true, "avoiding_visible_enemy_low_courage");
	}
	const bool Allowed = (Profile.CharacterAggression >= 100.f || Profile.CharacterAggression > State.Initiative) && Resolve >= Profile.CharacterRetreatThreshold;
	return Report(Allowed, Allowed ? "initiate" : Resolve < Profile.CharacterRetreatThreshold ? "risk_too_high" : "initiative_roll");
}

void CELComponentBase::RecordCombatHit(CEntityAlive& Opponent, float HealthFraction, bool Received)
{
	if (Profile.HasCharacter && HasAttached && !Profile.IsPassive && Received)
	{
		CBaseMonster* Other = smart_cast<CBaseMonster*>(&Opponent);
		if (!Other || !CELSharedEnemySharing::AreFriends(Adapter->Monster(), *Other))
		{
			Adapter->Monster().ProvokedEnemyIdentifier = Opponent.ID();
			Adapter->Monster().ProvokedEnemyUntil = Device.dwTimeGlobal + Profile.CharacterMemory;
		}
	}
	if (!Profile.HasCharacter || !HasAttached || !Adapter->IsEnemy(Opponent))
	{
		return;
	}
	SEncounter& State = Encounter(Opponent);
	State.LastContactAt = Device.dwTimeGlobal;
	State.Experience = std::clamp(State.Experience + HealthFraction * (Received ? -Profile.CharacterHitLoss : Profile.CharacterHitGain), -40.f, 20.f);
	if (Received)
	{
		State.ProvokedUntil = Device.dwTimeGlobal + Profile.CharacterMemory;
	}
	else
	{
		State.EnemyHealth = std::clamp(Opponent.GetfHealth() / std::max(EPS_L, Opponent.conditions().GetMaxHealth()), 0.f, 1.f);
	}
}

bool CELComponentBase::HasEnemyPressure()
{
	const SELObservation* Known = Memory.Find(TargetIdentifier);
	CEntityAlive* Target = Known ? ExtendedEntityes::FindEntity(TargetIdentifier) : nullptr;
	return Target && Adapter->IsEnemy(*Target) && (CELSharedTargets::ThreatPriority(*Known, *Adapter) == 3 ||
		(Adapter->CanSee(*Target) && Adapter->Monster().Position().distance_to_sqr(Known->Position) < _sqr(Profile.CharacterSafeDistance)));
}

bool CELComponentBase::IsCharacterCounterAttacking() const
{
	return CharacterCounterTarget == TargetIdentifier && s32(CharacterCounterAttackUntil - Device.dwTimeGlobal) > 0;
}

bool CELComponentBase::ShouldFleeCharacter(const SELObservation* Observation)
{
	IsWatchingThreat = false;
	CEntityAlive* Target = Observation ? ExtendedEntityes::FindEntity(Observation->TargetIdentifier) : nullptr;
	if (!Target || !Adapter->IsEnemy(*Target))
	{
		IsFleeingPredator = IsFleeingPredator && !Target && Device.dwTimeGlobal - LastFearAt < Profile.CharacterDecisionHold && Adapter->Monster().Position().distance_to_sqr(LastFearPosition) < _sqr(Profile.CharacterSafeDistance);
		return IsFleeingPredator;
	}
	CBaseMonster& Monster = Adapter->Monster();
	const u32 Now = Device.dwTimeGlobal;
	SEncounter& State = Encounter(*Target);
	const float Resolve = CharacterResolve(State);
	const bool WasFleeing = State.Fleeing;
	// Being caught repeatedly is failure to escape even when locomotion itself still makes progress.
	const bool Perceived = Adapter->CanSee(*Target);
	const float ContactRange = Monster.MeleeChecker.get_max_distance() + Target->Radius() + Monster.ExtendedMeleeHitTolerance;
	const bool ClosePressure = (State.Fleeing || State.Withdrawn) && Perceived &&
		Monster.Position().distance_to_sqr(Target->Position()) <= _sqr(ContactRange);
	if (ClosePressure)
	{
		if (!State.ClosePressureSince || Now - State.LastClosePressureAt > Profile.CharacterContactGrace)
		{
			State.ClosePressureSince = Now;
		}
		State.LastClosePressureAt = Now;
	}
	else if (Now - State.LastClosePressureAt > Profile.CharacterContactGrace || (!State.Fleeing && !State.Withdrawn))
	{
		State.ClosePressureSince = 0;
	}
	if (s32(Monster.CorneredBerserkUntil - Now) <= 0 && ClosePressure && Profile.CharacterCornerPressureTime && State.ClosePressureSince &&
		Now - State.ClosePressureSince >= Profile.CharacterCornerPressureTime)
	{
		const float Health = Monster.conditions().GetHealth() / std::max(EPS_L, Monster.conditions().GetMaxHealth());
		const bool Berserk = Profile.CanCornerBerserk && Health < Profile.CornerBerserkHealth;
		State.Fleeing = State.Withdrawn = false;
		State.Engaged = true;
		State.ClosePressureSince = 0;
		State.DecisionUntil = Now + Profile.CharacterDecisionHold;
		State.NextDecisionAt = State.DecisionUntil;
		State.HasCombatDecision = State.CombatAccepted = true;
		State.CombatDecisionAt = Now;
		State.CombatReason = Berserk ? "cornered_berserk" : "cornered_counterattack";
		CharacterCounterTarget = Target->ID();
		CharacterCounterAttackUntil = Berserk ? 0u : State.DecisionUntil;
		IsFleeingPredator = false;
		if (Berserk)
		{
			Monster.CorneredBerserkUntil = Now + Profile.CornerBerserkDuration;
			Monster.CorneredBerserkDamage = Profile.CornerBerserkDamage;
			Monster.set_berserk();
		}
		PublishNativeMemory();
		Monster.EnemyMan.update();
		if (CELSharedDebug::Instance().CanLogCombat(Monster.Position()))
		{
			CELSharedDebug::Instance().LogCombat(Monster.ID(), "[monsters_debug] COUNTERATTACK t=%u id=%u target=%u reason=%s hp_fraction=%.3f contact_range=%.2f hold_ms=%u", Now, Monster.ID(), Target->ID(), Berserk ? "cornered_berserk" : "cornered_counterattack", Health, ContactRange, Berserk ? Profile.CornerBerserkDuration : Profile.CharacterDecisionHold);
		}
		return false;
	}
	const auto TryCounterAttack = [&]()
	{
		const float Roll = Perceived ? Random.randF(0.f, 1.f) : -1.f;
		const bool Chosen = Perceived && Roll < Profile.CharacterCounterChance;
		if (Chosen)
		{
			CharacterCounterTarget = Target->ID();
			CharacterCounterAttackUntil = Now + Profile.CharacterDecisionHold;
		}
		if (CELSharedDebug::Instance().CanLogCombat(Monster.Position()))
		{
			CELSharedDebug::Instance().LogCombat(Monster.ID(), "[monsters_debug] COUNTER_CHECK t=%u id=%u target=%u visible=%d chance=%.3f roll=%.3f chosen=%d hp_fraction=%.3f berserk_health=%.3f berserk_enabled=%d close_pressure_ms=%u required_ms=%u", Now, Monster.ID(), Target->ID(), int(Perceived), Profile.CharacterCounterChance, Roll, int(Chosen), Monster.conditions().GetHealth() / std::max(EPS_L, Monster.conditions().GetMaxHealth()), Profile.CornerBerserkHealth, int(Profile.CanCornerBerserk), State.ClosePressureSince ? Now - State.ClosePressureSince : 0u, Profile.CharacterCornerPressureTime);
		}
		return Chosen;
	};
	Courage = std::clamp(CharacterConfidence(State) * .01f, 0.f, 1.f);
	Monster.CharacterCourage = Courage;
	if (s32(Monster.CorneredBerserkUntil - Now) > 0)
	{
		State.Fleeing = State.Withdrawn = false;
		State.Engaged = true;
		State.ClosePressureSince = 0;
	}
	else if (State.Withdrawn)
	{
		if (s32(Now - State.DecisionUntil) >= 0 && s32(Now - State.NextDecisionAt) >= 0)
		{
			State.NextDecisionAt = Now + Profile.CharacterReevaluate;
			State.Fleeing = Resolve < CharacterResumeResolve(State) && !TryCounterAttack();
			State.DecisionUntil = Now + Profile.CharacterDecisionHold;
			if (!State.Fleeing)
			{
				State.Withdrawn = false;
				State.Engaged = true;
			}
		}
		else
		{
			State.Fleeing = true;
		}
	}
	else if (State.Fleeing && Monster.Position().distance_to_sqr(Observation->Position) >= _sqr(Profile.CharacterSafeDistance) && CELSharedTargets::ThreatPriority(*Observation, *Adapter) != 3)
	{
		// Reaching safety ends this chase. New pressure may force another defensive response.
		State.Fleeing = false;
		State.Engaged = false;
		Hearing.SuppressInterest(Target->ID());
		State.Withdrawn = true;
		State.DecisionUntil = Now + Profile.CharacterDecisionHold;
		IsFleeingPredator = false;
		IsWatchingThreat = Perceived;
		return true;
	}
	else if (s32(Now - State.NextDecisionAt) >= 0 && s32(Now - State.DecisionUntil) >= 0)
	{
		State.NextDecisionAt = Now + Profile.CharacterReevaluate;
		const bool WantsRetreat = Resolve < (State.Fleeing ? CharacterResumeResolve(State) : Profile.CharacterRetreatThreshold);
		if (WantsRetreat != State.Fleeing || State.Fleeing)
		{
			// A frightened animal can still choose a counterattack; hold it long enough to act.
			State.Fleeing = WantsRetreat && !TryCounterAttack();
			State.DecisionUntil = Now + Profile.CharacterDecisionHold;
		}
	}
	if (WasFleeing && !State.Fleeing)
	{
		State.Withdrawn = false;
		State.Engaged = true;
	}
	IsFleeingPredator = State.Fleeing;
	if (State.Fleeing)
	{
		Hearing.SuppressInterest(Target->ID());
		if (!WasFleeing)
		{
			Sharing.RequestHelp(Monster, Profile);
		}
		LastFearPosition = Observation->Position;
		LastFearAt = Now;
		IsWatchingThreat = (!Perceived && State.Withdrawn) || (Perceived && Monster.Position().distance_to_sqr(Target->Position()) >= _sqr(Profile.CharacterWatchDistance * Profile.CharacterWatchRetreatRatio) &&
			CELSharedTargets::ThreatPriority(*Observation, *Adapter) != 3);
	}
	return State.Fleeing;
}

void CELComponentBase::TraceCombat(EELControl Control)
{
	CBaseMonster& Owner = Adapter->Monster();
	const u32 Now = Device.dwTimeGlobal;
	if (!CELSharedDebug::Instance().CanLogCombat(Owner.Position()) || s32(Now - NextCombatSample) < 0) { return; }
	NextCombatSample = Now + 500;
	CEntityAlive* Target = ExtendedEntityes::FindEntity(TargetIdentifier);
	const CEntityAlive* Native = Owner.EnemyMan.get_enemy();
	const SELObservation* Known = Memory.Find(TargetIdentifier);
	const bool Visible = Target && Adapter->CanSee(*Target);
	SELDebugRecord Details;
	Details.OwnerIdentifier = Owner.ID();
	Details.Position = Owner.Position();
	Details.Speed = Owner.movement().speed();
	Details.Mode = "base";
	Details.State = "unclassified";
	FillCombatDebug(Details);
	FillDebugRecord(Details);
	string512 State;
	xr_sprintf(State, "%u:%u:%u:%u:%u:%u:%u:%u", TargetIdentifier, Native ? Native->ID() : u16(-1), u32(Control), u32(Owner.anim().m_tAction), u32(Owner.anim().GetCurAnim()), u32(IsFleeingPredator), u32(IsFieldRecovering), u32(Visible));
	xr_string Signature = State;
	Signature += Details.State.c_str();
	Signature += Owner.NativeMeleeReason;
	if (LastCombatState == Signature && Now - LastCombatLog < 2000) { return; }
	LastCombatState = Signature;
	LastCombatLog = Now;
	CELSharedDebug::Instance().LogCombat(Owner.ID(), "[monsters_debug] STATE t=%u id=%u section=%s scale=%.2f hp=%.3f target=%u target_section=%s native=%u control=%s can_control=%d visible=%d source=%d distance=%.2f melee_max=%.2f action=%u motion=%u native_state=%u courage=%.1f pack=%.2f shock=%.1f flee=%d field=%d noise_panic=%d investigate=%d pos=%.2f,%.2f,%.2f",
		Now, Owner.ID(), *Owner.cNameSect(), Owner.GetVisualScale(), Owner.GetfHealth(), TargetIdentifier, Target ? *Target->cNameSect() : "none", Native ? Native->ID() : u16(-1), Control == EELControl::Native ? "native" : "extended", int(Adapter->CanControl()), int(Visible), Known ? int(Known->Source) : -1, Target ? Owner.Position().distance_to(Target->Position()) : -1.f, Owner.MeleeChecker.get_max_distance(), u32(Owner.anim().m_tAction), u32(Owner.anim().GetCurAnim()), Owner.StateMan ? u32(Owner.StateMan->get_state_type()) : u32(-1), Courage * 100.f, PackCourage, CharacterShock, int(IsFleeingPredator), int(IsFieldRecovering), int(s32(Hearing.PanicUntil - Now) > 0), int(s32(Hearing.InvestigateUntil - Now) > 0), Owner.Position().x, Owner.Position().y, Owner.Position().z);
	if (CActor* Player = Actor())
	{
		CELSharedDebug::Instance().LogCombat(Owner.ID(), "[monsters_debug] ACTOR_FILTER t=%u id=%u actor=%u ignored=%d alive=%d enemy=%d owner_team=%u actor_team=%u relation=%u distance=%.2f native_visible=%d", Now, Owner.ID(), Player->ID(), int(ExtendedEntityes::IsIgnored(*Player)), int(Player->g_Alive()), int(Adapter->IsEnemy(*Player)), u32(Owner.g_Team()), u32(Player->g_Team()), u32(Owner.tfGetRelationType(Player)), Owner.Position().distance_to(Player->Position()), int(Owner.EnemyMan.see_enemy_now(Player)));
	}
	CELSharedDebug::Instance().LogCombat(Owner.ID(), "[monsters_debug] REASON t=%u id=%u mode=%s reason=%s native_substate=%u native_decision=%s melee_check=%s melee_age_ms=%u hit_attempt_age_ms=%u hit_attempt_success=%d", Now, Owner.ID(), Details.Mode.c_str(), Details.State.c_str(), Owner.NativeAttackSubstate, Owner.NativeAttackDecision, Owner.NativeMeleeReason, Owner.NativeMeleeCheckedAt ? Now - Owner.NativeMeleeCheckedAt : u32(-1), Owner.NativeHitAttemptAt ? Now - Owner.NativeHitAttemptAt : u32(-1), int(Owner.NativeHitAttemptSuccess));
	CELSharedDebug::Instance().LogCombat(Owner.ID(), "[monsters_debug] MELEE_RESULT t=%u id=%u reason=%s distance=%.2f range=%.2f corrected=%d", Now, Owner.ID(), Owner.NativeHitReason, Owner.NativeHitDistance, Owner.NativeHitRange, int(Owner.NativeHitCorrected));
	if (Known)
	{
		CELSharedDebug::Instance().LogCombat(Owner.ID(), "[monsters_debug] OBSERVATION t=%u id=%u target=%u age_ms=%u known=%.2f,%.2f,%.2f", Now, Owner.ID(), TargetIdentifier, Now - Known->ObservedAt, Known->Position.x, Known->Position.y, Known->Position.z);
	}
	for (const shared_str& Detail : Details.Senses)
	{
		CELSharedDebug::Instance().LogCombat(Owner.ID(), "[monsters_debug] DETAIL t=%u id=%u %s", Now, Owner.ID(), Detail.c_str());
	}
}

// Rejoin only remembered allies, within communication reach and for a finite time.
bool CELComponentBase::RegroupPoint(Fvector& Point, bool& Wait)
{
	CBaseMonster& Owner = Adapter->Monster();
	if (!Profile.HasCharacter || Profile.IsPassive || !Profile.UsesPackCourage || s32(RallyUntil - Device.dwTimeGlobal) <= 0)
	{
		RallyMembers.clear();
		return false;
	}
	CBaseMonster* Leader = &Owner;
	bool Scattered = false;
	for (auto It = RallyMembers.begin(); It != RallyMembers.end();)
	{
		CEntityAlive* Entity = ExtendedEntityes::FindEntity(It->first);
		CBaseMonster* Ally = Entity ? smart_cast<CBaseMonster*>(Entity) : nullptr;
		if (!Ally || !Ally->g_Alive() || Ally->getDestroy() || ExtendedEntityes::OwnerGeneration(It->first) != It->second)
		{
			It = RallyMembers.erase(It);
			continue;
		}
		if (Ally->ID() < Leader->ID()) { Leader = Ally; }
		Scattered |= Owner.Position().distance_to_sqr(Ally->Position()) > _sqr(Profile.PackRegroupDistance);
		++It;
	}
	if (!Scattered) { RallyMembers.clear(); return false; }
	if (Leader->EnemyMan.get_enemy()) { return false; }
	Point = Leader->Position();
	if (Owner.Position().distance_to_sqr(Point) > _sqr(Profile.SharingRadius * std::max(2.f, Profile.CallSharingRatio))) { return false; }
	Wait = Leader == &Owner || Owner.Position().distance_to_sqr(Point) <= _sqr(Profile.PackRegroupDistance);
	return true;
}
