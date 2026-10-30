#include "FFxExtendedEntityesLogic/stdafx.h"
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

// Связывает общую память и восприятие владельца с необходимыми службами и состоянием владельца.
CELComponentBase::CELComponentBase(xr_unique_ptr<IELAdapter> OwnerAdapter, const SELPerceptionProfile& OwnerProfile) : Adapter(std::move(OwnerAdapter)), Profile(OwnerProfile) {}

// Подключает владельца к службам расширения и создаёт начальное состояние.
void CELComponentBase::Attach()
{
	if (HasAttached)
	{
		return;
	}
	ReceiveAfter = 0;
	HasAttached = true;
}

// Снимает управление расширения и освобождает регистрации владельца.
void CELComponentBase::Detach()
{
	if (!HasAttached)
	{
		return;
	}
	Adapter->ReleaseControl();
	Adapter->Monster().EnemyMemory.clear();
	Memory.Clear();
	Hearing.Clear();
	Sharing.Clear();
	VisualAcquisition.clear();
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
		Hearing.Investigate(Witnessed.Position, Witnessed.TargetIdentifier, Profile.NoiseSearchTime, Profile.NoiseDangerPriority + 1);
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
	const SELObservation* Selected = Profile.IsPassive ? nullptr : CELSharedTargets::Select(Memory, *Adapter, TargetIdentifier);
	if (Selected && CELSharedTargets::ThreatPriority(*Selected, *Adapter))
	{
		return Selected;
	}
	if (!Profile.IsPassive && s32(ProvokedUntil - Device.dwTimeGlobal) > 0)
	{
		CEntityAlive* Target = ExtendedEntityes::FindEntity(ProvokedTarget);
		const SELObservation* Known = Memory.Find(ProvokedTarget);
		if (Target && Known && Adapter->IsEnemy(*Target) && Adapter->CanPursue(*Target, Known->Position))
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
		if (!TargetEntity || !Adapter->IsEnemy(*TargetEntity))
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
	if (!HasAttached || Profile.IsPassive || (!Observation.HasParentalChannel && s32(Device.dwTimeGlobal - ReceiveAfter) < 0 && (LastSender != SourceMonster.ID() || LastReceivedAt != Device.dwTimeGlobal)) || !CELSharedEnemySharing::CanReceive(OwnerMonster, SourceMonster))
	{
		return false;
	}
	const SELPerceptionProfile& SourceProfile = CELSharedSettings::Profile(*SourceMonster.cNameSect());
	CEntityAlive* TargetEntity = ExtendedEntityes::FindEntity(Observation.TargetIdentifier);
	if (Observation.Kind == EELObservationKind::Enemy && (!TargetEntity || !Adapter->IsEnemy(*TargetEntity)))
	{
		return false;
	}
	const float Distance = OwnerMonster.Position().distance_to(SourceMonster.Position());
	if (!Observation.HasParentalChannel && (Distance > SourceProfile.SharingRadius || (!SourceProfile.IsWireless && Distance > SourceProfile.SharingRadius * (Observation.HasCall ? SourceProfile.CallSharingRatio : SourceProfile.CloseSharingRatio) && !Adapter->CanSee(SourceMonster))))
	{
		return false;
	}
	if (Observation.Kind != EELObservationKind::Enemy)
	{
		const bool IsInterest = Observation.Kind == EELObservationKind::Interest;
		if (!ExtendedEntityes::PerceptionBudget().TryTransfer() || !(IsInterest ? Hearing.RememberInterest(Observation, Profile) : CELSharedDeaths::Remember(OwnerMonster, Observation, Profile)))
		{
			return false;
		}
		ReceiveAfter = Device.dwTimeGlobal + Random.randI(Profile.SharingIntervalMin, Profile.SharingIntervalMax + 1);
		LastReceivedAt = Device.dwTimeGlobal;
		LastSender = SourceMonster.ID();
		return true;
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
	ReceiveAfter = Device.dwTimeGlobal + Random.randI(Profile.SharingIntervalMin, Profile.SharingIntervalMax + 1);
	LastReceivedAt = Device.dwTimeGlobal;
	LastSender = SourceMonster.ID();
	if (Observation.HasCall)
	{
		Sharing.OnReceivedCall(Profile.CallCooldown);
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
	const bool IsDeath = CELSharedHearing::Classify(SoundEvent) == EELNoiseType::Death;
	const bool WasHeard = Hearing.Hear(SoundEvent, Adapter->Monster().Position(), Profile, Adapter->Monster().ID());

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

// Use existing perception snapshots: debug drawing must not consume vision rays.
void CELComponentBase::FillDebugRecord(SELDebugRecord& Record) const
{
	CELSharedTargets::FillDebug(Record, Adapter->Monster().ID());
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
	string256 EnvironmentLabel;
	xr_sprintf(EnvironmentLabel, "ВОСПРИЯТИЕ: зрение ×%.2f | дождь %.2f | туман %.2f | нюх %s", CELSharedEnvironment::Vision(Profile), CELSharedEnvironment::Rain(), CELSharedEnvironment::Fog(), CELSharedEnvironment::CanSmell() ? "разрешён" : "отключён дождём");
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
	string512 Label;
	const SELObservation* Known = Memory.Find(Player->ID());
	if (ExtendedEntityes::IsIgnored(*Player))
	{
		Record.Senses.push_back("АКТОР ИГНОРИРУЕТСЯ: включён запрет реакции");
	}
	else if (HasActorVisualSample && Device.dwTimeGlobal - LastActorVisualAt <= 150)
	{
		xr_sprintf(Label, "ВИЖУ АКТОРА | подтверждение %u мс назад", Device.dwTimeGlobal - LastActorVisualAt);
		Record.Senses.push_back(Label);
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
	Stimulus.Kind = EELSoundKind::Light;
	Stimulus.SoundType = SOUND_TYPE_AMBIENT;
	Stimulus.Position = Point;
	Stimulus.Power = 1.f;
	Stimulus.OccurredAt = Device.dwTimeGlobal;
	Hearing.Hear(Stimulus, OwnerMonster.Position(), Profile, OwnerMonster.ID());
}
