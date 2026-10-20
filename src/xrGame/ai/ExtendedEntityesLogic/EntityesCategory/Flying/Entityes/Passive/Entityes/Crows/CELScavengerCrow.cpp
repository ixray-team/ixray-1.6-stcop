#include "stdafx.h"
#include <cmath>
#include <algorithm>
#include "ai/ExtendedEntityesLogic/EntityesCategory/Flying/Entityes/Passive/Entityes/Crows/CELScavengerCrow.h"
#include "ai/ExtendedEntityesLogic/EntityesCategory/Flying/Entityes/Passive/Entityes/Crows/CELCrowEntity.h"
#include "ai/ExtendedEntityesLogic/EntityesCategory/Flying/Entityes/Passive/Entityes/Crows/CELSharedCrowMemory.h"
#include "ai/ExtendedEntityesLogic/EntityesCategory/Flying/Navigation/CELFlyingGeometry.h"
#include "ai/ExtendedEntityesLogic/Perceptions/CELSharedSight.h"
#include "ai/ExtendedEntityesLogic/EntryPoint.h"
#include "ai_space.h"
#include "level_graph.h"
#include "Level.h"
#include "Actor.h"

// Связывает видовое поведение вороны с необходимыми службами и состоянием владельца.
CELScavengerCrow::CELScavengerCrow(xr_unique_ptr<IELAdapter> OwnerAdapter, const SELPerceptionProfile& OwnerProfile, CELCrowEntity& OwnerEntity) : CELFlyingNonAttackingBase(std::move(OwnerAdapter), OwnerProfile), Bird(OwnerEntity), Settings(CELCrowSettings::Read(Bird.cNameSect().c_str())), Persistence(Bird, Settings, State), CrowCruiseBehavior(*Adapter, FlightSettings, Movement, Settings, State), LandingBehavior(*Adapter, Movement, FlightSettings, Settings), RestBehavior(*Adapter, Movement, Bird.Animation(), Settings, State), FeedingBehavior(*Adapter, Movement, Bird.Animation(), Settings, State)
{
	BirdIdentifier = Bird.ID();
	State.Satiety = Settings.InitialSatiety.Sample();
	State.Altitude = Settings.Altitude.Sample();
	State.Remaining = Settings.Sky.Sample();
	State.Enabled = Settings.Enabled;
	Movement.SetSpeedScale(Settings.SpeedScale.Sample());
}
// Завершает видовое поведение вороны и освобождает связанные ресурсы.
CELScavengerCrow::~CELScavengerCrow()
{
	ClearBehavior();
	if (IsRegistered)
	{
		CELSharedCrowMemory::Instance().Unregister(BirdIdentifier);
	}
}
// Подключает владельца к службам расширения и создаёт начальное состояние.
void CELScavengerCrow::Attach()
{
	CELFlyingNonAttackingBase::Attach();
	CELSharedCrowMemory::Instance().Register(BirdIdentifier);
	IsRegistered = true;
	NextDecision = Device.dwTimeGlobal + u32(Settings.StartDelay.Sample() * 1000.f);
}
// Снимает управление расширения и освобождает регистрации владельца.
void CELScavengerCrow::Detach()
{
	ClearBehavior();
	Persistence.Sync(true);
	Bird.Animation().SetGrounded(false);
	CELSharedCrowMemory::Instance().Unregister(BirdIdentifier);
	IsRegistered = false;
	CELFlyingNonAttackingBase::Detach();
}
// Обновляет личные наблюдения владельца с учётом доступного бюджета.
void CELScavengerCrow::Observe()
{
	// The passive bird has no combat memory. Its bounded candidate requests are processed centrally.
	Hearing.PublishDebug(Bird.Position(), Profile, Bird.ID());
	Bird.EnemyMemory.clear();
	TargetIdentifier = u16(-1);
}
// Сохраняет или восстанавливает состояние через штатный интерфейс сериализации.
void CELScavengerCrow::Serialize(ISaveObject& Object)
{
	Persistence.Serialize(Object);
}
// Обрабатывает принятое звуковое событие по настроенным правилам реакции.
void CELScavengerCrow::Hear(const SELSoundEvent& Event)
{
	if (HasAttached && Settings.Enabled)
	{
		Perception.Hear(Bird, State, Settings, Event, Profile);
		if (Profile.InterestNoiseTypes & ELNoiseTypeMask(CELSharedHearing::Classify(Event)))
		{
			Hearing.Hear(Event, Bird.Position(), Profile, Bird.ID());
		}
	}
}
// Удаляет знания и ссылки, связанные с исчезнувшим объектом.
void CELScavengerCrow::RemoveObject(u16 ObjectIdentifier)
{
	if (Perception.ThreatIdentifier() == ObjectIdentifier)
	{
		State.Fear = 0.f;
		if (FlightMode == EELCrowFlightMode::Escape)
		{
			State.Remaining = 0.f;
		}
	}
	CELFlyingNonAttackingBase::RemoveObject(ObjectIdentifier);
	Perception.Remove(ObjectIdentifier);
	CELSharedCrowMemory::Instance().Invalidate(ObjectIdentifier);
	if (State.Corpse == ObjectIdentifier)
	{
		State.Corpse = u32(-1);
		State.Remaining = 0.f;
	}
	if (CircledFood == ObjectIdentifier)
	{
		CircledFood = u16(-1);
	}
}
// Переключает ворону на воздушную схему с выбранным назначением полёта.
void CELScavengerCrow::BeginFlight(EELCrowFlightMode Mode, float Duration)
{
	const bool IsTakingOff = Phase == EELCrowPhase::Rest;
	if (Mode == EELCrowFlightMode::Escape)
	{
		Movement.RetainRoutePrefix(std::max(FlightSettings.CollisionRadius * 2.f, Movement.GetSpeed() * FlightSettings.InterruptLookahead));
	}
	ClearBehavior();
	CELSharedCrowMemory::Instance().ReleaseFood(Bird.ID());
	Bird.Animation().SetGrounded(false);
	Phase = EELCrowPhase::Flight;
	FlightMode = Mode;
	State.Rest = EELCrowRest::None;
	State.Remaining = Duration;
	Fvector FlightAnchor = State.Danger;
	if (Mode == EELCrowFlightMode::Investigate)
	{
		FlightAnchor = Hearing.AttentionPosition;
	}
	else if (Mode == EELCrowFlightMode::Circle && State.Corpse < u16(-1))
	{
		FlightAnchor = LandingContact;
	}
	CrowCruiseBehavior.Configure(Mode, FlightAnchor, IsTakingOff);
	SelectFlight(CrowCruiseBehavior);
}
// Назначает проверяемую посадку и её конечную опору.
void CELScavengerCrow::BeginLanding(EELCrowRest Rest, const Fvector& Contact, const Fvector& Facing)
{
	ClearBehavior();
	Phase = EELCrowPhase::Landing;
	DesiredRest = Rest;
	LandingContact = Contact;
	State.Rest = EELCrowRest::None;
	LandingBehavior.Configure(Contact, Facing);
	BehaviorManager.Select(&LandingBehavior);
}
// Переключает ворону на наземный отдых или кормление.
void CELScavengerCrow::BeginRest(EELCrowRest Rest, bool Resume)
{
	ClearBehavior();
	Phase = EELCrowPhase::Rest;
	State.Rest = Rest;
	if (!Resume)
	{
		State.Remaining = (Rest == EELCrowRest::Corpse ? Settings.Corpse : Rest == EELCrowRest::Roof ? Settings.Roof
																	   : Rest == EELCrowRest::Night	 ? Settings.NightRest
																									 : Settings.Ground)
							  .Sample();
	}
	IsFeeding = Rest == EELCrowRest::Corpse || Rest == EELCrowRest::Ground || (Rest == EELCrowRest::Night && Random.randF() < Settings.NightForageChance);
	if (Rest == EELCrowRest::Night && !Resume)
	{
		State.Remaining = (IsFeeding ? Settings.NightForage : Settings.NightRest).Sample();
	}
	BehaviorManager.Select(IsFeeding ? static_cast<IELBehavior*>(&FeedingBehavior) : static_cast<IELBehavior*>(&RestBehavior));
}
// Обновляет видовое поведение вороны и обрабатывает текущий результат выполнения.
EELControl CELScavengerCrow::Update(float DeltaTime)
{
	if (!HasAttached || !Adapter->CanControl())
	{
		return CELFlying::Update(DeltaTime);
	}
	if (!Settings.Enabled)
	{
		if (FlightSettings.CanWander)
		{
			return CELFlyingNonAttackingBase::Update(DeltaTime);
		}
		ClearBehavior();
		Movement.Stop();
		return EELControl::Extended;
	}
	if (!Persistence.Restore())
	{
		return EELControl::Extended;
	}
	if (!HasStarted)
	{
		HasStarted = true;
		if (State.Rest != EELCrowRest::None)
		{
			BeginRest(State.Rest, true);
		}
		else
		{
			BeginFlight(EELCrowFlightMode::Roam, State.Remaining);
		}
	}
	DeltaTime = std::max(0.f, DeltaTime);
	State.Satiety = std::max(0.f, State.Satiety - DeltaTime / Settings.HungerTime);
	State.Fear = std::max(0.f, State.Fear - DeltaTime * Settings.FearDecay);
	State.Remaining -= DeltaTime;
	Perception.Observe(Bird, State, Settings, DeltaTime);
	if (State.Fear >= Settings.FearTrigger && (Phase != EELCrowPhase::Flight || FlightMode != EELCrowFlightMode::Escape))
	{
		BeginFlight(EELCrowFlightMode::Escape, Settings.Escape.Sample());
	}
	if (Phase == EELCrowPhase::Rest && !State.Rain.Active && !State.Night.Active && State.Fear < Settings.FearTrigger && s32(Device.dwTimeGlobal - Hearing.InvestigateUntil) < 0)
	{
		BeginFlight(EELCrowFlightMode::Investigate, float(Hearing.InvestigateUntil - Device.dwTimeGlobal) * .001f);
	}
	switch (Phase)
	{
		case EELCrowPhase::Flight: UpdateFlight(DeltaTime); break;
		case EELCrowPhase::Landing: UpdateLanding(DeltaTime); break;
		case EELCrowPhase::Rest: UpdateRest(DeltaTime); break;
	}
	Persistence.Sync();
	return EELControl::Extended;
}
// Обновляет активную воздушную схему и проверяет её состояние.
void CELScavengerCrow::UpdateFlight(float DeltaTime)
{
	BehaviorManager.Update(DeltaTime);
	if (FlightMode == EELCrowFlightMode::Escape)
	{
		if (State.Fear <= Settings.FearCalm && State.Remaining <= 0.f)
		{
			BeginFlight(EELCrowFlightMode::Roam, Settings.Safe.Sample());
		}
		return;
	}
	if (!BehaviorManager.IsActive(CrowCruiseBehavior))
	{
		BeginFlight(EELCrowFlightMode::Roam, Settings.Retry.Sample());
	}
	if (s32(Device.dwTimeGlobal - NextDecision) < 0)
	{
		return;
	}
	NextDecision = Device.dwTimeGlobal + u32(Settings.Retry.Sample() * 1000.f);
	if (State.Rain.Active)
	{
		TryPerch(EELCrowRest::Shelter);
		return;
	}
	if (State.Night.Active)
	{
		TryPerch(EELCrowRest::Night);
		return;
	}
	if (s32(Device.dwTimeGlobal - Hearing.InvestigateUntil) < 0)
	{
		if (FlightMode != EELCrowFlightMode::Investigate)
		{
			BeginFlight(EELCrowFlightMode::Investigate, float(Hearing.InvestigateUntil - Device.dwTimeGlobal) * .001f);
		}
		else
		{
			CrowCruiseBehavior.Configure(FlightMode, Hearing.AttentionPosition);
		}
		return;
	}
	if (FlightMode == EELCrowFlightMode::Investigate)
	{
		BeginFlight(EELCrowFlightMode::Roam, Settings.Sky.Sample());
		return;
	}
	if (State.Satiety <= Settings.Hungry && Perception.FoodIdentifier() != u16(-1))
	{
		if (FlightMode == EELCrowFlightMode::Circle && State.Remaining > 0.f)
		{
			return;
		}
		if (TryFood())
		{
			return;
		}
	}
	if (State.Remaining <= 0.f && FlightMode != EELCrowFlightMode::Circle)
	{
		TryPerch(Random.randF() < Settings.GroundChance ? EELCrowRest::Ground : EELCrowRest::Roof);
	}
}
// Проверяет ход посадки и обрабатывает её результат.
void CELScavengerCrow::UpdateLanding(float DeltaTime)
{
	BehaviorManager.Update(DeltaTime);
	if (LandingBehavior.HasLanded())
	{
		BeginRest(DesiredRest, false);
		return;
	}
	if (!BehaviorManager.IsActive(LandingBehavior))
	{
		CELSharedCrowMemory::Instance().RejectPerch(u16(State.Corpse), LandingContact);
		FailedPerch = LandingContact;
		FailedPerchUntil = Device.dwTimeGlobal + u32(Settings.RejectedPerchTime * 1000.f);
		BeginFlight(EELCrowFlightMode::Roam, Settings.Retry.Sample());
	}
}
// Обновляет таймер отдыха и условия его продолжения.
void CELScavengerCrow::UpdateRest(float DeltaTime)
{
	if ((State.Rain.Active && State.Rest != EELCrowRest::Shelter) || (State.Night.Active && State.Rest != EELCrowRest::Night && !State.Rain.Active))
	{
		BeginFlight(EELCrowFlightMode::Roam, Settings.Retry.Sample());
		return;
	}
	if (State.Rest == EELCrowRest::Shelter && State.Rain.Active)
	{
		State.Remaining = std::max(State.Remaining, Settings.Retry.Maximum);
	}
	if (State.Rest == EELCrowRest::Night && State.Night.Active && State.Remaining <= 0.f)
	{
		IsFeeding = !IsFeeding && State.Satiety < Settings.Full && Random.randF() < Settings.NightForageChance;
		State.Remaining = (IsFeeding ? Settings.NightForage : Settings.NightRest).Sample();
		BehaviorManager.Select(IsFeeding ? static_cast<IELBehavior*>(&FeedingBehavior) : static_cast<IELBehavior*>(&RestBehavior));
	}
	BehaviorManager.Update(DeltaTime);
	const bool HasRest = IsFeeding ? BehaviorManager.IsActive(FeedingBehavior) : BehaviorManager.IsActive(RestBehavior);
	if (!HasRest || BehaviorManager.GetValidation().State == EELBehaviorState::Completed)
	{
		BeginFlight(EELCrowFlightMode::Roam, Settings.Sky.Sample());
	}
}
// Выполняет покадровую часть активной схемы без повторного выбора поведения.
void CELScavengerCrow::FrameUpdate(float DeltaTime)
{
	CELFlying::FrameUpdate(DeltaTime);
	if (Phase == EELCrowPhase::Rest && HasAttached && Adapter->CanControl())
	{
		if (IsFeeding)
		{
			FeedingBehavior.FrameUpdate(DeltaTime);
		}
		else
		{
			RestBehavior.FrameUpdate(DeltaTime);
		}
	}
}
// Возвращает цель для отображения в общей отладке.
Fvector CELScavengerCrow::DebugGoal() const
{
	return Phase == EELCrowPhase::Flight ? CrowCruiseBehavior.Goal() : LandingContact;
}

// Выбирает доступную пищу и резервирует место кормления.
bool CELScavengerCrow::TryFood()
{
	const u16 FoodIdentifier = Perception.FoodIdentifier();
	CEntityAlive* Food = ExtendedEntityes::FindEntity(FoodIdentifier);
	if (!Food || Food->g_Alive())
	{
		return false;
	}
	if (!Perception.IsFoodSafe(Bird, Food->Position(), Settings.FoodSafeRadius))
	{
		return false;
	}
	State.Corpse = FoodIdentifier;
	if (CircledFood != FoodIdentifier && Random.randF() < Settings.CircleProbability)
	{
		CircledFood = FoodIdentifier;
		LandingContact = Food->Position();
		BeginFlight(EELCrowFlightMode::Circle, Settings.CircleTime.Sample());
		return true;
	}
	CircledFood = FoodIdentifier;
	const xr_vector<CELSharedCrowMemory::SPerch>* Perches = CELSharedCrowMemory::Instance().CorpsePerches(Bird, FoodIdentifier, Settings.CorpseGap, Settings.GroundOffset);
	if (!Perches)
	{
		return false;
	}
	xr_vector<CELSharedCrowMemory::SPerch>::const_iterator Perch;
	Fvector Facing;
	for (Perch = Perches->begin(); Perch != Perches->end(); ++Perch)
	{
		if (!Perch->Valid || (s32(Device.dwTimeGlobal - FailedPerchUntil) < 0 && Perch->Contact.distance_to_sqr(FailedPerch) < Settings.StepRadius * Settings.StepRadius))
		{
			continue;
		}
		if (!CELSharedCrowMemory::Instance().ClaimFood(Bird.ID(), FoodIdentifier, Perch->Contact, Settings.FoodSlots))
		{
			continue;
		}
		Facing.sub(Perch->Flesh, Perch->Contact).normalize_safe();
		BeginLanding(EELCrowRest::Corpse, Perch->Contact, Facing);
		return true;
	}
	return false;
}
// Ищет допустимое место отдыха, посадки или укрытия.
bool CELScavengerCrow::TryPerch(EELCrowRest Rest)
{
	const float Angle = float(Bird.ID()) * 2.39996323f + float(PerchAttempt++) * 2.39996323f;
	const float Radius = std::min(Settings.PerchMaxRadius, Settings.PerchRadius * (1.f + float(PerchAttempt % Settings.Attempts) / float(Settings.Attempts)));
	Fvector Candidate = Bird.Position();
	Candidate.x += std::cos(Angle) * Radius;
	Candidate.z += std::sin(Angle) * Radius;
	bool HasTree = false;
	Fvector Tree;
	if (Rest == EELCrowRest::Shelter && Settings.TreeShelter)
	{
		HasTree = CELSharedCrowMemory::Instance().NearestTree(Bird.Position(), Settings.ShelterRadius, Tree);
		if (HasTree)
		{
			Candidate = Tree;
			Candidate.x += std::cos(Angle) * Settings.TreeRadius * .5f;
			Candidate.z += std::sin(Angle) * Settings.TreeRadius * .5f;
		}
	}
	const Fbox& Bounds = Level().ObjectSpace.GetBoundingVolume();
	clamp(Candidate.x, Bounds.min.x + FlightSettings.CollisionRadius, Bounds.max.x - FlightSettings.CollisionRadius);
	clamp(Candidate.z, Bounds.min.z + FlightSettings.CollisionRadius, Bounds.max.z - FlightSettings.CollisionRadius);
	Candidate.y = Bounds.max.y + FlightSettings.CollisionRadius;
	Fvector Contact;
	Fvector Normal;
	if (!CELFlyingGeometry::Support(Candidate, Bounds.max.y - Bounds.min.y + FlightSettings.CollisionRadius * 2.f, Contact, Normal, &Bird) || Normal.y < std::cos(deg2rad(Settings.MaxSlope)))
	{
		return false;
	}
	if (s32(Device.dwTimeGlobal - FailedPerchUntil) < 0 && Contact.distance_to_sqr(FailedPerch) < Settings.StepRadius * Settings.StepRadius)
	{
		return false;
	}
	float GroundHeight = Contact.y;
	if (ai().get_level_graph() && ai().level_graph().valid_vertex_position(Contact))
	{
		const u32 Vertex = ai().level_graph().vertex_id(Contact);
		if (ai().level_graph().valid_vertex_id(Vertex))
		{
			GroundHeight = ai().level_graph().vertex_plane_y(Vertex, Contact.x, Contact.z);
		}
	}
	const float RoofHeight = Contact.y - GroundHeight;
	if (Rest == EELCrowRest::Ground && RoofHeight >= Settings.RoofMin)
	{
		return false;
	}
	if (Rest == EELCrowRest::Roof && (RoofHeight < Settings.RoofMin || RoofHeight > Settings.RoofMax))
	{
		return false;
	}
	Candidate = Contact;
	Candidate.y += Settings.ShelterClearance;
	if (Rest == EELCrowRest::Shelter && !(HasTree && Contact.y < Tree.y + Settings.TreeHeight) && !CELFlyingGeometry::HasCover(Candidate, Settings.ShelterHeight))
	{
		return false;
	}
	if (!CELSharedCrowMemory::Instance().ClaimPerch(Bird.ID(), Contact, Settings.PerchSpacing))
	{
		return false;
	}
	Fvector Facing = Bird.XFORM().k;
	BeginLanding(Rest, Contact, Facing);
	return true;
}
