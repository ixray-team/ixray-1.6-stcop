#include "stdafx.h"
#include <algorithm>
#include "ai/ExtendedEntityesLogic/EntityesCategory/Flying/Entityes/Passive/Entityes/Crows/Behaviors/CELCrowFeedingBehavior.h"
#include "ai/ExtendedEntityesLogic/EntityesCategory/Flying/Entityes/Passive/Entityes/Crows/CELCrowAnimation.h"
#include "ai/ExtendedEntityesLogic/EntityesCategory/Flying/Entityes/Passive/Entityes/Crows/CELSharedCrowMemory.h"
#include "ai/ExtendedEntityesLogic/EntityesCategory/Flying/Navigation/CELFlyingGeometry.h"
#include "ai/ExtendedEntityesLogic/Interfaces/IELAdapter.h"
#include "ai/ExtendedEntityesLogic/EntryPoint.h"
#include "ai/monsters/basemonster/base_monster.h"

// Подготавливает схему к запуску и сбрасывает состояние предыдущего выполнения.
void CELCrowFeedingBehavior::Enter()
{
	CELCrowRestBehavior::Enter();
	NextPeck = Device.dwTimeGlobal + u32(Settings.Peck.Sample() * 1000.f);
}
// Засчитывает питание только после подтверждённого контакта клюва.
bool CELCrowFeedingBehavior::FeedingContact(Fvector& Contact)
{
	Fvector From;
	if (!Animation.RestingBeak(From))
	{
		return false;
	}
	if (State.Rest == EELCrowRest::Corpse)
	{
		CEntityAlive* Corpse = State.Corpse <= u16(-1) ? ExtendedEntityes::FindEntity(u16(State.Corpse)) : nullptr;
		return Corpse && !Corpse->g_Alive() && CELSharedCrowMemory::FoodContact(Adapter.Monster(), *Corpse, From, Contact) && From.distance_to_sqr(Contact) <= Settings.CorpseReach * Settings.CorpseReach;
	}
	if (State.Rest != EELCrowRest::Ground && State.Rest != EELCrowRest::Night)
	{
		return false;
	}
	From.y += .1f;
	Fvector Normal;
	return CELFlyingGeometry::Support(From, Settings.GroundOffset + .7f, Contact, Normal, &Adapter.Monster());
}

// Обновляет кормление вороны с проверкой контакта клюва и обрабатывает текущий результат выполнения.
void CELCrowFeedingBehavior::Update(float DeltaTime)
{
	CELCrowRestBehavior::Update(DeltaTime);
	if (Validation.State != EELBehaviorState::Running)
	{
		return;
	}
	Fvector Contact;
	Fvector ConfirmedContact;
	if (Animation.ConsumeContact(ConfirmedContact) && FeedingContact(Contact) && Contact.distance_to_sqr(ConfirmedContact) <= Settings.Pose.ContactDistance * Settings.Pose.ContactDistance)
	{
		State.Satiety = std::min(1.f, State.Satiety + (State.Rest == EELCrowRest::Corpse ? Settings.MeatGain : Settings.SeedGain));
	}
	if (StepProgress >= 1.f && !Animation.IsPecking() && s32(Device.dwTimeGlobal - NextPeck) >= 0)
	{
		if (FeedingContact(Contact))
		{
			Animation.BeginPeck(Contact);
		}
		NextPeck = Device.dwTimeGlobal + u32((State.Satiety >= Settings.Full ? Settings.SatedPeck : State.Rest == EELCrowRest::Corpse ? Settings.MeatPeck
																																	  : Settings.Peck)
												 .Sample() *
											 1000.f);
	}

	Validation.Reason = State.Rest == EELCrowRest::Corpse ? "Кормление у тела" : "Поиск семян";
}
// Возвращает состояние выполнения схемы, включая ожидание, завершение и сбои.
SELBehaviorValidation CELCrowFeedingBehavior::Validate() const
{
	if (State.Rest == EELCrowRest::Corpse)
	{
		CEntityAlive* Corpse = State.Corpse < u16(-1) ? ExtendedEntityes::FindEntity(u16(State.Corpse)) : nullptr;
		if (!Corpse || Corpse->g_Alive() || !CELSharedCrowMemory::Instance().HasFoodClaim(Adapter.Monster().ID(), u16(State.Corpse)))
		{
			return {EELBehaviorState::InvalidTarget, "Пища исчезла"};
		}
	}
	return CELCrowRestBehavior::Validate();
}
