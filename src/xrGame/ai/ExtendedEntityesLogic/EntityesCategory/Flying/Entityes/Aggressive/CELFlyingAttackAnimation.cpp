#include "stdafx.h"
#include "ai/ExtendedEntityesLogic/EntityesCategory/Flying/Entityes/Aggressive/CELFlyingAttackAnimation.h"
#include "ai/ExtendedEntityesLogic/EntityesCategory/Flying/CELFlyingSettings.h"
#include "ai/ExtendedEntityesLogic/Interfaces/IELAdapter.h"
#include "ai/monsters/basemonster/base_monster.h"
#include "../Include/xrRender/KinematicsAnimated.h"
#include "ai/ExtendedEntityesLogic/EntityesCategory/Flying/CELFlyingEntityBase.h"
#include "ai/ExtendedEntityesLogic/EntityesCategory/Flying/CELFlyingAnimationCycles.h"

// Связывает выбор анимаций боевого летуна с необходимыми службами и состоянием владельца.
CELFlyingAttackAnimation::CELFlyingAttackAnimation(IELAdapter& OwnerAdapter, const SELFlyingSettings& FlightSettings) : Adapter(OwnerAdapter), Settings(FlightSettings) {}
// Выбирает существующий цикл модели с безопасной заменой отсутствующего.
void CELFlyingAttackAnimation::Play(const shared_str& Cycle)
{
	if (CurrentCycle == Cycle || !Adapter.Monster().g_Alive())
	{
		return;
	}
	IKinematicsAnimated* Skeleton = Adapter.Monster().Visual() ? Adapter.Monster().Visual()->dcast_PKinematicsAnimated() : nullptr;
	if (!Skeleton)
	{
		return;
	}
	const MotionID Animation = Skeleton->ID_Cycle_Safe(Cycle.c_str());
	if (Animation.valid())
	{
		if (Cycle == Settings.FlightAnimation)
		{
			ExtendedEntityesFlightAnimation::PlayLoop(*Skeleton, Animation);
		}
		else
		{
			Skeleton->PlayCycle(Animation);
		}
	}
	else
	{
		Msg("! [ExtendedEntityesLogic] Нет полётной анимации '%s' в '%s'", Cycle.c_str(), Adapter.Monster().cNameSect().c_str());
		const MotionID Fallback = Skeleton->ID_Cycle_Safe(Settings.FlightAnimation.c_str());
		if (Fallback.valid())
		{
			ExtendedEntityesFlightAnimation::PlayLoop(*Skeleton, Fallback);
		}
	}
	CurrentCycle = Cycle;
}
// Включает анимацию полёта и снимает признак опоры.
void CELFlyingAttackAnimation::Fly()
{
	SetGrounded(false);
	Play(Settings.FlightAnimation);
}
// Включает настроенную анимацию воздушной атаки.
void CELFlyingAttackAnimation::Attack()
{
	SetGrounded(false);
	Play(Settings.AttackAnimation);
}
// Включает анимацию отдыха на опоре.
void CELFlyingAttackAnimation::Rest()
{
	SetGrounded(true);
	Play(Settings.IdleAnimation);
}
// Сбрасывает временное состояние перед новым действием.
void CELFlyingAttackAnimation::Reset()
{
	CurrentCycle = nullptr;
}

// Переключает наземное состояние и связанные визуальные поправки.
void CELFlyingAttackAnimation::SetGrounded(bool Enabled)
{
	CELFlyingEntityBase* Entity = smart_cast<CELFlyingEntityBase*>(&Adapter.Monster());
	if (Entity)
	{
		Entity->SetFlightGrounded(Enabled);
	}
}
