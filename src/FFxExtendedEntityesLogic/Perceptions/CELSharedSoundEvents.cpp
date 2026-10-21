#include "FFxExtendedEntityesLogic/stdafx.h"
#include <cmath>
#include <algorithm>
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedSoundEvents.h"
#include "FFxExtendedEntityesLogic/EntryPoint.h"
#include "FFxExtendedEntityesLogic/CELSharedSettings.h"
#include "FFxExtendedEntityesLogic/CoreComponent/TExtendedEntityesLogic.h"
#include "xrGame/ai/monsters/basemonster/base_monster.h"
#include "xrGame/Actor.h"
#include "FFxExtendedEntityesLogic/Debug/Debug.h"
#include "xrGame/InventoryOwner.h"
#include "xrGame/Inventory.h"
#include "xrGame/Weapon.h"
#include "xrGame/WeaponMagazinedWGrenade.h"
#include "xrSound/ai_sounds.h"

namespace ExtendedEntityesSoundEvents
{
struct SPending
{
	SELSoundEvent Event;
	u32 ReceiverCursor = 0;
	xr_vector<float> DebugRadii;
};
xrCriticalSection EventLock;
xr_vector<SPending> Pending;
xr_vector<SELSoundEvent> Recent;
} // namespace ExtendedEntityesSoundEvents

// Дополняет звуковое событие сведениями об источнике и режиме оружия.
void CELSharedSoundEvents::Describe(const CObject* SourceObject, SELSoundEvent& Event)
{
	CObject* OwnerObject = const_cast<CObject*>(SourceObject);
	CWeapon* Weapon = OwnerObject ? smart_cast<CWeapon*>(OwnerObject) : nullptr;
	u32 ParentDepth;
	for (ParentDepth = 0; OwnerObject && OwnerObject->H_Parent() && ParentDepth < 4; ++ParentDepth)
	{
		OwnerObject = OwnerObject->H_Parent();
	}
	if (OwnerObject)
	{
		Event.SourceIdentifier = OwnerObject->ID();
	}
	CActor* Actor = OwnerObject ? OwnerObject->cast_actor() : nullptr;
	Event.IsActorSound = Actor != nullptr;
	if (Actor)
	{
		Event.ActorPosition = Actor->Position();
	}
	if (Actor && (Event.SoundType & SOUND_TYPE_STEP))
	{
		const u32 State = Actor->mstate_real;
		if (State & ACTOR_DEFS::mcCrouch)
		{
			Event.Movement = State & ACTOR_DEFS::mcAccel ? EELMovementNoise::LowCrouch : EELMovementNoise::HighCrouch;
		}
		else if (State & ACTOR_DEFS::mcSprint)
		{
			Event.Movement = EELMovementNoise::Sprint;
		}
		else
		{
			Event.Movement = isActorAccelerated(State, Actor->IsZoomAimingMode()) ? EELMovementNoise::Run : EELMovementNoise::Walk;
		}
	}
	if (!(Event.SoundType & SOUND_TYPE_SHOOTING) || Event.Kind == EELSoundKind::Explosion)
	{
		return;
	}
	CInventoryOwner* InventoryOwner = OwnerObject ? smart_cast<CInventoryOwner*>(OwnerObject) : nullptr;
	if (!Weapon && InventoryOwner && InventoryOwner->inventory().ActiveItem())
	{
		Weapon = InventoryOwner->inventory().ActiveItem()->cast_weapon();
	}
	if (!Weapon)
	{
		return;
	}
	Event.IsSuppressed = Weapon->IsSilencerAttached();
	const char* Section = Weapon->cNameSect().c_str();
	const char* Kind = (pSettings)->read_if_exists<str_c>(Section, "weapon_class", "");
	const int WeaponType = (pSettings)->read_if_exists<s32>(Section, "ef_weapon_type", 8);
	CWeaponMagazinedWGrenade* Launcher = Weapon->cast_weapon_magazined_w_grenade();
	if ((Launcher && Launcher->IsGrenadeMode()) || WeaponType == 12)
	{
		Event.WeaponKind = EELWeaponKind::Launcher;
	}
	else if (!xr_strcmp(Kind, "sniper_rifle") || WeaponType == 11)
	{
		Event.WeaponKind = EELWeaponKind::Sniper;
	}
	else if (!xr_strcmp(Kind, "shotgun") || WeaponType == 9)
	{
		Event.WeaponKind = EELWeaponKind::Shotgun;
	}
	else if (WeaponType == 5)
	{
		Event.WeaponKind = EELWeaponKind::Pistol;
	}
}

// Добавляет или обновляет запись в общей очереди подсистемы.
void CELSharedSoundEvents::Publish(const CObject* SourceObject, u32 SoundType, const Fvector& Position, float Delay, EELSoundKind Kind)
{
	if (!CELSharedSettings::IsEnabled() || SoundType == u32(-1) || SoundType == 0 || !_valid(Position))
	{
		return;
	}
	ExtendedEntityesSoundEvents::SPending PendingEvent;
	PendingEvent.Event.Position = Position;
	PendingEvent.Event.SoundType = SoundType;
	PendingEvent.Event.Power = 1.f;
	PendingEvent.Event.Kind = Kind;
	PendingEvent.Event.OccurredAt = Device.dwTimeGlobal + u32(std::max(0.f, std::min(Delay, 5.f)) * 1000.f);
	Describe(SourceObject, PendingEvent.Event);
	if (HasPublished(PendingEvent.Event))
	{
		return;
	}
	xrCriticalSectionGuard Guard(ExtendedEntityesSoundEvents::EventLock);
	if (ExtendedEntityesSoundEvents::Pending.size() >= 128)
	{
		return;
	}
	ExtendedEntityesSoundEvents::Pending.push_back(PendingEvent);
	if (ExtendedEntityesSoundEvents::Recent.size() >= 128)
	{
		ExtendedEntityesSoundEvents::Recent.erase(ExtendedEntityesSoundEvents::Recent.begin());
	}
	ExtendedEntityesSoundEvents::Recent.push_back(PendingEvent.Event);
}

// Проверяет, опубликовано ли уже соответствующее игровое звуковое событие.
bool CELSharedSoundEvents::HasPublished(const SELSoundEvent& Event)
{
	xrCriticalSectionGuard Guard(ExtendedEntityesSoundEvents::EventLock);
	xr_vector<SELSoundEvent>::const_iterator EventIterator;
	for (EventIterator = ExtendedEntityesSoundEvents::Recent.begin(); EventIterator != ExtendedEntityesSoundEvents::Recent.end(); ++EventIterator)
	{
		if (std::abs(s32(Event.OccurredAt - EventIterator->OccurredAt)) > 100 || Event.Position.distance_to_sqr(EventIterator->Position) > .25f)
		{
			continue;
		}
		if (EventIterator->Kind == EELSoundKind::Explosion ||
			(Event.SourceIdentifier == EventIterator->SourceIdentifier && Event.SoundType == EventIterator->SoundType && Event.Kind == EventIterator->Kind))
		{
			return true;
		}
	}
	return false;
}

// Порционно доставляет готовые звуковые события зарегистрированным слушателям.
void CELSharedSoundEvents::Dispatch()
{
	xrCriticalSectionGuard Guard(ExtendedEntityesSoundEvents::EventLock);
	u32 RemainingDeliveries = 512;
	xr_vector<ExtendedEntityesSoundEvents::SPending>::iterator EventIterator = ExtendedEntityesSoundEvents::Pending.begin();
	const xr_vector<u16>& Receivers = ExtendedEntityes::Owners();
	CEntityAlive* ReceiverEntity;
	CBaseMonster* ReceiverMonster;
	while (EventIterator != ExtendedEntityesSoundEvents::Pending.end() && RemainingDeliveries)
	{
		if (s32(Device.dwTimeGlobal - EventIterator->Event.OccurredAt) < 0)
		{
			++EventIterator;
			continue;
		}
		if (Device.dwTimeGlobal - EventIterator->Event.OccurredAt > 2000)
		{
			EventIterator = ExtendedEntityesSoundEvents::Pending.erase(EventIterator);
			continue;
		}
		while (EventIterator->ReceiverCursor < Receivers.size() && RemainingDeliveries)
		{
			--RemainingDeliveries;
			ReceiverEntity = ExtendedEntityes::FindEntity(Receivers[EventIterator->ReceiverCursor++]);
			ReceiverMonster = ReceiverEntity ? smart_cast<CBaseMonster*>(ReceiverEntity) : nullptr;
			if (!ReceiverMonster || ReceiverMonster->ID() == EventIterator->Event.SourceIdentifier)
			{
				continue;
			}
			if (TExtendedEntityesLogic* Component = ReceiverMonster->GetComponent<TExtendedEntityesLogic>())
			{
				const SELSoundEvent& Sound = EventIterator->Event;
				const s32 DebugAge = s32(Device.dwTimeGlobal - Sound.OccurredAt);
				if (Sound.IsActorSound && DebugAge >= 0 && DebugAge < 250 && CELSharedDebug::Instance().IsDrawEnabled())
				{
					const SELPerceptionProfile& Profile = CELSharedSettings::Profile(*ReceiverMonster->cNameSect());
					const u32 Mask = ELNoiseTypeMask(CELSharedHearing::Classify(Sound));
					const float Radius = CELSharedHearing::HearingRadius(Sound, Profile);
					if (Sound.Power >= Profile.NoiseMinPower && ((Profile.InterestNoiseTypes | Profile.FearNoiseTypes) & Mask) && Radius > 0.f &&
						std::none_of(EventIterator->DebugRadii.begin(), EventIterator->DebugRadii.end(), [Radius](float Existing)
									 { return std::abs(Existing - Radius) < .01f; }))
					{
						EventIterator->DebugRadii.push_back(Radius);
						SELDebugEvent DebugEvent;
						DebugEvent.Kind = EELDebugEvent::SoundEmission;
						DebugEvent.OwnerIdentifier = Sound.SourceIdentifier;
						DebugEvent.TargetIdentifier = u16(EventIterator->DebugRadii.size());
						DebugEvent.Finish = Sound.ActorPosition;
						DebugEvent.Finish.y += .3f;
						DebugEvent.Radius = Radius;
						DebugEvent.Label = CELSharedPerceptionProfiles::NoiseName(CELSharedHearing::Classify(Sound));
						CELSharedDebug::Instance().PublishEvent(std::move(DebugEvent), 250u - u32(DebugAge));
					}
				}
				Component->Hear(Sound);
			}
		}
		if (EventIterator->ReceiverCursor == Receivers.size())
		{
			EventIterator = ExtendedEntityesSoundEvents::Pending.erase(EventIterator);
		}
		else
		{
			break;
		}
	}
}

// Сбрасывает накопленные данные и незавершённую работу подсистемы.
void CELSharedSoundEvents::Clear()
{
	xrCriticalSectionGuard Guard(ExtendedEntityesSoundEvents::EventLock);
	ExtendedEntityesSoundEvents::Pending.clear();
	ExtendedEntityesSoundEvents::Recent.clear();
}
