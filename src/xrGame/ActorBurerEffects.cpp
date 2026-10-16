#include "StdAfx.h"
#include "ActorBurerEffects.h"
#include "Actor.h"
#include "ActorCondition.h"
#include "ActorEffector.h"
#include "Inventory.h"
#include "Weapon.h"
#include "ui/UIGameCustom.h"

namespace BurerCamera
{
static const char* const Files[] =
	{
		"camera_effects\\burer_gravi_attack_aim.anm",
		"camera_effects\\burer_snatch_the_weapon.anm",
		"camera_effects\\burer_gravi_attack_aim.anm",
		"camera_effects\\burer_antiaim_attack.anm"};
}

void CActorBurerEffects::Load()
{
	string_path Path;
	for (u32 Index = 0; Index < std::size(Available); ++Index)
	{
		Available[Index] = FS.exist(Path, "$game_anims$", BurerCamera::Files[Index]);
	}
}

void CActorBurerEffects::CameraRemoved()
{
	CameraId = 0;
}

bool CActorBurerEffects::Active(CActor& Actor) const
{
	return CameraId && Actor.Cameras().GetCamEffector(static_cast<ECamEffectorType>(CameraId));
}

bool CActorBurerEffects::CanAntiAim(CActor& Actor) const
{
	return !Active(Actor) || LastHit == EBurerHit::Risk;
}

bool CActorBurerEffects::Play(CActor& Actor, EBurerHit Hit, bool Blood)
{
	if (Blood)
	{
		if (auto* Ui = CurrentGameUI(); Ui && !Ui->GetCustomStatic("gunsl_bloodyscreen_main") && Ui->HasCustomStatic("gunsl_bloodyscreen_end"))
		{
			Ui->RemoveCustomStatic("gunsl_bloodyscreen_end");
			Ui->AddCustomStatic("gunsl_bloodyscreen_end", true);
		}
	}
	if (Active(Actor) && (LastHit != EBurerHit::Risk || Hit == EBurerHit::Risk))
	{
		return false;
	}
	const u32 Index = static_cast<u32>(Hit);
	if (!Available[Index])
	{
		return false;
	}
	if (CameraId)
	{
		Actor.Cameras().RemoveCamEffector(static_cast<ECamEffectorType>(CameraId));
	}
	CameraId = Actor.Cameras().RequestCamEffectorId();
	auto* Effector = new CAnimatorCamEffector();
	Effector->SetType(static_cast<ECamEffectorType>(CameraId));
	Effector->SetCyclic(false);
	Effector->Start(BurerCamera::Files[Index]);
	Effector->m_on_b_remove_callback = SBaseEffector::CB_ON_B_REMOVE(this, &CActorBurerEffects::CameraRemoved);
	Actor.Cameras().AddCamEffector(Effector);
	LastHit = Hit;
	return true;
}

void CActorBurerEffects::PrepareKnife(u32 Duration)
{
	KnifeUntil = Device.dwTimeGlobal + Duration;
}

bool CActorBurerEffects::KnifeWindow() const
{
	return Device.dwTimeGlobal < KnifeUntil;
}

void CActorBurerEffects::RestoreStamina(float Floor, float Health, bool Blocked)
{
	RestoreUntil = Device.dwTimeGlobal + 100;
	StaminaFloor = Floor;
	HealthThreshold = Health;
	RestoreBlocked = Blocked;
}

void CActorBurerEffects::Update(CActor& Actor)
{
	if (!Actor.g_Alive())
	{
		Reset(Actor);
		return;
	}
	if (RestoreBlocked || Device.dwTimeGlobal >= RestoreUntil || Actor.conditions().GetHealth() <= HealthThreshold)
	{
		return;
	}
	auto* Item = Actor.inventory().ActiveItem();
	auto* Weapon = Item ? Item->cast_weapon() : nullptr;
	bool Explosive = Weapon && (Weapon->cast_weapon_rpg7() || Weapon->cast_weapon_rg6() || Weapon->IsGrenadeMode());
	bool ExplosiveReady = Explosive && (Weapon->GetCurrentElapsed(Weapon->IsGrenadeMode()) + (Weapon->IsGrenadeMode() ? 0 : Weapon->GetAmmoChamberElapsed()) > 0 || (Weapon->m_iLastShotTime && Device.dwTimeGlobal - Weapon->m_iLastShotTime < 2000));
	if (Weapon && (Weapon->BurerSniper || ExplosiveReady))
	{
		return;
	}
	if (Actor.conditions().GetPower() < StaminaFloor)
	{
		Actor.conditions().SetPower(StaminaFloor);
	}
}

void CActorBurerEffects::Reset(CActor& Actor)
{
	if (CameraId)
	{
		Actor.Cameras().RemoveCamEffector(static_cast<ECamEffectorType>(CameraId));
	}
	CameraId = KnifeUntil = RestoreUntil = 0;
	RestoreBlocked = false;
}
