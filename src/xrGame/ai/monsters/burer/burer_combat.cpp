#include "StdAfx.h"
#include "burer.h"
#include "burer_combat.h"
#include "../anti_aim_ability.h"
#include "../../../Actor.h"
#include "../../../ActorCondition.h"
#include "../../../ActorEffector.h"
#include "../../../Inventory.h"
#include "../../../Weapon.h"
#include "../../../WeaponKnife.h"
#include "../../../Grenade.h"
#include "../../../HUDManager.h"
#include "../../../Level.h"
#include "../../../CharacterPhysicsSupport.h"
#include "../../../PHMovementControl.h"
#include "../../../ui/UIGameCustom.h"
#include "../../../ui/UIActorMenuBase.h"
#include "../../../game_cl_single.h"

void CBurerCombat::Load()
{
	const char* Section = "gunslinger_base";
	Enabled = IsGameTypeSingle() && pSettings->section_exist(Section);
	if (!Enabled)
	{
		return;
	}
	SuperDistance = pSettings->read_if_exists<float>(Section, "burer_superstaminahit_dist", 15.f);
	StaminaHit = pSettings->read_if_exists<float>(Section, "burer_superstaminahit_value", 1000.f);
	StaminaFloor = pSettings->read_if_exists<float>(Section, "burer_superstaminahit_minimal_stamina", .05f);
	StaminaHealth = pSettings->read_if_exists<float>(Section, "burer_superstaminahit_minimal_stamina_health", .5f);
	HealthHit = pSettings->read_if_exists<float>(Section, "burer_superhealthhit_power", -1.f);
	HealthImpulse = pSettings->read_if_exists<float>(Section, "burer_superhealthhit_impulse", 10.f);
	HealthHitType = ALife::g_tfString2HitType(pSettings->read_if_exists<str_c>(Section, "burer_superhealthhit_type", "wound"));
	HideSkipProbability = pSettings->read_if_exists<float>(Section, "burer_superstaminahit_forceitemshideprob", 1.f);
	ConditionMin = pSettings->read_if_exists<float>(Section, "burer_superstaminahit_conditiondecmin", .05f);
	ConditionMax = pSettings->read_if_exists<float>(Section, "burer_superstaminahit_conditiondecmax", .1f);
	AntiAimDistance = pSettings->read_if_exists<float>(Section, "burer_forceantiaim_dist", 7.f);
	ShieldDistance = pSettings->read_if_exists<float>(Section, "burer_forceshield_dist", 3.f);
	RiskProbability = pSettings->read_if_exists<float>(Section, "burer_shielded_risky_factor", .02f);
	MinGrenadeTimer = pSettings->read_if_exists<u32>(Section, "burer_min_gren_timer", 1000);
	CriticalGrenadeTimer = pSettings->read_if_exists<u32>(Section, "burer_critical_gren_timer", 1000);
	ForceFireDelay = pSettings->read_if_exists<u32>(Section, "burer_forcetelefire_min_delta", 1000);
	GraviImpulse = pSettings->read_if_exists<float>(Section, "burer_gravi_impulse_for_actor", 1000.f);
	RiskImpulse = pSettings->read_if_exists<float>(Section, "burer_risky_impulse_for_actor", 1000.f);
	FireAllProbability = pSettings->read_if_exists<float>(Section, "burer_skip_fireall_probability", .7f);
	AimDelay = pSettings->read_if_exists<u32>(Section, "burer_aim_shield_delay_const", 500);
	AimRandomDelay = pSettings->read_if_exists<u32>(Section, "burer_aim_shield_delay_random", 1000);
	KnifeWindow = pSettings->read_if_exists<u32>(Section, "burer_knife_selfkick_window_time", 500);
}

void CBurerCombat::Reset()
{
	Context = {};
	ContextTime = u32(-1);
	AimUntil = AimDuration = RiskNext = TeleDecisionNext = 0;
	WasAimed = StopTele = false;
	Nearby.clear();
}

void CBurerCombat::Refresh(CBurer& Burer)
{
	if (!Enabled || ContextTime == Device.dwTimeGlobal)
	{
		return;
	}
	ContextTime = Device.dwTimeGlobal;
	Context = {};
	if (auto* AntiAim = Burer.get_anti_aim())
	{
		AntiAim->SetBurerThreat(false);
	}
	auto* Actor = ::Actor();
	if (!Actor || !Actor->g_Alive() || Burer.EnemyMan.get_enemy() != Actor)
	{
		WasAimed = false;
		AimUntil = 0;
		return;
	}
	Context.ActorEnemy = true;
	Context.Distance = Burer.Position().distance_to(Actor->Position());
	Fvector Head = Burer.Position();
	Head.y += 1.f;
	Fvector ToActor;
	ToActor.sub(Actor->Cameras().Position(), Head);
	float Range = ToActor.magnitude();
	ToActor.normalize_safe();
	collide::rq_result Hit;
	Context.Visible = !Level().ObjectSpace.RayPick(Head, ToActor, Range, collide::rqtBoth, Hit, &Burer) || Hit.O == Actor;
	Fvector ToBurer;
	ToBurer.sub(Head, Actor->Cameras().Position());
	ToBurer.normalize_safe();
	float AimCos = ToBurer.dotproduct(Actor->Cameras().Direction());
	Context.Facing = AimCos >= 0.f;
	auto* Item = Actor->inventory().ActiveItem();
	auto* Weapon = Item ? Item->cast_weapon() : nullptr;
	auto* Grenade = Item ? Item->cast_grenade() : nullptr;
	Context.HasItem = Item != nullptr;
	Context.Knife = Item && Item->cast_weapon_knife();
	Context.Consumable = Actor->HudAnimator()->ItemAnimator()->IsActive() && Actor->HudAnimator()->ItemAnimator()->GetSection() != Actor->m_sBurerKickAnimator;
	if (Actor->m_sQuickKickAnimator.size() && Actor->HudAnimator()->ItemAnimator()->IsActive() && Actor->HudAnimator()->ItemAnimator()->GetSection() == Actor->m_sQuickKickAnimator)
	{
		Context.Knife = true;
		Context.Consumable = false;
	}
	Context.Moving = Actor->AnyMove();
	if (auto* Ui = CurrentGameUI())
	{
		Context.Inventory = Ui->GetInventoryMenu() && Ui->GetInventoryMenu()->IsShown();
	}
	if (Weapon)
	{
		Context.Reloading = Weapon->GetState() == CWeapon::eReload;
		Context.Empty = Weapon->GetCurrentElapsed(Weapon->IsGrenadeMode()) + (Weapon->IsGrenadeMode() ? 0 : Weapon->GetAmmoChamberElapsed()) == 0;
		Context.Firing = Weapon->GetState() == CWeapon::eFire || Weapon->GetState() == CWeapon::eFire2;
		if (Weapon->m_iLastShotTime && Device.dwTimeGlobal - Weapon->m_iLastShotTime > 100)
		{
			Context.ReadyIn = std::max(0.f, Weapon->m_fLastRechargeTime - float(Device.dwTimeGlobal - Weapon->m_iLastShotTime) / 1000.f);
		}
		Context.Aiming = Weapon->IsZoomed();
		Context.Sniper = Weapon->BurerSniper;
		bool ExplosiveWeapon = Weapon->cast_weapon_rpg7() || Weapon->cast_weapon_rg6() || Weapon->IsGrenadeMode();
		Context.Explosive = ExplosiveWeapon && !Context.Empty;
		Context.RecentExplosion = ExplosiveWeapon && Weapon->m_iLastShotTime && Device.dwTimeGlobal - Weapon->m_iLastShotTime < 2000;
		Context.Dangerous = Context.Knife ? Context.Distance < AntiAimDistance : Weapon->WpnCanShoot() && !Context.Empty && !Context.Reloading && !Weapon->IsMisfire();
	}
	if (Context.Knife)
	{
		Context.Dangerous = Context.Distance < AntiAimDistance && !Actor->BurerEffects.KnifeWindow() && !Actor->DeathActionStarted;
	}
	Context.Throwable = Item && Item->cast_missile();
	Context.PreparedThrow = Grenade && (Grenade->GetState() == CGrenade::eReady || Grenade->GetState() == CGrenade::eThrowStart);
	const auto& Ray = HUD().GetCurrentRayQuery();
	Context.UnderExactAim = Context.Dangerous && Ray.O == &Burer;
	Context.UnderAim = Context.UnderExactAim || (Context.Dangerous && AimCos > .94f && Ray.range > Context.Distance * .5f);
	Context.UnderWideAim = Context.UnderExactAim || (Context.Dangerous && AimCos > .88f && Ray.range > Context.Distance * .5f);

	// One spatial query includes held grenades; no double counting candidates and controllers.
	Fvector Centre;
	Centre.add(Burer.Position(), Actor->Position()).mul(.5f);
	g_SpatialSpace->q_sphere(Nearby, 0, ESPATIAL_TYPE::COLLIDEABLE, Centre, Burer.tele_find_radius + Context.Distance * .5f);
	for (auto& Spatial : Nearby)
	{
		auto* Object = Spatial ? Spatial->dcast_CObject() : nullptr;
		auto* Gren = Object ? Object->cast_grenade() : nullptr;
		if (!Gren || Gren->getDestroy() || Gren->H_Parent() || Gren->IsExploding() || Gren->destroy_time() == u32(-1))
		{
			continue;
		}
		if (Gren->Position().distance_to(Burer.Position()) > Burer.tele_find_radius && Gren->Position().distance_to(Actor->Position()) > Burer.tele_find_radius)
		{
			continue;
		}
		++Context.Grenades;
		u32 Remaining = Gren->destroy_time() > Device.dwTimeGlobal ? Gren->destroy_time() - Device.dwTimeGlobal : 0;
		if (Gren->ContactParams().ExplosionOnKick)
		{
			Remaining = 1;
		}
		Context.GrenadeTimer = std::min(Context.GrenadeTimer, Remaining);
	}
	Nearby.clear();
	if (Context.UnderAim)
	{
		if (!WasAimed)
		{
			AimDuration = AimDelay + (AimRandomDelay ? Random.randI(AimRandomDelay) : 0);
		}
		AimUntil = Device.dwTimeGlobal + AimDuration;
	}
	WasAimed = Context.UnderAim;
	if (auto* AntiAim = Burer.get_anti_aim())
	{
		AntiAim->SetBurerThreat(Context.UnderAim || (Context.Visible && (Context.Distance < AntiAimDistance || Context.Inventory || Context.Consumable || Context.Sniper || Context.Explosive)));
	}
}

bool CBurerCombat::CloseProtection() const
{
	return Context.ActorEnemy && Context.Visible && Context.Distance < ShieldDistance && Context.Dangerous && Context.Facing;
}

bool CBurerCombat::TeleAllowed() const
{
	return !Context.ActorEnemy || Context.Grenades || Context.Visible || Context.UnderExactAim || Context.Firing || Context.PreparedThrow || Context.Consumable || Context.Moving;
}

bool CBurerCombat::CanTeleObject(CPhysicsShellHolder& Object) const
{
	if (!Enabled)
	{
		return true;
	}
	if (Level().IsSecretItem(Object.ID()))
	{
		return false;
	}
	auto* Actor = ::Actor();
	if (Actor && Object.cast_weapon() && Object.Position().distance_to(Actor->Position()) > 10.f)
	{
		return false;
	}
	auto* Grenade = Object.cast_grenade();
	if (!Grenade || Grenade->destroy_time() == u32(-1))
	{
		return true;
	}
	return !Grenade->ContactParams().ExplosionOnKick && Grenade->destroy_time() > Device.dwTimeGlobal && Grenade->destroy_time() - Device.dwTimeGlobal >= MinGrenadeTimer;
}

void CBurerCombat::CorrectReadiness(CBurer& Burer, bool& HealthLoss, bool& AntiAim, bool& Gravi, bool& Tele, bool& Shield, bool PreviousShield, bool PreviousTele)
{
	Refresh(Burer);
	if (!Enabled || !Context.ActorEnemy || (!AntiAim && !Gravi && !Tele && !Shield))
	{
		return;
	}
	const auto& C = Context;
	auto* Actor = ::Actor();
	if (PreviousTele && (AntiAim || Gravi))
	{
		Tele = false;
	}
	if ((!C.HasItem && !C.Grenades && !C.RecentExplosion) || (Actor->BurerEffects.Active(*Actor) && !C.RecentExplosion))
	{
		Shield = false;
	}
	if (AntiAim && C.Facing && C.Dangerous && C.ReadyIn <= 1.5f && !C.Reloading && Random.randF() < .85f)
	{
		Gravi = false;
	}
	enum class EChoice
	{
		None,
		Anti,
		Shield,
		Tele,
		Gravi
	} Choice = EChoice::None;
	if (C.Grenades && C.GrenadeTimer < CriticalGrenadeTimer && Shield)
	{
		Choice = EChoice::Shield;
	}
	else if (CloseProtection() && Shield)
	{
		Tele = false;
		if (C.Grenades && C.GrenadeTimer < MinGrenadeTimer)
		{
			Choice = EChoice::Shield;
		}
		else if (C.Consumable && AntiAim)
		{
			Choice = EChoice::Anti;
		}
		else if (HealthLoss)
		{
			Choice = EChoice::Shield;
		}
		else if (C.Sniper)
		{
			Gravi = false;
			Choice = AntiAim && (C.Empty || C.Reloading || C.ReadyIn > .2f || !C.Facing || Random.randF() < .3f || (PreviousShield && !C.UnderWideAim)) ? EChoice::Anti : EChoice::Shield;
		}
		else if (AntiAim && PreviousShield)
		{
			Actor->BurerEffects.Play(*Actor, EBurerHit::Gravi);
			Actor->BurerEffects.PrepareKnife(KnifeWindow);
			Choice = EChoice::Anti;
		}
		else
		{
			Choice = EChoice::Shield;
		}
	}
	else if (g_SingleGameDifficulty >= egdStalker && C.Consumable && AntiAim && Random.randF() < .95f)
	{
		Choice = C.Grenades && C.GrenadeTimer < MinGrenadeTimer && Shield && Random.randF() < .8f ? EChoice::Shield : EChoice::Anti;
	}
	else if (C.Knife && !C.Grenades && Gravi)
	{
		Choice = EChoice::Gravi;
	}
	else if (C.Inventory && !C.RecentExplosion && !C.Grenades && AntiAim)
	{
		Choice = EChoice::Anti;
	}
	else if (C.Explosive || C.RecentExplosion)
	{
		Gravi = false;
		if ((C.Facing && C.Visible) || Random.randF() >= .1f)
		{
			Tele = false;
		}
		if (C.Grenades && C.GrenadeTimer < MinGrenadeTimer && Shield && Random.randF() < .8f)
		{
			Choice = EChoice::Shield;
		}
		else if (!C.RecentExplosion && PreviousShield && AntiAim)
		{
			Choice = EChoice::Anti;
		}
		else if (Shield && (C.UnderWideAim || (AntiAim && Random.randF() < .4f) || C.RecentExplosion || !AntiAim))
		{
			Choice = EChoice::Shield;
		}
		else if (AntiAim)
		{
			Choice = EChoice::Anti;
		}
	}
	else if (C.Sniper)
	{
		if ((C.Facing && C.Visible) || Random.randF() >= .1f)
		{
			Tele = false;
		}
		if (C.Grenades && Shield)
		{
			Choice = EChoice::Shield;
		}
		else if (C.ReadyIn > 1.5f && Gravi && Random.randF() < .6f)
		{
			Choice = EChoice::Gravi;
		}
		else if ((C.ReadyIn > .2f || C.Reloading || !C.Facing) && AntiAim)
		{
			Choice = EChoice::Anti;
		}
		else if (Shield && (!PreviousShield || Random.randF() < .75f || C.UnderAim))
		{
			Choice = EChoice::Shield;
		}
		else if (!C.UnderAim && AntiAim)
		{
			Choice = EChoice::Anti;
		}
		if (C.UnderWideAim)
		{
			Gravi = false;
		}
	}
	else if (C.Visible && C.Distance < AntiAimDistance)
	{
		if (C.Grenades && C.GrenadeTimer < MinGrenadeTimer && Shield)
		{
			Choice = EChoice::Shield;
		}
		else if (C.HasItem && AntiAim)
		{
			Choice = EChoice::Anti;
		}
		else if (!C.Facing && Gravi)
		{
			if (C.Grenades > 3 && Shield)
			{
				Choice = EChoice::Shield;
			}
			else if (C.Grenades && C.GrenadeTimer > MinGrenadeTimer && Tele && Random.randF() < .75f)
			{
				Choice = EChoice::Tele;
			}
			else
			{
				Choice = C.Grenades && Shield ? EChoice::Shield : EChoice::Gravi;
			}
		}
		else if (C.Grenades && Shield)
		{
			Choice = EChoice::Shield;
		}
		else if (C.Throwable && !C.Grenades)
		{
			Shield = false;
			if (C.PreparedThrow)
			{
				Gravi = false;
			}
		}
		else if (C.Knife && Shield)
		{
			Choice = EChoice::Shield;
		}
	}
	else if (C.UnderAim)
	{
		if (C.Grenades && Shield)
		{
			Choice = EChoice::Shield;
		}
		else if (C.HasItem && AntiAim)
		{
			Choice = EChoice::Anti;
		}
		else if (!PreviousShield && Shield)
		{
			Choice = EChoice::Shield;
		}
		else
		{
			Gravi = false;
		}
	}
	else if (C.Grenades && Random.randF() < .9f)
	{
		if (Shield && (C.Grenades > 3 || !Tele || C.GrenadeTimer <= MinGrenadeTimer || Random.randF() < .8f))
		{
			Choice = EChoice::Shield;
		}
		else if (Tele)
		{
			Choice = EChoice::Tele;
		}
	}
	else if (C.Throwable)
	{
		Shield = false;
		if (C.PreparedThrow)
		{
			Gravi = false;
		}
	}
	else if (AntiAim && ((C.Reloading && Random.randF() < .9f) || (C.Empty && Random.randF() < .6f)))
	{
		Choice = EChoice::Anti;
	}
	if (Choice != EChoice::None)
	{
		AntiAim = Choice == EChoice::Anti;
		Gravi = Choice == EChoice::Gravi;
		Tele = Choice == EChoice::Tele;
		Shield = Choice == EChoice::Shield;
		HealthLoss = Shield;
	}
}

bool CBurerCombat::HoldShield(CBurer& Burer)
{
	Refresh(Burer);
	const auto& C = Context;
	if (!Enabled || !C.ActorEnemy)
	{
		return false;
	}
	if (C.Grenades)
	{
		return true;
	}
	if (!C.HasItem || C.Reloading || C.ReadyIn > .3f)
	{
		return false;
	}
	bool Hold = CloseProtection() || Device.dwTimeGlobal < AimUntil || (C.Sniper && C.Facing) || C.RecentExplosion || (C.Explosive && C.Facing);
	if (!Hold || Device.dwTimeGlobal < RiskNext)
	{
		return Hold;
	}
	RiskNext = Device.dwTimeGlobal + 500;
	if (C.Firing || !Burer.get_anti_aim() || !Burer.get_anti_aim()->BurerCanRisk() || Random.randF() >= RiskProbability)
	{
		return true;
	}
	auto* Actor = ::Actor();
	if ((C.UnderAim || C.Sniper) && !Actor->BurerEffects.Play(*Actor, EBurerHit::Risk))
	{
		return true;
	}
	Actor->BurerEffects.PrepareKnife(KnifeWindow);
	if (C.Knife)
	{
		if (auto* Item = Actor->inventory().ActiveItem(); Item && Item->cast_weapon_knife())
		{
			Item->cast_weapon_knife()->StartBurerSelfKick();
		}
	}
	else if (C.UnderAim || C.Sniper)
	{
		Fvector Side = Device.vCameraRight;
		Side.mul(Random.randI(2) ? 1.f : -1.f);
		Actor->character_physics_support()->movement()->ApplyImpulse(Side, RiskImpulse);
	}
	Burer.set_force_anti_aim(true);
	return false;
}

bool CBurerCombat::NeedStopTele(CBurer& Burer, u32 Started)
{
	Refresh(Burer);
	if (!Enabled || !Context.ActorEnemy || !Started)
	{
		return false;
	}
	const auto& C = Context;
	bool Long = Device.dwTimeGlobal - Started > ForceFireDelay;
	if (C.GrenadeTimer < CriticalGrenadeTimer || CloseProtection() || (C.Sniper && C.Facing && C.Visible) || (C.UnderAim && (C.Firing || Long || C.UnderExactAim)) || (C.Explosive && C.Facing && C.Visible) || (Long && (C.Consumable || C.PreparedThrow)))
	{
		return true;
	}
	if (Device.dwTimeGlobal >= TeleDecisionNext)
	{
		TeleDecisionNext = Device.dwTimeGlobal + 500;
		StopTele = C.Visible && ((C.Reloading && Random.randF() < .4f) || (C.Empty && Random.randF() < .003f));
	}
	return StopTele;
}

bool CBurerCombat::CanFireAll(u32 Started, u32 End) const
{
	return Started && (Context.GrenadeTimer < CriticalGrenadeTimer || Device.dwTimeGlobal > End || (Context.Grenades && Device.dwTimeGlobal - Started > 500) || Device.dwTimeGlobal - Started > ForceFireDelay);
}

void CBurerCombat::StaminaAttack(CBurer& Burer)
{
	ContextTime = u32(-1);
	Refresh(Burer);
	auto* Actor = ::Actor();
	if (!Context.ActorEnemy || GodMode() || (!Context.Visible && !Context.UnderAim))
	{
		return;
	}
	auto* Item = Actor->inventory().ActiveItem();
	auto* Weapon = Item ? Item->cast_weapon() : nullptr;
	bool Super = (Context.Visible && (Context.Distance < SuperDistance || Context.Inventory || (g_SingleGameDifficulty >= egdStalker && Context.Consumable && Random.randF() < .95f) || Context.Explosive || Context.Sniper || (Context.Aiming && Context.Facing))) || Context.UnderAim;
	float Damage = Super ? StaminaHit : (Weapon ? Weapon->Weight() * Burer.weight_to_stamina_hit : 0.f);
	bool Drop = Weapon && Actor->conditions().GetPower() < Damage * Burer.weapon_drop_stamina_k && !Actor->inventory().SlotIsPersistent(Weapon->BaseSlot());
	if (Super && Weapon && Weapon->cast_weapon_magazined())
	{
		Weapon->ChangeCondition(-Random.randF(ConditionMin, ConditionMax));
	}
	Actor->conditions().PowerHit(Damage, false);
	Actor->BurerEffects.RestoreStamina(StaminaFloor, StaminaHealth, Context.Sniper || Context.Explosive || Context.RecentExplosion);
	if (Context.Knife)
	{
		bool SelfKick = Actor->BurerEffects.KnifeWindow() && Item && Item->cast_weapon_knife() && Item->cast_weapon_knife()->StartBurerSelfKick();
		if (!SelfKick && Random.randF() > HideSkipProbability)
		{
			Actor->BurerEffects.Play(*Actor, EBurerHit::Gravi, true);
			Actor->inventory().SetActiveSlot(NO_ACTIVE_SLOT);
			if (Actor->m_sBurerKickAnimator.size())
			{
				Actor->HudAnimator()->ItemAnimator()->StartAnimator(Actor->m_sBurerKickAnimator);
			}
		}
		Drop = false;
	}
	else if (auto* Grenade = Item ? Item->cast_grenade() : nullptr)
	{
		if (Context.PreparedThrow)
		{
			Grenade->PrepareGrenadeForSuicideThrow(5.f);
			Grenade->DropGrenade();
		}
		else if (Grenade->GetState() == CGrenade::eIdle)
		{
			Grenade->SetDropManual(true);
			Actor->inventory().SetActiveSlot(NO_ACTIVE_SLOT);
		}
	}
	if (Item && Context.Distance < ShieldDistance && Context.Visible && HealthHit > 0.f)
	{
		Fvector Direction;
		Direction.sub(Actor->Position(), Burer.Position()).normalize_safe();
		Burer.HitEntity(Actor, HealthHit, HealthImpulse, Direction, HealthHitType, false);
	}
	if (Drop)
	{
		Fvector Direction = Actor->Direction();
		Direction.y = std::abs(Direction.y);
		Direction.normalize_safe();
		Direction.mul(Burer.weapon_drop_velocity);
		Weapon->SetActivationSpeedOverride(Direction);
		Weapon->SetDropManual(true);
		if (auto* LeftDevice = Actor->GetDevice())
		{
			LeftDevice->SetActivationSpeedOverride(Direction);
			LeftDevice->SetDropManual(true);
		}
		Actor->BurerEffects.Play(*Actor, EBurerHit::Drop);
		Actor->inventory().SetActiveSlot(NO_ACTIVE_SLOT);
		if (Actor->m_sBurerKickAnimator.size())
		{
			Actor->HudAnimator()->ItemAnimator()->StartAnimator(Actor->m_sBurerKickAnimator);
		}
	}
	else if (!Context.Knife)
	{
		Actor->BurerEffects.Play(*Actor, EBurerHit::Stamina);
	}
	Actor->BurerEffects.Update(*Actor);
}
