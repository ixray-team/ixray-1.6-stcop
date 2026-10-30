#pragma once
#include <cmath>
#include "FFxExtendedEntityesLogic/CELSharedSettings.h"
#include "FFxExtendedEntityesLogic/Interfaces/IELAdapter.h"
#include "xrGame/ai/monsters/basemonster/base_monster.h"
#include "xrGame/Actor.h"
#include "xrGame/inventory.h"
#include "xrGame/Weapon.h"
#include "xrGame/WeaponKnife.h"

struct SELWeaponThreat
{
	bool IsAiming = false;
	bool IsFocusing = false;
	bool IsPreciseAim = false;
	bool IsLeading = false;
	float PreferredDodgeSide = 0.f;
	float DodgeEscapeDistance = 0.f;
	Fvector AimOrigin = {0.f, 0.f, 0.f};
	Fvector AimDirection = {0.f, 0.f, 1.f};
	bool HasWeapon = false;
	bool ObserverFacesTarget = false;
	bool IsMelee = false;
	bool IsSwinging = false;
	bool HasRecoveryWindow = false;
	EELNoiseType RecoveryType = EELNoiseType::Other;
	float Reach = 0.f;
};

class CELSharedWeaponThreat
{
public:
	static SELWeaponThreat Observe(IELAdapter& Adapter, CEntityAlive& Target, const Fvector& OwnerVelocity)
	{
		SELWeaponThreat Result;
		CActor* Player = smart_cast<CActor*>(&Target);
		if (!Player || !Adapter.CanSee(Target))
		{
			return Result;
		}
		CInventoryItem* Item = Player->inventory().ActiveItem();
		CWeapon* Weapon = Item ? Item->cast_weapon() : nullptr;
		if (!Weapon)
		{
			return Result;
		}
		Result.HasWeapon = true;
		// Retained contact is knowledge, not permission to dodge a weapon behind the observer.
		const CBaseMonster& Observer = Adapter.Monster();
		Fvector TargetCenter;
		Target.Center(TargetCenter);
		const Fvector ToTarget = Fvector().sub(TargetCenter, Observer.eye_matrix.c);
		Fvector Forward = Observer.eye_matrix.k;
		Fvector Right = Observer.eye_matrix.i;
		Fvector Up = Observer.eye_matrix.j;
		Forward.normalize_safe();
		Right.normalize_safe();
		Up.normalize_safe();
		const float AlongView = ToTarget.dotproduct(Forward);
		const float ViewWidth = AlongView * std::tan(deg2rad(clampr(Observer.eye_fov, 1.f, 179.f) * .5f));
		Result.ObserverFacesTarget = AlongView > EPS_L && std::abs(ToTarget.dotproduct(Right)) <= ViewWidth && std::abs(ToTarget.dotproduct(Up)) <= ViewWidth;
		const SELPerceptionProfile& Profile = CELSharedSettings::Profile(*Adapter.Monster().cNameSect());
		Fvector Origin;
		Fvector Direction;
		Player->g_fireParams(Weapon, Origin, Direction);
		Direction.normalize_safe();
		Result.AimOrigin = Origin;
		Result.AimDirection = Direction;
		Fvector Body;
		Adapter.Monster().Center(Body);
		Fvector Offset;
		Offset.sub(Body, Origin);
		const float Along = Offset.dotproduct(Direction);
		const float Radius = Adapter.Monster().Radius() + .25f;
		if (CWeaponKnife* Knife = Weapon->cast_weapon_knife())
		{
			Result.IsMelee = true;
			Result.Reach = std::max(Knife->GetHit1Dist(), Knife->GetHit2Dist()) + Adapter.Monster().Radius();
			Result.IsSwinging = Weapon->GetState() == CWeapon::eFire || Weapon->GetState() == CWeapon::eFire2;
			const float SwingWidth = std::max(Knife->GetHit1SplashRadius(), Knife->GetHit2SplashRadius());
			const float SideSquared = std::max(0.f, Offset.square_magnitude() - Along * Along);
			Result.IsAiming = Result.ObserverFacesTarget && Along > -Radius && SideSquared <= _sqr(Radius + SwingWidth) && Offset.magnitude() <= Result.Reach + .5f;
		}
		else
		{
			const u32 State = Weapon->GetState();
			Result.HasRecoveryWindow = Result.ObserverFacesTarget && (State == CWeapon::eReload || State == CWeapon::eMisfire || State == CWeapon::eEmptyClick);
			Result.RecoveryType = State == CWeapon::eReload ? EELNoiseType::Reload : EELNoiseType::EmptyClick;
			const bool IsFiring = State == CWeapon::eFire || State == CWeapon::eFire2;
			const bool IsReady = State == CWeapon::eIdle || IsFiring;
			// Aim cones are full angles from the species profile. Hip aiming is sufficient.
			const float SideSquared = std::max(0.f, Offset.square_magnitude() - Along * Along);
			const auto InCone = [Along, SideSquared, &Adapter](float SinHalfAngle, float CosHalfAngle)
			{
				const float Boundary = (Along * SinHalfAngle + Adapter.Monster().Radius()) / CosHalfAngle;
				return Along > 0.f && SideSquared <= _sqr(Boundary);
			};
			const float AimHalfAngle = deg2rad(Profile.DodgeAimAngle * .5f);
			const float ReleaseHalfAngle = deg2rad(Profile.DodgeReleaseAngle * .5f);
			Result.IsAiming = Result.ObserverFacesTarget && IsReady && InCone(std::sin(AimHalfAngle), std::cos(AimHalfAngle));
			// A slightly wider release cone avoids toggling on the ten-degree boundary.
			Result.IsFocusing = Result.ObserverFacesTarget && IsReady && InCone(std::sin(ReleaseHalfAngle), std::cos(ReleaseHalfAngle));
			Fvector TowardPlayer = Fvector().sub(Player->Position(), Adapter.Monster().Position());
			TowardPlayer.y = 0.f;
			TowardPlayer.normalize_safe();
			const Fvector Side = {TowardPlayer.z, 0.f, -TowardPlayer.x};
			const Fvector AwayFromAim = Fvector().mad(Offset, Direction, -Along);
			const float SignedOffset = AwayFromAim.dotproduct(Side);
			const float LateralSpeed = OwnerVelocity.dotproduct(Side);
			const Fvector FutureOffset = Fvector().mad(Offset, OwnerVelocity, Profile.DodgePredictionTime);
			const float FutureAlong = FutureOffset.dotproduct(Direction);
			const float FutureSideSquared = std::max(0.f, FutureOffset.square_magnitude() - FutureAlong * FutureAlong);
			const float FutureBoundary = (FutureAlong * std::sin(AimHalfAngle) + Radius) / std::cos(AimHalfAngle);
			// React to a visible gun leading the actual measured motion, not to unseen intentions.
			Result.IsLeading = Result.ObserverFacesTarget && IsReady && Profile.DodgePredictionTime > 0.f && FutureAlong > 0.f &&
				SignedOffset * LateralSpeed < 0.f && std::abs(LateralSpeed) > .5f &&
				std::abs(SignedOffset) > .1f && FutureSideSquared <= _sqr(FutureBoundary);
			const float ConeBoundary = (Along * std::sin(ReleaseHalfAngle) + Radius) / std::cos(ReleaseHalfAngle);
			Result.DodgeEscapeDistance = std::max(0.f, ConeBoundary - std::abs(SignedOffset)) + Profile.DodgeConeMargin * Adapter.Monster().GetVisualScale();
			const float DeadZone = .05f * Adapter.Monster().GetVisualScale();
			Result.PreferredDodgeSide = SignedOffset > DeadZone ? 1.f : SignedOffset < -DeadZone ? -1.f : 0.f;
			Fmatrix Inverse;
			Inverse.invert(Adapter.Monster().XFORM());
			Fvector LocalOrigin;
			Fvector LocalDirection;
			Inverse.transform_tiny(LocalOrigin, Origin);
			Inverse.transform_dir(LocalDirection, Direction);
			Fbox Bounds = Adapter.Monster().BoundingBox();
			Fvector Contact;
			Result.IsPreciseAim = Result.ObserverFacesTarget && IsReady && Along > 0.f && Bounds.Pick2(LocalOrigin, LocalDirection, Contact) != Fbox::rpNone;
		}
		return Result;
	}
};
