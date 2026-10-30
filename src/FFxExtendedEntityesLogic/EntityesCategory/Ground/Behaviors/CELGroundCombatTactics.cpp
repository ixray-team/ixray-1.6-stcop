#include "FFxExtendedEntityesLogic/stdafx.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/Behaviors/CELGroundCombatTactics.h"
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedWeaponThreat.h"
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedHearing.h"
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedHuntGroups.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/Navigation/CELGroundLocalSearch.h"
#include "FFxExtendedEntityesLogic/Debug/Debug.h"
#include "xrGame/ai/monsters/control_manager_custom.h"
#include "xrGame/movement_manager.h"
#include "xrGame/detail_path_manager.h"
#include "xrGame/ai_monster_space.h"
#include "FFxExtendedEntityesLogic/EntryPoint.h"
#include "FFxExtendedEntityesLogic/Navigation/CELSharedNavigationScheduler.h"
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedAnomalies.h"
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedDeaths.h"
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedSight.h"

void CELGroundCombatTactics::Update(IELAdapter& Adapter, const SELPerceptionProfile& Profile, CEntityAlive& Target, bool Visible, const CELSharedHearing* Hearing)
{
	CBaseMonster& Owner = Adapter.Monster();
	const u32 Now = Device.dwTimeGlobal;
	const float Elapsed = LastSampleAt ? std::min(10.f, float(Now - LastSampleAt) * .001f) : 0.f;
	DodgeStrength = Profile.DodgeStrength;
	const bool SameTarget = TargetIdentifier == Target.ID();
	const bool LandedEvasiveJump = Intent == EELCombatIntent::EvasionJump;
	if (LandedEvasiveJump)
	{
		// Update runs again only after the native jump releases movement control.
		DodgeAfter = std::max(DodgeAfter, Now + Profile.DodgePauseMin);
		// The jump cooldown runs from takeoff, independently of landing animation and steps.
		Intent = EELCombatIntent::Close;
	}
	if (!SameTarget || !Profile.CanCounterAttack)
	{
		CounterUntil = 0;
	}
	else if (LandedEvasiveJump && Visible)
	{
		CounterUntil = Now + Profile.CounterAttackDuration;
	}
	const bool IsSprinting = s32(SprintUntil - Now) > 0 && Owner.movement().detail().GetMovementFactor() > 1.001f;
	Stamina = clampr(Stamina + Elapsed * (IsSprinting ? -1000.f / float(Profile.SprintDuration) : 1000.f / float(Profile.SprintCooldown)), 0.f, 1.f);
	JumpReason = Owner.com_man().get_jump_control() ? (s32(Now - JumpAfter) < 0 ? "пауза между прыжками" : "нет подходящего окна") : "вид не умеет прыгать";
	CELSharedHuntGroups::ReportStamina(Owner, Stamina);
	if (!Visible)
	{
		LastOwnerPosition = Owner.Position();
		OwnerVelocity.set(0.f, 0.f, 0.f);
		JumpReason = "цель не видна";
		// Finish only the already checked step during a brief visibility gap; never sample hidden coordinates.
		if (SameTarget && IsEvading() && HasTacticalGoal && Now - LastVisibleAt < Profile.DodgeVisibilityGrace && s32(ManeuverUntil - Now) > 0)
		{
			DodgeReason = "завершаю шаг по последнему подтверждению";
			Owner.movement().detail().SetMovementFactor(1.f);
			LastSampleAt = Now;
			return;
		}
		DodgeReason = "нет зрительного контакта";
		HasTacticalGoal = false;
		Intent = EELCombatIntent::Close;
		SprintUntil = 0;
		Owner.movement().detail().SetMovementFactor(1.f);
		LastSampleAt = Now;
		TargetVelocity.set(0.f, 0.f, 0.f);
		return;
	}
	LastVisibleAt = Now;
	Fvector Position;
	Target.Center(Position);
	if (SameTarget && LastSampleAt && Elapsed > .001f && Elapsed < .5f)
	{
		Fvector Sample;
		Sample.sub(Position, LastTargetPosition).div(Elapsed);
		if (Sample.magnitude() <= 50.f)
		{
			TargetVelocity.lerp(TargetVelocity, Sample, clampr(Elapsed * 6.f, 0.f, 1.f));
		}
	}
	else if (!SameTarget)
	{
		TargetVelocity.set(0.f, 0.f, 0.f);
		ManeuverUntil = FocusUntil = 0;
	}
	else if (Elapsed >= .5f)
	{
		TargetVelocity.set(0.f, 0.f, 0.f);
	}
	const float Distance = Owner.Position().distance_to(Position);
	const bool IsEscaping = SameTarget && TargetVelocity.square_magnitude() > 1.f && Distance > LastDistance + .05f;
	LastDistance = Distance;
	LastTargetPosition = Position;
	TargetIdentifier = Target.ID();
	LastSampleAt = Now;
	if (SameTarget && LastSampleAt && Elapsed > .001f && Elapsed < .5f)
	{
		Fvector Sample = Fvector().sub(Owner.Position(), LastOwnerPosition).div(Elapsed);
		Sample.y = 0.f;
		if (Sample.square_magnitude() < 2500.f)
		{
			OwnerVelocity.lerp(OwnerVelocity, Sample, clampr(Elapsed * 10.f, 0.f, 1.f));
		}
	}
	else
	{
		OwnerVelocity.set(0.f, 0.f, 0.f);
	}
	LastOwnerPosition = Owner.Position();
	const SELWeaponThreat Weapon = CELSharedWeaponThreat::Observe(Adapter, Target, OwnerVelocity);
	u32 HeardAt = 0;
	const bool HeardOpportunity = Hearing && Hearing->GetCounterOpportunity(Target.ID(), HeardAt) && s32(HeardAt - LastCounterEventAt) > 0;
	const bool SeenOpportunity = Weapon.HasRecoveryWindow && ((Weapon.RecoveryType == EELNoiseType::Reload && Profile.CounterOnReload) || (Weapon.RecoveryType == EELNoiseType::EmptyClick && Profile.CounterOnEmptyClick));
	if (Profile.CanCounterAttack && s32(Now - OpportunityAfter) >= 0 && (HeardOpportunity || SeenOpportunity))
	{
		CounterUntil = Now + Profile.CounterAttackDuration;
		OpportunityAfter = Now + Profile.CounterOpportunityCooldown;
		if (HeardOpportunity)
		{
			LastCounterEventAt = HeardAt;
		}
	}
	const bool WasFireEvasion = Intent == EELCombatIntent::DodgeFire;
	if (!Weapon.IsMelee && (Weapon.IsAiming || (WasFireEvasion && s32(FocusUntil - Now) > 0 && Weapon.IsFocusing)))
	{
		FocusUntil = Now + Profile.DodgeFocusTime;
	}
	const bool HasFireFocus = !Weapon.IsMelee && Weapon.IsFocusing && s32(FocusUntil - Now) > 0;
	const bool WeaponThreat = Weapon.IsMelee ? Weapon.IsAiming : Weapon.IsAiming || Weapon.IsLeading || HasFireFocus;
	if (Intent == EELCombatIntent::DodgeFire && Weapon.HasWeapon && !Weapon.ObserverFacesTarget)
	{
		// Cancel the grounded evasive segment; retained target knowledge still serves normal combat.
		HasTacticalGoal = false;
		ManeuverUntil = Now;
		FocusUntil = 0;
		Intent = EELCombatIntent::Close;
	}
	DodgeReason = !Profile.CanDodge || DodgeStrength <= 0.f ? "отключён в профиле" : !Weapon.HasWeapon ? "нет видимого оружия" : !Weapon.ObserverFacesTarget ? "цель вне собственного фрустума: уворот запрещен" : !WeaponThreat ? "вне конуса прицеливания/оружие не готово" : s32(Now - DodgeAfter) < 0 ? "пауза между уворотами" : "ищу безопасный шаг";
	const float MeleeDistance = Owner.MeleeChecker.distance_to_enemy(&Target);
	const bool InMelee = MeleeDistance <= Owner.MeleeChecker.get_min_distance();
	CControlJump* Jump = Owner.com_man().get_jump_control();
	const bool GroundTarget = Target.UsedAI_Locations() || smart_cast<CActor*>(&Target);
	const bool JumpAttackRange = Profile.CanAttackJump && Jump && GroundTarget && Distance <= Jump->get_max_distance();
	const bool AttackSoon = GroundTarget && MeleeDistance <= Owner.MeleeChecker.get_max_distance() + std::max(.5f, Owner.movement().speed() * Profile.DodgeAttackCommitTime);
	// A long native jump range is not an unconditional ban on evasion.
	const bool CloseJumpAttackRange = JumpAttackRange && Distance <= Profile.LungeMaxDistance;
	const bool CommitAttack = InMelee || CloseJumpAttackRange || AttackSoon;
	if (Profile.CanDodgeFire && WeaponThreat && !CommitAttack && !HeardOpportunity && !SeenOpportunity)
	{
		// Tracking fire keeps evasion available; a remote counter window must not force a straight run.
		CounterUntil = 0;
	}
	const auto IsClosingPoint = [&](const Fvector& Point, float MaxAngle)
	{
		Fvector ToTarget = Fvector().sub(Position, Owner.Position());
		Fvector Remaining = Fvector().sub(Position, Point);
		Fvector Displacement = Fvector().sub(Point, Owner.Position());
		ToTarget.y = Remaining.y = Displacement.y = 0.f;
		if (Remaining.square_magnitude() >= ToTarget.square_magnitude() || Displacement.square_magnitude() <= EPS_L)
		{
			return false;
		}
		ToTarget.normalize_safe();
		Displacement.normalize_safe();
		// Validate the resolved point: navigation projection may distort a forward candidate.
		return Displacement.dotproduct(ToTarget) + EPS_L >= std::cos(deg2rad(MaxAngle));
	};
	const auto TrySafeJump = [&](const Fvector& Point, CEntityAlive* HitTarget, float MaxHeadingAngle = -1.f)
	{
		Fvector Landing;
		u32 Vertex;
		const bool Evasive = MaxHeadingAngle >= 0.f;
		const auto Refuse = [&](const char* Reason)
		{
			JumpReason = Reason;
			if (Evasive)
			{
				EvasionJumpReason = Reason;
			}
			return false;
		};
		if (!CELGroundLocalSearch::Resolve(Adapter, Point, Landing, Vertex))
		{
			return Refuse("нет достижимой точки приземления");
		}
		if (CELSharedAnomalies::Crosses(Owner.ID(), Owner.Position(), Landing) || CELSharedDeaths::Crosses(Owner.ID(), Owner.Position(), Landing))
		{
			return Refuse("опасная зона на траектории");
		}
		if (!ExtendedEntityes::Navigation().TryGeometryRays(12))
		{
			return Refuse("ожидание квоты геометрии");
		}
		// Check the actual ballistic corridor before giving the native controller ownership.
		if (MaxHeadingAngle >= 0.f)
		{
			Fvector LandingOffset = Fvector().sub(Landing, Owner.Position());
			LandingOffset.y = 0.f;
			if (!IsClosingPoint(Landing, Profile.DodgeJumpMaxAngle) || (LandingOffset.square_magnitude() > _sqr(Profile.DodgeJumpDistanceMax * Owner.GetVisualScale()) + EPS_L || LandingOffset.square_magnitude() + EPS_L < _sqr(Profile.DodgeJumpDistanceMin * Owner.GetVisualScale())))
			{
				return Refuse("приземление нарушает сближение или длину уворота");
			}
			Fvector LandingBody;
			Owner.Center(LandingBody);
			LandingBody.add(Fvector().sub(Landing, Owner.Position()));
			const Fvector AimOffset = Fvector().sub(LandingBody, Weapon.AimOrigin);
			const float AlongAim = AimOffset.dotproduct(Weapon.AimDirection);
			const float AimSideSquared = std::max(0.f, AimOffset.square_magnitude() - AlongAim * AlongAim);
			// The wide aim cone triggers the reaction; landing must clear the actual line of fire.
			Fvector Radial = Fvector().mad(AimOffset, Weapon.AimDirection, -AlongAim);
			Radial.normalize_safe();
			Fvector HalfSize;
			Owner.BoundingBox().getradius(HalfSize);
			// Project the actual oriented box, not a sphere inflated by the full body length.
			const Fmatrix& Transform = Owner.XFORM();
			const float BodySupport = std::abs(Radial.dotproduct(Transform.i)) * HalfSize.x +
				std::abs(Radial.dotproduct(Transform.j)) * HalfSize.y + std::abs(Radial.dotproduct(Transform.k)) * HalfSize.z;
			const float RequiredSide = BodySupport + Profile.DodgeConeMargin * Owner.GetVisualScale();
			EvasionJumpSide = std::sqrt(AimSideSquared);
			EvasionJumpRequiredSide = RequiredSide;
			// Test the resolved landing, not the unprojected candidate or an arbitrary side hop.
			if (AlongAim <= 0.f || AimSideSquared <= _sqr(RequiredSide) || Landing.distance_to_sqr(Position) >= Owner.Position().distance_to_sqr(Position))
			{
				return Refuse("приземление под прицелом или без сближения");
			}
			// Candidate angles are relative to the target; current facing is not that cone.
			if (!Jump || !Jump->can_jump(Landing, false, PI))
			{
				return Refuse("нативные угол, дальность, высота или пауза");
			}
			if (Jump->jump_intersect_geometry(Landing, nullptr, true))
			{
				return Refuse("препятствие на баллистической траектории");
			}
		}
		const bool Accepted = Owner.com_man().jump_if_possible(HitTarget ? Point : Landing, HitTarget, false, MaxHeadingAngle < 0.f, true, Evasive ? PI : MaxHeadingAngle, true);
		if (!Accepted)
		{
			return Refuse("нативный контроллер занят или запретил старт");
		}
		return true;
	};
	if (JumpAttackRange && (CloseJumpAttackRange || !WeaponThreat) && !InMelee && s32(Now - JumpAfter) >= 0)
	{
		JumpAfter = Now + Profile.DodgeJumpRetry;
		if (TrySafeJump(Position, &Target))
		{
			HasTacticalGoal = false;
			Intent = EELCombatIntent::PursuitJump;
			CounterUntil = 0;
			JumpAfter = Now + Profile.TacticalJumpCooldown;
			DodgeReason = "дистанция атакующего прыжка: атакую";
			JumpReason = "атакующий прыжок вместо уворота";
			Owner.movement().detail().SetMovementFactor(1.f);
			return;
		}
	}
	if (CommitAttack)
	{
		EvasionJumpReason = "ближняя атака приоритетнее уворота";
		DodgeReason = "приоритет атаки/короткого сближения";
	}
	const bool JumpReady = Profile.CanDodgeFire && Profile.CanDodgeJump && Jump && s32(Now - DodgeJumpAfter) >= 0;
	const bool ActiveManeuver = s32(ManeuverUntil - Now) > 0 && HasTacticalGoal && !CommitAttack;
	const bool CorrectFireEvasion = ActiveManeuver && Intent == EELCombatIntent::DodgeFire && Profile.CanDodgeFire && Profile.CanDodgeStep && DodgeStrength > 0.f && WeaponThreat && !Weapon.IsMelee;
	if (ActiveManeuver && !CorrectFireEvasion)
	{
		DodgeReason = "завершение начатого манёвра";
		Owner.movement().detail().SetMovementFactor(1.f);
		return;
	}
	if (CorrectFireEvasion && !JumpReady && s32(Now - DodgeDirectionAfter) < 0 &&
		Owner.Position().distance_to_sqr(TacticalGoal) > _sqr(Profile.DodgeStepArrivalDistance * Owner.GetVisualScale()) && IsClosingPoint(TacticalGoal, Profile.DodgeStepMaxAngle))
	{
		// Execute the chosen forward segment instead of rebuilding a sliding goal every frame.
		DodgeReason = "сближающий шаг: сохраняю траекторию до коррекции";
		Owner.movement().detail().SetMovementFactor(1.f);
		return;
	}
	const bool WasEvasive = Intent == EELCombatIntent::DodgeFire || Intent == EELCombatIntent::DodgeMelee || Intent == EELCombatIntent::Feint;
	HasTacticalGoal = false;
	if (Profile.CanCounterAttack && SameTarget && WasEvasive && (!WeaponThreat || CommitAttack) && (CommitAttack || (s32(Now - ManeuverUntil) >= 0 && s32(Now - ManeuverUntil) < s32(Profile.DodgeCounterTime))))
	{
		CounterUntil = Now + Profile.CounterAttackDuration;
	}
	if (Profile.CanCounterAttack && s32(CounterUntil - Now) > 0)
	{
		Intent = EELCombatIntent::Counter;
		DodgeReason = InMelee ? "контратака: доступный удар" : "контратака: прямое сближение";
		Owner.movement().detail().SetMovementFactor(1.f);
		return;
	}
	Intent = EELCombatIntent::Close;
	// Do not replace a guaranteed strike with endless evasive movement.
	// A ready jump preempts stepping; step cooldowns never suppress its chance roll.
	const bool ShouldDodge = Profile.CanDodge && DodgeStrength > 0.f && (JumpReady || CorrectFireEvasion || s32(Now - DodgeAfter) >= 0) && WeaponThreat && !CommitAttack;
	if (ShouldDodge && (!Weapon.IsMelee || Weapon.IsSwinging))
	{
		Fvector Forward;
		Forward.sub(Position, Owner.Position());
		Forward.y = 0.f;
		const float HorizontalDistance = Forward.magnitude();
		Forward.normalize_safe();
		Fvector Side = {Forward.z, 0.f, -Forward.x};
		const float BaseWidth = Profile.DodgeWidth * Owner.GetVisualScale();
		const float EscapeWidth = Weapon.IsMelee ? BaseWidth : std::max(BaseWidth, Weapon.DodgeEscapeDistance);
		const float Width = std::min(std::min(EscapeWidth * DodgeStrength, Profile.DodgeMaxWidth * Owner.GetVisualScale()), Distance * .4f);
		// Keep the committed side while the player tracks the same maneuver.
		const bool ReverseLead = CorrectFireEvasion && (Weapon.IsLeading || Weapon.IsAiming) && s32(Now - DodgeDirectionAfter) >= 0 && Weapon.PreferredDodgeSide * DodgeSide < 0.f;
		const float FirstSide = CorrectFireEvasion && !ReverseLead ? DodgeSide : !Weapon.IsMelee && Weapon.PreferredDodgeSide != 0.f ? Weapon.PreferredDodgeSide : Random.randI(2) ? 1.f : -1.f;
		if (Profile.CanDodgeFire && Profile.CanDodgeJump && Profile.DodgeJumpChance > 0.f && (Profile.DodgeJumpOnAim || Weapon.IsPreciseAim || Weapon.IsLeading || !Profile.CanDodgeStep) && !Weapon.IsMelee)
		{
			DodgeReason = "точный прицел: прыжок недоступен, продолжаю бег";
			if (Jump && s32(Now - DodgeJumpAfter) >= 0)
			{
				DodgeJumpAfter = Now + Profile.DodgeJumpRetry;
				const bool SelectedJump = Random.randF() < Profile.DodgeJumpChance;
				EvasionJumpReason = SelectedJump ? "подбор безопасного приземления" : "шанс не сработал";
				EvasionJumpSide = EvasionJumpRequiredSide = 0.f;
				const float ApproachAngle = deg2rad(Random.randF(Profile.DodgeApproachAngle, Profile.DodgeJumpMaxAngle));
				const float JumpWidth = Random.randF(Profile.DodgeJumpWidthMin, Profile.DodgeJumpWidthMax) * Owner.GetVisualScale();
				const float MaximumReach = std::min(std::min(Jump->get_max_distance() * .9f, Profile.DodgeJumpDistanceMax * Owner.GetVisualScale()), HorizontalDistance * .75f);
				const float MinimumReach = std::max(Jump->get_min_distance(), Profile.DodgeJumpDistanceMin * Owner.GetVisualScale());
				const auto ReachFor = [&](float Angle, float DesiredWidth)
				{
					return std::min(MaximumReach, std::max(MinimumReach, DesiredWidth / std::max(EPS_L, std::sin(Angle))));
				};
				const float Reach = ReachFor(ApproachAngle, JumpWidth);
				if (SelectedJump && MaximumReach < MinimumReach)
				{
					EvasionJumpReason = "для минимальной длины уворота слишком близко к цели";
				}
				for (u32 Attempt = 0; SelectedJump && MaximumReach >= MinimumReach && Attempt < 2; ++Attempt)
				{
					const float CandidateAngle = Attempt ? deg2rad(Profile.DodgeJumpMaxAngle) : ApproachAngle;
					const float CandidateWidth = Attempt ? Profile.DodgeJumpWidthMax * Owner.GetVisualScale() : JumpWidth;
					const float CandidateReach = Attempt ? ReachFor(CandidateAngle, CandidateWidth) : Reach;
					Fvector Escape = Owner.Position();
					Escape.mad(Forward, CandidateReach * std::cos(CandidateAngle));
					Escape.mad(Side, CandidateReach * std::sin(CandidateAngle) * FirstSide * (Attempt && Weapon.PreferredDodgeSide == 0.f ? -1.f : 1.f));
					if (TrySafeJump(Escape, nullptr, deg2rad(Profile.DodgeJumpMaxAngle)))
					{
						HasTacticalGoal = false;
						Intent = EELCombatIntent::EvasionJump;
						CELSharedSight::KeepEvasionTarget(Owner, Target, Now + Profile.DodgeDurationMax + Profile.DodgeVisibilityGrace);
						DodgeJumpAfter = Now + Profile.DodgeJumpCooldown;
						DodgeReason = Weapon.IsLeading ? "упреждение: резкий уход прыжком" : "точный прицел: резкий уход прыжком";
						JumpReason = EvasionJumpReason = "уклоняющийся прыжок принят";
						Owner.movement().detail().SetMovementFactor(1.f);
						return;
					}
				}
			}
		}
		if (Profile.CanDodgeStep && (CorrectFireEvasion || (s32(Now - DodgeAfter) >= 0 && s32(Now - DodgeStepAfter) >= 0)))
		{
			if (!CorrectFireEvasion || ReverseLead)
			{
				// Sample once per maneuver, not every correction frame.
				DodgeStepScale = Profile.DodgeStepVariation > 0.f ? Random.randF(1.f - Profile.DodgeStepVariation, 1.f + Profile.DodgeStepVariation) : 1.f;
			}
			for (u32 Attempt = 0; Attempt < 2; ++Attempt)
			{
				Fvector Point = Owner.Position();
				const float ForwardStep = std::min(Profile.DodgeStepDistance * DodgeStepScale, HorizontalDistance * .5f);
				const float SideStep = std::min(Width * (2.f - DodgeStepScale), ForwardStep * std::tan(deg2rad(Profile.DodgeStepMaxAngle)));
				Point.mad(Forward, ForwardStep);
				Point.mad(Side, SideStep * FirstSide * (Attempt ? -1.f : 1.f));
				u32 Vertex;
				if (CELGroundLocalSearch::Resolve(Adapter, Point, TacticalGoal, Vertex) && IsClosingPoint(TacticalGoal, Profile.DodgeStepMaxAngle))
				{
					Intent = Weapon.IsMelee ? EELCombatIntent::DodgeMelee : EELCombatIntent::DodgeFire;
					if (!CorrectFireEvasion && DodgeStrength < 1.f && !Weapon.IsMelee && Profile.CanFeint && Distance > 3.f && Distance < 7.f && Random.randF() < Profile.FeintChance)
					{
						Fvector Feint = Owner.Position();
						const float FeintForward = std::min(4.f, HorizontalDistance * .3f);
						Feint.mad(Forward, FeintForward);
						Feint.mad(Side, std::min(Width * .25f, FeintForward * std::tan(deg2rad(Profile.DodgeJumpMaxAngle))) * (Owner.ID() % 2 ? 1.f : -1.f));
						Fvector FeintGoal;
						if (CELGroundLocalSearch::Resolve(Adapter, Feint, FeintGoal, Vertex))
						{
							TacticalGoal = FeintGoal;
							Intent = EELCombatIntent::Feint;
						}
					}
					if (!Weapon.IsMelee && !Profile.CanDodgeFire && Intent != EELCombatIntent::Feint)
					{
						// A disabled firearm dodge cannot leak through a failed/unselected feint.
						Intent = EELCombatIntent::Close;
						DodgeStepAfter = Now + Profile.DodgeStepCooldown;
						break;
					}
					DodgeReason = ReverseLead ? "прицел с упреждением: меняю угол" : CorrectFireEvasion ? "ухожу из конуса с запасом" : Weapon.IsLeading ? "прицел впереди: срываю упреждение" : "прицельный конус: уворот с запасом";
					DodgeDirectionAfter = Now + Profile.DodgeDirectionInterval;
					DodgeSide = FirstSide * (Attempt ? -1.f : 1.f);
					HasTacticalGoal = true;
					if (!CorrectFireEvasion)
					{
						ManeuverUntil = Now + Profile.DodgeDurationMin + u32(float(Profile.DodgeDurationMax - Profile.DodgeDurationMin) * DodgeStrength);
						DodgeAfter = ManeuverUntil + Profile.DodgePauseMin;
						DodgeStepAfter = ManeuverUntil + Profile.DodgeStepCooldown;
					}
					if (Intent == EELCombatIntent::DodgeFire)
					{
						CELSharedSight::KeepEvasionTarget(Owner, Target, ManeuverUntil + Profile.DodgeVisibilityGrace);
					}
					break;
				}
			}
		}
	}
	if (!HasTacticalGoal && InMelee && Profile.PairedAttackDelay)
	{
		const SELHuntGroup Group = CELSharedHuntGroups::Observe(Owner, u16(Target.ID()), Profile.SharingRadius);
		if (Group.Rank > 0 && Group.AllyPosition.distance_to_sqr(Owner.Position()) <= _sqr(Profile.NativeAttackDistance * 2.f) && Group.AllyAttackStartedAt && Now - Group.AllyAttackStartedAt < Profile.PairedAttackDelay)
		{
			Fvector Forward;
			Forward.sub(Position, Owner.Position()).normalize_safe();
			Fvector Side = {Forward.z, 0.f, -Forward.x};
			Fvector Point = Owner.Position();
			Point.mad(Side, std::max(1.f, Owner.Radius()) * (Group.Rank % 2 ? 1.f : -1.f));
			u32 Vertex;
			if (CELGroundLocalSearch::Resolve(Adapter, Point, TacticalGoal, Vertex))
			{
				Intent = EELCombatIntent::PairedAttack;
				HasTacticalGoal = true;
				ManeuverUntil = Group.AllyAttackStartedAt + Profile.PairedAttackDelay;
			}
		}
	}
	if (!HasTacticalGoal && InMelee)
	{
		Intent = EELCombatIntent::Strike;
	}
	if (!HasTacticalGoal && !InMelee && s32(Now - JumpAfter) >= 0 && Owner.com_man().get_jump_control())
	{
		const bool Flying = !GroundTarget;
		Fvector OwnerSize;
		Fvector TargetSize;
		Owner.BoundingBox().getsize(OwnerSize);
		Target.BoundingBox().getsize(TargetSize);
		const bool Smaller = TargetSize.x * TargetSize.y * TargetSize.z < OwnerSize.x * OwnerSize.y * OwnerSize.z;
		const float Reach = Owner.com_man().get_jump_control()->get_max_distance();
		const bool CanIntercept = Flying && Profile.CanInterceptFlying && Smaller;
		const bool CanPursue = !Flying && Profile.CanPursuitJump && IsEscaping && Distance > Reach * .5f;
		if (CanIntercept || CanPursue)
		{
			JumpAfter = Now + Profile.TacticalJumpCooldown;
			JumpReason = "шанс перехвата не сработал";
			if (!CanIntercept || Random.randF() < Profile.InterceptChance)
			{
				JumpReason = "штатная проверка траектории/позы не разрешила";
				Fvector JumpGoal = Position;
				JumpGoal.mad(TargetVelocity, std::min(.5f, Distance / 15.f));
				if (Distance > Reach && CanPursue)
				{
					Fvector Direction;
					Direction.sub(JumpGoal, Owner.Position()).normalize_safe();
					JumpGoal.mad(Owner.Position(), Direction, Reach * .9f);
				}
				if (Owner.com_man().jump_if_possible(JumpGoal, &Target, false, true, true, -1.f, true))
				{
					JumpReason = "прыжок принят штатным контроллером";
					Intent = Flying ? EELCombatIntent::FlyingIntercept : EELCombatIntent::PursuitJump;
				}
			}
		}
	}
	if (Profile.CanEvadeDive && !GroundTarget && !InMelee && !HasTacticalGoal && !StartedJump())
	{
		// Take a lateral interception position instead of idling below an unreachable flyer.
		Fvector Horizontal;
		Horizontal.sub(Position, Owner.Position());
		Horizontal.y = 0.f;
		const float HorizontalDistance = Horizontal.magnitude();
		if (HorizontalDistance < std::max(4.f, Owner.Radius() * 2.f) || (TargetVelocity.y < -1.f && Distance < 12.f))
		{
			if (HorizontalDistance > EPS_L)
			{
				Horizontal.div(HorizontalDistance);
			}
			else
			{
				Horizontal = Owner.XFORM().k;
			}
			Fvector Side = {Horizontal.z, 0.f, -Horizontal.x};
			Fvector Point = Owner.Position();
			Point.mad(Side, std::max(4.f, Owner.Radius() * 2.f) * (Owner.ID() % 2 ? 1.f : -1.f));
			Point.mad(Horizontal, -2.f);
			u32 Vertex;
			if (CELGroundLocalSearch::Resolve(Adapter, Point, TacticalGoal, Vertex))
			{
				Intent = EELCombatIntent::EvadeDive;
				HasTacticalGoal = true;
				ManeuverUntil = Now + 1200;
			}
		}
	}
	if (Profile.CanSprint && !InMelee && !HasTacticalGoal && (IsEscaping || Weapon.HasRecoveryWindow) && Distance > Profile.SprintDistance && Stamina > .95f && s32(Now - SprintAfter) >= 0)
	{
		SprintUntil = Now + Profile.SprintDuration;
		SprintAfter = SprintUntil + Profile.SprintCooldown;
	}
	const bool Sprinting = s32(SprintUntil - Now) > 0 && Stamina > .01f && !InMelee && !HasTacticalGoal;
	Owner.movement().detail().SetMovementFactor(Sprinting ? 1.f + Profile.SprintBonus : 1.f);
	if (Sprinting)
	{
		Intent = EELCombatIntent::Sprint;
	}
}

void CELGroundCombatTactics::Release(IELAdapter& Adapter)
{
	Adapter.Monster().movement().detail().SetMovementFactor(1.f);
	SprintUntil = ManeuverUntil = FocusUntil = LastVisibleAt = CounterUntil = 0;
	HasTacticalGoal = false;
}

void CELGroundCombatTactics::FillDebug(SELDebugRecord& Record) const
{
	static const char* Names[] = {"Сближение", "Завершение атаки", "Уклон от огнестрела", "Уклон от взмаха", "Контратака", "Спринт", "Прыжок вдогонку", "Перехват летуна", "Уход от налёта", "Ложный выпад", "Парная атака: смена угла", "Резкий уход прыжком"};
	string256 Label;
	xr_sprintf(Label, "ТАКТИКА: %s | стамина %.0f%% | пауза шага %.1f с | уклон-прыжка %.1f с", Names[u32(Intent)], Stamina * 100.f, float(std::max(0, s32(std::max(DodgeAfter, DodgeStepAfter) - Device.dwTimeGlobal))) * .001f, float(std::max(0, s32(DodgeJumpAfter - Device.dwTimeGlobal))) * .001f);
	Record.Senses.push_back(Label);
	xr_sprintf(Label, "УВОРОТ: %s | сила %.2f", DodgeReason, DodgeStrength);
	Record.Senses.push_back(Label);
	xr_sprintf(Label, "ПРЫЖОК-УВОРОТ: %s | увод %.2f / нужен %.2f м", EvasionJumpReason, EvasionJumpSide, EvasionJumpRequiredSide);
	Record.Senses.push_back(Label);
	xr_sprintf(Label, "ПРЫЖОК: %s", JumpReason);
	Record.Senses.push_back(Label);
}
