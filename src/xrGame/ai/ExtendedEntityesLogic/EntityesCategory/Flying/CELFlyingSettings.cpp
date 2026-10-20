#include "stdafx.h"
#include <cmath>
#include <algorithm>
#include "ai/ExtendedEntityesLogic/EntityesCategory/Flying/CELFlyingSettings.h"

namespace ExtendedEntityesFlightSettings
{
xrCriticalSection CacheLock;
xr_map<shared_str, SELFlyingSettings> Profiles;

// Читает числовой параметр, заменяет недопустимое значение и ограничивает диапазон.
float Number(const char* Section, const char* Key, float DefaultValue, float Minimum, float Maximum)
{
	float Value = (pSettings)->read_if_exists<float>(Section, Key, DefaultValue);
	if (!_valid(Value))
	{
		Value = DefaultValue;
	}
	clamp(Value, Minimum, Maximum);
	return Value;
}

// Читает скорости, ускорение, повороты и размеры тела летуна.
void ReadKinematics(SELFlyingSettings& Result, const char* Section)
{
	Result.Speed = Number(Section, "flight_speed", 8.0f, 1.0f, 30.0f);
	Result.Acceleration = Number(Section, "flight_acceleration", 8.0f, 1.0f, 30.0f);
	Result.TurningSpeedRatio = Number(Section, "flight_turn_speed_ratio", 0.6f, 0.1f, 1.0f);
	Result.OrientationResponse = Number(Section, "flight_direction_sync_speed", 12.0f, 1.0f, 30.0f);
	Result.TurnRate = Number(Section, "flight_turn_speed", 2.5f, 0.2f, 10.0f);
	Result.ClimbSpeedRatio = Number(Section, "flight_climb_speed_ratio", 0.75f, 0.1f, 1.0f);
	Result.DiveSpeedRatio = Number(Section, "flight_dive_speed_ratio", 1.25f, 1.0f, 2.0f);
	Result.CollisionRadius = Number(Section, "flight_collision_radius", 0.35f, 0.1f, 3.0f);
	Result.CollisionCenterHeight = Number(Section, "flight_body_center_height", 0.4f, 0.1f, 4.0f);
	Result.LandingSpeedMinimum = Number(Section, "flight_landing_min_speed", 0.35f, 0.05f, 5.0f);
	Result.MaximumBank = Number(Section, "flight_max_bank_angle", 0.6f, 0.0f, 1.5f);
	Result.CollisionCenterHeight = std::max(Result.CollisionCenterHeight, Result.CollisionRadius);
}

// Читает ограничения маршрутов, ожидания и локального восстановления полёта.
void ReadNavigation(SELFlyingSettings& Result, const char* Section)
{
	Result.Altitude = Number(Section, "flight_wander_height", 10.0f, 2.0f, 50.0f);
	Result.OrbitRadius = Number(Section, "flight_combat_orbit_radius", 14.0f, 5.0f, 50.0f);
	Result.BoundaryMargin = Number(Section, "flight_boundary_margin", 2.0f, 0.0f, 200.0f);
	Result.CurveRatio = Number(Section, "flight_curve_distance_ratio", 0.65f, 0.1f, 2.0f);
	Result.CurveMinimum = Number(Section, "flight_curve_distance_min", 1.0f, 0.1f, 20.0f);
	Result.CurveMaximum = Number(Section, "flight_curve_distance_max", 30.0f, 1.0f, 100.0f);
	Result.ObstacleLookahead = Number(Section, "flight_obstacle_lookahead", .75f, .1f, 3.f);
	Result.ObstacleMemory = u32(Number(Section, "flight_obstacle_memory", 2.f, .25f, 10.f) * 1000.f);
	Result.InterruptLookahead = Number(Section, "flight_interrupt_lookahead", .75f, .25f, 3.f);
	Result.ProgressDistance = Number(Section, "flight_progress_distance", 0.5f, 0.05f, 5.0f);
	Result.RecoveryForward = Number(Section, "flight_recovery_forward_distance", 3.0f, 0.5f, 20.0f);
	Result.RecoveryClimb = Number(Section, "flight_recovery_climb_distance", 2.0f, 0.5f, 20.0f);
	Result.RecoveryStep = Number(Section, "flight_recovery_step_distance", 1.0f, 0.1f, 10.0f);
	Result.GroundClearance = Number(Section, "flight_ground_clearance", 0.1f, 0.01f, 2.0f);
	Result.CurveMaximum = std::max(Result.CurveMinimum, Result.CurveMaximum);
	Result.ProgressTimeout = u32(Number(Section, "flight_progress_timeout", 8.0f, 1.0f, 60.0f) * 1000.f);
	Result.RouteWaitTimeout = u32(Number(Section, "flight_route_wait_timeout", 10.0f, 1.0f, 60.0f) * 1000.f);
	Result.RouteRetryDelay = u32(Number(Section, "flight_route_retry_delay", 0.25f, 0.05f, 5.0f) * 1000.f);
	Result.SearchCellSize = Number(Section, "flight_search_cell_size", 4.f, 1.f, 20.f);
	Result.SearchNodeLimit = u32(Number(Section, "flight_search_node_limit", 128.f, 8.f, 2048.f));
	Result.SearchTimeout = u32(Number(Section, "flight_search_timeout", 8.f, 1.f, 50.f) * 1000.f);
	Result.RouteWaitTimeout = std::max(Result.RouteWaitTimeout, Result.SearchTimeout + 2000u);
	Result.RecoverySteps = u32(Number(Section, "flight_recovery_steps", 6.f, 1.f, 32.f));
}


// Читает разрешённые высоты и длины свободных перелётов отдельно от боевых виражей.
void ReadWander(SELFlyingSettings& Result, const char* Section)
{
	const char* const ManeuverNames[] = {"glide", "arch", "wave", "weave", "spiral", "loop"};
	static_assert(sizeof(ManeuverNames) / sizeof(ManeuverNames[0]) == u32(EELFlightManeuver::Count));
	const char* Names = (pSettings)->read_if_exists<str_c>(Section, "flight_wander_modes", "glide, arch, wave, weave, spiral, loop");
	xr_string Token;
	int TokenIndex;
	u32 ModeIndex;
	bool IsKnown;
	Result.WanderManeuvers = 0;
	for (TokenIndex = 0; Names && TokenIndex < _GetItemCount(Names); ++TokenIndex)
	{
		_GetItem(Names, TokenIndex, Token);
		IsKnown = false;
		for (ModeIndex = 0; ModeIndex < u32(EELFlightManeuver::Count); ++ModeIndex)
		{
			if (Token == ManeuverNames[ModeIndex])
			{
				Result.WanderManeuvers |= 1u << ModeIndex;
				IsKnown = true;
				break;
			}
		}
		if (!IsKnown)
		{
			Msg("! [ExtendedEntityesLogic] Неизвестный режим полёта '%s' в '%s'", Token.c_str(), Section);
		}
	}
	if (!Result.WanderManeuvers)
	{
		Result.WanderManeuvers = 1u << u32(EELFlightManeuver::Glide);
	}
	Result.ManeuverRadiusMinimum = Number(Section, "flight_maneuver_radius_min", 4.f, 1.f, 30.f);
	Result.ManeuverRadiusMaximum = std::max(Result.ManeuverRadiusMinimum, Number(Section, "flight_maneuver_radius_max", 7.f, 1.f, 30.f));
	const float LegacyHeight = Result.Altitude + Result.CollisionCenterHeight;
	Result.WanderHeightMinimum = Number(Section, "flight_wander_height_min", LegacyHeight, .1f, 100.f);
	Result.WanderHeightMaximum = Number(Section, "flight_wander_height_max", LegacyHeight, .1f, 100.f);
	Result.WanderHeightMinimum = std::max(Result.WanderHeightMinimum, Result.CollisionRadius + Result.GroundClearance);
	Result.WanderHeightMaximum = std::max(Result.WanderHeightMinimum, Result.WanderHeightMaximum);
	Result.WanderDistanceMinimum = Number(Section, "flight_wander_distance_min", 12.f, 1.f, 200.f);
	Result.WanderDistanceMaximum = std::max(Result.WanderDistanceMinimum, Number(Section, "flight_wander_distance_max", 40.f, 1.f, 200.f));
	Result.WanderTurnAngle = deg2rad(Number(Section, "flight_wander_turn_angle", 90.f, 1.f, 170.f));
}

// Читает интервалы отдыха и требования к площадке посадки.
void ReadRest(SELFlyingSettings& Result, const char* Section)
{
	Result.RestInterval = Number(Section, "flight_rest_interval", 120.f, 0.f, 3600.f);
	Result.RestDuration = Number(Section, "flight_rest_duration", 6.f, 1.f, 300.f);
	Result.RestRetryDelay = u32(Number(Section, "flight_rest_retry_delay", 15.f, 1.f, 300.f) * 1000.f);
	Result.RestSupportInterval = u32(Number(Section, "flight_rest_support_interval", .5f, .1f, 5.f) * 1000.f);
	Result.LandingApproachDistance = Number(Section, "flight_landing_approach_distance", 3.f, 1.f, 30.f);
	Result.LandingHeight = Number(Section, "flight_landing_height", 1.5f, .5f, 10.f);
	Result.LandingMinimumNormalY = std::cos(deg2rad(Number(Section, "flight_landing_max_slope", 45.f, 0.f, 65.f)));
	Result.LandingHeightTolerance = Number(Section, "flight_landing_height_tolerance", .2f, .01f, 1.f);
}

// Читает дистанции, урон и параметры воздушной атаки.
void ReadAttack(SELFlyingSettings& Result, const char* Section)
{
	Result.AttackDistance = Number(Section, "flight_attack_contact_distance", 0.35f, 0.05f, 0.5f);
	Result.AttackTargetScale = Number(Section, "flight_attack_target_shape_scale", 1.3f, 1.f, 1.5f);
	Result.AttackDamage = Number(Section, "flight_attack_damage", 0.15f, 0.001f, 10.0f);
	Result.AttackImpulse = Number(Section, "flight_attack_impulse", 10.0f, 0.0f, 1000.0f);
	Result.AttackApproachRatio = Number(Section, "flight_attack_approach_ratio", 0.6f, 0.1f, 1.0f);
	Result.AttackApproachHeightRatio = Number(Section, "flight_attack_approach_height_ratio", 0.6f, 0.1f, 1.0f);
	Result.AttackPulloutPitch = Number(Section, "flight_attack_pullout_pitch", 0.2f, 0.0f, 0.8f);
	Result.AttackReplanDistance = Number(Section, "flight_attack_replan_distance", .5f, .05f, 5.f);
	Result.AttackDiveProbability = Number(Section, "flight_attack_dive_probability", .4f, 0.f, 1.f);
	Result.AttackArcSideRatio = Number(Section, "flight_attack_arc_side_ratio", .65f, 0.f, 1.f);
	Result.AttackLeadTime = Number(Section, "flight_attack_lead_time", 2.f, 0.f, 4.f);
	Result.AttackCooldown = u32(Number(Section, "flight_attack_cooldown", 2.5f, .25f, 30.f) * 1000.f);
	Result.AttackIndicatorTime = u32(Number(Section, "flight_attack_indicator_time", .2f, .05f, 2.f) * 1000.f);
}

} // namespace ExtendedEntityesFlightSettings

// Возвращает кэшированный профиль секции, загружая его при первом обращении.
const SELFlyingSettings& CELFlyingSettings::Read(const char* Section)
{
	xrCriticalSectionGuard Guard(ExtendedEntityesFlightSettings::CacheLock);
	xr_map<shared_str, SELFlyingSettings>::const_iterator ProfileIterator = ExtendedEntityesFlightSettings::Profiles.find(Section);
	if (ProfileIterator != ExtendedEntityesFlightSettings::Profiles.end())
	{
		return ProfileIterator->second;
	}
	SELFlyingSettings Result;
	Result.CanWander = (pSettings)->read_if_exists<bool>(Section, "flight_free_wander", true);
	ExtendedEntityesFlightSettings::ReadKinematics(Result, Section);
	ExtendedEntityesFlightSettings::ReadNavigation(Result, Section);
	ExtendedEntityesFlightSettings::ReadWander(Result, Section);
	ExtendedEntityesFlightSettings::ReadAttack(Result, Section);
	ExtendedEntityesFlightSettings::ReadRest(Result, Section);
	const char* FlightCycle = (pSettings)->read_if_exists<str_c>(Section, "flight_animation", "fly_idle");
	Result.FlightAnimation = FlightCycle && *FlightCycle ? FlightCycle : "fly_idle";
	const char* AttackCycle = (pSettings)->read_if_exists<str_c>(Section, "flight_attack_animation", Result.FlightAnimation.c_str());
	Result.AttackAnimation = AttackCycle && *AttackCycle ? AttackCycle : Result.FlightAnimation.c_str();
	const char* IdleCycle = (pSettings)->read_if_exists<str_c>(Section, "flight_idle_animation", Result.FlightAnimation.c_str());
	Result.IdleAnimation = IdleCycle && *IdleCycle ? IdleCycle : Result.FlightAnimation.c_str();
	return ExtendedEntityesFlightSettings::Profiles.emplace(shared_str(Section), std::move(Result)).first->second;
}

// Сбрасывает накопленные данные и незавершённую работу подсистемы.
void CELFlyingSettings::Clear()
{
	xrCriticalSectionGuard Guard(ExtendedEntityesFlightSettings::CacheLock);
	ExtendedEntityesFlightSettings::Profiles.clear();
}
