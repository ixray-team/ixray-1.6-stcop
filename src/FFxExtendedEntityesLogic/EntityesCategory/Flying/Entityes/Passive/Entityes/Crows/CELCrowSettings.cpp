#include "FFxExtendedEntityesLogic/stdafx.h"
#include <algorithm>
#include "FFxExtendedEntityesLogic/EntityesCategory/Flying/Entityes/Passive/Entityes/Crows/CELCrowSettings.h"

namespace ExtendedEntityesCrowSettings
{
xrCriticalSection CacheLock;
xr_map<shared_str, SELCrowSettings> Cache;
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
// Читает упорядоченные нижнюю и верхнюю границы случайного диапазона.
SELCrowRange Range(const char* Section, const char* Prefix, float Minimum, float Maximum)
{
	string128 Key;
	SELCrowRange Result;
	xr_sprintf(Key, "%s_min", Prefix);
	Result.Minimum = Number(Section, Key, Minimum, 0.f, 86400.f);
	xr_sprintf(Key, "%s_max", Prefix);
	Result.Maximum = std::max(Result.Minimum, Number(Section, Key, Maximum, 0.f, 86400.f));
	return Result;
}
// Читает имена костей и параметры визуальных поз вороны.
SELCrowPoseSettings LoadPose(const char* Section)
{
	SELCrowPoseSettings Result;
	const char* WingKeys[] = {"crow_left_shoulder_bone", "crow_left_elbow_bone", "crow_left_wrist_bone", "crow_left_finger_bone", "crow_right_shoulder_bone", "crow_right_elbow_bone", "crow_right_wrist_bone", "crow_right_finger_bone"};
	const char* AngleKeys[] = {"crow_fold_shoulder", "crow_fold_elbow", "crow_fold_wrist", "crow_fold_finger"};
	u32 BoneIndex;
	for (BoneIndex = 0; BoneIndex < 8; ++BoneIndex)
	{
		Result.WingNames[BoneIndex] = (pSettings)->read_if_exists<str_c>(Section, WingKeys[BoneIndex], "");
	}
	for (BoneIndex = 0; BoneIndex < 4; ++BoneIndex)
	{
		Result.WingAngles[BoneIndex] = (pSettings)->read_if_exists<Fvector>(Section, AngleKeys[BoneIndex], Fvector().set(0.f, 0.f, 0.f));
		if (!_valid(Result.WingAngles[BoneIndex]))
		{
			Result.WingAngles[BoneIndex].set(0.f, 0.f, 0.f);
		}
		Result.WingAngles[BoneIndex].mul(PI / 180.f);
	}
	Result.NeckName = (pSettings)->read_if_exists<str_c>(Section, "crow_peck_neck_bone", "bip01_neck");
	Result.HeadName = (pSettings)->read_if_exists<str_c>(Section, "crow_peck_head_bone", "bip01_head");
	Result.BodyName = (pSettings)->read_if_exists<str_c>(Section, "crow_idle_body_bone", "bip01_pelvis");
	Result.TailName = (pSettings)->read_if_exists<str_c>(Section, "crow_idle_tail_bone", "bip01_tail");
	Result.LegNames[0] = (pSettings)->read_if_exists<str_c>(Section, "crow_idle_left_leg_bone", "bip01_l_thigh");
	Result.LegNames[1] = (pSettings)->read_if_exists<str_c>(Section, "crow_idle_right_leg_bone", "bip01_r_thigh");
	Result.BeakOffset = (pSettings)->read_if_exists<Fvector>(Section, "crow_peck_beak_offset", Result.BeakOffset);
	if (!_valid(Result.BeakOffset))
	{
		Result.BeakOffset.set(.12f, 0.f, 0.f);
	}
	Result.LookYawAxis = (pSettings)->read_if_exists<Fvector>(Section, "crow_look_yaw_axis", Result.LookYawAxis);
	Result.LookPitchAxis = (pSettings)->read_if_exists<Fvector>(Section, "crow_look_pitch_axis", Result.LookPitchAxis);
	if (!_valid(Result.LookYawAxis) || Result.LookYawAxis.square_magnitude() < EPS_L)
	{
		Result.LookYawAxis.set(0.f, 1.f, 0.f);
	}
	if (!_valid(Result.LookPitchAxis) || Result.LookPitchAxis.square_magnitude() < EPS_L)
	{
		Result.LookPitchAxis.set(0.f, 0.f, 1.f);
	}
	Result.LookYawAxis.normalize_safe();
	Result.LookPitchAxis.normalize_safe();
	Result.PeckAngles[0] = (pSettings)->read_if_exists<Fvector>(Section, "crow_peck_neck_angles", Fvector().set(35.f, 0.f, 0.f));
	Result.PeckAngles[1] = (pSettings)->read_if_exists<Fvector>(Section, "crow_peck_head_angles", Fvector().set(20.f, 0.f, 0.f));
	for (BoneIndex = 0; BoneIndex < 2; ++BoneIndex)
	{
		if (!_valid(Result.PeckAngles[BoneIndex]))
		{
			Result.PeckAngles[BoneIndex].set(0.f, 0.f, 0.f);
		}
		Result.PeckAngles[BoneIndex].mul(PI / 180.f);
	}
	Result.HasInverseKinematics = (pSettings)->read_if_exists<bool>(Section, "crow_peck_ik_enabled", true);
	Result.FoldSpeed = Number(Section, "crow_fold_speed", 8.f, .1f, 20.f);
	Result.OpenSpeed = Number(Section, "crow_fold_open_speed", 10.f, .1f, 20.f);
	Result.FeatherWidth = Number(Section, "crow_fold_feather_width", .35f, .2f, 1.f);
	Result.FeatherLength = Number(Section, "crow_fold_feather_length", .9f, .7f, 1.f);
	Result.HopWingAmount = Number(Section, "crow_hop_wing_weight", .65f, 0.f, 1.f);
	Result.IdleAmount = Number(Section, "crow_idle_amount", 1.f, 0.f, 2.f);
	Result.SettleDuration = Number(Section, "crow_landing_settle_duration", .55f, .1f, 5.f);
	Result.PeckMaxAngle = Number(Section, "crow_peck_ik_max_angle", 90.f, 1.f, 180.f) * PI / 180.f;
	Result.ContactDistance = Number(Section, "crow_peck_contact_distance", .02f, .005f, .15f);
	Result.NeckExtension = Number(Section, "crow_peck_ik_neck_extension", .12f, 0.f, .5f);
	Result.LookSpeed = Number(Section, "crow_head_look_speed", 4.f, .1f, 20.f);
	Result.LookAngle = Number(Section, "crow_look_angle", 30.f, 0.f, 90.f) * PI / 180.f;
	Result.PitchRange = Number(Section, "crow_head_pitch_range", 12.f, 0.f, 90.f) * PI / 180.f;
	Result.NeckShare = Number(Section, "crow_neck_look_share", .35f, 0.f, 1.f);
	Result.LookDuration = Number(Section, "crow_look_duration", 1.f, .1f, 10.f);
	Result.DetailDistance = Number(Section, "crow_animation_detail_distance", 30.f, 1.f, 1000.f);
	Result.FocusDistance = Number(Section, "crow_animation_focus_distance", 100.f, 1.f, 2000.f);
	Result.FocusFraction = Number(Section, "crow_animation_focus_fraction", .3f, 0.f, 1.f);

	Result.LookInterval = Range(Section, "crow_look_interval", 2.f, 5.f);
	Result.RuffleInterval = Range(Section, "crow_idle_ruffle_interval", 4.f, 10.f);
	return Result;
}

// Читает параметры секции и подготавливает связанные ресурсы.
SELCrowSettings Load(const char* Section)
{
	SELCrowSettings Result;
	Result.Pose = LoadPose(Section);
	Result.Enabled = (pSettings)->read_if_exists<bool>(Section, "crow_ai_enabled", true);
	Result.Mapwide = (pSettings)->read_if_exists<bool>(Section, "crow_roam_entire_level", true);
	Result.NightEnabled = (pSettings)->read_if_exists<bool>(Section, "crow_night_enabled", true);
	Result.RainEnabled = (pSettings)->read_if_exists<bool>(Section, "crow_rain_shelter_enabled", true);
	Result.TreeShelter = (pSettings)->read_if_exists<bool>(Section, "crow_tree_shelter_enabled", true);
	const char* GroundMovement = (pSettings)->read_if_exists<str_c>(Section, "flight_ground_movement", "walk");
	Result.GroundHops = (pSettings)->read_if_exists<bool>(Section, "crow_ground_hops", GroundMovement && !xr_strcmp(GroundMovement, "hop"));
	Result.HungerTime = Number(Section, "crow_hunger_time", 900.f, 1.f, 86400.f);
	Result.Hungry = Number(Section, "crow_hungry_threshold", .35f, 0.f, 1.f);
	Result.Full = Number(Section, "crow_full_threshold", .9f, 0.f, 1.f);
	Result.SeedGain = Number(Section, "crow_seed_gain_per_peck", .002f, 0.f, 1.f);
	Result.MeatGain = Number(Section, "crow_meat_gain_per_peck", .07f, 0.f, 1.f);
	Result.WeatherInterval = Number(Section, "crow_weather_interval", .5f, .1f, 10.f);
	Result.PerceptionInterval = Number(Section, "crow_perception_interval", .25f, 0.05f, 10.f);
	Result.RoamRadius = Number(Section, "crow_roam_radius", 40.f, 1.f, 1000.f);
	Result.LegDistance = Number(Section, "crow_sky_leg_distance", 40.f, 5.f, 300.f);
	Result.FearRadius = Number(Section, "crow_fear_radius", 10.f, 0.f, 100.f);
	Result.FoodRadius = Number(Section, "crow_corpse_search_radius", 70.f, 0.f, 500.f);
	Result.FoodSafeRadius = Number(Section, "crow_corpse_safe_radius", 30.f, 0.f, 200.f);
	Result.FearRise = Number(Section, "crow_fear_rise_speed", 1.5f, 0.f, 10.f);
	Result.FearDecay = Number(Section, "crow_fear_decay_speed", .2f, 0.f, 10.f);
	Result.FearTrigger = Number(Section, "crow_fear_trigger_threshold", .35f, 0.f, 1.f);
	Result.FearCalm = Number(Section, "crow_fear_calm_threshold", .1f, 0.f, 1.f);
	Result.ShotRadius = Number(Section, "crow_shot_fear_radius", 250.f, 0.f, 1000.f);
	Result.ExplosionRadius = Number(Section, "crow_explosion_fear_radius", 400.f, 0.f, 2000.f);
	Result.ShotFear = Number(Section, "crow_fear_shot_gain", 1.f, 0.f, 10.f);
	Result.ExplosionFear = Number(Section, "crow_fear_explosion_gain", 1.5f, 0.f, 10.f);
	Result.NoisePower = Number(Section, "crow_noise_min_power", .01f, 0.f, 1.f);
	Result.NoiseRadius = Number(Section, "crow_noise_fear_radius", 400.f, 0.f, 2000.f);
	Result.NoiseFearGain = Number(Section, "crow_fear_noise_gain", .35f, 0.f, 10.f);
	Result.EscapeDistance = Number(Section, "crow_escape_distance", 18.f, 1.f, 200.f);
	Result.TakeoffForwardRatio = Number(Section, "crow_takeoff_forward_ratio", 1.5f, .5f, 4.f);
	Result.EscapeHeight = Number(Section, "crow_escape_height", 10.f, 1.f, 100.f);
	Result.NightStart = Number(Section, "crow_night_start_hour", 20.f, 0.f, 24.f);
	Result.NightEnd = Number(Section, "crow_night_end_hour", 6.f, 0.f, 24.f);
	Result.NightForageChance = Number(Section, "crow_night_forage_probability", .35f, 0.f, 1.f);
	Result.RainStart = Number(Section, "crow_rain_start_threshold", .1f, 0.f, 1.f);
	Result.RainStop = Number(Section, "crow_rain_stop_threshold", .03f, 0.f, 1.f);
	Result.ShelterRadius = Number(Section, "crow_shelter_search_radius", 65.f, 1.f, 300.f);
	Result.ShelterHeight = Number(Section, "crow_shelter_cover_height", 40.f, 1.f, 100.f);
	Result.ShelterClearance = Number(Section, "crow_shelter_clearance", .5f, 0.1f, 10.f);
	Result.TreeRadius = Number(Section, "crow_tree_cover_radius", 3.f, 0.1f, 20.f);
	Result.TreeHeight = Number(Section, "crow_tree_cover_height", 10.f, 1.f, 40.f);
	Result.PerchRadius = Number(Section, "crow_perch_search_radius", 25.f, 1.f, 100.f);
	Result.PerchMaxRadius = Number(Section, "crow_perch_search_max_radius", 50.f, 1.f, 200.f);
	Result.PerchSpacing = Number(Section, "crow_perch_spacing", 3.f, 0.1f, 20.f);
	Result.RoofMin = Number(Section, "crow_roof_height_min", 3.f, 0.f, 100.f);
	Result.RoofMax = Number(Section, "crow_roof_height_max", 35.f, 1.f, 200.f);
	Result.MaxSlope = Number(Section, "crow_roof_max_slope", 35.f, 0.f, 80.f);
	Result.GroundChance = Number(Section, "crow_ground_choice_probability", .6f, 0.f, 1.f);
	Result.GroundSpeed = Number(Section, "crow_ground_speed", .5f, 0.05f, 5.f);
	Result.StepRadius = Number(Section, "crow_ground_step_radius", .7f, 0.05f, 5.f);
	Result.GroundOffset = Number(Section, "flight_ground_offset", .24f, 0.f, 2.f);
	Result.HopHeight = Number(Section, "flight_ground_hop_height", .12f, 0.f, 1.f);
	Result.PeckDuration = Number(Section, "crow_peck_duration", .28f, 0.05f, 5.f);
	Result.CircleProbability = Number(Section, "crow_corpse_circle_probability", .8f, 0.f, 1.f);
	Result.LandingHeight = Number(Section, "flight_landing_height", 1.5f, .1f, 10.f);
	Result.LandingApproachDistance = Number(Section, "crow_landing_approach_distance", 2.f, .1f, 20.f);
	Result.LandingCurveRatio = Number(Section, "crow_landing_curve_ratio", .4f, .1f, 1.f);
	Result.LandingCurveMin = Number(Section, "crow_landing_curve_distance_min", .3f, .1f, 10.f);
	Result.LandingCurveMax = Number(Section, "crow_landing_curve_distance_max", 10.f, 1.f, 50.f);
	Result.CorpseGap = Number(Section, "crow_corpse_side_distance", .22f, 0.02f, 2.f);
	Result.CorpseReach = Number(Section, "crow_corpse_reach_distance", 1.f, 0.05f, 3.f);
	Result.LandingLimit = Number(Section, "crow_landing_total_limit", 30.f, 1.f, 120.f);
	Result.LandingStall = Number(Section, "crow_landing_stall_limit", 1.5f, 0.2f, 10.f);
	Result.RejectedPerchTime = Number(Section, "crow_rejected_perch_time", 10.f, 1.f, 120.f);
	Result.SyncInterval = Number(Section, "crow_offline_sync_interval", .25f, 0.05f, 10.f);
	Result.FoodSlots = u32(Number(Section, "crow_corpse_slots", 3.f, 1.f, 64.f));
	Result.Attempts = u32(Number(Section, "crow_navigation_attempts", 12.f, 1.f, 64.f));
	Result.Sky = Range(Section, "crow_sky_time", 20.f, 50.f);
	Result.Ground = Range(Section, "crow_ground_time", 6.f, 14.f);
	Result.Roof = Range(Section, "crow_roof_time", 10.f, 30.f);
	Result.Corpse = Range(Section, "crow_corpse_time", 60.f, 180.f);
	Result.Escape = Range(Section, "crow_escape_time", 12.f, 25.f);
	Result.Safe = Range(Section, "crow_safe_time", 20.f, 40.f);
	Result.Retry = Range(Section, "crow_retry_time", 0.15f, 0.4f);
	Result.StartDelay = Range(Section, "crow_ai_start_delay", 0.05f, 0.2f);
	Result.InitialSatiety = Range(Section, "crow_initial_satiety", 0.25f, 0.9f);
	Result.Altitude = Range(Section, "crow_sky_height", 12.f, 25.f);
	Result.SpeedScale = Range(Section, "crow_flight_speed_scale", 0.85f, 1.15f);
	Result.Peck = Range(Section, "crow_peck_interval", 0.5f, 1.1f);
	Result.MeatPeck = Range(Section, "crow_corpse_peck_interval", 0.8f, 1.6f);
	Result.SatedPeck = Range(Section, "crow_sated_peck_interval", 2.f, 4.f);
	Result.Step = Range(Section, "crow_ground_step_interval", 2.f, 5.f);
	Result.NightRest = Range(Section, "crow_night_rest_time", 30.f, 90.f);
	Result.NightForage = Range(Section, "crow_night_forage_time", 6.f, 14.f);
	Result.NightStep = Range(Section, "crow_night_step_interval", 4.f, 8.f);
	Result.NightDelay = Range(Section, "crow_night_reaction_delay", 5.f, 120.f);
	Result.WakeDelay = Range(Section, "crow_day_reaction_delay", 5.f, 120.f);
	Result.RainDelay = Range(Section, "crow_rain_reaction_delay", 5.f, 90.f);
	Result.DryDelay = Range(Section, "crow_shelter_dry_delay", 5.f, 120.f);
	Result.OfflineTick = Range(Section, "crow_offline_update_interval", 1.f, 3.f);
	Result.CircleTime = Range(Section, "crow_corpse_circle_time", 12.f, 30.f);
	Result.CircleRadius = Range(Section, "crow_corpse_circle_radius", 8.f, 14.f);
	Result.CircleHeight = Range(Section, "crow_corpse_circle_height", 4.f, 10.f);
	Result.HeightChange = Range(Section, "crow_sky_height_change_interval", 3.f, 6.f);
	Result.TakeoffHeight = Range(Section, "crow_takeoff_height", 8.f, 20.f);
	Result.Altitude.Minimum = std::max(1.f, Result.Altitude.Minimum);
	Result.Altitude.Maximum = std::max(Result.Altitude.Minimum, Result.Altitude.Maximum);
	Result.SpeedScale.Minimum = std::max(.1f, Result.SpeedScale.Minimum);
	Result.SpeedScale.Maximum = std::max(Result.SpeedScale.Minimum, Result.SpeedScale.Maximum);
	Result.OfflineTick.Minimum = std::max(.1f, Result.OfflineTick.Minimum);
	Result.OfflineTick.Maximum = std::max(Result.OfflineTick.Minimum, Result.OfflineTick.Maximum);
	Result.InitialSatiety.Minimum = std::min(1.f, Result.InitialSatiety.Minimum);
	Result.InitialSatiety.Maximum = std::min(1.f, Result.InitialSatiety.Maximum);
	Result.Full = std::max(Result.Hungry, Result.Full);
	Result.FearTrigger = std::max(Result.FearCalm, Result.FearTrigger);
	return Result;
}
} // namespace ExtendedEntityesCrowSettings

// Возвращает кэшированный профиль секции, загружая его при первом обращении.
const SELCrowSettings& CELCrowSettings::Read(const char* Section)
{
	xrCriticalSectionGuard Guard(ExtendedEntityesCrowSettings::CacheLock);
	xr_map<shared_str, SELCrowSettings>::const_iterator Found = ExtendedEntityesCrowSettings::Cache.find(Section);
	if (Found != ExtendedEntityesCrowSettings::Cache.end())
	{
		return Found->second;
	}
	return ExtendedEntityesCrowSettings::Cache.emplace(Section, ExtendedEntityesCrowSettings::Load(Section)).first->second;
}
// Сбрасывает накопленные данные и незавершённую работу подсистемы.
void CELCrowSettings::Clear()
{
	xrCriticalSectionGuard Guard(ExtendedEntityesCrowSettings::CacheLock);
	ExtendedEntityesCrowSettings::Cache.clear();
}
