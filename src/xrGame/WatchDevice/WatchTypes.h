#pragma once

struct SWatchBones
{
	shared_str root;
	shared_str compass;
	shared_str compass_light;
	shared_str display;
	shared_str rad_indicator;
	shared_str anom_indicator;
};

struct SWatchDisplay
{
	shared_str time_format;
	shared_str date_format;
	shared_str font_time;
	shared_str font_date;
	shared_str font_barometer;
	float update_hz = 1.0f;
	bool barometer_enabled = true;
	float barometer_lookahead_far = 300.0f;
	float barometer_lookahead_near = 30.0f;
};

struct SWatchCompass
{
	shared_str mode;
	shared_str no_target;
	float stiffness = 12.0f;
	float damping = 5.0f;
	float gravity_strength = 35.0f;
	float gravity_falloff = 1.5f;
	float max_deflection = 110.0f;
};

struct SWatchLag
{
	float max_delay = 0.0f;
	float min_update_hz = 1.0f;
	float freeze_chance = 0.0f;
	float flicker = 0.0f;
};

struct SWatchChannelPresent
{
	bool enabled = true;
	bool present_shader = true;
	bool present_ui = false;
	shared_str bone;
	shared_str ui_id;
};

struct SWatchIndicators
{
	SWatchChannelPresent motion;
	SWatchChannelPresent luminosity;
	SWatchChannelPresent noise;
	SWatchChannelPresent anomaly;
	float motion_idle = 0.0f;
	float motion_walk = 0.30f;
	float motion_run = 0.65f;
	float motion_sprint = 1.0f;
	float motion_crouch = 0.15f;
	float motion_creep = 0.10f;
	float motion_climb = 0.40f;
};

struct SWatchLight
{
	float day_emission = 0.0f;
	float dusk_emission = 0.25f;
	float night_emission = 0.75f;
	float dark_emission = 1.0f;
};

struct SWatchStatusMetricConfig
{
	bool enabled = true;
	bool present_indicator = true;
	bool present_bar = false;
	shared_str ui_id;
	float thresholds[4] = {0.0f, 0.25f, 0.50f, 0.75f};
};

struct SWatchDisplayStatus
{
	bool enabled = false;
	SWatchStatusMetricConfig bleeding;
	SWatchStatusMetricConfig hunger;
	SWatchStatusMetricConfig thirst;
	SWatchStatusMetricConfig radiation;
	SWatchStatusMetricConfig health;
	SWatchStatusMetricConfig stamina;
};

struct SWatchConfig
{
	bool enabled = true;
	SWatchBones bones;
	SWatchDisplay display;
	SWatchDisplayStatus display_status;
	SWatchCompass compass;
	SWatchLag surge_lag;
	SWatchLag anomaly_lag;
	SWatchIndicators indicators;
	SWatchLight light;
};

struct SWatchStatusMetric
{
	float value = 0.0f;
	float intensity = 0.0f;
	u8 severity = 0;
	bool active = false;
};

struct SWatchState
{
	float compass_angle = 0.0f;
	float compass_error = 0.0f;
	float barometer = 0.0f;
	float surge_factor = 0.0f;
	float anomaly_factor = 0.0f;
	float noticeability = 0.0f;
	float luminosity = 0.0f;
	float noise = 0.0f;
	float anomaly_intensity = 0.0f;
	float emission = 0.0f;
	float display_quality = 1.0f;
	u8 motion_state = 0;
	xr_string time_text;
	xr_string date_text;
	bool display_glitched = false;
	bool target_valid = false;
	SWatchStatusMetric bleeding;
	SWatchStatusMetric hunger;
	SWatchStatusMetric thirst;
	SWatchStatusMetric radiation;
	SWatchStatusMetric health;
	SWatchStatusMetric stamina;
};
