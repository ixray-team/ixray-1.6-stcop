#include "StdAfx.h"
#include "WatchDevice.h"

namespace
{
shared_str MakeWatchSubSection(const shared_str& root, const char* suffix)
{
	string256 buffer = {};
	xr_sprintf(buffer, "%s.%s", root.c_str(), suffix);
	return shared_str(buffer);
}

shared_str ReadWatchString(const char* section, const char* line, const char* fallback)
{
	return READ_IF_EXISTS(pSettings, r_string, section, line, fallback);
}
}

void CWatchDevice::Load(const shared_str& section)
{
	m_section = section;
	m_loaded = false;
	m_config = SWatchConfig{};
	m_state = SWatchState{};
	m_compass_angle = 0.0f;
	m_compass_velocity = 0.0f;
	m_display_error = 0.0f;
	m_display_update_timer = 0.0f;
	m_display_glitch_phase = 0.0f;
	m_display_freeze_until = 0.0f;
	m_last_time = 0;
	m_last_date = 0;

	if (!pSettings || !pSettings->section_exist(section.c_str()))
	{
		return;
	}

	m_config.enabled = pSettings->read_if_exists<bool>(section.c_str(), "enabled", true);

	LoadBones(MakeWatchSubSection(section, "bones"));
	LoadDisplay(MakeWatchSubSection(section, "display"));
	LoadDisplayStatus(MakeWatchSubSection(section, "display.status"));
	LoadCompass(MakeWatchSubSection(section, "compass"));
	LoadLag(MakeWatchSubSection(section, "lag.surge"), m_config.surge_lag);
	LoadLag(MakeWatchSubSection(section, "lag.anomaly"), m_config.anomaly_lag);
	LoadIndicators(MakeWatchSubSection(section, "indicators"));
	LoadLight(MakeWatchSubSection(section, "light"));

	const shared_str fonts_section = MakeWatchSubSection(section, "fonts");
	if (pSettings->section_exist(fonts_section.c_str()))
	{
		if (pSettings->line_exist(fonts_section.c_str(), "time"))
			m_config.display.font_time = pSettings->r_string(fonts_section.c_str(), "time");
		if (pSettings->line_exist(fonts_section.c_str(), "date"))
			m_config.display.font_date = pSettings->r_string(fonts_section.c_str(), "date");
		if (pSettings->line_exist(fonts_section.c_str(), "barometer"))
			m_config.display.font_barometer = pSettings->r_string(fonts_section.c_str(), "barometer");
	}

	m_loaded = true;
}

void CWatchDevice::LoadBones(const shared_str& section)
{
	if (!pSettings->section_exist(section.c_str()))
	{
		m_config.bones.root = "watch_hud";
		m_config.bones.compass = "j_compas";
		m_config.bones.compass_light = "j_compas_light";
		m_config.bones.display = "j_watch_ui";
		m_config.bones.rad_indicator = "j_rad_indicator";
		m_config.bones.anom_indicator = "j_anom_indicator";
		return;
	}

	m_config.bones.root = ReadWatchString(section.c_str(), "root", "watch_hud");
	m_config.bones.compass = ReadWatchString(section.c_str(), "compass", "j_compas");
	m_config.bones.compass_light = ReadWatchString(section.c_str(), "compass_light", "j_compas_light");
	m_config.bones.display = ReadWatchString(section.c_str(), "display", "j_watch_ui");
	m_config.bones.rad_indicator = ReadWatchString(section.c_str(), "rad_indicator", "j_rad_indicator");
	m_config.bones.anom_indicator = ReadWatchString(section.c_str(), "anom_indicator", "j_anom_indicator");
}

void CWatchDevice::LoadDisplay(const shared_str& section)
{
	if (!pSettings->section_exist(section.c_str()))
	{
		m_config.display.time_format = "HH:MM";
		m_config.display.date_format = "DD:MM:YY";
		return;
	}

	m_config.display.time_format = ReadWatchString(section.c_str(), "time_format", "HH:MM");
	m_config.display.date_format = ReadWatchString(section.c_str(), "date_format", "DD:MM:YY");
	m_config.display.update_hz = pSettings->read_if_exists<float>(section.c_str(), "update_hz", 1.0f);
	m_config.display.barometer_enabled = pSettings->read_if_exists<bool>(section.c_str(), "barometer_enabled", true);
	m_config.display.barometer_lookahead_far = pSettings->read_if_exists<float>(section.c_str(), "barometer_lookahead_far", 300.0f);
	m_config.display.barometer_lookahead_near = pSettings->read_if_exists<float>(section.c_str(), "barometer_lookahead_near", 30.0f);
}

void CWatchDevice::LoadDisplayStatus(const shared_str& section)
{
	m_config.display_status = SWatchDisplayStatus{};

	if (!pSettings->section_exist(section.c_str()))
	{
		return;
	}

	m_config.display_status.enabled = pSettings->read_if_exists<bool>(section.c_str(), "enabled", false);

	LoadStatusMetricConfig(MakeWatchSubSection(m_section, "display.status.bleeding"), m_config.display_status.bleeding);
	LoadStatusMetricConfig(MakeWatchSubSection(m_section, "display.status.hunger"), m_config.display_status.hunger);
	LoadStatusMetricConfig(MakeWatchSubSection(m_section, "display.status.thirst"), m_config.display_status.thirst);
	LoadStatusMetricConfig(MakeWatchSubSection(m_section, "display.status.radiation"), m_config.display_status.radiation);
	LoadStatusMetricConfig(MakeWatchSubSection(m_section, "display.status.health"), m_config.display_status.health);
	LoadStatusMetricConfig(MakeWatchSubSection(m_section, "display.status.stamina"), m_config.display_status.stamina);
}

void CWatchDevice::LoadStatusMetricConfig(const shared_str& section, SWatchStatusMetricConfig& metric)
{
	if (!pSettings->section_exist(section.c_str()))
	{
		return;
	}

	metric.enabled = pSettings->read_if_exists<bool>(section.c_str(), "enabled", metric.enabled);
	metric.present_indicator = pSettings->read_if_exists<bool>(section.c_str(), "present_indicator", metric.present_indicator);
	metric.present_bar = pSettings->read_if_exists<bool>(section.c_str(), "present_bar", metric.present_bar);

	if (pSettings->line_exist(section.c_str(), "ui_id"))
		metric.ui_id = pSettings->r_string(section.c_str(), "ui_id");

	if (pSettings->line_exist(section.c_str(), "thresholds"))
	{
		const char* line = pSettings->r_string(section.c_str(), "thresholds");
		const int count = _GetItemCount(line);
		string64 token = {};
		for (int i = 0; i < 4 && i < count; ++i)
		{
			_GetItem(line, i, token);
			metric.thresholds[i] = (float)atof(token);
		}
	}
}

void CWatchDevice::LoadCompass(const shared_str& section)
{
	if (!pSettings->section_exist(section.c_str()))
	{
		m_config.compass.mode = "target";
		m_config.compass.no_target = "hide";
		return;
	}

	m_config.compass.mode = ReadWatchString(section.c_str(), "mode", "target");
	m_config.compass.no_target = ReadWatchString(section.c_str(), "no_target", "hide");
	m_config.compass.stiffness = pSettings->read_if_exists<float>(section.c_str(), "stiffness", 12.0f);
	m_config.compass.damping = pSettings->read_if_exists<float>(section.c_str(), "damping", 5.0f);
	m_config.compass.gravity_strength = pSettings->read_if_exists<float>(section.c_str(), "gravity_strength", 35.0f);
	m_config.compass.gravity_falloff = pSettings->read_if_exists<float>(section.c_str(), "gravity_falloff", 1.5f);
	m_config.compass.max_deflection = pSettings->read_if_exists<float>(section.c_str(), "max_deflection", 110.0f);
}

void CWatchDevice::LoadLag(const shared_str& section, SWatchLag& lag)
{
	if (!pSettings->section_exist(section.c_str()))
	{
		return;
	}

	lag.max_delay = pSettings->read_if_exists<float>(section.c_str(), "max_delay", lag.max_delay);
	lag.min_update_hz = pSettings->read_if_exists<float>(section.c_str(), "min_update_hz", lag.min_update_hz);
	lag.freeze_chance = pSettings->read_if_exists<float>(section.c_str(), "freeze_chance", lag.freeze_chance);
	lag.flicker = pSettings->read_if_exists<float>(section.c_str(), "flicker", lag.flicker);
}

void CWatchDevice::LoadIndicators(const shared_str& section)
{
	m_config.indicators.motion = SWatchChannelPresent{};
	m_config.indicators.luminosity = SWatchChannelPresent{};
	m_config.indicators.noise = SWatchChannelPresent{};
	m_config.indicators.anomaly = SWatchChannelPresent{};

	if (!pSettings->section_exist(section.c_str()))
	{
		return;
	}

	const bool motion_enabled = pSettings->read_if_exists<bool>(section.c_str(), "motion_enabled", true);
	const bool luminosity_enabled = pSettings->read_if_exists<bool>(section.c_str(), "luminosity_enabled", true);
	const bool noise_enabled = pSettings->read_if_exists<bool>(section.c_str(), "noise_enabled", true);
	const bool anomaly_enabled = pSettings->read_if_exists<bool>(section.c_str(), "anomaly_enabled", true);

	m_config.indicators.motion.enabled = motion_enabled;
	m_config.indicators.luminosity.enabled = luminosity_enabled;
	m_config.indicators.noise.enabled = noise_enabled;
	m_config.indicators.anomaly.enabled = anomaly_enabled;

	m_config.indicators.motion_idle = pSettings->read_if_exists<float>(section.c_str(), "motion_idle", 0.0f);
	m_config.indicators.motion_walk = pSettings->read_if_exists<float>(section.c_str(), "motion_walk", 0.30f);
	m_config.indicators.motion_run = pSettings->read_if_exists<float>(section.c_str(), "motion_run", 0.65f);
	m_config.indicators.motion_sprint = pSettings->read_if_exists<float>(section.c_str(), "motion_sprint", 1.0f);
	m_config.indicators.motion_crouch = pSettings->read_if_exists<float>(section.c_str(), "motion_crouch", 0.15f);
	m_config.indicators.motion_creep = pSettings->read_if_exists<float>(section.c_str(), "motion_creep", 0.10f);
	m_config.indicators.motion_climb = pSettings->read_if_exists<float>(section.c_str(), "motion_climb", 0.40f);

	LoadChannelPresent(MakeWatchSubSection(m_section, "indicators.motion"), m_config.indicators.motion);
	LoadChannelPresent(MakeWatchSubSection(m_section, "indicators.luminosity"), m_config.indicators.luminosity);
	LoadChannelPresent(MakeWatchSubSection(m_section, "indicators.noise"), m_config.indicators.noise);
	LoadChannelPresent(MakeWatchSubSection(m_section, "indicators.anomaly"), m_config.indicators.anomaly);

	m_config.indicators.motion.enabled = motion_enabled && m_config.indicators.motion.enabled;
	m_config.indicators.luminosity.enabled = luminosity_enabled && m_config.indicators.luminosity.enabled;
	m_config.indicators.noise.enabled = noise_enabled && m_config.indicators.noise.enabled;
	m_config.indicators.anomaly.enabled = anomaly_enabled && m_config.indicators.anomaly.enabled;
}

void CWatchDevice::LoadChannelPresent(const shared_str& section, SWatchChannelPresent& channel)
{
	if (!pSettings->section_exist(section.c_str()))
	{
		return;
	}

	channel.enabled = pSettings->read_if_exists<bool>(section.c_str(), "enabled", channel.enabled);
	channel.present_shader = pSettings->read_if_exists<bool>(section.c_str(), "present_shader", channel.present_shader);
	channel.present_ui = pSettings->read_if_exists<bool>(section.c_str(), "present_ui", channel.present_ui);
	if (pSettings->line_exist(section.c_str(), "bone"))
		channel.bone = pSettings->r_string(section.c_str(), "bone");
	if (pSettings->line_exist(section.c_str(), "ui_id"))
		channel.ui_id = pSettings->r_string(section.c_str(), "ui_id");
}

void CWatchDevice::LoadLight(const shared_str& section)
{
	if (!pSettings->section_exist(section.c_str()))
	{
		return;
	}

	m_config.light.day_emission = pSettings->read_if_exists<float>(section.c_str(), "day_emission", 0.0f);
	m_config.light.dusk_emission = pSettings->read_if_exists<float>(section.c_str(), "dusk_emission", 0.25f);
	m_config.light.night_emission = pSettings->read_if_exists<float>(section.c_str(), "night_emission", 0.75f);
	m_config.light.dark_emission = pSettings->read_if_exists<float>(section.c_str(), "dark_emission", 1.0f);
}

void CWatchDevice::BindModel(IKinematics* model)
{
	m_model = model;
}

void CWatchDevice::Unbind()
{
	m_model = nullptr;
}

void CWatchDevice::Update(float dt)
{
	if (!IsEnabled() || !m_model)
	{
		return;
	}

	(void)dt;
}

void CWatchDevice::DrawImGui()
{
}

void CWatchDevice::HotSave()
{
}
