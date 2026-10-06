#pragma once

#include "WatchTypes.h"

class IKinematics;

class CWatchDevice
{
public:
	void Load(const shared_str& section);
	void BindModel(IKinematics* model);
	void Unbind();
	void Update(float dt);

	void DrawImGui();
	void HotSave();

	const SWatchConfig& Config() const { return m_config; }
	const SWatchState& State() const { return m_state; }
	SWatchConfig& Config() { return m_config; }
	SWatchState& State() { return m_state; }

	bool IsLoaded() const { return m_loaded; }
	bool IsEnabled() const { return m_loaded && m_config.enabled; }
	IKinematics* Model() const { return m_model; }
	const shared_str& Section() const { return m_section; }

private:
	void LoadBones(const shared_str& section);
	void LoadDisplay(const shared_str& section);
	void LoadDisplayStatus(const shared_str& section);
	void LoadStatusMetricConfig(const shared_str& section, SWatchStatusMetricConfig& metric);
	void LoadCompass(const shared_str& section);
	void LoadLag(const shared_str& section, SWatchLag& lag);
	void LoadIndicators(const shared_str& section);
	void LoadChannelPresent(const shared_str& section, SWatchChannelPresent& channel);
	void LoadLight(const shared_str& section);

	shared_str m_section;
	SWatchConfig m_config;
	SWatchState m_state;
	IKinematics* m_model = nullptr;
	bool m_loaded = false;

	float m_compass_angle = 0.0f;
	float m_compass_velocity = 0.0f;
	float m_display_error = 0.0f;
	float m_display_update_timer = 0.0f;
	float m_display_glitch_phase = 0.0f;
	float m_display_freeze_until = 0.0f;
	u32 m_last_time = 0;
	u32 m_last_date = 0;
};
