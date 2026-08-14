#pragma once

#include "AnomalyZone.h"

class CGameObject;
class CSE_Abstract;

struct TAnomalyRandomHudVertexParticles final
{
private:

	CAnomalyZone* m_currentAnomalyObject = nullptr;
	bool m_use_procedural_vertex_hud_particles = false;
	float m_max_procedural_vertex_hud_particles_field_distance = 15.f;
	u32 m_r0_max_count_procedural_vertex_hud_particles = 64;
	u32 m_r1_max_count_procedural_vertex_hud_particles = 64;
	Fvector m_initial_spawn_position;
	float max_processing_distance = 200.f;
	float m_update_particles_interval_milliseconds = 500.f;
	float m_update_timer = 0;

	shared_str m_identity_group_name = "group_1";
	bool m_use_r0 = true;
	bool m_use_r1 = true;
	float m_before_play_delay_ms_min = 250.f;
	float m_before_play_delay_ms_max = 800.f;
	float m_r0_playing_time_ms_min = 1350.f;
	float m_r0_playing_time_ms_max = 2350.f;
	float m_r1_playing_time_ms_min = 1350.f;
	float m_r1_playing_time_ms_max = 2350.f;
	float m_r0_after_play_delay_time_ms_min = 300.f;
	float m_r0_after_play_delay_time_ms_max = 600.f;
	float m_r1_after_play_delay_time_ms_min = 300.f;
	float m_r1_after_play_delay_time_ms_max = 600.f;



	xr_vector<shared_str> r0_particles_paths;
	xr_vector<shared_str> r1_particles_paths;

	Fmatrix& XFORM() { return m_currentAnomalyObject->XFORM(); }
	void UpdateParticles();

public:
	void SetInitialSpawnPosition(Fvector vector) { m_initial_spawn_position = Fvector(vector.x, vector.y, vector.z); } // clone
	void BeginComponent(IECSOwner* O);
	void EndComponent();

	void Load(const char* section);
	bool IsEnabled();
	bool AlwaysTheCrow();
	bool IsUseElectricStreams();
	void Update(bool isUpdateCL);
	void net_Spawn(CSE_Abstract* DC);

private:
	ECS_COMPONENT(TAnomalyRandomHudVertexParticles)
		ECS_END
};
