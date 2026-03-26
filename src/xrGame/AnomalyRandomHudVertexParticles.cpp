#include "stdafx.h"
#include "AnomalyRandomHudVertexParticles.h"
#include "HudVertexAssignedPatricles.h"
#include "AnomalyZone.h"
#include "Actor.h"

void TAnomalyRandomHudVertexParticles::BeginComponent(IECSOwner* O)
{
	m_currentAnomalyObject = smart_cast<CAnomalyZone*>(O);
}

void TAnomalyRandomHudVertexParticles::EndComponent() {}


void ParseRandomParticlesPaths(const char* iniSection, const char* iniParameter, xr_vector<shared_str>& particlesPaths)
{
	particlesPaths.clear();

	if (pSettings->line_exist(iniSection, iniParameter))
	{
		xr_string unsplittedPaths = pSettings->r_string(iniSection, iniParameter);
		if (!unsplittedPaths.empty())
		{
			xr_vector<xr_string> paths = unsplittedPaths.RemoveWhitespaces().Split();
			for (xr_string& pg_path : paths)
			{
				particlesPaths.push_back(pg_path.c_str());
			}
		}
	}
}

void TAnomalyRandomHudVertexParticles::Load(const char* section)
{
	xr_string options_section = pSettings->read_if_exists<str_c>(section, "procedural_vertex_hud_particles_options_section", "");
	if (!options_section.empty())
	{
		const char* sect = options_section.c_str();
		m_use_procedural_vertex_hud_particles = pSettings->read_if_exists<bool>(sect, "use_procedural_vertex_hud_particles", false);

		if (m_use_procedural_vertex_hud_particles)
		{
			max_processing_distance = pSettings->read_if_exists<float>(sect, "max_processing_distance", max_processing_distance);
			m_max_procedural_vertex_hud_particles_field_distance = pSettings->read_if_exists<float>(sect, "max_procedural_vertex_hud_particles_field_distance", m_max_procedural_vertex_hud_particles_field_distance);
			m_update_particles_interval_milliseconds = pSettings->read_if_exists<float>(sect, "update_particles_interval_milliseconds", m_update_particles_interval_milliseconds);

			m_identity_group_name = pSettings->read_if_exists<str_c>(sect, "identity_group_name", m_identity_group_name.c_str());
			m_before_play_delay_ms_min = pSettings->read_if_exists<float>(sect, "before_play_delay_ms_min", m_before_play_delay_ms_min);
			m_before_play_delay_ms_max = pSettings->read_if_exists<float>(sect, "before_play_delay_ms_max", m_before_play_delay_ms_max);

			m_use_r0 = pSettings->read_if_exists<bool>(sect, "is_allow_particles_for_right_hand", m_use_r0);
			m_use_r1 = pSettings->read_if_exists<bool>(sect, "is_allow_particles_for_left_hand", m_use_r1);

			m_r0_max_count_procedural_vertex_hud_particles = pSettings->read_if_exists<u32>(sect, "right_hand_max_count_particles", m_r0_max_count_procedural_vertex_hud_particles);
			m_r1_max_count_procedural_vertex_hud_particles = pSettings->read_if_exists<u32>(sect, "left_hand_max_count_particles", m_r1_max_count_procedural_vertex_hud_particles);

			m_r0_playing_time_ms_min = pSettings->read_if_exists<float>(sect, "right_hand_particle_playing_time_ms_min", m_r0_playing_time_ms_min);
			m_r0_playing_time_ms_max = pSettings->read_if_exists<float>(sect, "right_hand_particle_playing_time_ms_max", m_r0_playing_time_ms_max);
			m_r1_playing_time_ms_min = pSettings->read_if_exists<float>(sect, "left_hand_particle_playing_time_ms_min", m_r1_playing_time_ms_min);
			m_r1_playing_time_ms_max = pSettings->read_if_exists<float>(sect, "left_hand_particle_playing_time_ms_max", m_r1_playing_time_ms_max);

			m_r0_after_play_delay_time_ms_min = pSettings->read_if_exists<float>(sect, "right_hand_after_play_delay_time_ms_min", m_r0_after_play_delay_time_ms_min);
			m_r0_after_play_delay_time_ms_max = pSettings->read_if_exists<float>(sect, "right_hand_after_play_delay_time_ms_max", m_r0_after_play_delay_time_ms_max);
			m_r1_after_play_delay_time_ms_min = pSettings->read_if_exists<float>(sect, "left_hand_after_play_delay_time_ms_min", m_r1_after_play_delay_time_ms_min);
			m_r1_after_play_delay_time_ms_max = pSettings->read_if_exists<float>(sect, "left_hand_after_play_delay_time_ms_max", m_r1_after_play_delay_time_ms_max);

			ParseRandomParticlesPaths(sect, "right_hand_particles_paths", r0_particles_paths);
			ParseRandomParticlesPaths(sect, "left_hand_particles_paths", r1_particles_paths);
		}
	}
}

bool TAnomalyRandomHudVertexParticles::AlwaysTheCrow()
{
	return IsEnabled() && Actor()->Position().distance_to_xz(m_initial_spawn_position) <= max_processing_distance;
}

bool TAnomalyRandomHudVertexParticles::IsUseElectricStreams()
{
	return m_use_procedural_vertex_hud_particles;
}

bool TAnomalyRandomHudVertexParticles::IsEnabled()
{
	return IsUseElectricStreams() && m_currentAnomalyObject && m_currentAnomalyObject->IsEnabled();
}

void TAnomalyRandomHudVertexParticles::net_Spawn(CSE_Abstract* DC)
{
}

void TAnomalyRandomHudVertexParticles::Update(bool isUpdateCL)
{
	if (!IsEnabled() || !isUpdateCL || Actor() == nullptr || m_max_procedural_vertex_hud_particles_field_distance <= 0)
	{
		return;
	}

	m_update_timer -= Device.fTimeDelta * 1000;

	if (m_update_timer <= 0)
	{
		m_update_timer = m_update_particles_interval_milliseconds;
		UpdateParticles();
	}
}

void TAnomalyRandomHudVertexParticles::UpdateParticles()
{
	float distance = Actor()->Position().distance_to(XFORM().c);
	if (distance > m_max_procedural_vertex_hud_particles_field_distance)
	{
		return;
	}

	if (THudVertexAssignedPatricles* HudVertexAssignedPatricles = Actor()->GetHudVertexAssignedPatriclesComponent())
	{
		float r0_playing_time_ms = Random.randF(m_r0_playing_time_ms_min, m_r0_playing_time_ms_max);
		float r1_playing_time_ms = Random.randF(m_r1_playing_time_ms_min, m_r1_playing_time_ms_max);

		float r0_after_play_delay_time_ms = Random.randF(m_r0_after_play_delay_time_ms_min, m_r0_after_play_delay_time_ms_max);
		float r1_after_play_delay_time_ms = Random.randF(m_r1_after_play_delay_time_ms_min, m_r1_after_play_delay_time_ms_max);

		float percent_distance_to_center = 1.0f - ((distance * 100) / m_max_procedural_vertex_hud_particles_field_distance) / 100;

		u32 r0_max_particles = percent_distance_to_center * m_r0_max_count_procedural_vertex_hud_particles;
		u32 r1_max_particles = percent_distance_to_center * m_r1_max_count_procedural_vertex_hud_particles;

		HudVertexAssignedPatricles->ScheduleRenderParticles(
			m_use_r0, 
			m_identity_group_name, 
			r0_particles_paths,
			r0_playing_time_ms, 
			r0_after_play_delay_time_ms, 
			r0_max_particles, 
			m_use_r1, 
			r1_particles_paths, 
			r1_playing_time_ms, 
			r1_after_play_delay_time_ms,
			r1_max_particles,
			m_before_play_delay_ms_min,
			m_before_play_delay_ms_max
		);
	
	}
}