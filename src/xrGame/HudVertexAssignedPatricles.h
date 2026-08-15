#pragma once

class CParticlesObject;
class CActor;


struct _triangle
{
	Fvector face_a_pos;
	Fvector face_b_pos;
	Fvector face_c_pos;
};

struct bind_particle_to_vertex
{
	xr_shared_ptr<CParticlesObject> particle;
	_triangle local_hud_triangle;
	u32 item_id;
	int rid = 2;
	float left_play_time = 0;
	float left_after_play_time = 0;
	bool need_remove = false;
	shared_str identity_group_name;
	float left_before_play_ms = 0.f;
};

struct THudVertexAssignedPatricles final
{
private:
	CActor* m_actor = nullptr;

	xr_vector<bind_particle_to_vertex> hudParticles;

	u32 last_r0_item_id = 0;
	u32 last_r0_faces_count = 0;
	xr_vector<Fvector> m_last_r0_vertices;

	u32 last_r1_faces_count = 0;
	u32 last_r1_item_id = 0;
	xr_vector<Fvector> m_last_r1_vertices;

	xr_vector<_triangle> r0_triangles;
	xr_vector<_triangle> r1_triangles;
	size_t count_r0_triangles = 0;
	size_t count_r1_triangles = 0;

	void UpdateHudPArticles();
	void UpdateLRData();
	void StopAndRemoveHudPArticlesForItemId(u32 id);
	void AddParticleToQueue(bool left_hand, bool right_hand, float play_time, float left_after_play_time, const char* particle_path, const char* identity_group_name, float left_before_play_ms);

public:
	void BeginComponent(IECSOwner* O);
	void EndComponent() {}
	void OnFrame();
	void ScheduleRenderParticles(bool use_r0, shared_str identity_group_name, xr_vector<shared_str>& r0_particles_paths, float r0_playing_time_ms, float r0_delay_time_ms, u32 r0_max_particles, bool use_r1, xr_vector<shared_str>& r1_particles_paths, float r1_playing_time_ms, float r1_delay_time_ms, u32 r1_max_particles, float delay_before_play_min, float delay_before_play_max);
	

private:
	ECS_COMPONENT(THudVertexAssignedPatricles)
	ECS_END
};