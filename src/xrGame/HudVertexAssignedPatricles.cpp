#include "stdafx.h"
#include "HudVertexAssignedPatricles.h"
#include "Actor.h"

void THudVertexAssignedPatricles::BeginComponent(IECSOwner* O)
{
	m_actor = smart_cast<CActor*>(O);
}

void THudVertexAssignedPatricles::OnFrame()
{
	UpdateLRData();
	UpdateHudPArticles();
}

void THudVertexAssignedPatricles::StopAndRemoveHudPArticlesForItemId(u32 id)
{
	std::erase_if(
		hudParticles,
		[id](bind_particle_to_vertex& entry)
		{
			if (entry.item_id == id && entry.particle->IsPlaying())
			{
				entry.particle->Stop();
			}

			return entry.item_id == id;
		}
	);
}

void THudVertexAssignedPatricles::UpdateLRData()
{
	Fvector buff[3];
	u32 counter = 0;

	if (g_player_hud->attached_item(0) && g_player_hud->attached_item(0)->need_renderable() && last_r0_item_id != g_player_hud->attached_item(0)->m_parent_hud_item->object().ID())
	{
		StopAndRemoveHudPArticlesForItemId(last_r0_item_id);
		last_r0_item_id = 0;
		m_last_r0_vertices.clear();

		if (CHudItem* HI = g_player_hud->attached_item(0)->m_parent_hud_item)
		{
			if (IKinematics* KI = HI->HudItemData()->m_model)
			{
				last_r0_faces_count = KI->GetFacesCount();
				last_r0_item_id = HI->object().ID();
				m_last_r0_vertices.reserve(last_r0_faces_count * 3);
				KI->EnumBoneVertices(m_last_r0_vertices);
				
				counter = 0;
				r0_triangles.clear();
				count_r0_triangles = 0;

				for (Fvector& vert : m_last_r0_vertices)
				{
					buff[counter] = vert;
					counter++;

					if (counter == 3)
					{
						counter = 0;
						r0_triangles.emplace_back(buff[0], buff[1], buff[2]);
					}
				}

				count_r0_triangles = r0_triangles.size();
			}
		}
	}

	if (g_player_hud->attached_item(1) && g_player_hud->attached_item(1)->need_renderable() && last_r1_item_id != g_player_hud->attached_item(1)->m_parent_hud_item->object().ID())
	{
		StopAndRemoveHudPArticlesForItemId(last_r1_item_id);
		last_r1_item_id = 0;
		m_last_r1_vertices.clear();

		if (CHudItem* HI = g_player_hud->attached_item(1)->m_parent_hud_item)
		{
			if (IKinematics* KI = HI->HudItemData()->m_model)
			{
				last_r1_faces_count = KI->GetFacesCount();
				last_r1_item_id = HI->object().ID();
				m_last_r1_vertices.reserve(last_r1_faces_count * 3);
				KI->EnumBoneVertices(m_last_r1_vertices);

				counter = 0;
				r1_triangles.clear();
				count_r1_triangles = 0;

				for (Fvector& vert : m_last_r1_vertices)
				{
					buff[counter] = vert;
					counter++;

					if (counter == 3)
					{
						counter = 0;
						r1_triangles.emplace_back(buff[0], buff[1], buff[2]);
					}
				}

				count_r1_triangles = r1_triangles.size();
			}
		}
	}
}

void THudVertexAssignedPatricles::UpdateHudPArticles()
{
	std::erase_if(
		hudParticles,
		[](bind_particle_to_vertex& pg_vertex_info)
		{
			if (pg_vertex_info.need_remove)
			{
				if (pg_vertex_info.particle->IsPlaying())
				{
					pg_vertex_info.particle->Stop();
				}
				pg_vertex_info.particle->Destroy();
			}

			return pg_vertex_info.need_remove;
		}
	);

	bool b_r0 = (g_player_hud->attached_item(0) && g_player_hud->attached_item(0)->need_renderable());
	bool b_r1 = (g_player_hud->attached_item(1) && g_player_hud->attached_item(1)->need_renderable());
	
	_triangle trix;
	Fmatrix item_transform;
	Fmatrix XF;
	Fvector normal_dir;

	if (b_r0)
	{
		Fvector box_c, box_hs;
		g_player_hud->attached_item(0)->m_model->dcast_RenderVisual()->getVisData().box.get_CD(box_c, box_hs);
		float dist_to_cam_sqr = Device.vCameraPosition.distance_to_sqr(item_transform.c);
	}

	for (bind_particle_to_vertex& pg_vertex_info : hudParticles)
	{
		trix = pg_vertex_info.local_hud_triangle;

		if (pg_vertex_info.rid == 0 && b_r0)
		{
			item_transform.set(g_player_hud->attached_item(0)->m_item_transform);
		}

		if (pg_vertex_info.rid == 1 && b_r1)
		{
			item_transform.set(g_player_hud->attached_item(1)->m_item_transform);
		}

		item_transform.transform_tiny(trix.face_a_pos);
		item_transform.transform_tiny(trix.face_b_pos);
		item_transform.transform_tiny(trix.face_c_pos);

		normal_dir.mknormal(trix.face_a_pos, trix.face_b_pos, trix.face_c_pos);
		normal_dir.normalize();
		XF.j.set(normal_dir);
		Fvector::generate_orthonormal_basis(XF.j, XF.k, XF.i);
		XF.c.set(trix.face_c_pos);
		pg_vertex_info.particle->SetXFORM(XF);

		if (pg_vertex_info.left_before_play_ms > 0)
		{
			pg_vertex_info.left_before_play_ms -= (Device.fTimeDelta * 1000);
		}
		else
		{
			pg_vertex_info.left_play_time -= (Device.fTimeDelta * 1000);
			if (pg_vertex_info.left_play_time > 0)
			{
				if (!pg_vertex_info.particle->IsPlaying())
				{
					pg_vertex_info.particle->Play(true);
				}
			}
			else
			{
				if (pg_vertex_info.particle->IsPlaying())
				{
					pg_vertex_info.particle->Stop();
				}
				pg_vertex_info.left_after_play_time -= (Device.fTimeDelta * 1000);
			}

			if (pg_vertex_info.left_play_time <= 0.f && pg_vertex_info.left_after_play_time <= 0.f)
			{
				pg_vertex_info.need_remove = true;
			}
		}
	}
}

void THudVertexAssignedPatricles::AddParticleToQueue(bool left_hand, bool right_hand, float play_time, float left_after_play_time, const char* particle_path, const char* identity_group_name, float left_before_play_ms)
{
	bool b_r0 = (g_player_hud->attached_item(0) && g_player_hud->attached_item(0)->need_renderable());
	bool b_r1 = (g_player_hud->attached_item(1) && g_player_hud->attached_item(1)->need_renderable());

	if (b_r0 && left_hand && g_player_hud->attached_item(0)->m_parent_hud_item && !m_last_r0_vertices.empty())
	{
		bind_particle_to_vertex item {
			Particles::Details::Create(particle_path, false),
			r0_triangles[Random.randI(count_r0_triangles - 1)],
			g_player_hud->attached_item(0)->m_parent_hud_item->object().ID(),
			0,
			play_time,
			left_after_play_time,
			false,
			identity_group_name,
			left_before_play_ms
		};

		hudParticles.push_back(item);
	}

	if (b_r1 && right_hand && g_player_hud->attached_item(1)->m_parent_hud_item && !m_last_r1_vertices.empty())
	{
		bind_particle_to_vertex item{
			Particles::Details::Create(particle_path, false),
			r1_triangles[Random.randI(count_r1_triangles - 1)],
			g_player_hud->attached_item(1)->m_parent_hud_item->object().ID(),
			1,
			play_time,
			left_after_play_time,
			false,
			identity_group_name,
			left_before_play_ms
		};

		hudParticles.push_back(item);
	}
}

void THudVertexAssignedPatricles::ScheduleRenderParticles(bool use_r0, shared_str identity_group_name, xr_vector<shared_str>& r0_particles_paths, float r0_playing_time_ms, float r0_delay_time_ms, u32 r0_max_particles, bool use_r1, xr_vector<shared_str>& r1_particles_paths, float r1_playing_time_ms, float r1_delay_time_ms, u32 r1_max_particles, float delay_before_play_min, float delay_before_play_max)
{
	if (!use_r0 && !use_r1)
	{
		return;
	}

	if (r0_max_particles <= 0 && r1_max_particles <= 0)
	{
		return;
	}

	u32 r1_count = 0;
	u32 r0_count = 0;

	for (bind_particle_to_vertex& bind_particle_to_vertex_item : hudParticles)
	{
		if (bind_particle_to_vertex_item.identity_group_name == identity_group_name)
		{
			if (bind_particle_to_vertex_item.rid == 0)
			{
				r0_count++;
			}

			if (bind_particle_to_vertex_item.rid == 1)
			{
				r1_count++;
			}
		}
	}

	if (use_r0 && r0_max_particles > 0 && r0_count < r0_max_particles)
	{
		for (u32 i = 0; i < r0_max_particles - r0_count; i++)
		{
			AddParticleToQueue(
				true, 
				false,
				r0_playing_time_ms,
				r0_delay_time_ms,
				r0_particles_paths[Random.randI(r0_particles_paths.size())].c_str(),
				identity_group_name.c_str(),
				Random.randF(delay_before_play_min, delay_before_play_max)
			);
		}
	}

	if (use_r1 && r1_max_particles > 0 && r1_count < r1_max_particles)
	{
		for (u32 i = 0; i < r1_max_particles - r1_count; i++)
		{
			AddParticleToQueue(
				false,
				true,
				r1_playing_time_ms,
				r1_delay_time_ms,
				r1_particles_paths[Random.randI(r1_particles_paths.size())].c_str(),
				identity_group_name.c_str(),
				Random.randF(delay_before_play_min, delay_before_play_max)
			);
		}
	}
}