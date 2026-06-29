#pragma once

#include "StdAfx.h"
#include "AnomalyZone.h"
#include "object_broker.h"
#include "AnomalyGravity.h"
#include "Actor.h"
#include "ai_object_location.h"
#include "alife_simulator_base.h"
#include "alife_simulator.h"
#include "..\xrPhysics\IPHWorld.h"
#include "..\xrGame\PHSimpleCalls.h"
#include "HUDManager.h"

extern CSE_Abstract* CALifeSimulator__spawn_item2(
	CALifeSimulator* self_,
	const char* section,
	const Fvector& position,
	u32 level_vertex_id,
	GameGraph::_GRAPH_ID game_vertex_id,
	ALife::_OBJECT_ID id_parent
);

void TAnomalyGravity::Load(const char* section)
{
	shared_str gravity_options_section = READ_IF_EXISTS(pSettings, r_string, section, "gravity_options_section", nullptr);
	if (gravity_options_section)
	{
		m_use_gravity_effect = READ_IF_EXISTS(pSettings, r_bool, gravity_options_section, "use_gravity_effect", m_use_gravity_effect);
		m_max_count_spawn_trash = READ_IF_EXISTS(pSettings, r_u16, gravity_options_section, "max_count_spawn_trash", m_max_count_spawn_trash);


		m_gravity_radius = READ_IF_EXISTS(pSettings, r_float, gravity_options_section, "gravity_radius", m_gravity_radius);
		m_max_processing_distance = READ_IF_EXISTS(pSettings, r_float, gravity_options_section, "max_processing_distance", m_max_processing_distance);
		if (m_gravity_radius <= 0.f || m_max_processing_distance <= 0.f)
		{
			m_gravity_radius = 0.f;
			m_max_processing_distance = 0.f;
			m_use_gravity_effect = false;
		}

		m_trash_items_sections.clear();
		if (pSettings->line_exist(gravity_options_section, "trash_sections"))
		{
			m_trash_items_sections = xr_string(pSettings->r_string(gravity_options_section, "trash_sections")).RemoveWhitespaces().Split(',');
		}
	}
}

bool TAnomalyGravity::AlwaysTheCrow()
{
	return IsEnabled() && Actor()->Position().distance_to_xz(XFORM().c) <= m_max_processing_distance;
}

static Fvector GenerateRandomPointInUpperHemisphere(const Fvector& center, float radius)
{
    float theta = acosf(::Random.randF(0.f, 1.f));
    float phi   = ::Random.randF(0.f, 2.f * PI);
    Fvector result;

	return result.set(
		sinf(theta) * cosf(phi) * radius,
		cosf(theta) * radius,
		sinf(theta) * sinf(phi) * radius
	).add(center);
}

void TAnomalyGravity::Update()
{
	if (!IsEnabled())
	{
		return;
	}

	float MovementSpeed = 0.25f * ::Random.randF(3.0f, 7.0f);
	float RotationSpeed = ::Random.randF(0.4f, 0.7f);
	float rollSpeed = ::Random.randF(1.f, 1.6f);

	Fvector anomaly_center;
	anomaly_center.set(XFORM().c);
	
	// обработка тех обьектов что уже есть в поле
	if (!m_current_anomaly->lastScannedObjects.empty())
	{
		for (CGameObject* obj : m_current_anomaly->lastScannedObjects)
		{
			Fvector target_pos;
			auto it = m_target_positions_map.find(obj->ID());
			if (it != m_target_positions_map.end())
			{
				target_pos.set(it->second);
				if (target_pos.distance_to(obj->Position()) < 0.5f)
				{
					target_pos.set(GenerateRandomPointInUpperHemisphere(anomaly_center, m_gravity_radius));
					m_target_positions_map[obj->ID()] = target_pos;
				}
			}
			else
			{
				target_pos.set(GenerateRandomPointInUpperHemisphere(anomaly_center, m_gravity_radius));
				m_target_positions_map[obj->ID()] = target_pos;
			}

			Fvector force_dir;
			force_dir.sub(target_pos, obj->Position());
			force_dir.normalize_safe();

			SetForce(obj, force_dir, MovementSpeed, RotationSpeed, rollSpeed);
		}
	}
}

void TAnomalyGravity::SetForce(CGameObject* obj, Fvector dir, float value, float rotationSpeed, float rollSpeed)
{
	if (!obj || !physics_world())
	{
		return;
	}

	if (CPhysicsShellHolder* shellHolder = obj->cast_physics_shell_holder())
	{
		if (CPhysicsShell* shell = shellHolder->PPhysicsShell())
		{
			float dt = Device.fTimeDelta;

			if (XFORM().c.distance_to_sqr(obj->Position()) <= m_gravity_radius * m_gravity_radius)
			{
				shell->Deactivate();
				shell->set_ApplyByGravity(false);

				dir.normalize_safe();
				obj->Position().mad(dir, value * dt);

				Fvector cur_dir;
				cur_dir.set(obj->XFORM().k).normalize_safe();

				float step = 1.f - expf(-rotationSpeed * dt);
				clamp(step, 0.f, 1.f);

				Fvector interpolated;
				interpolated.lerp(cur_dir, dir, step);
				interpolated.normalize_safe();

				obj->XFORM().SetDirection(interpolated);

				float rot = obj->XFORM().GetRotation();
				rot += rollSpeed * dt;
				obj->XFORM().SetRotation(rollSpeed * dt);
			}
			else
			{
				shell->Activate();
				shell->set_ApplyByGravity(true);
			}
		}
	}
}


void TAnomalyGravity::save(NET_Packet& output_packet)
{
	output_packet.w_u8(m_is_trash_spawned ? 1 : 0);
}

void TAnomalyGravity::load(IReader& input_packet)
{
	m_is_trash_spawned = input_packet.r_u8() == 1;
}

void TAnomalyGravity::net_Spawn(CSE_Abstract* DC)
{
	if (!m_is_trash_spawned && m_max_count_spawn_trash > 0 && m_trash_items_sections.size() > 0)
	{
		if (CALifeSimulator* Sim = const_cast<CALifeSimulator*>(&ai().alife()))
		{
			if (CGameObject* gObj = m_current_anomaly->cast_game_object())
			{
				u16 lvid = gObj->ai_location().level_vertex_id();
				u32 gvid = gObj->ai_location().game_vertex_id();
				u16 rndCount = Random.randI(1, m_max_count_spawn_trash);
				
				Fvector pos;
				Fvector dir;

				for (size_t i = 0; i < rndCount; i++)
				{
					pos.set(XFORM().c);
					dir.random_dir();
					dir.normalize();
					pos.mad(dir, Random.randF(m_gravity_radius / 2.f, m_gravity_radius));

					if (pos.y < (XFORM().c.y + 0.25))
					{
						pos.y = XFORM().c.y + 0.25;
					}

					CALifeSimulator__spawn_item2(
						Sim,
						m_trash_items_sections[Random.randI(m_trash_items_sections.size())].c_str(),
						pos,
						lvid,
						gvid,
						u16(-1)
					);
				}
			}
		}
	}
}