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
	shared_str gravity_options_section = pSettings->read_if_exists<str_c>(section, "gravity_options_section", nullptr);
	if (gravity_options_section)
	{
		m_use_gravity_effect = pSettings->read_if_exists<bool>(gravity_options_section, "use_gravity_effect", m_use_gravity_effect);
		m_max_count_spawn_trash = pSettings->read_if_exists<u16>(gravity_options_section, "max_count_spawn_trash", m_max_count_spawn_trash);

		m_min_gravity_radius_factor = pSettings->read_if_exists<float>(gravity_options_section, "min_gravity_radius_factor", m_min_gravity_radius_factor);
		m_gravity_radius = pSettings->read_if_exists<float>(gravity_options_section, "gravity_radius", m_gravity_radius);
		m_max_processing_distance = pSettings->read_if_exists<float>(gravity_options_section, "max_processing_distance", m_max_processing_distance);
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

static void buildBasis(const Fvector& axis, Fvector& u, Fvector& v)
{
	Fvector ref;
	if (fabsf(axis.y) < 0.9f)
	{
		ref.set(0, 1, 0);
	}
	else
	{
		ref.set(1, 0, 0);
	}
	u.crossproduct(axis, ref);
	u.normalize();
	v.crossproduct(axis, u);
	v.normalize();
}

void TAnomalyGravity::Update()
{
	if (!IsEnabled())
	{
		return;
	}

	const float dt = Device.fTimeDelta;
	const Fvector& center = XFORM().c;
	const float radius = m_gravity_radius;
	const float minRadius = radius * m_min_gravity_radius_factor;

	for (CGameObject* obj : m_current_anomaly->lastScannedObjects)
	{
		if (!obj)
		{
			continue;
		}

		u32 id = obj->ID();
		auto& state = m_wanderStates[id];

		if (!state.initialized)
		{
			Fvector axis;
			axis.random_dir();
			buildBasis(axis, state.u, state.v);

			float r_min = minRadius;
			float r_max = radius;
			float r = pow(r_min * r_min * r_min + (r_max * r_max * r_max - r_min * r_min * r_min) * ::Random.randF(0.f, 1.f), 1.f / 3.f);

			state.a = r * ::Random.randF(0.6f, 1.4f);
			state.b = r * ::Random.randF(0.6f, 1.4f);
			state.a = std::clamp(state.a, r_min, r_max);
			state.b = std::clamp(state.b, r_min, r_max);

			state.speed = ::Random.randF(0.3f, 1.2f);
			state.angle = ::Random.randF(0.f, 2.f * PI);

			state.moveSpeed = 0.25f * ::Random.randF(3.0f, 7.0f);
			state.rotSpeed = ::Random.randF(0.4f, 0.7f);
			state.rollSpeed = ::Random.randF(2.0f, 3.6f);

			state.initialized = true;
		}

		state.angle += state.speed * dt;

		float cosA = cosf(state.angle);
		float sinA = sinf(state.angle);
		Fvector offset;
		offset.x = state.u.x * state.a * cosA + state.v.x * state.b * sinA;
		offset.y = state.u.y * state.a * cosA + state.v.y * state.b * sinA;
		offset.z = state.u.z * state.a * cosA + state.v.z * state.b * sinA;

		Fvector targetPos;
		targetPos.x = center.x + offset.x;
		targetPos.y = center.y + centerOffsetY + offset.y;
		targetPos.z = center.z + offset.z;

		if (targetPos.y < center.y + 0.5f)
		{
			targetPos.y = center.y + 0.5f;
		}

		Fvector dir;
		dir.sub(targetPos, obj->Position());
		float dist = dir.magnitude();
		if (dist < 0.001f)
		{
			continue;
		}
		dir.div(dist);

		SetForce(obj, dir, state.moveSpeed, state.rotSpeed, state.rollSpeed);
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
			if (!shell->isActive())
			{
				return;
			}

			// за пределами радиуса вихря — не управляем объектом, физика сама всё решит
			if (XFORM().c.distance_to_sqr(obj->Position()) > m_gravity_radius * m_gravity_radius)
			{
				return;
			}

			dir.normalize_safe();

			// линейная скорость к орбитальной точке (цель сама движется => объект закручивается)
			Fvector vel = dir;
			vel.mul(value);
			shell->set_LinearVel(vel);

			// угловая скорость: разворачиваем "вперёд" объекта по направлению движения + крен
			Fvector cur_dir;
			cur_dir.set(obj->XFORM().k).normalize_safe();

			Fvector ang_vel;
			ang_vel.crossproduct(cur_dir, dir);
			ang_vel.mul(rotationSpeed);
			ang_vel.add(dir * rollSpeed);
			shell->set_AngularVel(ang_vel);
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
						m_trash_items_sections[::Random.randI(m_trash_items_sections.size())].c_str(),
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

bool TAnomalyGravity::IsObjectIgnored(CGameObject* obj)
{
	xr_string obj_section = obj->cNameSect().c_str();
	for (xr_string& section : m_trash_items_sections)
	{
		if (strcmp(obj_section.c_str(), section.c_str()) == 0)
		{
			return true;
		}
	}

	return false;
}