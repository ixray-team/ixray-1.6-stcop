#include "stdafx.h"
#include "AnomalyMovement.h"
#include "object_broker.h"
#include "..\xrServerEntities\xrServer_Object_Base.h"
#include "..\xrServerEntities\xrServer_Objects_ALife_Monsters.h"
#include "AnomalyZone.h"
#include "Actor.h"
#include "Artefact.h"
#include "GameObject.h"
#include "HUDManager.h"
#include "ai_object_location.h"

void TAnomalyMovement::BeginComponent(IECSOwner* O)
{
	m_currentAnomalyObject = smart_cast<CAnomalyZone*>(O);
}

void TAnomalyMovement::EndComponent() { }

// �� ����� �������� � ����� ��� �� � �������� 160 ������ �� ��������� �� ��������
void TAnomalyMovement::OnActorTakeArtefact(float scan_radius, CArtefact* artefact, Fvector actorPos)
{
	if (!m_use_movement)
	{
		return;
	}
	
	if (!m_use_movement_magnetic_on_take_artefacts_mode)
	{
		return;
	}

	if (m_movement_radius > m_initial_spawn_position.distance_to(Actor()->Position()))
	{
		m_timer_magnetic_on_take_artefacts = m_max_timer_magnetic_on_take_artefacts;
	}
}

void TAnomalyMovement::AffectBlast(CGameObject* blastetObject)
{
	
}

bool TAnomalyMovement::IsNeedScanObjects()
{
	return m_use_movement_magnetic_on_inside_alive_mode && IsEnabled();
}

void TAnomalyMovement::Load(const char* section)
{
	xr_string options_section = pSettings->read_if_exists<str_c>(section, "movement_options_section", "");
	if (!options_section.empty())
	{
		const char* sect = options_section.c_str();
		m_use_movement = pSettings->read_if_exists<bool>(sect, "use_movement", false);
		if (m_use_movement)
		{
			draw_dbg = pSettings->read_if_exists<bool>(sect, "draw_debug", false); // �������

			max_processing_distance = pSettings->read_if_exists<float>(sect, "max_processing_distance", max_processing_distance); // ��������� ����������

			m_use_movement_always_mode = pSettings->read_if_exists<bool>(sect, "use_movement_always_mode", false);

			m_use_movement_magnetic_on_inside_alive_mode = pSettings->read_if_exists<bool>(sect, "use_movement_magnetic_on_inside_alive_mode", false);
			movement_magnetic_on_inside_alive_mode_speed = pSettings->read_if_exists<float>(sect, "movement_magnetic_on_inside_alive_mode_speed", false);

			m_use_movement_magnetic_on_take_artefacts_mode = pSettings->read_if_exists<bool>(sect, "use_movement_magnetic_on_take_artefacts_mode", false);
			m_max_timer_magnetic_on_take_artefacts = pSettings->read_if_exists<float>(sect, "milliseconds_time_magnetic_on_take_artefacts", 0.f);
			movement_magnetic_on_take_artefacts_mode_speed = pSettings->read_if_exists<float>(sect, "movement_magnetic_on_take_artefacts_mode_speed", 0.f);

			m_movement_speed = pSettings->read_if_exists<float>(sect, "movement_speed", 1.5f);
			m_movement_radius = pSettings->read_if_exists<float>(sect, "movement_radius", 15.f);
		}
	}
}

Fvector TAnomalyMovement::GetLVPos(Fvector newPos)
{
	u32 lvid = (ai().level_graph().vertex(m_currentAnomalyObject->ai_location().level_vertex_id(), newPos));
	if (ai().level_graph().valid_vertex_id(lvid))
	{
		Fvector LVPosition = (ai().level_graph().vertex_position(lvid));
		return LVPosition;
	}

	return newPos;
}

bool TAnomalyMovement::IsEnabled()
{
	return m_use_movement && m_currentAnomalyObject != nullptr && m_currentAnomalyObject->IsEnabled();
}

bool TAnomalyMovement::AlwaysTheCrow()
{
	return IsEnabled() && Actor()->Position().distance_to_xz(m_initial_spawn_position) <= max_processing_distance;
}

void TAnomalyMovement::Update(CGameObject* m_best_magnetic_target, bool isUpdateCL)
{
	if (!IsEnabled() || !isUpdateCL)
	{
		return;
	}

	Fvector min = m_initial_spawn_position - m_movement_radius;
	Fvector max = m_initial_spawn_position + m_movement_radius;

	if (m_use_movement_magnetic_on_take_artefacts_mode && m_timer_magnetic_on_take_artefacts > 0.f)
	{
		m_timer_magnetic_on_take_artefacts -= Device.dwTimeDelta;
		MoveToFromDelta(Actor()->Position(), movement_magnetic_on_take_artefacts_mode_speed);
	}
	else if (m_use_movement_magnetic_on_inside_alive_mode && m_best_magnetic_target != nullptr)
	{
		MoveToFromDelta(m_best_magnetic_target->Position(), movement_magnetic_on_inside_alive_mode_speed);
		if (draw_dbg)
		{
			HUD().world_prims.append_sphere(m_best_magnetic_target->Position(), 1.25f, color_rgba(10, 10, 10, 255), color_rgba(255, 70, 70, 50));
		}
	}
	else if (m_use_movement_always_mode)
	{
		float dist_to_target = XFORM().c.distance_to_xz(m_target_position);

		if (m_best_magnetic_target == nullptr && ((m_target_position.x == 0.f && m_target_position.y == 0.f && m_target_position.z == 0.f) || (dist_to_target <= (m_currentAnomalyObject->Radius() * 0.5) )))
		{
			m_target_position.x = Random.randF(min.x, max.x);
			m_target_position.z = Random.randF(min.z, max.z);

			if (std::isnan(dist_to_target)) {
				dist_to_target = 0.f;
				XFORM().c.set(m_target_position);
			}

			Fvector dir;
			dir.sub(m_target_position, XFORM().c);
			float range = dir.magnitude();
			dir.normalize();

			collide::rq_result R;
			Fvector pos = XFORM().c;
			pos.y += m_currentAnomalyObject->Radius() * 0.5;

			if (g_pGameLevel->ObjectSpace.RayPick(pos, dir, range, collide::rqtStatic, R, m_currentAnomalyObject))
			{
				float rq_range = R.range;
				if (rq_range > m_currentAnomalyObject->Radius())
				{
				//	rq_range -= m_currentAnomalyObject->Radius();
				}

				if (rq_range > m_currentAnomalyObject->Radius() * 0.5)
				{
					m_target_position.mad(pos, dir, rq_range);
				}
			}

			m_target_position.y = GetLVPos(m_target_position).y + 0.5;
		}

		MoveToFromDelta(m_target_position, m_movement_speed);
	}

	if (!m_use_movement_always_mode && m_best_magnetic_target == nullptr && m_timer_magnetic_on_take_artefacts <= 0.f && XFORM().c.distance_to_xz(m_initial_spawn_position) > m_currentAnomalyObject->Radius() * 0.5)
	{
		MoveToFromDelta(m_initial_spawn_position, m_movement_speed);
	}

	if (m_use_movement_always_mode)
	{	
		if (lastPosition.distance_to(XFORM().c) <= EPS_L)
		{
			m_target_position.set(0, 0, 0);
		}
		lastPosition.set(XFORM().c);
	}
}

void TAnomalyMovement::MoveToFromDelta(Fvector newPos, float speed)
{
	if (!m_use_movement)
	{
		return;
	}

	Fvector _pos;
	_pos.set(newPos);

	_pos.y = GetLVPos(_pos).y + 0.15;

	XFORM().c.set(Fvector().set(XFORM().c).add(((Fvector().sub(_pos, XFORM().c)).normalize()).mul(speed * Device.fTimeDelta)));

	XFORM().c.y = GetLVPos(XFORM().c).y + 0.25;

	if (draw_dbg)
	{
		Fvector draw_dir_c;
		draw_dir_c.sub(newPos, XFORM().c);
		float draw_range_c = draw_dir_c.magnitude();
		draw_dir_c.normalize();
		HUD().world_prims.append_line(XFORM().c, newPos, color_rgba(255, 0, 0, 255));
		HUD().world_prims.append_sphere(newPos, 0.25f, color_rgba(10, 10, 10, 255), color_rgba(255, 255, 255, 50));
	}
}