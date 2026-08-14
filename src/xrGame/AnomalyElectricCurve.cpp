#pragma once
#include "StdAfx.h"
#include "AnomalyElectricCurve.h"
#include "object_broker.h"
#include "..\xrServerEntities\xrServer_Object_Base.h"
#include "..\xrServerEntities\xrServer_Objects_ALife_Monsters.h"
#include "AnomalyZone.h"
#include "Actor.h"
#include "Artefact.h"
#include "GameObject.h"
#include "HUDManager.h"

void TAnomalyElectricCurve::BeginComponent(IECSOwner* O)
{
	m_currentAnomalyObject = smart_cast<CAnomalyZone*>(O);
}

void TAnomalyElectricCurve::EndComponent()
{
	//delete_data(m_snd_emmiter_electric_curve_start);
	//delete_data(m_snd_emmiter_electric_curve_loop);
	//delete_data(m_snd_emmiter_electric_curve_end);
	//delete_data(m_snd_emmiter_electric_core_loop);
	//delete_data(m_snd_emmiter_electric_core_target_damage);
}

void TAnomalyElectricCurve::InitElectricCurves()
{
	if (!m_use_electric_curve)
	{
		return;
	}

	for (u8 i = 0; i < m_max_count_electric_curves; i++)
	{
		SElectricCurve& curva = m_electric_curves.emplace_back();
		curva.particleName = m_electric_curve_particle_path;
		curva.particleContactGroundName = m_electric_curve_ground_contact_particle_path;
		curva.initialPos.set(RecalculateStartPosition(XFORM().c));
		curva.destinationPos.set(XFORM().c);
		curva.MAX_CURVE_DISTANCE = m_max_curve_radius;
		curva.Disable();
	}
}

// ГГ берет артефакт с земли где то в пределах 160 метров радиуса от аномалии
void TAnomalyElectricCurve::OnActorTakeArtefact(float scan_radius, CArtefact* artefact, Fvector actorPos)
{

}

void TAnomalyElectricCurve::AffectBlast(CGameObject* blastedObject)
{
	if (!m_use_electric_curve)
	{
		return;
	}

	lastDamagedObject = blastedObject;
	blastTimeProcessing = max_blastTimeProcessing;
	if (m_snd_emmiter_electric_core_target_damage[0])
	{
		m_snd_emmiter_electric_core_target_damage[0]->PlayRandomSound(nullptr, blastedObject->Position(), 0U, 0.f, 1.0f);
	}
}

bool TAnomalyElectricCurve::IsNeedScanObjects()
{
	return m_use_electric_curve && IsEnabled();
}

void TAnomalyElectricCurve::Load(const char* section)
{
	m_snd_emmiter_electric_core_target_damage.clear();
	m_snd_emmiter_electric_core_loop.clear();
	m_snd_emmiter_electric_curve_start.clear();
	m_snd_emmiter_electric_curve_loop.clear();
	m_snd_emmiter_electric_curve_end.clear();

	xr_string options_section = READ_IF_EXISTS(pSettings, r_string, section, "electric_curve_options_section", "");
	if (!options_section.empty())
	{
		const char* sect = options_section.c_str();
		m_use_electric_curve = READ_IF_EXISTS(pSettings, r_bool, sect, "use_electric_curve", false);

		if (m_use_electric_curve)
		{
			draw_dbg = READ_IF_EXISTS(pSettings, r_bool, sect, "draw_debug", false); // отладка

			max_processing_distance = READ_IF_EXISTS(pSettings, r_float, sect, "max_processing_distance", max_processing_distance); // дистанция обновления

			m_electric_curve_particle_path = READ_IF_EXISTS(pSettings, r_string, sect, "electric_curve_particle_path", "");
			m_electric_curve_ground_contact_particle_path = READ_IF_EXISTS(pSettings, r_string, sect, "electric_curve_ground_contact_particle_path", "");
			m_max_count_electric_curves = READ_IF_EXISTS(pSettings, r_u8, sect, "max_count_electric_curves", m_max_count_electric_curves);
			max_trace_curve_distance = READ_IF_EXISTS(pSettings, r_float, sect, "max_trace_curve_distance", max_trace_curve_distance);

			max_blastTimeProcessing = READ_IF_EXISTS(pSettings, r_float, sect, "max_curve_atack_damage_time", max_blastTimeProcessing);
			m_max_curve_radius = READ_IF_EXISTS(pSettings, r_float, sect, "max_curve_atack_radius", m_max_curve_radius);// для атак
			m_max_curve_damage = READ_IF_EXISTS(pSettings, r_float, sect, "max_curve_atack_damage", m_max_curve_damage);
			m_max_curve_impulse = READ_IF_EXISTS(pSettings, r_float, sect, "max_curve_atack_impulse", m_max_curve_impulse);

			m_change_target_timeout_ms_min = READ_IF_EXISTS(pSettings, r_float, sect, "change_target_timeout_ms_min", m_change_target_timeout_ms_min);
			m_change_target_timeout_ms_max = READ_IF_EXISTS(pSettings, r_float, sect, "change_target_timeout_ms_max", m_change_target_timeout_ms_max);
			m_curve_start_y_offset = READ_IF_EXISTS(pSettings, r_float, sect, "curve_start_y_offset", m_curve_start_y_offset);

			m_cascade_curves = READ_IF_EXISTS(pSettings, r_bool, sect, "use_cascade_electric_curves", m_cascade_curves); // цепные молнии
			if (m_cascade_curves)
			{
				m_touch_objects_by_curves = READ_IF_EXISTS(pSettings, r_bool, sect, "use_touch_objects_by_curves", m_touch_objects_by_curves); // щупать неживые обьекты по близости
			}

			m_snd_emmiter_electric_core_loop.push_back(new CRandomSoundEmmiter(sect, "sounds_electric_core_loop"));
			m_snd_emmiter_electric_core_target_damage.push_back(new CRandomSoundEmmiter(sect, "sounds_electric_core_target_damage"));

			for (u8 i = 0; i < m_max_count_electric_curves; i++)
			{
				m_snd_emmiter_electric_curve_start.push_back(new CRandomSoundEmmiter(sect, "sounds_electric_curve_start"));
				m_snd_emmiter_electric_curve_loop.push_back(new CRandomSoundEmmiter(sect, "sounds_electric_curve_loop"));
				m_snd_emmiter_electric_curve_end.push_back(new CRandomSoundEmmiter(sect, "sounds_electric_curve_end"));
			}
		}
	}
}

void  TAnomalyElectricCurve::AffectCurveDamade(CGameObject* obj)
{
	if (!m_use_electric_curve)
	{
		return;
	}

	if (obj == nullptr || obj->getDestroy())
	{
		return;
	}

	float mass = 0.5;
	if (mass <= 0.f)
	{
		return;
	}

	Fvector hit_dir;
	hit_dir.sub(obj->Position(), XFORM().c);
	hit_dir.normalize();
	hit_dir.y = hit_dir.y + 0.4f;

	float effective = (100 - (obj->Position().distance_to(XFORM().c) * 100) / m_max_curve_radius) / 100;
	float power = effective * m_max_curve_damage;
	float impulse = mass * (m_max_curve_impulse * effective);

	if (power > 0.0f)
	{
		m_currentAnomalyObject->CreateHit(obj->ID(), m_currentAnomalyObject->ID(), hit_dir, power, 0, zero_vel, impulse, m_currentAnomalyObject->m_eHitTypeBlowout);
	}
}


bool TAnomalyElectricCurve::IsEnabled()
{
	return m_use_electric_curve && m_currentAnomalyObject && m_currentAnomalyObject->IsEnabled();
}

bool TAnomalyElectricCurve::AlwaysTheCrow()
{
	return IsEnabled() && Actor()->Position().distance_to_xz(XFORM().c) <= max_processing_distance;
}

void TAnomalyElectricCurve::Update(bool isUpdateCL)
{
	if (!IsEnabled() || !isUpdateCL)
	{
		return;
	}

	m_snd_emmiter_electric_core_loop[0]->UpdatePosition(XFORM().c);
	if (!m_snd_emmiter_electric_core_loop[0]->IsPlaying())
	{
		m_snd_emmiter_electric_core_loop[0]->PlayRandomSound(nullptr, XFORM().c, sm_Looped, 0.f, 0.5f);
	}

	Fvector startTracePos;
	startTracePos.set(XFORM().c);
	startTracePos.y += m_currentAnomalyObject->Radius() * 0.5;

	if (blastTimeProcessing > 0)
	{
		blastTimeProcessing -= Device.dwTimeDelta;
		if (lastDamagedObject != nullptr)
		{
			OnBlastElectricCurvesUpdate(lastDamagedObject);
		}
		else
		{
			blastTimeProcessing = 0;
		}

		if (blastTimeProcessing <= 0)
		{
			for (u8 i = 0; i < m_max_count_electric_curves; i++)
			{
				m_electric_curves[i].m_upd_timer = m_electric_curves[i].m_max_upd_timer;
			}
		}
	}

	Fvector dir;
	float rq_range = 0.f;
	Fvector destPosition;
	collide::rq_result R;

	size_t cnt = m_currentAnomalyObject->lastScannedObjects.size();
	for (u8 i = 0; i < m_max_count_electric_curves; i++)
	{
		SElectricCurve& spline = m_electric_curves[i];
		if (blastTimeProcessing <= 0)
		{
			spline.initialPos = RecalculateStartPosition(XFORM().c);
		}
		spline.UpdateMovement();
		spline.Enable();

		m_snd_emmiter_electric_curve_loop[i]->UpdatePosition(spline.destinationPos);

		if (!m_snd_emmiter_electric_curve_loop[i]->IsPlaying())
		{
			m_snd_emmiter_electric_curve_loop[i]->PlayRandomSound(nullptr, spline.destinationPos, 0U, 0.f, 1.5f);
		}

		if (blastTimeProcessing <= 0)
		{
			spline.m_upd_timer += Device.dwTimeDelta;

			if (spline.m_upd_timer > spline.m_max_upd_timer)
			{
				spline.m_max_upd_timer = Random.randF(m_change_target_timeout_ms_min, m_change_target_timeout_ms_max);
				spline.m_upd_timer = 0.f;

				bool needTrace = true;
				if (m_touch_objects_by_curves && cnt > 0)
				{
					CGameObject* obj = m_currentAnomalyObject->lastScannedObjects[Random.randI(0, cnt - 1)];
					if (obj != nullptr)
					{
						if (CEntityAlive* entity = obj->cast_entity_alive())
						{
							if (!entity->g_Alive())
							{
								float distance = obj->Position().distance_to(XFORM().c);
								if (distance <= max_trace_curve_distance && Random.randF(0.0f, 100.0f) <= 100 - ((distance * 100) / max_trace_curve_distance))
								{
									spline.destinationPos = obj->Position();
									needTrace = false;
								}
							}
						}
					}
				}

				if (needTrace)
				{
					bool is_found_point = false;
					Fvector beforeTracePos;

					for (u8 i = 0; i < 5; i++)
					{
						dir.set(0, 0, 0);
						rq_range = 0.f;
						dir.random_dir();
						dir.normalize();

						if (g_pGameLevel->ObjectSpace.RayPick(startTracePos, dir, max_trace_curve_distance, collide::rqtStatic, R, m_currentAnomalyObject))
						{
							beforeTracePos.set(spline.destinationPos);
							rq_range = R.range;
							if (blastTimeProcessing <= 0)
							{
								spline.destinationPos = destPosition.mad(startTracePos, dir, rq_range);
							}

							is_found_point = true;
							break;
						}
					}

					if (is_found_point)
					{
						for (CRandomSoundEmmiter* snd :m_snd_emmiter_electric_curve_end)
						{
							if (!snd->IsPlaying())
							{
								snd->PlayRandomSound(nullptr, beforeTracePos, 0U, 0.f, 1.5f);
								break;
							}
						}

						for (CRandomSoundEmmiter* snd : m_snd_emmiter_electric_curve_start)
						{
							if (!snd->IsPlaying())
							{
								snd->PlayRandomSound(nullptr, spline.destinationPos, 0U, 0.f, 1.5f);
								break;
							}
						}
					}
				}
			}
			/*
			if (spline.destinationPos.distance_to(XFORM().c) > m_max_curve_radius)
			{
				spline.m_upd_timer = spline.m_max_upd_timer;
			}
			*/
		}
	}
}

Fvector TAnomalyElectricCurve::RecalculateStartPosition(Fvector& anomalyCenter)
{
	Fvector res;
	res.set(anomalyCenter);
	res.y = res.y + m_curve_start_y_offset;

	return res;
}


void TAnomalyElectricCurve::OnBlastElectricCurvesUpdate(CGameObject* obj)
{
	if (!m_use_electric_curve)
	{
		return;
	}

	size_t cnt = m_currentAnomalyObject->lastScannedObjects.size();
	if (cnt == 0)
	{
		blastTimeProcessing = 0;
		return;
	}

	if (!m_cascade_curves)
	{
		if (m_currentAnomalyObject->lastScannedObjects[0])
		{
			for (u8 i = 0; i < m_max_count_electric_curves; i++)
			{
				SElectricCurve& spline = m_electric_curves[i];

				if (m_currentAnomalyObject->lastScannedObjects[0] != nullptr && !m_currentAnomalyObject->lastScannedObjects[0]->getDestroy())
				{
					spline.initialPos.set(RecalculateStartPosition(XFORM().c));
					spline.destinationPos.set(m_currentAnomalyObject->lastScannedObjects[0]->Position());
					AffectCurveDamade(m_currentAnomalyObject->lastScannedObjects[0]);
				}

				spline.UpdateMovement();
			}
		}
	}
	else
	{
		CGameObject* targetObj = nullptr;
		for (u8 i = 0; i < m_max_count_electric_curves; i++)
		{
			SElectricCurve& spline = m_electric_curves[i];
			if (obj != nullptr && !obj->getDestroy())
			{
				targetObj = obj;
			}

			if (m_currentAnomalyObject->lastScannedObjects[0] != nullptr && !m_currentAnomalyObject->lastScannedObjects[0]->getDestroy())
			{
				targetObj = m_currentAnomalyObject->lastScannedObjects[0];
			}

			if (targetObj != nullptr)
			{
				spline.initialPos.set(RecalculateStartPosition(XFORM().c));
				spline.destinationPos.set(targetObj->Position());
			}

			if (i > 0 && i < cnt && m_currentAnomalyObject->lastScannedObjects[i - 1] != nullptr && m_currentAnomalyObject->lastScannedObjects[i] != nullptr && !m_currentAnomalyObject->lastScannedObjects[i - 1]->getDestroy() && !m_currentAnomalyObject->lastScannedObjects[i]->getDestroy())
			{
				spline.initialPos.set(RecalculateStartPosition(m_currentAnomalyObject->lastScannedObjects[i]->Position()));
				spline.destinationPos.set(m_currentAnomalyObject->lastScannedObjects[i - 1]->Position());
				AffectCurveDamade(m_currentAnomalyObject->lastScannedObjects[i]);
			}

			if (targetObj == nullptr)
			{
				blastTimeProcessing = 0;
			}

			spline.UpdateMovement();
		}
	}
}