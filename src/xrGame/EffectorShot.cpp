// EffectorShot.cpp: implementation of the CCameraShotEffector class.
//
//////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "EffectorShot.h"
#include "Weapon.h"

//-----------------------------------------------------------------------------
// Weapon shot effector
//-----------------------------------------------------------------------------

static constexpr float FIXED_STEP = 0.006f;

CWeaponShotEffector::CWeaponShotEffector()
{
	Reset();
}

void CWeaponShotEffector::Initialize(const CameraRecoil& cam_recoil)
{
	current_recoil.Clone(cam_recoil);
	Reset();
}

void CWeaponShotEffector::Reset()
{
	m_angle_vert = 0.0f;
	m_angle_horz = 0.0f;

	m_target_angle_vert = 0.0f;
	m_target_angle_horz = 0.0f;
	m_velocity_vert = 0.0f;
	m_velocity_horz = 0.0f;

	current_recoil.Reset();

	m_prev_angle_vert = 0.0f;
	m_prev_angle_horz = 0.0f;

	m_delta_vert = 0.0f;
	m_delta_horz = 0.0f;

	m_LastSeed = 0;
	m_actived = false;
	m_using_pattern = false;
	m_shot_end = true;

	// Сбрасываем накопленное время
	m_accumulated_time = 0.0f;
}

void CWeaponShotEffector::Shot(CWeapon* weapon)
{
	R_ASSERT(weapon);
	m_shot_numer = weapon->ShotsFired() - 1;
	if (m_shot_numer <= 0)
	{
		m_shot_numer = 0;
		Reset();
	}

	current_recoil = weapon->IsZoomed() ? weapon->zoom_cam_recoil : weapon->cam_recoil;

	// Получаем паттерн отдачи от оружия
	float pattern_x = 0.0f;
	float pattern_y = 0.0f;

	if (weapon->GetCurrentRecoilPattern(pattern_x, pattern_y))
	{
		// Используем паттернную систему
		m_using_pattern = true;

//		Msg("Shot: Factor=%.3f,", current_recoil.Pattern.Factor);

		// Получаем множители паттерна от оружия
		float pattern_factor = current_recoil.Pattern.Factor;

		float final_x = pattern_x * pattern_factor;
		float final_y = pattern_y * pattern_factor;

		// Используем значения из паттерна
		ShotFromPattern(final_x, final_y);
	}
	else
	{
		// Используем СТАРУЮ ЛОГИКУ
		m_using_pattern = false;
		float angle = current_recoil.Dispersion * weapon->cur_silencer_koef.cam_dispersion;
		angle += current_recoil.DispersionInc * weapon->cur_silencer_koef.cam_disper_inc * (float)m_shot_numer;

		Shot2Legacy(angle);
	}
}

void CWeaponShotEffector::ShotFromPattern(float pattern_x, float pattern_y)
{
	// Добавляем мгновенную скорость для резкого начала отдачи
	m_velocity_vert += pattern_y * current_recoil.Pattern.Impulse;
	m_velocity_horz += pattern_x * current_recoil.Pattern.Impulse;

	// Обновляем целевые углы (добавляем к текущим, а не заменяем)
	m_target_angle_vert += pattern_y;
	m_target_angle_horz += pattern_x;

//	Msg("Recoil impulse: vert=%.3f (vel=%.3f), horz=%.3f (vel=%.3f), target_vert=%.3f, target_horz=%.3f",
//		pattern_y, pattern_y * current_recoil.Pattern.Impulse,
//		pattern_x, pattern_x * current_recoil.Pattern.Impulse,
//		m_target_angle_vert, m_target_angle_horz);

	m_actived = true;
	m_shot_end = false;
}

void CWeaponShotEffector::Shot2Legacy(float angle)
{
	m_angle_vert += angle * (current_recoil.DispersionFrac + m_Random.randF(-1.0f, 1.0f) * (1.0f - current_recoil.DispersionFrac));

	clamp(m_angle_vert, -current_recoil.MaxAngleVert, current_recoil.MaxAngleVert);
	if (fis_zero(m_angle_vert - current_recoil.MaxAngleVert))
	{
		m_angle_vert *= m_Random.randF(0.96f, 1.04f);
	}

	float rdm = m_Random.randF(-1.0f, 1.0f);
	m_angle_horz += (m_angle_vert / current_recoil.MaxAngleVert) * rdm * current_recoil.StepAngleHorz;

	clamp(m_angle_horz, -current_recoil.MaxAngleHorz, current_recoil.MaxAngleHorz);

	m_actived = true;
	m_shot_end = false;
}

void CWeaponShotEffector::SpringPhysics(float dt, float spring_stiffness, float damping)
{
	// Физика пружины для вертикальной оси
	float acceleration_vert = (m_target_angle_vert - m_angle_vert) * spring_stiffness;
	acceleration_vert -= m_velocity_vert * damping;
	m_velocity_vert += acceleration_vert * dt;
	m_angle_vert += m_velocity_vert * dt;

	// Физика пружины для горизонтальной оси
	float acceleration_horz = (m_target_angle_horz - m_angle_horz) * spring_stiffness;
	acceleration_horz -= m_velocity_horz * damping;
	m_velocity_horz += acceleration_horz * dt;
	m_angle_horz += m_velocity_horz * dt;
}

void CWeaponShotEffector::UpdateSpringRecoil(float dt)
{
	if (!m_using_pattern) return;

	if (m_shot_end && current_recoil.Pattern.ReturnEnable)
	{
		float return_speed = current_recoil.Pattern.ReturnSpeed * dt; 
		clamp(return_speed, 0.0f, 1.0f);

		//Постепенное уменьшение паттернных целей
		m_target_angle_vert *= (1.0f - return_speed);
		m_target_angle_horz *= (1.0f - return_speed);
	}

	SpringPhysics(dt, current_recoil.Pattern.Stiffness, current_recoil.Pattern.Damping);
	
	// Проверка стабилизации
	bool is_vert_stable = std::abs(m_velocity_vert) < 0.001f && std::abs(m_angle_vert - m_target_angle_vert) < 0.001f;
	bool is_horz_stable = std::abs(m_velocity_horz) < 0.001f && std::abs(m_angle_horz - m_target_angle_horz) < 0.001f;

	if (is_vert_stable && is_horz_stable)
	{
		m_angle_vert = m_target_angle_vert;
		m_angle_horz = m_target_angle_horz;

		// Если стабилизировались и цели близки к нулю, деактивируем
		if (m_shot_end && std::abs(m_target_angle_vert) < 0.001f && std::abs(m_target_angle_horz) < 0.001f)
		{
			m_actived = false;
		}
	}
}

void CWeaponShotEffector::Relax(float dt)
{
	float time_to_relax = std::abs(m_angle_vert) / current_recoil.RelaxSpeed;
	float relax_speed_horz = (fis_zero(time_to_relax)) ? 0.0f : std::abs(m_angle_horz) / time_to_relax;

	if (m_angle_horz >= 0.0f)
	{
		m_angle_horz -= relax_speed_horz * dt;
	}
	else
	{
		m_angle_horz += relax_speed_horz * dt;
	}

	if (m_angle_vert >= 0.0f)
	{
		m_angle_vert -= current_recoil.RelaxSpeed * dt;
		if (m_angle_vert < 0.0f)
		{
			m_angle_vert = 0.0f;
			m_actived = false;
		}
	}
	else
	{
		m_angle_vert += current_recoil.RelaxSpeed * dt;
		if (m_angle_vert > 0.0f)
		{
			m_angle_vert = 0.0f;
			m_actived = false;
		}
	}
}

void CWeaponShotEffector::Update()
{
	// Общая логика получения дельты времени для фиксированного шага
	float dt = Device.fTimeDelta;

	m_accumulated_time += dt;
	if (m_accumulated_time > 0.1f)
		m_accumulated_time = 0.1f;

	while (m_accumulated_time >= FIXED_STEP)
	{
		if (m_using_pattern)
		{
			// Паттернная система
			UpdateSpringRecoil(FIXED_STEP);
		}
		else
		{
			if (m_actived && current_recoil.ReturnMode)
			{
				if (m_shot_end)
					Relax(FIXED_STEP);
			}

			if (!current_recoil.ReturnMode && m_shot_end)
			{
				m_actived = false;
			}
		}
		m_accumulated_time -= FIXED_STEP;
	}

		// Общие вычисления дельт
	m_delta_vert = m_angle_vert - m_prev_angle_vert;
	m_delta_horz = m_angle_horz - m_prev_angle_horz;
	m_prev_angle_vert = m_angle_vert;
	m_prev_angle_horz = m_angle_horz;
}

void CWeaponShotEffector::GetDeltaAngle(Fvector& angle)
{
	angle.x = -m_angle_vert;
	angle.y = -m_angle_horz;
	angle.z = 0.0f;
}

void CWeaponShotEffector::GetLastDelta(Fvector& delta_angle)
{
	delta_angle.x = -m_delta_vert;
	delta_angle.y = -m_delta_horz;
	delta_angle.z = 0.0f;
}

void CWeaponShotEffector::SetRndSeed(s32 Seed)
{
	if (m_LastSeed == 0)
	{
		m_LastSeed = Seed;
		m_Random.seed(Device.dwFrame);
	}
}

void CWeaponShotEffector::ChangeHP(float* pitch, float* yaw)
{
	*pitch -= m_delta_vert; // y = pitch = p = vert
	*yaw -= m_delta_horz; // x = yaw   = h = horz
}

//-----------------------------------------------------------------------------
// Camera shot effector
//-----------------------------------------------------------------------------

CCameraShotEffector::CCameraShotEffector()
	: CEffectorCam(eCEShot, 100000.0f)
{
	m_pActor = nullptr;
	m_WeaponID = 0;
}

CCameraShotEffector::~CCameraShotEffector()
{
}

BOOL CCameraShotEffector::ProcessCam(SCamEffectorInfo& info)
{
	Update();
	return TRUE;
}