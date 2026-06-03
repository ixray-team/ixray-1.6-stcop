// EffectorShot.cpp: implementation of the CCameraShotEffector class.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "EffectorShot.h"
#include "Weapon.h"

//-----------------------------------------------------------------------------
// Weapon shot effector
//-----------------------------------------------------------------------------

static constexpr float FIXED_STEP = 0.006f;
static constexpr float PATTERN_MAX_STEP_X = 0.5f;   // Максимальный финальный шаг
static constexpr float PATTERN_MAX_STEP_Y = 0.5f;   // Максимальный финальный шаг
static constexpr float MAX_PATTERN_VELOCITY_VERT = 7.5f;   // Максимальная вертикальная скорость
static constexpr float MAX_PATTERN_VELOCITY_HORZ = 7.5f;   // Максимальная горизонтальная скорость

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
	m_return_target_vert = 0.0f;   
	m_return_target_horz = 0.0f;    

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

		float addon_factor = weapon->GetAddonRecoil();
		clamp(addon_factor, 0.01f, addon_factor);

		float agility_factor = Actor()->GetAgility();
		clamp(agility_factor, 0.01f, agility_factor);

		float pattern_factor = current_recoil.Pattern.Factor;
		clamp(pattern_factor, 0.01f, pattern_factor);

		float final_factor = pattern_factor * addon_factor * agility_factor;
		clamp(final_factor, 0.001f, final_factor);

		float final_x = pattern_x * final_factor;
		float final_y = pattern_y * final_factor;

		// Ограничиваем максимальное смещение за один выстрел
		clamp(final_x, -PATTERN_MAX_STEP_X, PATTERN_MAX_STEP_X);
		clamp(final_y, -PATTERN_MAX_STEP_Y, PATTERN_MAX_STEP_Y);


//		Msg("Recoil: final_factor=%.3f [pattern=%.3f * addon=%.3f * agility=%.3f] → final_offset=(%.3f, %.3f)",
//			final_factor, pattern_factor, addon_factor, agility_factor, final_x, final_y);

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
	// Сохраняем текущую позицию как предпоследнюю (куда будем возвращаться)
	m_return_target_vert = m_target_angle_vert;
	m_return_target_horz = m_target_angle_horz;

	// Вычисляем новую позицию последнего выстрела
	float new_target_vert = m_target_angle_vert + pattern_y;
	float new_target_horz = m_target_angle_horz + pattern_x;

	// Добавляем мгновенную скорость для резкого начала отдачи
	// Скорость направляем к новой цели
	m_velocity_vert += (new_target_vert - m_target_angle_vert) * current_recoil.Pattern.Impulse;
	m_velocity_horz += (new_target_horz - m_target_angle_horz) * current_recoil.Pattern.Impulse;

	clamp(m_velocity_vert, -MAX_PATTERN_VELOCITY_VERT, MAX_PATTERN_VELOCITY_VERT);
	clamp(m_velocity_horz, -MAX_PATTERN_VELOCITY_HORZ, MAX_PATTERN_VELOCITY_HORZ);

	// Обновляем целевые углы (позиция последнего выстрела)
	m_target_angle_vert = new_target_vert;
	m_target_angle_horz = new_target_horz;

	//	Msg("Recoil impulse: vert=%.3f (vel=%.3f), horz=%.3f (vel=%.3f), new_target_vert=%.3f, new_target_horz=%.3f",
	//	pattern_y, pattern_y * current_recoil.Pattern.Impulse,
	//	pattern_x, pattern_x * current_recoil.Pattern.Impulse,
	//		new_target_vert, new_target_horz);
	//Msg("(m_velocity_vert) %.3f || %.3f (m_velocity_horz)", m_velocity_vert, m_velocity_horz);
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

		// Возвращаем m_target_angle к позиции предпоследнего выстрела
		m_target_angle_vert = m_target_angle_vert * (1.0f - return_speed) + m_return_target_vert * return_speed;
		m_target_angle_horz = m_target_angle_horz * (1.0f - return_speed) + m_return_target_horz * return_speed;
	}

	// Всегда применяем физику пружины
	SpringPhysics(dt, current_recoil.Pattern.Stiffness, current_recoil.Pattern.Damping);

	// Проверяем, достаточно ли мы близки к цели И скорость достаточно мала
	bool is_near_target = std::abs(m_angle_vert - m_target_angle_vert) < 0.005f &&
		std::abs(m_angle_horz - m_target_angle_horz) < 0.005f;

	bool is_slow = std::abs(m_velocity_vert) < 0.01f &&
		std::abs(m_velocity_horz) < 0.01f;

	if (is_near_target && is_slow)
	{
		// Плавно "приклеиваем" к цели через интерполяцию
		float blend = std::min(1.0f, dt ); 
		m_angle_vert = m_angle_vert * (1.0f - blend) + m_target_angle_vert * blend;
		m_angle_horz = m_angle_horz * (1.0f - blend) + m_target_angle_horz * blend;
		m_velocity_vert = 0.0f;
		m_velocity_horz = 0.0f;

		// Если вернулись к предпоследней позиции И выстрелы кончились
		if (m_shot_end &&
			std::abs(m_angle_vert - m_return_target_vert) < 0.001f &&
			std::abs(m_angle_horz - m_return_target_horz) < 0.001f)
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

bool CCameraShotEffector::ProcessCam(SCamEffectorInfo& info)
{
	if (current_recoil.LegacyRecoil)
	{
		if (m_actived)
		{
			float h, p;
			info.d.getHP(h, p);
			if (m_single_shot)
			{
				if (!m_shot_end)
				{
					info.d.setHP(h + m_delta_horz, p + m_delta_vert);
				}
			}
			else
			{
				info.d.setHP(h + m_angle_horz, p + m_angle_vert);
			}

			Update();
		}
	}
	else
	{
		Update();
	}
	return true;
}