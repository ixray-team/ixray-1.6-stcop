// Level_Bullet_Manager.cpp:	для обеспечения полета пули по траектории
//								все пули и осколки передаются сюда
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "Level.h"
#include "Level_Bullet_Manager.h"
#include "game_cl_base.h"
#include "Actor.h"
#include "FlamethrowerTraceCollision.h"
#include "GamePersistent.h"
#include "game_cl_base_weapon_usage_statistic.h"
#include "game_cl_mp.h"

#include "../Include/xrRender/UIRender.h"
#include "../Include/xrRender/Kinematics.h"
#include "ParticlesObject.h"
#include "AnomalyZone.h"
#include "HUDManager.h"
#include "../xrEngine/xr_ioc_cmd.h"

#define HIT_POWER_EPSILON 0.05f
#define WALLMARK_SIZE 0.04f

constexpr float MIN_RAYPICK_ERROR_DISTANCE = .1f;

float const CBulletManager::parent_ignore_distance = 3.f;
float CBulletManager::m_fMinBulletSpeed = 2.f;
Fvector CBulletManager::wind;

float air_resistance_epsilon = .1f;

float g_bullet_time_factor = 1.f;
bool g_bullet_debug_trj = false;
bool g_bullets_stop = false;

int bp_update_idx = 0;
int bp_render_idx = 1;

void SBullet::Init(const Fvector& position, const Fvector& direction, float starting_speed, float power,
				   //.				   float power_critical,
				   float impulse,
				   ALife::_OBJECT_ID sender_id,
				   ALife::_OBJECT_ID sendersweapon_id,
				   ALife::EHitType e_hit_type,
				   float maximum_distance,
				   const CCartridge& cartridge,
				   float const air_resistance_factor,
				   bool SendHit)
{
	flags._storage = 0;

	bullet_pos = position;
	tracer_pos[0] = position;
	tracer_pos[1] = position;
	tracer_last_pos[0] = position;
	tracer_last_pos[1] = position;

	speed = max_speed = starting_speed;
	VERIFY(speed > 0.f);

	start_position = position;
	start_velocity.mul(direction, starting_speed);
	born_time = Device.dwTimeGlobal;
	life_time = 0.f;

	VERIFY(direction.magnitude() > 0.f);
	dir.normalize(direction);

	hit_param.power = power * cartridge.param_s.kHit;
	hit_param.impulse = impulse * cartridge.param_s.kImpulse;

	max_dist = maximum_distance * cartridge.param_s.kDist;
	fly_dist = 0;
	parent_id = sender_id;
	flags.allow_sendhit = SendHit;
	weapon_id = sendersweapon_id;
	hit_type = e_hit_type;

	armor_piercing = cartridge.param_s.kAP;
	air_resistance = cartridge.param_s.kAirRes * air_resistance_factor;
	wallmark_size = cartridge.param_s.fWallmarkSize;
	m_u8ColorID = cartridge.param_s.u8ColorID;

	bullet_material_idx = cartridge.bullet_material_idx;
	VERIFY(u16(-1) != bullet_material_idx);

	flags.allow_tracer = !!cartridge.m_flags.test(CCartridge::cfTracer);
	flags.allow_ricochet = !!cartridge.m_flags.test(CCartridge::cfRicochet);
	flags.explosive = !!cartridge.m_flags.test(CCartridge::cfExplosive);
	flags.magnetic_beam = !!cartridge.m_flags.test(CCartridge::cfMagneticBeam);

	targetID = 0;
	density_mode = false;
}


void CBulletManager::PlayWhineSound(SBullet* bullet, CObject* object, const Fvector& pos)
{
	if (m_WhineSounds.empty())
	{
		return;
	}
	if (bullet->m_whine_snd.is_playing())
	{
		return;
	}
	if (bullet->hit_type != ALife::eHitTypeFireWound)
	{
		return;
	}

	bullet->m_whine_snd = m_WhineSounds[Random.randI(0, (u32)m_WhineSounds.size())];
	bullet->m_whine_snd.play_at_pos(object, pos);
}

void CBulletManager::PlayExplodePS(const Fmatrix& xf)
{
	if (m_ExplodeParticles.empty())
	{
		return;
	}

	shared_str const& ps_name = m_ExplodeParticles[Random.randI(0, (u32)m_ExplodeParticles.size())];
	xr_shared_ptr<CParticlesObject> const ps = Particles::Details::Create(*ps_name, true);
	ps->UpdateParent(xf, zero_vel);
	GamePersistent().ps_needtoplay.push_back(ps);
}

void CBulletManager::UpdateWorkload()
{
	PROF_EVENT("CBulletManager::UpdateWorkload");
	// this is because of ugly nature of removing bullets
	// when index in vector passed through the tgt_material field
	// and we can remove them only in case when we iterate bullets
	// in the reversed order

	if (m_Bullets.empty())
	{
		return;
	}

	rq_storage.r_clear();
	collide::rq_result dummy;
	BulletVec::reverse_iterator begin = m_Bullets.rbegin();
	BulletVec::reverse_iterator end = m_Bullets.rend();

	float dt =	Device.fTimeDelta;

	for (auto& it = begin; it < end; ++it)
	{
#if 0
		constexpr u32 SubstepsCount = 10;
		u32 SubSteps = it->speed * dt * SubstepsCount;
		float SubDt = dt / SubSteps;

		for (u32 i = 0u; i < SubSteps; ++i)
		{
			if (!process_bullet(rq_storage, *it, SubDt * g_bullet_time_factor))
			{
				break;
			}
		}
#else
		if (process_bullet(rq_storage, *it, dt * g_bullet_time_factor))
		{
			continue;
		}
#endif

		if (g_bullet_debug_trj && Device.dwTimeGlobal < (*it).born_time + 10000)
		{
			continue;
		}

		RegisterEvent(
			EVENT_REMOVE,
			false,
			&*it,
			Fvector().set(0, 0, 0),
			dummy,
			static_cast<u16>(&*it - &*m_Bullets.begin())
		);
	}
}

CBulletManager::CBulletManager()
{
	m_Bullets.clear();
	m_Bullets.reserve(100);
	Device.seqFrame.Add(this, REG_PRIORITY_LOW);
}

CBulletManager::~CBulletManager()
{
	m_Bullets.clear();
	m_WhineSounds.clear();
	m_Events.clear();
	Device.seqFrame.Remove(this);
}

void CBulletManager::Load()
{
	char const* bullet_manager_sect = "bullet_manager";
	if (!IsGameTypeSingle() && pSettings->section_exist("mp_bullet_manager"))
	{
		bullet_manager_sect = "mp_bullet_manager";
	}
	m_fTracerWidth = pSettings->r_float(bullet_manager_sect, "tracer_width");
	m_fTracerLengthMax = pSettings->r_float(bullet_manager_sect, "tracer_length_max");
	m_fTracerLengthMin = pSettings->r_float(bullet_manager_sect, "tracer_length_min");

	m_fGravityConst = pSettings->r_float(bullet_manager_sect, "gravity_const");
	m_fAirResistanceK = pSettings->r_float(bullet_manager_sect, "air_resistance_k");

	m_fMinBulletSpeed = pSettings->r_float(bullet_manager_sect, "min_bullet_speed");
	m_fCollisionEnergyMin = pSettings->r_float(bullet_manager_sect, "collision_energy_min");
	m_fCollisionEnergyMax = pSettings->r_float(bullet_manager_sect, "collision_energy_max");

	m_fHPMaxDist = pSettings->r_float(bullet_manager_sect, "hit_probability_max_dist");

	if (pSettings->line_exist(bullet_manager_sect, "bullet_velocity_time_factor"))
	{
		g_bullet_time_factor = pSettings->r_float(bullet_manager_sect, "bullet_velocity_time_factor");
	}


	const char* whine_sounds = pSettings->r_string(bullet_manager_sect, "whine_sounds");
	int cnt = _GetItemCount(whine_sounds);
	xr_string tmp;
	for (int k = 0; k < cnt; ++k)
	{
		m_WhineSounds.emplace_back();
		m_WhineSounds.back().create(_GetItem(whine_sounds, k, tmp), st_Effect, sg_SourceType);
	}

	const char* explode_particles = pSettings->r_string(bullet_manager_sect, "explode_particles");
	cnt = _GetItemCount(explode_particles);
	for (int k = 0; k < cnt; ++k)
	{
		m_ExplodeParticles.emplace_back(_GetItem(explode_particles, k, tmp));
	}

	const char* sh_name = pSettings->read_if_exists<str_c>(bullet_manager_sect,"tracer_shader","effects\\bullet_tracer");
	const char* tx_name = pSettings->read_if_exists<str_c>(bullet_manager_sect,"tracer_texture","fx\\fx_tracer");
	m_circle_size_k = pSettings->read_if_exists<float>(bullet_manager_sect,"fire_circle_k",.5f);

	sh_Tracer->create(sh_name, tx_name);

	m_aColors.clear();
	string64 LineName;

	for (u8 i = 0; i < 255; i++)
	{
		xr_sprintf(LineName, "color_%d", i);
		if (!pSettings->line_exist("tracers_color_table", LineName))
		{
			break;
		}
		u32 clr = pSettings->r_color("tracers_color_table", LineName);

		m_aColors.push_back(clr);
	};

	circle_uv.min.set(32.0f / 64.0f, 0.0f);
	circle_uv.max.set(1.0f, 32.0f / 512.0f);

	sprite_uv.min.set(0.0f, 1.0f);
	sprite_uv.max.set(16.0f / 64.0f, 0.0f);

	m_trj_shader->create("portal");
}

void CBulletManager::Clear()
{
	m_Bullets.clear();
	m_Events.clear();
	m_Bullets_Tracers.clear();
	m_WhineSounds.clear();
#ifdef DEBUG
	FlameManagersToDraw.clear();
#endif
}

void CBulletManager::AddBullet(
	const Fvector& position,
	const Fvector& direction,
	float starting_speed,
	float power,
	float impulse,
	ALife::_OBJECT_ID sender_id,
	ALife::_OBJECT_ID sendersweapon_id,
	ALife::EHitType e_hit_type,
	float maximum_distance,
	const CCartridge& cartridge,
	float const air_resistance_factor,
	bool SendHit,
	bool AimBullet
)
{
	VERIFY(u16(-1) != cartridge.bullet_material_idx);

	SBullet& bullet = m_Bullets.emplace_back();
	if (g_bullet_debug_trj)
	{
		bullet.lines.reserve(256);
	}
	
	bullet.Init(position, direction, starting_speed, power, impulse, sender_id, sendersweapon_id, e_hit_type, maximum_distance, cartridge, air_resistance_factor, SendHit);
	bullet.flags.aim_bullet = AimBullet;
	
	if (!IsGameTypeSingle())
	{
		if (SendHit)
		{
			Game().m_WeaponUsageStatistic->OnBullet_Fire(&bullet, cartridge);
		}
	}
}

namespace
{
ICF float effective_air_resistance(
	const SBullet& bullet,
	Fvector wind_factor,
	float const air_resistance
)
{
	Fvector b_vel;
	b_vel.mul(bullet.dir, bullet.speed);

	Fvector rel_vel;
	rel_vel.sub(b_vel, wind_factor);
	
	float rel_v_mag = rel_vel.magnitude();
	float rel_v_factor = rel_v_mag / bullet.speed;
	
	return air_resistance * rel_v_factor;
}

ICF Fvector parabolic_velocity(
	Fvector const& start_velocity,
	Fvector const& gravity,
	Fvector wind_factor,
	float const air_resistance,
	float const time
)
{
	return Fvector(start_velocity)
		.mul(
			std::max(
				0.f,
				1.f - air_resistance * time
			)
		)
		.mad(
			gravity,
			time
		)
		.mad(
			wind_factor,
			time
		);
}

ICF Fvector trajectory_velocity(
	Fvector const& start_velocity,
	Fvector const& gravity,
	Fvector wind_factor,
	float const air_resistance,
	float const time
)
{
	float const parabolic_time = std::max(0.f, 2.f / air_resistance - air_resistance_epsilon);
	float const fall_down_time = time - parabolic_time;
	//	float const fake_velocity	= start_velocity*2.f;
	if (fall_down_time < 0.f)
	{
		Fvector const xz_velocity = Fvector().set(start_velocity.x, 0.f, start_velocity.z);
		// this could be since we could fire in different directions
		// for example, vertically into the ground
		if (!fis_zero(xz_velocity.square_magnitude()))
		{
			return parabolic_velocity(
				start_velocity,
				gravity,
				wind_factor,
				air_resistance,
				time
			);
		}

		// this fake since our formula doesn't take into account
		// directions correctly
		return Fvector(start_velocity).mad(gravity, time);
	}

	Fvector parabolic_velocity =
		::parabolic_velocity(
			start_velocity,
			gravity,
			wind_factor,
			air_resistance,
			parabolic_time
		);

	VERIFY(!fis_zero(air_resistance_epsilon) || fis_zero(_sqr(parabolic_velocity.x) + _sqr(parabolic_velocity.z), EPS_L));
	
	return parabolic_velocity
		.mad(
			gravity,
			fall_down_time
		)
		.mad(
			wind_factor,
			fall_down_time
		);
}

ICF Fvector parabolic_position(
	Fvector const& start_position,
	Fvector const& start_velocity,
	Fvector const& gravity,
	Fvector wind_factor,
	float const air_resistance,
	float const time
)
{
	float const sqr_t_div_2 = _sqr(time) * .5f;

	return Fvector()
		.mad(
			start_position,
			start_velocity,
			time
		)
		.mad(
			Fvector(start_velocity).mul(-air_resistance),
			sqr_t_div_2
		)
		.mad(
			gravity,
			sqr_t_div_2
		)
		.mad(
			wind_factor,
			sqr_t_div_2
		);
}

ICF Fvector trajectory_position(
	Fvector const& start_position,
	Fvector const& base_start_velocity,
	Fvector const& base_gravity,
	Fvector wind_factor,
	float base_air_resistance,
	float const base_time
)
{
	Fvector const& gravity = base_gravity;
	float const& air_resistance = base_air_resistance;
	Fvector const& start_velocity = base_start_velocity;
	float const time = base_time;

	float const parabolic_time = std::max(0.f, 1.f / air_resistance - air_resistance_epsilon);
	float const fall_down_time = time - parabolic_time;
	
	if (fall_down_time < 0.f)
	{
		Fvector const xz_velocity = Fvector().set(start_velocity.x, 0.f, start_velocity.z);
		if (!fis_zero(xz_velocity.square_magnitude()))
		{
			return parabolic_position(
				start_position,
				start_velocity,
				gravity,
				wind_factor,
				air_resistance,
				time
			);
		}

		return Fvector(start_position)
			.mad(
				start_velocity, time
			)
			.mad(
				gravity, _sqr(time) * .5f
			)
			.mad(
				wind_factor, _sqr(time) * .5f
			);
	}

	Fvector const parabolic_position =
		::parabolic_position(
			start_position,
			start_velocity,
			gravity,
			wind_factor,
			air_resistance,
			parabolic_time
		);

	Fvector const parabolic_velocity =
		::parabolic_velocity(
			start_velocity,
			gravity,
			wind_factor,
			air_resistance,
			parabolic_time
		);

	return Fvector(parabolic_position)
		.mad(
			parabolic_velocity, fall_down_time
		)
		.mad(
			gravity, _sqr(fall_down_time) * .5f
		)
		.mad(
			wind_factor, _sqr(fall_down_time) * .5f
		);
}

ICF  float trajectory_max_error_time(
	float const t0,
	float const t1
)
{
	return (t1 + t0) * .5f;
	// this is correct even in our case
	// y(t) = V0y*t - V0y*ar*t^2/2 - g*t^2/2
	// x(t) = V0x*t - V0x*ar*t^2/2
}

ICF float trajectory_pick_error(
	float const low,
	float const high,
	Fvector const& position,
	Fvector const& velocity,
	Fvector const& gravity,
	Fvector wind_factor,
	float const air_resistance
)
{
	float max_error_time = trajectory_max_error_time(low, high);

	Fvector const start = trajectory_position(position, velocity, gravity, wind_factor, air_resistance, low);
	Fvector const target = trajectory_position(position, velocity, gravity, wind_factor, air_resistance, high);
	Fvector const max_error = trajectory_position(position, velocity, gravity, wind_factor, air_resistance, max_error_time);

	Fvector start_to_max_error = Fvector().sub(max_error, start);
	float magnitude = start_to_max_error.magnitude();
	start_to_max_error.mul(1.f / magnitude);
	Fvector start_to_target = Fvector().sub(target, start).normalize();
	float cosine_alpha = std::max(-1.f, std::min(start_to_max_error.dotproduct(start_to_target), 1.f));
	float sine_alpha = _sqrt(1.f - _sqr(cosine_alpha));
	return magnitude * sine_alpha;
}

ICF float trajectory_select_pick_gravity(
	SBullet& bullet,
	float start_low,
	float const high,
	Fvector const& gravity,
	float const air_resistance
)
{
	float const max_test_distance = bullet.max_dist - bullet.fly_dist;
	float const time_delta = high - start_low;
	float const time_to_fly = Fvector(bullet.start_velocity).mul(time_delta).mad(gravity, _sqr(time_delta) * .5f).magnitude();
	if (time_to_fly <= max_test_distance)
	{
		return high;
	}

	float const fall_down_velocity_magnitude = bullet.speed;
	float const positive_gravity = -gravity.y;
	float time = (_sqrt(_sqr(fall_down_velocity_magnitude) + 2.f * max_test_distance * positive_gravity) -
				  fall_down_velocity_magnitude) /
				 positive_gravity;
	VERIFY(time >= 0.f);

	VERIFY(high >= start_low);
	float result = start_low + time;
	clamp(result, start_low, high);
	VERIFY2(result <= high, make_string<const char*>("result[%f], high[%f], start_low[%f], air_resistance[%f]", result, high, start_low, air_resistance));
	return result;
}

ICF float trajectory_select_pick_parabolic(
	const SBullet& bullet,
	float const start_low,
	float high,
	Fvector const& gravity,
	Fvector wind_factor,
	float const air_resistance
)
{
	float const max_test_distance = bullet.max_dist - bullet.fly_dist;
	Fvector const start = trajectory_position(bullet.start_position, bullet.start_velocity, gravity, wind_factor, air_resistance, start_low);
	float const start_high = high;
	float low = start_low;
	float check_time = high;
	while (!fsimilar(low, high))
	{
		Fvector const intermediate = trajectory_position(bullet.start_position, bullet.start_velocity, gravity, wind_factor, air_resistance, start_low + (check_time - start_low) * .5f);
		Fvector const target = trajectory_position(bullet.start_position, bullet.start_velocity, gravity, wind_factor, air_resistance, check_time);
		float const distance = start.distance_to(intermediate) + intermediate.distance_to(target);
		if (distance < max_test_distance)
		{
			low = check_time;
		}
		else
		{
			high = check_time;
		}

		check_time = (low + high) * .5f;
	}

	VERIFY(low <= start_high);
	return low;
}

ICF bool trajectory_select_pick_ranges(
	float& result,
	SBullet& bullet,
	float const low,
	float const high,
	Fvector const& gravity,
	Fvector wind_factor,
	float const air_resistance
)
{
	float const max_test_distance = bullet.max_dist - bullet.fly_dist;
	VERIFY(max_test_distance > 0.f);

	if (air_resistance * (low + air_resistance_epsilon) >= 1.f)
	{
		result = trajectory_select_pick_gravity(bullet, low, high, gravity, air_resistance);
		return true;
	}

	if (air_resistance * (high + air_resistance_epsilon) < 1.f)
	{
		result = trajectory_select_pick_parabolic(bullet, low, high, gravity, wind_factor, air_resistance);
		return false;
	}

	float const fall_down_time = std::max(0.f, 1.f / air_resistance - air_resistance_epsilon);
	if (!fsimilar(fall_down_time, low))
	{
		result = trajectory_select_pick_parabolic(bullet, low, fall_down_time, gravity, wind_factor, air_resistance);
		return false;
	}

	result = trajectory_select_pick_gravity(bullet, fall_down_time, high, gravity, air_resistance);
	return false;
}

ICF float trajectory_select_pick_time(
	SBullet& bullet,
	float const start_low,
	float high,
	Fvector const& gravity,
	Fvector wind_factor,
	float const air_resistance
)
{
	VERIFY2(start_low < high, make_string<const char*>("start_low[%f] high[%f]", start_low, high));
	float const start_high = high;
	if (trajectory_select_pick_ranges(high, bullet, start_low, high, gravity, wind_factor, air_resistance))
	{
		if (high <= start_high)
		{
			return high;
		}

		return start_high;
	}

	float low = start_low;
	float check_time = high;

	while (!fsimilar(low, high))
	{
		float distance = trajectory_pick_error(start_low, check_time, bullet.start_position, bullet.start_velocity, gravity, wind_factor, air_resistance);

		if (distance < MIN_RAYPICK_ERROR_DISTANCE)
		{
			low = check_time;
		}
		else
		{
			high = check_time;
		}

		check_time = (low + high) * .5f;
	}

	VERIFY2(low <= start_high, make_string<const char*>("low[%f], high[%f]", low, start_high));
	return low;
}

ICF void update_bullet_parabolic(
	SBullet& bullet,
	bullet_test_callback_data& data,
	Fvector const& gravity,
	Fvector wind_factor,
	float const air_resistance
)
{
	Fvector xz_projection = Fvector(data.collide_position).sub(bullet.start_position);
	xz_projection.y = 0;
	float const xz_range = xz_projection.magnitude();
	Fvector const xz_velocity = Fvector().set(bullet.start_velocity.x, 0.f, bullet.start_velocity.z);

	VERIFY(air_resistance >= 0.f);
	if (air_resistance > 0.f)
	{
		float value = 2 * air_resistance * xz_range / xz_velocity.magnitude();
		clamp(value, 0.f, 1.f);
		VERIFY(value <= 1.f);
		VERIFY(value >= 0.f);
		data.collide_time = (1.f - _sqrt(1.f - value)) / air_resistance;
	}
	else
	{
		data.collide_time = xz_range / xz_velocity.magnitude();
	}

	VERIFY(data.collide_time >= 0.f);

	clamp(data.collide_time, bullet.life_time, data.high_time);

	data.collide_position = trajectory_position(bullet.start_position, bullet.start_velocity, gravity, wind_factor, air_resistance, data.collide_time);
	Fvector const new_velocity = trajectory_velocity(bullet.start_velocity, gravity, wind_factor, air_resistance, data.collide_time);
	bullet.speed = new_velocity.magnitude();
	bullet.dir = Fvector(new_velocity).normalize_safe();
}

ICF void update_bullet_gravitation(
	SBullet& bullet,
	bullet_test_callback_data& data,
	Fvector const& gravity,
	Fvector wind_factor,
	float const air_resistance,
	float const fall_down_time
)
{
	Fvector const fall_down_position = trajectory_position(bullet.start_position, bullet.start_velocity, gravity, wind_factor, air_resistance, fall_down_time);
	Fvector const fall_down_velocity = trajectory_velocity(bullet.start_velocity, gravity, wind_factor, air_resistance, fall_down_time);
	VERIFY(!fis_zero(air_resistance_epsilon) || fis_zero(_sqr(fall_down_velocity.x) + _sqr(fall_down_velocity.z), EPS_L));
	float const fall_down_velocity_magnitude = fall_down_velocity.magnitude();

	Fvector xz_projection = Fvector(data.collide_position).sub(fall_down_position);
	xz_projection.y = 0;
	float const xz_range = xz_projection.magnitude();
	Fvector const xz_velocity = Fvector().set(fall_down_velocity.x, 0.f, fall_down_velocity.z);

	if (!fis_zero(xz_velocity.magnitude()))
	{
		data.collide_time = fall_down_time + xz_range / xz_velocity.magnitude();
		VERIFY(data.collide_time >= 0.f);
		clamp(data.collide_time, bullet.life_time, data.high_time);
	}
	else
	{
		float const positive_gravity = -gravity.y;
		float const distance = fall_down_position.distance_to(data.collide_position);
		data.collide_time = fall_down_time +
							(_sqrt(_sqr(fall_down_velocity_magnitude) + 2.f * distance * positive_gravity) -
							 fall_down_velocity_magnitude) /
								positive_gravity;
		VERIFY(data.collide_time >= 0.f);
		clamp(data.collide_time, bullet.life_time, data.high_time);
	}

	Fvector const new_velocity = trajectory_velocity(bullet.start_velocity, gravity, wind_factor,  air_resistance, data.collide_time);
	bullet.speed = new_velocity.magnitude();
	bullet.dir = Fvector(new_velocity).normalize_safe();
}

ICF void update_bullet(
	SBullet& bullet,
	bullet_test_callback_data& data,
	Fvector const& gravity,
	Fvector wind_factor,
	float const air_resistance
)
{
	if (air_resistance * (bullet.life_time + air_resistance_epsilon) >= 1.f)
	{
		update_bullet_gravitation(bullet, data, gravity,wind_factor, air_resistance, std::max(0.f, 1.f / air_resistance - air_resistance_epsilon));
		return;
	}

	Fvector const xz_velocity = Fvector().set(bullet.start_velocity.x, 0.f, bullet.start_velocity.z);
	if (fis_zero(xz_velocity.square_magnitude()))
	{
		update_bullet_gravitation(bullet, data, gravity, wind_factor, air_resistance, 0.f);
		return;
	}

	update_bullet_parabolic(bullet, data, gravity, wind_factor,air_resistance);
}

ICF bool try_update_bullet(SBullet& bullet, Fvector const& gravity, Fvector wind_factor,float const air_resistance, float const time)
{
	Fvector const new_position = trajectory_position(bullet.start_position, bullet.start_velocity, gravity, wind_factor, air_resistance, time);
	bullet.fly_dist += bullet.bullet_pos.distance_to(new_position);

	if (bullet.fly_dist >= bullet.max_dist)
	{
		return false;
	}

	Fbox const level_box = Level().ObjectSpace.GetBoundingVolume();
	if (
		bullet.bullet_pos.x < level_box.x1 ||
		bullet.bullet_pos.x > level_box.x2 ||
		bullet.bullet_pos.y < level_box.y1 ||
		//		(bullet.bullet_pos.y > level_box.y2) ||
		bullet.bullet_pos.z < level_box.z1 ||
		bullet.bullet_pos.z > level_box.z2
	)
	{
		return false;
	}

	Fvector const new_velocity = trajectory_velocity(bullet.start_velocity, gravity, wind_factor, air_resistance, bullet.life_time);
	bullet.speed = new_velocity.magnitude();

	if (fis_zero(bullet.speed))
	{
		return false;
	}

	bullet.tracer_last_pos[bp_update_idx] = bullet.bullet_pos;
	bullet.bullet_pos = new_position;
	bullet.tracer_pos[bp_update_idx] = new_position;
	bullet.dir = Fvector(new_velocity).normalize_safe();
	bullet.life_time = time;

	return true;
}

IC float SqrDistancePointToSegment(const Fvector& pt, const Fvector& orig, const Fvector& dir)
{
	Fvector diff;
	diff.sub(pt, orig);
	float fT = diff.dotproduct(dir);

	if (fT <= 0.0f)
	{
		fT = 0.0f;
	}
	else
	{
		float fSqrLen = dir.square_magnitude();
		if (fT >= fSqrLen)
		{
			fT = 1.0f;
			diff.sub(dir);
		}
		else
		{
			fT /= fSqrLen;
			diff.sub(Fvector().mul(dir, fT));
		}
	}

	return diff.square_magnitude();
}
}

bool CBulletManager::firetrace_callback(const collide::rq_result& result, LPVOID params)
{
	bullet_test_callback_data& data = *(bullet_test_callback_data*)params;
	SBullet& bullet = *data.pBullet;

	Fvector& collide_position = data.collide_position;
	collide_position = Fvector().mad(bullet.bullet_pos, bullet.dir, result.range);

	float const air_resistance = IsGameTypeSingle() ? Level().BulletManager().m_fAirResistanceK : bullet.air_resistance;

	if (!result.IsStatic() && (result.GetDynamic()->SpatialComponent->type & ESPATIAL_TYPE::SHAPE) != ESPATIAL_TYPE::NONE)
	{
		auto Obj = const_cast<CObject*>(result.GetDynamic());
		if ((Obj->SpatialComponent->spatial.type & ESPATIAL_TYPE::SHAPE) != ESPATIAL_TYPE::NONE)
		{
			CGameObject* go = Obj->cast_game_object();

				if (go != nullptr)
				{
					if (CAnomalyZone* CZ = go->cast_anomaly_zone())
					{
						u8 flag = CZ->PlayEntranceSmallParticles(collide_position, bullet.dir, bullet.start_velocity, true);
						if (flag == u8(1))
						{
							data.collide_time = 1.f;
							bullet.speed = 0.f;

							return true;
						}

						if (flag == u8(2))
						{
							bullet.start_position = collide_position;
							bullet.bullet_pos = collide_position;
							bullet.tracer_pos[bp_update_idx] = collide_position;

							Fvector C;
							CZ->Center(C);
							float radius = CZ->Radius();

							Fvector normal;
							normal.sub(collide_position, C);
							normal.normalize();

							Fvector incoming_dir = bullet.dir;
							incoming_dir.normalize();

							float dot = incoming_dir.dotproduct(normal);
							Fvector reflected_dir;
							reflected_dir.mad(incoming_dir, normal, -2.0f * dot);
							reflected_dir.normalize();

							reflected_dir.random_dir(reflected_dir, deg2rad(5.0f));

							bullet.dir = reflected_dir;

							float energy_loss = 0.8f; //-80%
							bullet.speed *= 1.0f - energy_loss;

							bullet.start_velocity.set(bullet.dir);
							bullet.start_velocity.mul(bullet.speed);
						}

						if (flag == u8(3))
						{
							bullet.start_position = collide_position;
							bullet.bullet_pos = collide_position;
							bullet.tracer_pos[bp_update_idx] = collide_position;
							bullet.dir.random_dir();
							bullet.start_velocity = Fvector(bullet.dir).mul(bullet.speed * 0.2f);
					}
				}
			}
		}
	}

	CBulletManager& bullet_manager = Level().BulletManager();
	Fvector const gravity = {0.f, -bullet_manager.m_fGravityConst, 0.f};
	update_bullet(bullet, data, gravity, wind, air_resistance);
	if (fis_zero(bullet.speed))
	{
		return false;
	}

	if (fis_zero(data.collide_time))
	{
		return true;
	}

	//статический объект
	if (result.IsStatic()) {
		auto& triangle	= result.GetStatic()->tris[result.element];
		bullet_manager.RegisterEvent(EVENT_HIT, false, &bullet, collide_position, result, triangle.material);
		return false;
	}

	//динамический объект
	VERIFY(!(result.GetDynamic()->ID() == bullet.parent_id &&  bullet.fly_dist < parent_ignore_distance) );
	IKinematics* const kinematics = PKinematics(result.GetDynamic()->Visual());
	if (!kinematics)
	{
		return false;
	}

	CBoneData const& bone_data = kinematics->LL_GetData((u16)result.element);
	bullet_manager.RegisterEvent(EVENT_HIT, true, &bullet, collide_position, result, bone_data.game_mtl_idx);
	return false;
}

void CBulletManager::add_bullet_point(
	Fvector const& start_position,
	Fvector& previous_position,
	Fvector const& start_velocity,
	Fvector const& gravity,
	Fvector wind_factor,
	float const air_resistance,
	float const current_time,
	SBullet& bullet
)
{
	Fvector const temp = trajectory_position(start_position, start_velocity, gravity, wind_factor, air_resistance, current_time);

	if (!g_bullet_debug_trj)
	{
		return;
	}

	bullet.lines.emplace_back(previous_position, temp);
	previous_position = temp;
}

bool CBulletManager::trajectory_check_error(
	Fvector& previous_position,
	collide::rq_results& storage,
	SBullet& bullet,
	float& low,
	float& high,
	Fvector const& gravity,
	Fvector wind_factor,
	float air_resistance
)
{
	Fvector const& position = bullet.start_position;
	Fvector const& velocity = bullet.start_velocity;
	Fvector const start = trajectory_position(position, velocity, gravity, wind_factor, air_resistance, low);
	Fvector const target = trajectory_position(position, velocity, gravity, wind_factor, air_resistance, high);
	Fvector start_to_target = Fvector().sub(target, start);
	float const distance = start_to_target.magnitude();
	if (fis_zero(distance))
	{
		return true;
	}

	start_to_target.mul(1.f / distance);

	bullet_test_callback_data data;
	data.pBullet = &bullet;
	data.high_time = high;
	bullet.flags.ricochet_was = 0;
	bullet.dir = start_to_target;

	collide::ray_defs RD(start, start_to_target, distance, CDB::OPT_FULL_TEST, collide::rq_target(collide::rqtBoth | collide::rqtShape));
	bool const result = Level().ObjectSpace.RayQuery(storage, RD, firetrace_callback, &data, test_callback, nullptr);

	if (!result || data.collide_time == 0.f)
	{
		add_bullet_point(
			bullet.start_position,
			previous_position,
			bullet.start_velocity,
			gravity,
			wind_factor,
			air_resistance,
			high,
			bullet
		);

		return true;
	}

	add_bullet_point(
		bullet.start_position,
		previous_position,
		bullet.start_velocity,
		gravity,
		wind_factor,
		air_resistance,
		data.collide_time,
		bullet
	);

	low = 0.f;

	VERIFY(high >= data.collide_time);
	high -= data.collide_time;

	++bullet.change_rajectory_count;
	bullet.start_position = data.collide_position;
	bullet.bullet_pos = data.collide_position;

	bullet.start_velocity = Fvector().mul(bullet.dir, bullet.speed);
	bullet.born_time += iFloor(data.collide_time * 1000.f);
	bullet.life_time = 0.f;
	return false;
}

bool CBulletManager::process_bullet(collide::rq_results& storage, SBullet& bullet, float dt)
{
	Fvector const gravity = Fvector().set(0.f, -m_fGravityConst, 0.f);
	float const air_resistance = IsGameTypeSingle() ? m_fAirResistanceK : bullet.air_resistance;

	Fvector const& start_position = bullet.bullet_pos;
	Fvector previous_position = start_position;

	float low = bullet.life_time;
	float high = bullet.life_time + dt;

	bullet.change_rajectory_count = 0;

	for (;;)
	{
		for (;;)
		{
			if (bullet.speed < 1.f)
			{
				return false;
			}

			if (bullet.change_rajectory_count >= 32)
			{
				return false;
			}

			float time = trajectory_select_pick_time(bullet, low, high, gravity, wind, air_resistance);

			if (fsimilar(time, low))
			{
				return false;
			}

			float safe_time = time;
			VERIFY2(safe_time <= high, make_string<const char*>("safe_time[%f], high[%f]", safe_time, high));
			if (!trajectory_check_error(previous_position, storage, bullet, low, time, gravity, wind, air_resistance))
			{
				VERIFY2(safe_time >= time, make_string<const char*>("safe_time[%f], time[%f]", safe_time, time));
				VERIFY2(safe_time <= high, make_string<const char*>("safe_time[%f], high[%f]", safe_time, high));

				high = high - safe_time + time;
				VERIFY2(low <= high, make_string<const char*>("start_low[%f] high[%f]", low, high));
				if (fsimilar(low, high))
				{
					return !fis_zero(bullet.speed);
				}

				break;
			}

			if (!try_update_bullet(bullet, gravity, wind, air_resistance, time))
			{
				return false;
			}

			if (fsimilar(time, high))
			{
				return true;
			}

			VERIFY2(low < high, make_string<const char*>("start_low[%f] high[%f]", low, high));
			low = time;
			VERIFY2(low < high, make_string<const char*>("start_low[%f] high[%f]", low, high));
		}

		if (fis_zero(bullet.speed))
		{
			return false;
		}
	}
}

void CBulletManager::Render()
{
	PROF_EVENT("CBulletManager::Render")

	u32 g_bullet_debug_trj_totalLines = 0u;
	static xr_vector<SBullet*> visible_tracers;
	visible_tracers.clear();
	visible_tracers.reserve(m_Bullets.size());

	if (!g_bullet_debug_trj)
	{
		for (SBullet& bullet : m_Bullets)
		{
			if (!bullet.flags.allow_tracer)
			{
				continue;
			}
			Fvector tracer_last_pos = bullet.tracer_last_pos[bp_render_idx];
			Fvector const tracer = Fvector().sub(bullet.tracer_pos[bp_render_idx], tracer_last_pos);
			float length = tracer.magnitude();

			if (length < m_fTracerLengthMin)
			{
				continue;
			}

			Fvector const tracer_direction = length >= EPS_L ? Fvector(tracer).mul(1.f / length) : bullet.dir;
			Fvector center;
			center.mad(tracer_last_pos, tracer_direction, length * .5f);

			if (!::Render->ViewBase.testSphere_dirty(center, length * .5f))
			{
				continue;
			}

			visible_tracers.push_back(&bullet);
		}
	}
	else
	{
		for (SBullet& bullet : m_Bullets)
		{
			g_bullet_debug_trj_totalLines += (u32)bullet.lines.size();
		}
	}

	if (!visible_tracers.empty())
	{
		UIRender->CacheSetXformWorld(Fidentity);

		// 3d tracer
		// UIRender->SetShader(*m_trj_shader);
		// 2d tracer
		UIRender->SetShader(*sh_Tracer);

		UIRender->CacheSetCullMode(ERHI_CULLMODE::NONE);
		const Fvector& cam_P = Device.vCameraPosition;
		const Fvector& cam_D = Device.vCameraDirection;
		const Fvector& cam_T = Device.vCameraTop;
		const Fvector& cam_R = Device.vCameraRight;
		constexpr float MaxDistSqr = 1.0f;
		constexpr float MinDistSqr = 0.09f;

		// 3d tracer
		// constexpr u32 VERTICES_PER_TRACER = 672u;
		// u32 MAX_TRACERS = UIRender->VBuffMaxSize() / sizeof(IUIRender::r_vertL<VERTICES_PER_TRACER>);
		// 2d tracer
		constexpr u32 VERTICES_PER_TRACER = 12u;
		u32 MAX_TRACERS = UIRender->VBuffMaxSize() / sizeof(IUIRender::r_vertLIT<VERTICES_PER_TRACER>);

		u32 total_tracers = visible_tracers.size();
		for (u32 start_idx = 0u; start_idx < total_tracers; start_idx += MAX_TRACERS)
		{
			u32 batch_size = std::min(MAX_TRACERS, total_tracers - start_idx);
			// 3d tracer
			// void** buffer = UIRender->StartPrimitive(batch_size * VERTICES_PER_TRACER, IUIRender::ptTriList, IUIRender::pttL);
			// 2d tracer
			IUIRender::r_vertLIT<VERTICES_PER_TRACER>** buffer = (IUIRender::r_vertLIT<VERTICES_PER_TRACER>**)UIRender->StartPrimitive(batch_size * VERTICES_PER_TRACER, IUIRender::ptTriList, IUIRender::pttLIT);
			IUIRender::r_vertLIT<VERTICES_PER_TRACER>* buff = *buffer;
			for (u32 i = 0; i < batch_size; ++i)
			{
				SBullet* bullet = visible_tracers[start_idx + i];

				Fvector tracer_last_pos = bullet->tracer_last_pos[bp_render_idx];
				Fvector tracer_pos = bullet->tracer_pos[bp_render_idx];

				Fvector const tracer = Fvector().sub(tracer_pos, tracer_last_pos);
				float length = tracer.magnitude();

				if (length > m_fTracerLengthMax)
				{
					length = m_fTracerLengthMax;
				}

				Fvector const tracer_direction = length >= EPS_L ? Fvector(tracer).mul(1.f / length) : bullet->dir;

				float width = m_fTracerWidth;
				float dist2segSqr = SqrDistancePointToSegment(cam_P, tracer_pos, tracer);

				if (dist2segSqr < MaxDistSqr)
				{
					if (dist2segSqr < MinDistSqr)
					{
						dist2segSqr = MinDistSqr;
					}
					width *= _sqrt(dist2segSqr / MaxDistSqr);
				}

				if (cam_P.distance_to_sqr(tracer_pos) < length * length)
				{
					length = cam_P.distance_to(tracer_pos) - 0.3f;
				}

				bool bActor = false;
				if (Level().CurrentViewEntity())
				{
					bActor = bullet->parent_id == Level().CurrentViewEntity()->ID();
				}

				R_ASSERT(bullet->m_u8ColorID < m_aColors.size());
				u32 color = m_aColors[bullet->m_u8ColorID];
				Fvector& pos = tracer_pos;

				// 3d tracer
				//{
				//	Fvector dir = tracer_direction;
				//	dir.normalize_safe();
				//
				//	Fvector up(0.f, 1.f, 0.f);
				//	float dot = dir.dotproduct(up);
				//
				//	if (std::abs(dot) > 0.999f)
				//	{
				//		up.set(0.f, 0.f, 1.f);
				//		dot = dir.dotproduct(up);
				//
				//		if (std::abs(dot) > 0.999f)
				//			up.set(1.f, 0.f, 0.f);
				//	}
				//
				//	Fvector right;
				//	right.crossproduct(up, dir);
				//	right.normalize_safe();
				//
				//	Fvector real_up;
				//	real_up.crossproduct(dir, right);
				//	real_up.normalize_safe();
				//
				//	Fmatrix mR;
				//	mR.i = right;	mR._14 = 0.f;
				//	mR.j = real_up;	mR._24 = 0.f;
				//	mR.k = dir;		mR._34 = 0.f;
				//	mR.c = pos;		mR._44 = 1.f;
				//
				//	mR.k.mul(length);
				//	mR.i.mul(width * .15f);
				//	mR.j.mul(width * .15f);
				//
				//	HUD().world_prims.append_ellipse(mR, 0, color, buffer);
				//}

				// 2d tracer
				{
					float k_speed = bullet->speed / 1000.0f;
					float sprite_size = k_speed * width * m_circle_size_k * (std::abs(cam_D.dotproduct(tracer_direction)) * 0.95f);
					float sprite_width = width * .5f;
					float sprite_length = length * .5f;

					// sprite circle
					Fvector Vr, Vt;
					Vr.mul(cam_R, sprite_size);
					Vt.mul(cam_T, sprite_size);

					Fvector a_circle, b_circle, c_circle, d_circle;
					a_circle.sub(Vt, Vr);
					b_circle.add(Vt, Vr);
					c_circle.invert(a_circle);
					d_circle.invert(b_circle);
					Fvector center_circle;
					center_circle.mad(tracer_last_pos, tracer_direction, length * .95f);

					// sprite line
					Vr.mul(Fvector().crossproduct(tracer_direction, cam_D).normalize_safe(), sprite_width);
					Vt.mul(tracer_direction, sprite_length);

					Fvector a_sprite, b_sprite, c_sprite, d_sprite;
					a_sprite.sub(Vt, Vr);
					b_sprite.add(Vt, Vr);
					c_sprite.invert(a_sprite);
					d_sprite.invert(b_sprite);
					Fvector center_sprite;
					center_sprite.mad(tracer_last_pos, tracer_direction, length * .5f);

					Fbox2 crcuv = circle_uv;
					Fbox2 spruv = sprite_uv;

					Fvector2 a_c_uf{crcuv.min.x, crcuv.min.y};
					Fvector2 c_c_uf{crcuv.max.x, crcuv.max.y};

					Fvector2 a_s_uf{spruv.min.x, spruv.min.y};
					Fvector2 c_s_uf{spruv.max.x, spruv.max.y};

					Fvector a_c_vert{a_circle + center_circle};
					Fvector c_c_vert{c_circle + center_circle};

					Fvector a_s_vert{a_sprite + center_sprite};
					Fvector c_s_vert{c_sprite + center_sprite};

					buff[i] =
						{
							d_circle + center_circle,
							color,
							{crcuv.min.x, crcuv.max.y},
							a_c_vert,
							color,
							a_c_uf,
							c_c_vert,
							color,
							c_c_uf,
							c_c_vert,
							color,
							c_c_uf,
							a_c_vert,
							color,
							a_c_uf,
							b_circle + center_circle,
							color,
							{crcuv.max.x, crcuv.min.y},

							d_sprite + center_sprite,
							color,
							{spruv.min.x, spruv.max.y},
							a_s_vert,
							color,
							a_s_uf,
							c_s_vert,
							color,
							c_s_uf,
							c_s_vert,
							color,
							c_s_uf,
							a_s_vert,
							color,
							a_s_uf,
							b_sprite + center_sprite,
							color,
							{spruv.max.x, spruv.min.y},
						};
				}
			}
			*buffer += batch_size;
			UIRender->FlushPrimitive();
		}

		UIRender->CacheSetCullMode(ERHI_CULLMODE::BACK);
	}

	if (g_bullet_debug_trj && !m_Bullets.empty() && g_bullet_debug_trj_totalLines > 0u)
	{
		constexpr u32 DEFAULT_COLOR = color_rgba(100u, 255u, 100u, 255u);
		UIRender->SetShader(*m_trj_shader);

		static xr_vector<std::pair<Fvector, Fvector>> all_lines;
		all_lines.clear();
		all_lines.reserve(g_bullet_debug_trj_totalLines);

		for (SBullet& bullet : m_Bullets)
		{
			all_lines.insert(all_lines.end(), bullet.lines.begin(), bullet.lines.end());
		}

		u32 MAX_LINES = UIRender->VBuffMaxSize() / sizeof(IUIRender::r_vertL<2u>);
		u32 total_lines = (u32)all_lines.size();

		for (u32 start_idx = 0u; start_idx < total_lines; start_idx += MAX_LINES)
		{
			u32 batch_size = std::min(MAX_LINES, total_lines - start_idx);
			IUIRender::r_vertL<2u>** buffer = (IUIRender::r_vertL<2u>**)UIRender->StartPrimitive(batch_size * 2u, IUIRender::ptLineList, IUIRender::pttL);
			IUIRender::r_vertL<2u>* buff = *buffer;
			for (u32 i = 0u; i < batch_size; ++i)
			{
				auto& line = all_lines[start_idx + i];

				buff[i] =
					{
						line.first, DEFAULT_COLOR, line.second, DEFAULT_COLOR
					};
			}
			*buffer += batch_size;

			UIRender->FlushPrimitive();
		}
	}
}

void CBulletManager::OnFrame()
{
	static bool EnableWindEffectOnABullet = EngineExternal()[EEngineExternalGame::EnableWindEffectOnABullet];

	if (EnableWindEffectOnABullet)
	{
		static CGameFont* F = g_FontManager->CloneFont("stat_font");

		static float velocity = 0.f;
		static float direction = 0.f;

		static float target_velocity = 0.f;
		static float target_dir = 0.f;

		constexpr float cmp_threshold = 0.1f;
		constexpr bool debug_draw = true;
		
		float max_wind_rnd_velocity = CCC_Float::FastCommand("bullet_manager_max_wind_rnd_velocity", 2.5f, EPS_S, FLT_MAX);
		float max_wind_rnd_direction = CCC_Float::FastCommand("bullet_manager_max_wind_rnd_direction", 360.f, EPS_S, FLT_MAX);

		bool w_horizontal = CCC_Boolean::FastCommand("bullet_manager_wind_horizontal", true);

		if (fsimilar(velocity, target_velocity, cmp_threshold) && fsimilar(direction, target_dir, cmp_threshold))
		{
			target_velocity = Random.randF(0.f, max_wind_rnd_velocity);
			target_dir = deg2rad(Random.randF(0.f, max_wind_rnd_direction));
		}

		float lerp_factor = CCC_Float::FastCommand("bullet_manager_wind_lerp_factor", .1f, EPS_S, .99f);
		float lerp_scale = lerp_factor * Device.fTimeDelta;

		velocity += (target_velocity - velocity) * lerp_scale;
		direction = angle_inertion(direction, target_dir, .1f, M_PI, Device.fTimeDelta);
		direction = angle_normalize(direction);
		
		if (w_horizontal)
		{
			// xz clockwise rotation x = sin (cos default) && z = cos (sin default)
			wind.set(sinf(direction), 0.f, cos(direction)).mul(velocity);
		}
		else
		{
			// xy counter clock-wise rotation x = cos (cos default) && y = sin (sin default)
			wind.set(cosf(direction), sinf(direction), 0.f).mul(velocity);
		}

		if (debug_draw)
		{
			F->SetColor(color_rgba(255, 255, 255, 255));
			F->OutSet(400, 300);

			shared_str wind_dir = "???";
			float wd_angle = rad2deg(direction);

			if (wd_angle >= 337.5f || wd_angle < 22.5f)
			{
				wind_dir = "North";
			}
			else if (wd_angle < 67.5f)
			{
				wind_dir = "North-East";
			}
			else if (wd_angle < 112.5f)
			{
				wind_dir = "East";
			}
			else if (wd_angle < 157.5f)
			{
				wind_dir = "South-East";
			}
			else if (wd_angle < 202.5f)
			{
				wind_dir = "South";
			}
			else if (wd_angle < 247.5f)
			{
				wind_dir = "South-West";
			}
			else if (wd_angle < 292.5f)
			{
				wind_dir = "West";
			}
			else
			{
				wind_dir = "North-West";
			}

			F->OutNext("Wind accel: [%.3f, %.3f, %.3f]", VPUSH(wind));
			F->OutNext("Velocity: %.3f (lerp to %.3f)", velocity, target_velocity);
			F->OutNext("Direction: %.3fdeg (lerp to %.3fdeg)", rad2deg(direction), rad2deg(target_dir));
			F->OutNext("%s (%.1f deg)", *wind_dir, wd_angle);

			// kuda duet
			HUD().world_prims.append_lines_arrow(Fvector().set(0.f, 50.f, 0.f), wind, 10.f, color_rgba(255, 165, 0, 255));

			Fmatrix world_basis;
			world_basis.identity();
			Level().ObjectSpace.GetBoundingVolume().getcenter(world_basis.c);
			world_basis.setXYZ(0.f, 0.f, 0.f);

			// World basis (dlya orientacii kuda duet)
			HUD().world_prims.append_lines_arrow(Fvector().set(0.f, 50.f, 0.f), world_basis.i, 10.f, color_rgba(255, 0, 0, 255));
			HUD().world_prims.append_lines_arrow(Fvector().set(0.f, 50.f, 0.f), world_basis.j, 10.f, color_rgba(0, 255, 0, 255));
			HUD().world_prims.append_lines_arrow(Fvector().set(0.f, 50.f, 0.f), world_basis.k, 10.f, color_rgba(0, 0, 255, 255));
		}
	}
}

void CBulletManager::CommitEvents() // @ the start of frame
{
	PROF_EVENT("CBulletManager::CommitEvents");
	if (Device.Paused())
	{
		return;
	}

	if (g_bullets_stop)
	{
		return;
	}

	if (m_Events.size() > 1000)
	{
		Msg("! too many bullets during single frame: %d", m_Events.size());
	}

	for (_event& e : m_Events)
	{
		switch (e.Type)
		{
			case EVENT_HIT:
			{
				if (e.dynamic)
				{
					DynamicObjectHit(e);
				}
				else
				{
					StaticObjectHit(e);
				}
			}
			break;
			case EVENT_REMOVE:
			{
				if (e.bullet.flags.allow_sendhit && !IsGameTypeSingle())
				{
					Game().m_WeaponUsageStatistic->OnBullet_Remove(&e.bullet);
				}

				if (e.tgt_material < m_Bullets.size())
				{
					m_Bullets[e.tgt_material] = m_Bullets.back();
					m_Bullets.pop_back();
				}
			}
			break;
		}
	}

	m_Events.clear();
	Device.seqParallel.push_back(xr_make_delegate(this, &CBulletManager::UpdateWorkload));

	std::swap(bp_update_idx, bp_render_idx);
}

void CBulletManager::RegisterEvent(EventType Type, bool _dynamic, SBullet* bullet, const Fvector& end_point, const collide::rq_result& R, u16 tgt_material)
{
	m_Events.emplace_back();
	_event& E = m_Events.back();
	E.Type = Type;
	E.bullet = *bullet;

	switch (Type)
	{
		case EVENT_HIT:
		{
			E.dynamic = _dynamic;
			E.point = end_point;
			E.R = R;
			E.tgt_material = tgt_material;

			ObjectHit(&E.hit_result, bullet, end_point, R, tgt_material, E.normal);

			if (_dynamic)
			{
				//	E.Repeated = (R.O->ID() == E.bullet.targetID);
				//	bullet->targetID = R.O->ID();

				E.Repeated = (R.GetDynamic()->ID() == E.bullet.targetID);
				if (IsGameTypeSingle())
				{
					bullet->targetID = R.GetDynamic()->ID();
				}
				else
				{
					if (bullet->targetID != R.GetDynamic()->ID())
					{
						const CGameObject* pGO = const_cast<CObject*>(R.GetDynamic())->cast_game_object();
						if (pGO == nullptr || !pGO->BonePassBullet((u16)R.element))
						{
							bullet->targetID = R.GetDynamic()->ID();
						}
					}
				}
			};
		}
		break;
		case EVENT_REMOVE:
		{
			E.tgt_material = tgt_material;
		}
		break;
	}
}
