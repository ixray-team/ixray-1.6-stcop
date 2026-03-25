//////////////////////////////////////////////////////////////////////
// ShootingObject.cpp:  интерфейс для семейства стреляющих объектов 
//						(оружие и осколочные гранаты) 	
//////////////////////////////////////////////////////////////////////

#include "StdAfx.h"

#include "ShootingObject.h"
#include "WeaponAmmo.h"

#include "Actor.h"
#include "Spectator.h"
#include "game_cl_base.h"
#include "Level.h"
#include "Level_Bullet_Manager.h"
#include "game_cl_single.h"
#include "ParticlesObject.h"

#define HIT_POWER_EPSILON 0.05f
#define WALLMARK_SIZE 0.04f

CShootingObject::CShootingObject(void)
{
	fShotTimeCounter							= 0;
 	fOneShotTime						= 0;
	//fHitPower						= 0.0f;
	fvHitPower.set					(0.0f,0.0f,0.0f,0.0f);
	fvHitPowerCritical.set			(0.0f,0.0f,0.0f,0.0f);
	m_fStartBulletSpeed				= 1000.f;

	m_fPredBulletTime				= 0.0f;
	m_bUseAimBullet					= false;
	m_fTimeToAim					= 0.0f;
	
	bWorking						= false;

	light_render					= 0;
}

void CShootingObject::Load	(const char* section)
{
	if(pSettings->line_exist(section,"light_disabled"))
	{
		m_bLightShotEnabled		= !pSettings->r_bool(section,"light_disabled");
	}else
		m_bLightShotEnabled		= true;

	//время затрачиваемое на выстрел
	fOneShotTimeSaved			= pSettings->r_float		(section,"rpm");
	VERIFY2(fOneShotTimeSaved >0.f, make_string<const char*>("Section [%s], line rpm = %f", section, fOneShotTimeSaved));
	fOneShotTime			= 60.f / fOneShotTimeSaved;

	LoadFireParams		(section);
	LoadLights			(section, "");

	LoadParticle(section, "flame_particles", m_pFlameParticles);
	LoadParticle(section, "silencer_flame_particles", m_pFlameSilencerParticles);
	LoadParticle(section, "grenade_flame_particles", m_pFlameGlaucherParticles);

	LoadParticle(section, "smoke_particles", m_pSmokeParticles);
	LoadParticle(section, "silencer_smoke_particles", m_pSmokeSilencerParticles);

	if (pSettings->line_exist(section, "shell_particles"))
	{
		if (const char* pname = pSettings->r_string(section, "shell_particles"))
			m_sShellParticles = pname;

		vLoadedShellPoint = pSettings->line_exist(section, "shell_point") ? pSettings->r_fvector3(section, "shell_point") : zero_vel;
	}

	m_air_resistance_factor	= READ_IF_EXISTS(pSettings,r_float,section,"air_resistance_factor",1.f);

	light_render = ::Render->light_create();
	if (::Render->get_generation() == IRender_interface::GENERATION_R2)
		light_render->set_shadow(true);
	else 
		light_render->set_shadow(false);
}

void CShootingObject::DestroyEffects()
{
	light_render.destroy();

	if (m_pSmokeParticles)
		m_pSmokeParticles->Destroy();
	if (m_pFlameParticles)
		m_pFlameParticles->Destroy();
	if (m_pSmokeSilencerParticles)
		m_pSmokeSilencerParticles->Destroy();
	if (m_pFlameSilencerParticles)
		m_pFlameSilencerParticles->Destroy();
	if (m_pFlameGlaucherParticles)
		m_pFlameGlaucherParticles->Destroy();
}

void CShootingObject::LoadFireParams( const char* section )
{
	string32	buffer;
	shared_str	s_sHitPower;
	shared_str	s_sHitPowerCritical;

	//базовая дисперсия оружия
	fireDispersionBase	= deg2rad( pSettings->r_float(section,"fire_dispersion_base"	) );

	//сила выстрела и его мощьность
	s_sHitPower			= pSettings->r_string_wb(section, "hit_power" );//читаем строку силы хита пули оружия
	s_sHitPowerCritical	= READ_IF_EXISTS(pSettings, r_string_wb, section, "hit_power_critical", "0.0, 0.0, 0.0, 0.0");
	fvHitPower[egdMaster]			= (float)atof(_GetItem(*s_sHitPower,0,buffer));//первый параметр - это хит для уровня игры мастер
	fvHitPowerCritical[egdMaster]	= (float)atof(_GetItem(*s_sHitPowerCritical,0,buffer));//первый параметр - это хит для уровня игры мастер

	fvHitPower[egdNovice] = fvHitPower[egdStalker] = fvHitPower[egdVeteran] = fvHitPower[egdMaster];//изначально параметры для других уровней сложности такие же
	fvHitPowerCritical[egdNovice] = fvHitPowerCritical[egdStalker] = fvHitPowerCritical[egdVeteran] = fvHitPowerCritical[egdMaster];//изначально параметры для других уровней сложности такие же

	int num_game_diff_param=_GetItemCount(*s_sHitPower);//узнаём колличество параметров для хитов
	if (num_game_diff_param>1)//если задан второй параметр хита
	{
		fvHitPower[egdVeteran]	= (float)atof(_GetItem(*s_sHitPower,1,buffer));//то вычитываем его для уровня ветерана
	}
	if (num_game_diff_param>2)//если задан третий параметр хита
	{
		fvHitPower[egdStalker]	= (float)atof(_GetItem(*s_sHitPower,2,buffer));//то вычитываем его для уровня сталкера
	}
	if (num_game_diff_param>3)//если задан четвёртый параметр хита
	{
		fvHitPower[egdNovice]	= (float)atof(_GetItem(*s_sHitPower,3,buffer));//то вычитываем его для уровня новичка
	}

	num_game_diff_param=_GetItemCount(*s_sHitPowerCritical);//узнаём колличество параметров
	if (num_game_diff_param>1)//если задан второй параметр хита
	{
		fvHitPowerCritical[egdVeteran]	= (float)atof(_GetItem(*s_sHitPowerCritical,1,buffer));//то вычитываем его для уровня ветерана
	}
	if (num_game_diff_param>2)//если задан третий параметр хита
	{
		fvHitPowerCritical[egdStalker]	= (float)atof(_GetItem(*s_sHitPowerCritical,2,buffer));//то вычитываем его для уровня сталкера
	}
	if (num_game_diff_param>3)//если задан четвёртый параметр хита
	{
		fvHitPowerCritical[egdNovice]	= (float)atof(_GetItem(*s_sHitPowerCritical,3,buffer));//то вычитываем его для уровня новичка
	}

	fHitImpulse			= pSettings->r_float	(section, "hit_impulse" );
	//максимальное расстояние полета пули
	fireDistance		= pSettings->r_float	(section, "fire_distance" );
	//начальная скорость пули
	m_fStartBulletSpeed = pSettings->r_float	(section, "bullet_speed" );
	m_bUseAimBullet		= pSettings->r_bool		(section, "use_aim_bullet" );
	if (m_bUseAimBullet)
	{
		m_fTimeToAim		= pSettings->r_float	(section, "time_to_aim" );
	}
}

void CShootingObject::LoadLights		(const char* section, const char* prefix)
{
	string256				full_name;
	// light
	if(m_bLightShotEnabled) 
	{
		Fvector clr			= pSettings->r_fvector3		(section, xr_strconcat(full_name, prefix, "light_color"));
		light_base_color.set(clr.x,clr.y,clr.z,1);
		light_base_range	= pSettings->r_float		(section, xr_strconcat(full_name, prefix, "light_range")		);
		light_var_color		= pSettings->r_float		(section, xr_strconcat(full_name, prefix, "light_var_color")	);
		light_var_range		= pSettings->r_float		(section, xr_strconcat(full_name, prefix, "light_var_range")	);
		light_lifetime		= pSettings->r_float		(section, xr_strconcat(full_name, prefix, "light_time")		);
		light_time			= -1.f;

		m_bLightShotEnabled = light_var_range + light_base_range <= 0.f ? false : true;
	}
}

void CShootingObject::Light_Start	()
{
	if (!m_bLightShotEnabled)
	{
		return;
	}

	if (Device.dwFrame	!= light_frame)
	{
		light_frame					= Device.dwFrame;
		light_time					= light_lifetime;
		
		light_build_color.set		(Random.randFs(light_var_color,light_base_color.r),Random.randFs(light_var_color,light_base_color.g),Random.randFs(light_var_color,light_base_color.b),1);
		light_build_range			= Random.randFs(std::min(light_var_range, light_base_range), std::max(light_var_range, light_base_range));
	}
}

void CShootingObject::Light_Render	(const Fvector& P)
{
	if (!m_bLightShotEnabled)
	{
		return;
	}

	float light_scale			= light_time/light_lifetime;
	R_ASSERT(light_render);

	light_render->set_position	(P);
	light_render->set_color		(light_build_color.r*light_scale,light_build_color.g*light_scale,light_build_color.b*light_scale);
	light_render->set_range		(light_build_range*light_scale);

	if(	!light_render->get_active() )
	{
		light_render->set_active	(true);
	}
}


//////////////////////////////////////////////////////////////////////////
// Particles
//////////////////////////////////////////////////////////////////////////
void CShootingObject::LoadParticle(const char* section, const char* line, xr_shared_ptr<CParticlesObject>& particle)
{
	if (LPCSTR pname = pSettings->r_string_nullable(section, line))
	{
		if (const char* pname = pSettings->r_string(section, line))
		{
			if (particle){
				particle->Destroy();
			}

			particle = Particles::Details::Create(pname, false);
			particle->m_bAutoStop = true;
			particle->SetLiveUpdate(false);
		}
	}
}


void CShootingObject::StartShellParticle(const Fvector& parent_vel)
{
	if(!m_sShellParticles || Device.vCameraPosition.distance_to_sqr(get_CurrentShellPoint())>25.f ) return;
	xr_shared_ptr<CParticlesObject> m_pShellParticles = Particles::Details::Create(*m_sShellParticles, true);
	m_pShellParticles->SetLiveUpdate(true);
	Fmatrix pos;
	pos.set(get_ParticlesXFORM());
	pos.c.set(get_CurrentShellPoint());

	m_pShellParticles->UpdateParent(pos, parent_vel);

	CSpectator* tmp_spectr = Level().CurrentControlEntity() ? Level().CurrentControlEntity()->cast_spectator() : nullptr;
	bool in_hud_mode = IsHudModeNow();
	if (in_hud_mode && tmp_spectr && (tmp_spectr->GetActiveCam() != CSpectator::eacFirstEye))
		in_hud_mode = false;

	m_pShellParticles->Play(in_hud_mode);
	
}

void CShootingObject::StartSmokeParticle(const Fvector& parent_vel)
{
	xr_shared_ptr<CParticlesObject>& particles_ptr = fire_mode == eSilencerFire ? m_pSmokeSilencerParticles : m_pSmokeParticles;

	if (!particles_ptr) return;

	particles_ptr->Stop(false);
	Fmatrix pos;
	pos.set(get_ParticlesXFORM());
	pos.c.set(get_CurrentFirePoint());

	particles_ptr->UpdateParent(pos, parent_vel);

	CSpectator* tmp_spectr = Level().CurrentControlEntity() ? Level().CurrentControlEntity()->cast_spectator() : nullptr;
	bool in_hud_mode = IsHudModeNow();
	if (in_hud_mode && tmp_spectr && (tmp_spectr->GetActiveCam() != CSpectator::eacFirstEye))
		in_hud_mode = false;

	particles_ptr->Play(in_hud_mode);
}


void CShootingObject::StartFlameParticle()
{
	xr_shared_ptr<CParticlesObject>& particles_ptr = fire_mode == eGlauncherFire ? m_pFlameGlaucherParticles : 
													 fire_mode == eSilencerFire ? m_pFlameSilencerParticles : m_pFlameParticles;

	if(!particles_ptr) return;

	particles_ptr->Stop(false);

	Fmatrix pos;
	pos.set(get_ParticlesXFORM());
	pos.c.set(fire_mode == eGlauncherFire ? get_CurrentFirePoint2() : get_CurrentFirePoint());

	particles_ptr->SetXFORM(pos);
	
	CSpectator* tmp_spectr = Level().CurrentControlEntity() ? Level().CurrentControlEntity()->cast_spectator() : nullptr;
	bool in_hud_mode = IsHudModeNow();
	if (in_hud_mode && tmp_spectr && (tmp_spectr->GetActiveCam() != CSpectator::eacFirstEye))
		in_hud_mode = false;

	particles_ptr->Play(in_hud_mode);
}

void CShootingObject::UpdateEffects()
{
	Fmatrix pos;
	pos.set(get_ParticlesXFORM());
	pos.c.set(fire_mode == eGlauncherFire ? get_CurrentFirePoint2() : get_CurrentFirePoint());

	if (m_pFlameParticles && m_pFlameParticles->m_bPlaying)
		m_pFlameParticles->SetXFORM(pos);
	if (m_pFlameGlaucherParticles && m_pFlameGlaucherParticles->m_bPlaying)
		m_pFlameGlaucherParticles->SetXFORM(pos);
	if (m_pFlameSilencerParticles && m_pFlameSilencerParticles->m_bPlaying)
		m_pFlameSilencerParticles->SetXFORM(pos);

	if (m_pSmokeParticles && m_pSmokeParticles->m_bPlaying)
		m_pSmokeParticles->UpdateParent(pos, zero_vel);
	if (m_pSmokeSilencerParticles && m_pSmokeSilencerParticles->m_bPlaying)
		m_pSmokeSilencerParticles->UpdateParent(pos, zero_vel);

	if (light_render && light_time>0)		
	{
		light_time -= Device.fTimeDelta;
		if (light_time<=0) StopLight();
	}
}

void CShootingObject::StopLight			()
{
	if(light_render){
		light_render->set_active(false);
	}
}

void CShootingObject::RenderLight()
{
	if ( light_render && light_time>0 ) 
	{
		Light_Render(get_CurrentFirePoint());
	}
}

bool CShootingObject::SendHitAllowed		(CObject* pUser)
{
	if (Game().IsServerControlHits())
		return OnServer();

	if (OnServer())
	{
		if (pUser->cast_actor())
		{
			if (Level().CurrentControlEntity() != pUser)
			{
				return false;
			}
		}
		return true;
	}
	else
	{
		if (pUser->cast_actor())
		{
			if (Level().CurrentControlEntity() == pUser)
			{
				return true;
			}
		}
		return false;
	}
};

extern void random_dir(Fvector& tgt_dir, const Fvector& src_dir, float dispersion);

void CShootingObject::FireBullet(const Fvector& pos, 
								 const Fvector& shot_dir, 
								 float fire_disp,
								 const CCartridge& cartridge,
								 ALife::_OBJECT_ID parent_id,
								 ALife::_OBJECT_ID weapon_id,
								 bool send_hit)
{
	Fvector dir;
	random_dir(dir,shot_dir,fire_disp);
	
	bool aim_bullet;
	if (m_bUseAimBullet)
	{
		if (ParentMayHaveAimBullet())
		{
			if (m_fPredBulletTime==0.0)
			{
				aim_bullet=true;
			}
			else
			{
				if ((Device.fTimeGlobal-m_fPredBulletTime)>=m_fTimeToAim)
				{
					aim_bullet=true;
				}
				else
				{
					aim_bullet=false;
				}
			}
		}
		else
		{
			aim_bullet=false;
		}
	}
	else
	{
		aim_bullet=false;
	}
	m_fPredBulletTime = Device.fTimeGlobal;

	float l_fHitPower = 0.0f;
	if (ParentIsActor())//если из оружия стреляет актёр(игрок)
	{
		if (IsGameTypeSingle())
		{
			l_fHitPower			= fvHitPower[g_SingleGameDifficulty];
		}
		else
		{
			l_fHitPower			= fvHitPower[egdMaster];
		}
	}
	else
	{
		l_fHitPower			= fvHitPower[egdMaster];
	}

	Level().BulletManager().AddBullet( pos, 
										dir,
										m_fStartBulletSpeed * cur_silencer_koef.bullet_speed,
										l_fHitPower * cur_silencer_koef.hit_power,
										fHitImpulse * cur_silencer_koef.hit_impulse,
										parent_id, 
										weapon_id,
										ALife::eHitTypeFireWound, 
										fireDistance, 
										cartridge, 
										m_air_resistance_factor,
										send_hit, 
										aim_bullet);
}
void CShootingObject::FireStart	()
{
	bWorking=true;	
}
void CShootingObject::FireEnd	()				
{ 
	bWorking=false;	
}

void CShootingObject::setFireDistance(float value)
{
	fireDistance = value;
}

void CShootingObject::setFireDispersionBase(float value)
{
	fireDispersionBase = value;
}

void CShootingObject::setStartBulletSpeed(float value)
{
	m_fStartBulletSpeed = value;
}

void CShootingObject::setHitImpulse(float value)
{
	fHitImpulse = value;
}

void CShootingObject::setRPM(float value)
{
	fOneShotTime = value;
}

void CShootingObject::setHitPower(const Fvector4& vec)
{
	fvHitPower = vec;
}

void CShootingObject::setHitPowerCritical(const Fvector4& vec)
{
	fvHitPowerCritical = vec;
}

void CShootingObject::DumpActiveParams(shared_str const& section_name, CInifile& dst_ini) const
{
	dst_ini.w_fvector4(section_name.c_str(), "hit_power", fvHitPower);
	dst_ini.w_float(section_name.c_str(), "hit_impulse", fHitImpulse);
	dst_ini.w_float(section_name.c_str(), "bullet_speed", m_fStartBulletSpeed);
	dst_ini.w_float(section_name.c_str(), "max_distance", fireDistance);
	dst_ini.w_float(section_name.c_str(), "disp_base", fireDispersionBase);
	//dst_ini.w_float		(section_name.c_str(), "shot_time_counter", 	fShotTimeCounter);

	dst_ini.w_float(section_name.c_str(), "sil_hit_power", m_silencer_koef.hit_power);
	dst_ini.w_float(section_name.c_str(), "sil_hit_impulse", m_silencer_koef.hit_impulse);
	dst_ini.w_float(section_name.c_str(), "sil_bullet_speed", m_silencer_koef.bullet_speed);
	dst_ini.w_float(section_name.c_str(), "sil_disp_base", m_silencer_koef.fire_dispersion);
}