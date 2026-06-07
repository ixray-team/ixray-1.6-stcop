#include "StdAfx.h"
#include "../xrEngine/XR_IOConsole.h"
#include "AnomalyZone.h"
#include "Hit.h"
#include "PHDestroyable.h"
#include "Actor.h"
#include "xrServer_Objects_ALife_Monsters.h"
#include "../xrEngine/LightAnimLibrary.h"
#include "Level.h"
#include "game_cl_base.h"
#include "../xrEngine/IGame_Persistent.h"
#include "../xrEngine/xr_collide_form.h"
#include "Artefact.h"
#include "ai_object_location.h"
#include "../Include/xrRender/Kinematics.h"
#include "zone_effector.h"
#include "BreakableObject.h"
#include "GamePersistent.h"
#include "ParticlesObject.h"
#include "object_broker.h"
#include "HUDManager.h"
#include "AnomalyElectricCurve.h"
#include "AnomalyMovement.h"

#define WIND_RADIUS (4*Radius())	//расстояние до актера, когда появляется ветер 
#define FASTMODE_DISTANCE (50.f)	//distance to camera from sphere, when zone switches to fast update sequence

CAnomalyZone::CAnomalyZone(void) 
{
	m_zone_flags.zero			();

	m_fMaxPower					= 100.f;
	m_fAttenuation				= 1.f;
	m_fEffectiveRadius			= 1.0f;
	m_zone_flags.set			(eZoneIsActive, false);
	m_eHitTypeBlowout			= ALife::eHitTypeWound;
	m_pIdleParticles			= nullptr;
	m_pLight					= nullptr;
	m_pIdleLight				= nullptr;
	m_pIdleLAnim				= nullptr;
	

	m_StateTime.resize(eZoneStateMax);
	for(int i=0; i<eZoneStateMax; i++)
		m_StateTime[i] = 0;


	m_dwAffectFrameNum			= 0;
	m_fBlowoutWindPowerMax = m_fStoreWindPower = 0.f;
	m_fDistanceToCurEntity		= flt_max;
	m_ef_weapon_type			= u32(-1);
	m_owner_id					= u32(-1);

	m_actor_effector			= nullptr;
	m_zone_flags.set			(eIdleObjectParticlesDontStop, false);
	m_zone_flags.set			(eBlowoutWindActive, false);
	m_zone_flags.set			(eFastMode, true);

	m_eZoneState				= eZoneStateIdle;

	m_idle_sounds_variants.clear();
	m_awaking_sounds_variants.clear();
	m_accum_sounds_variants.clear();
	m_blowout_sounds_variants.clear();
	m_hit_sounds_variants.clear();
	m_entrance_sounds_variants.clear();

	TAnomalyElectricCurve& oAnomalyElectricCurve = CreateComponent<TAnomalyElectricCurve>();
	oAnomalyElectricCurve.BeginComponent(this);

	TAnomalyMovement& oAnomalyMovement = CreateComponent<TAnomalyMovement>();
	oAnomalyMovement.BeginComponent(this);
}

CAnomalyZone::~CAnomalyZone(void) 
{	
	xr_delete(m_actor_effector);

	DestroySoundsArray(m_idle_sounds_variants);
	DestroySoundsArray(m_awaking_sounds_variants);
	DestroySoundsArray(m_accum_sounds_variants);
	DestroySoundsArray(m_blowout_sounds_variants);
	DestroySoundsArray(m_hit_sounds_variants);
	DestroySoundsArray(m_entrance_sounds_variants);
}

void CAnomalyZone::DestroySoundsArray(xr_vector<ref_sound>& soundsArray)
{	
	for (ref_sound& sound : soundsArray)
	{
		sound.destroy();
	}

	soundsArray.clear();
}

void CAnomalyZone::StopAllSounds(xr_vector<ref_sound>& soundsArray)
{
	for (ref_sound& sound : soundsArray)
	{
		if (sound.is_playing()) {
			sound.stop();
		}
	}
}

void CAnomalyZone::UpdateSoundsPosition(xr_vector<ref_sound>& soundsArray, const Fvector& pos)
{
	for (ref_sound& sound : soundsArray)
	{
		if (sound.is_playing()) {
			// if (sound.slot())
			if (sound._feedback())
			{
				sound.set_position(pos);
			}
		}
	}
}

void CAnomalyZone::ParseRandomSounds(const char* section, const char* soundParameter, xr_vector<ref_sound> &soundsArray)
{
	soundsArray.clear();

	if (pSettings->line_exist(section, soundParameter))
	{
		xr_string unsplittedPaths = pSettings->r_string(section, soundParameter);
		if (!unsplittedPaths.empty()) 
		{
			xr_vector<xr_string> paths = unsplittedPaths.RemoveWhitespaces().Split();
			for (xr_string& sound_path : paths)
			{
				soundsArray.emplace_back().create(sound_path.c_str(), st_Effect, sg_SourceType);
			}
		}
	}
}

void CAnomalyZone::Load(const char* section) 
{
	inherited::Load(section);

	m_iDisableHitTime		= pSettings->r_s32(section,				"disable_time");	
	m_iDisableHitTimeSmall	= pSettings->r_s32(section,				"disable_time_small");	
	m_iDisableIdleTime		= pSettings->r_s32(section,				"disable_idle_time");	
	m_fHitImpulseScale		= pSettings->r_float(section,			"hit_impulse_scale");
	m_fEffectiveRadius		= pSettings->r_float(section,			"effective_radius");
	m_eHitTypeBlowout		= ALife::g_tfString2HitType(pSettings->r_string(section, "hit_type"));

	m_zone_flags.set(eIgnoreNonAlive,	pSettings->r_bool(section,	"ignore_nonalive"));
	m_zone_flags.set(eIgnoreSmall,		pSettings->r_bool(section,	"ignore_small"));
	m_zone_flags.set(eIgnoreArtefact,	pSettings->r_bool(section,	"ignore_artefacts"));

	//загрузить времена для зоны
	m_StateTime[eZoneStateIdle]			= -1;
	m_StateTime[eZoneStateAwaking]		= pSettings->r_s32(section, "awaking_time");
	m_StateTime[eZoneStateBlowout]		= pSettings->r_s32(section, "blowout_time");
	m_StateTime[eZoneStateAccumulate]	= pSettings->r_s32(section, "accamulate_time");
	
	const char* sound_str = nullptr;
	
	ParseRandomSounds(section, "idle_sound", m_idle_sounds_variants);
	ParseRandomSounds(section, "accum_sound", m_accum_sounds_variants);
	ParseRandomSounds(section, "awake_sound", m_awaking_sounds_variants);
	ParseRandomSounds(section, "blowout_sound", m_blowout_sounds_variants);
	ParseRandomSounds(section, "hit_sound", m_hit_sounds_variants);
	ParseRandomSounds(section, "entrance_sound", m_entrance_sounds_variants);

	if(pSettings->line_exist(section,"idle_particles")) 
		m_sIdleParticles	= pSettings->r_string(section,"idle_particles");

	if(pSettings->line_exist(section,"blowout_particles")) 
		m_sBlowoutParticles = pSettings->r_string(section,"blowout_particles");

	m_bBlowoutOnce = false;
	if (pSettings->line_exist(section, "blowout_once"))
		m_bBlowoutOnce		= pSettings->r_bool(section,"blowout_once");

	if(pSettings->line_exist(section,"accum_particles")) 
		m_sAccumParticles = pSettings->r_string(section,"accum_particles");

	if(pSettings->line_exist(section,"awake_particles")) 
		m_sAwakingParticles = pSettings->r_string(section,"awake_particles");
	

	if(pSettings->line_exist(section,"entrance_small_particles")) 
		m_sEntranceParticlesSmall = pSettings->r_string(section,"entrance_small_particles");
	if(pSettings->line_exist(section,"entrance_big_particles")) 
		m_sEntranceParticlesBig = pSettings->r_string(section,"entrance_big_particles");

	if(pSettings->line_exist(section,"hit_small_particles")) 
		m_sHitParticlesSmall = pSettings->r_string(section,"hit_small_particles");
	if(pSettings->line_exist(section,"hit_big_particles")) 
		m_sHitParticlesBig = pSettings->r_string(section,"hit_big_particles");

	if(pSettings->line_exist(section,"idle_small_particles")) 
		m_sIdleObjectParticlesBig = pSettings->r_string(section,"idle_big_particles");
	
	if(pSettings->line_exist(section,"idle_big_particles")) 
		m_sIdleObjectParticlesSmall = pSettings->r_string(section,"idle_small_particles");
	
	if(pSettings->line_exist(section,"idle_particles_dont_stop"))
		m_zone_flags.set(eIdleObjectParticlesDontStop, pSettings->r_bool(section,"idle_particles_dont_stop"));

	if(pSettings->line_exist(section,"postprocess")) 
	{
		m_actor_effector				= new CZoneEffector();
		m_actor_effector->Load			(pSettings->r_string(section,"postprocess"));
	};


	if(pSettings->line_exist(section,"bolt_entrance_particles")) 
	{
		m_sBoltEntranceParticles	= pSettings->r_string(section, "bolt_entrance_particles");
		m_zone_flags.set			(eBoltEntranceParticles, (m_sBoltEntranceParticles.size()!=0));
	}

	if (pSettings->line_exist(section, "bullet_entrance_action"))
	{
		m_zone_flags.set(eBulletEntranceAction, true);

		if (pSettings->line_exist(section, "bullet_entrance_particles"))
			m_sBulletEntranceParticles = pSettings->r_string(section, "bullet_entrance_particles");

		if (pSettings->line_exist(section, "bullet_flies_through"))
			m_zone_flags.set(eBulletFliesThrough, true);

		if (pSettings->line_exist(section, "bullet_ricochet"))
			m_zone_flags.set(eBulletRicochet, true);

		if (pSettings->line_exist(section, "bullet_ricochet_random"))
			m_zone_flags.set(eBulletRandom, true);
	}

	if(pSettings->line_exist(section,"blowout_particles_time")) 
	{
		m_dwBlowoutParticlesTime = pSettings->r_u32(section,"blowout_particles_time");
		if (s32(m_dwBlowoutParticlesTime)>m_StateTime[eZoneStateBlowout])	{
			m_dwBlowoutParticlesTime=m_StateTime[eZoneStateBlowout];
#ifndef MASTER_GOLD
			Msg("! ERROR: invalid 'blowout_particles_time' in '%s'",section);
#endif // #ifndef MASTER_GOLD
		}
	}
	else
		m_dwBlowoutParticlesTime = 0;

	if(pSettings->line_exist(section,"blowout_light_time")) 
	{
		m_dwBlowoutLightTime = pSettings->r_u32(section,"blowout_light_time");
		if (s32(m_dwBlowoutLightTime)>m_StateTime[eZoneStateBlowout])	{
			m_dwBlowoutLightTime=m_StateTime[eZoneStateBlowout];
#ifndef MASTER_GOLD
			Msg("! ERROR: invalid 'blowout_light_time' in '%s'",section);
#endif // #ifndef MASTER_GOLD
		}
	}
	else
		m_dwBlowoutLightTime = 0;

	if(pSettings->line_exist(section,"blowout_sound_time")) 
	{
		m_dwBlowoutSoundTime = pSettings->r_u32(section,"blowout_sound_time");
		if (s32(m_dwBlowoutSoundTime)>m_StateTime[eZoneStateBlowout])	{
			m_dwBlowoutSoundTime=m_StateTime[eZoneStateBlowout];
#ifndef MASTER_GOLD
			Msg("! ERROR: invalid 'blowout_sound_time' in '%s'",section);
#endif // #ifndef MASTER_GOLD
		}
	}
	else
		m_dwBlowoutSoundTime = 0;

	if(pSettings->line_exist(section,"blowout_explosion_time"))	{
		m_dwBlowoutExplosionTime = pSettings->r_u32(section,"blowout_explosion_time"); 
		if (s32(m_dwBlowoutExplosionTime)>m_StateTime[eZoneStateBlowout])	{
			m_dwBlowoutExplosionTime=m_StateTime[eZoneStateBlowout];
#ifndef MASTER_GOLD
			Msg("! ERROR: invalid 'blowout_explosion_time' in '%s'",section);
#endif // #ifndef MASTER_GOLD
		}
	}
	else
		m_dwBlowoutExplosionTime = 0;

	m_zone_flags.set(eBlowoutWind,  pSettings->r_bool(section,"blowout_wind"));
	if( m_zone_flags.test(eBlowoutWind) ){
		m_dwBlowoutWindTimeStart = pSettings->r_u32(section,"blowout_wind_time_start"); 
		m_dwBlowoutWindTimePeak = pSettings->r_u32(section,"blowout_wind_time_peak"); 
		m_dwBlowoutWindTimeEnd = pSettings->r_u32(section,"blowout_wind_time_end"); 
		R_ASSERT(m_dwBlowoutWindTimeStart < m_dwBlowoutWindTimePeak);
		R_ASSERT(m_dwBlowoutWindTimePeak < m_dwBlowoutWindTimeEnd);

		if((s32)m_dwBlowoutWindTimeEnd < m_StateTime[eZoneStateBlowout]){
			m_dwBlowoutWindTimeEnd =u32( m_StateTime[eZoneStateBlowout]-1);
#ifndef MASTER_GOLD
			Msg("! ERROR: invalid 'blowout_wind_time_end' in '%s'",section);
#endif // #ifndef MASTER_GOLD
		}

		
		m_fBlowoutWindPowerMax = pSettings->r_float(section,"blowout_wind_power");
	}

	//загрузить параметры световой вспышки от взрыва
	m_zone_flags.set(eBlowoutLight, pSettings->r_bool (section, "blowout_light"));
	if(m_zone_flags.test(eBlowoutLight) )
	{
		m_LightColor			= pSettings->r_fcolor(section, "light_color");
		m_fLightRange			= pSettings->r_float(section,"light_range");
		m_fLightTime			= pSettings->r_float(section,"light_time");
		m_fLightTimeLeft		= 0;
		m_fLightHeight		= pSettings->r_float(section,"light_height");
	}

	// volumetric light
	m_bVolumetricBlowout   = READ_IF_EXISTS(pSettings, r_bool,  section, "volumetric_blowout",   false);
	m_fVolumetricQuality   = READ_IF_EXISTS(pSettings, r_float, section, "volumetric_quality",   1.f);
	m_fVolumetricDistance  = READ_IF_EXISTS(pSettings, r_float, section, "volumetric_distance",  1.f);
	m_fVolumetricIntensity = READ_IF_EXISTS(pSettings, r_float, section, "volumetric_intensity", 1.f);

	//загрузить параметры idle подсветки
	m_zone_flags.set(eIdleLight,	pSettings->r_bool (section, "idle_light"));
	if( m_zone_flags.test(eIdleLight) )
	{
		m_fIdleLightRange		= pSettings->r_float(section,"idle_light_range");
		const char* light_anim		= pSettings->r_string(section,"idle_light_anim");
		m_pIdleLAnim			= LALib.FindItem(light_anim);
		m_fIdleLightHeight		= pSettings->r_float(section,"idle_light_height");
		m_zone_flags.set(eIdleLightVolumetric,READ_IF_EXISTS(pSettings, r_bool, section, "idle_light_volumetric", false) );
		m_zone_flags.set(eIdleLightShadow,READ_IF_EXISTS(pSettings, r_bool, section, "idle_light_shadow", true) );
		m_zone_flags.set(eIdleLightR1,READ_IF_EXISTS(pSettings, r_bool, section, "idle_light_r1", true) );
	}

	bool use = !!READ_IF_EXISTS(pSettings, r_bool, section, "use_secondary_hit", false);
	m_zone_flags.set(eUseSecondaryHit, use);
	if(use)
		m_fSecondaryHitPower	= pSettings->r_float(section,"secondary_hit_power");

	m_ef_anomaly_type			= pSettings->r_u32(section,"ef_anomaly_type");
	m_ef_weapon_type			= pSettings->r_u32(section,"ef_weapon_type");
	
	m_zone_flags.set			(eAffectPickDOF, READ_IF_EXISTS(pSettings, r_bool, section, "pick_dof_effector", false));

	if (TAnomalyMovement* AnomalyMovement = GetComponent<TAnomalyMovement>())
	{
		AnomalyMovement->Load(section);
	}

	if (TAnomalyElectricCurve* AnomalyElectricCurve = GetComponent<TAnomalyElectricCurve>())
	{
		AnomalyElectricCurve->Load(section);
	}
}

bool CAnomalyZone::net_Spawn(CSE_Abstract* DC) 
{
	if (!inherited::net_Spawn(DC))
		return					(false);

	CSE_Abstract				*e = (CSE_Abstract*)(DC);
	CSE_ALifeAnomalyZone			*Z = smart_cast<CSE_ALifeAnomalyZone*>(e);
	VERIFY						(Z);
	
	m_fMaxPower					= pSettings->r_float(cNameSect(),"max_start_power");
	m_fAttenuation				= pSettings->r_float(cNameSect(),"attenuation");
	m_owner_id					= Z->m_owner_id;
	if(m_owner_id != u32(-1))
		m_ttl					= Device.dwTimeGlobal + 40000;// 40 sec
	else
		m_ttl					= u32(-1);

	m_TimeToDisable				= Z->m_disabled_time*1000;
	m_TimeToEnable				= Z->m_enabled_time*1000;
	m_TimeShift					= Z->m_start_time_shift*1000;
	m_StartTime					= Device.dwTimeGlobal;
	m_zone_flags.set			(eUseOnOffTime,	(m_TimeToDisable!=0)&&(m_TimeToEnable!=0) );

	//добавить источники света
	bool br1 = (0==psDeviceFlags.test(rsR2|rsR4));
	
	
	bool render_ver_allowed = !br1 || (br1&&m_zone_flags.test(eIdleLightR1)) ;

	if ( m_zone_flags.test(eIdleLight) && render_ver_allowed)
	{
		m_pIdleLight = ::Render->light_create();
		m_pIdleLight->set_shadow(!!m_zone_flags.test(eIdleLightShadow));

		if(m_zone_flags.test(eIdleLightVolumetric))
		{
			m_pIdleLight->set_volumetric(true);	
			m_pIdleLight->set_volumetric_quality(m_fVolumetricQuality);
			m_pIdleLight->set_volumetric_distance(m_fVolumetricDistance);
			m_pIdleLight->set_volumetric_intensity(m_fVolumetricIntensity);
		}
	}
	else m_pIdleLight = nullptr;

	if (m_zone_flags.test(eBlowoutLight))
	{
		m_pLight = ::Render->light_create();
		m_pLight->set_shadow(true);

		if (m_bVolumetricBlowout)
		{
			m_pLight->set_volumetric(m_bVolumetricBlowout);
			m_pLight->set_volumetric_quality(m_fVolumetricQuality);
			m_pLight->set_volumetric_distance(m_fVolumetricDistance);
			m_pLight->set_volumetric_intensity(m_fVolumetricIntensity);
		}
	}
	else m_pLight = nullptr;
	
	if (IsEnabled())
	{
		PlayIdleParticles();
	}

	m_iPreviousStateTime		= m_iStateTime = 0;

	m_dwLastTimeMoved			= Device.dwTimeGlobal;
	m_vPrevPos.set				(Position());


	if(spawn_ini() && spawn_ini()->line_exist("fast_mode","always_fast"))
	{
		m_zone_flags.set(eAlwaysFastmode, spawn_ini()->r_bool("fast_mode","always_fast"));
	}
	SpatialComponent->spatial.type |= ESPATIAL_TYPE::ANOMALY_ZONE;
	SpatialComponent->spatial.type &= ~ESPATIAL_TYPE::SPACE_RESTRICTOR;
	if (Visual()) 
	{
		setEnabled(true);
	}

	if (TAnomalyElectricCurve* AnomalyElectricCurve = GetComponent<TAnomalyElectricCurve>())
	{
		AnomalyElectricCurve->SetInitialSpawnPosition(Z->o_Position);
		AnomalyElectricCurve->InitElectricCurves();
	}

	if (TAnomalyMovement* AnomalyMovement = GetComponent<TAnomalyMovement>())
	{
		AnomalyMovement->SetInitialSpawnPosition(Z->o_Position);
	}

	m_initial_spawn_position = Z->o_Position;

	return true;
}

void CAnomalyZone::net_Destroy() 
{
	StopIdleParticles		();

	inherited::net_Destroy	();

	StopWind				();

	m_pLight.destroy		();
	m_pIdleLight.destroy	();

	Particles::Details::Destroy(m_pIdleParticles);

	if(m_actor_effector)			
		m_actor_effector->Stop		(); 

	for(SZoneObjectInfo& info : m_ObjectInfoMap)
		exit_Zone(info);

	m_ObjectInfoMap.clear();	
}

void CAnomalyZone::net_Import(NET_Packet& P)
{
	inherited::net_Import(P);
}

void CAnomalyZone::net_Export(NET_Packet& P)
{
	inherited::net_Export(P);
}

bool CAnomalyZone::IdleState()
{
	UpdateOnOffState	();

	return false;
}

bool CAnomalyZone::AwakingState()
{
	if(m_iStateTime>=m_StateTime[eZoneStateAwaking])
	{
		SwitchZoneState(eZoneStateBlowout);
		return true;
	}
	return false;
}

bool CAnomalyZone::BlowoutState()
{
	if(m_iStateTime>=m_StateTime[eZoneStateBlowout])
	{
		SwitchZoneState(eZoneStateAccumulate);
		if (m_bBlowoutOnce){
			ZoneDisable();
		}
		
		return true;
	}
	return false;
}
bool CAnomalyZone::AccumulateState()
{
	if(m_iStateTime>=m_StateTime[eZoneStateAccumulate])
	{
		if(m_zone_flags.test(eZoneIsActive) )
			SwitchZoneState(eZoneStateBlowout);
		else
			SwitchZoneState(eZoneStateIdle);

		return true;
	}
	return false;
}
void CAnomalyZone::UpdateWorkload	(u32 dt)
{
	m_iPreviousStateTime	= m_iStateTime;
	m_iStateTime			+= (int)dt;

	if (!IsEnabled())		{
		if (m_actor_effector)
			m_actor_effector->Stop();
		return;
	};

	UpdateIdleLight			();

	switch(m_eZoneState)
	{
	case eZoneStateIdle:
		IdleState();
		break;
	case eZoneStateAwaking:
		AwakingState();
		break;
	case eZoneStateBlowout:
		BlowoutState();
		break;
	case eZoneStateAccumulate:
		AccumulateState();
		break;
	case eZoneStateDisabled:
		break;
	default: NODEFAULT;
	}

	if (Level().CurrentEntity()) 
	{
		Fvector P			= Device.vCameraPosition;
		P.y					-= 0.9f;
		float radius		= 1.0f;
		CalcDistanceTo		(P, m_fDistanceToCurEntity, radius);

		if (m_actor_effector)
		{
			m_actor_effector->Update(m_fDistanceToCurEntity, radius, m_eHitTypeBlowout);
		}
	}

	if(m_pLight && m_pLight->get_active())
		UpdateBlowoutLight	();

	if(m_zone_flags.test(eUseSecondaryHit) && m_eZoneState!=eZoneStateIdle && m_eZoneState!=eZoneStateDisabled)
	{
		UpdateSecondaryHit();
	}

	
}

// ГГ берет артефакт с земли где то в пределах 160 метров от артефакта до аномалии
void CAnomalyZone::OnActorTakeArtefact(float scan_radius, CArtefact* artefact, Fvector actorPos)
{
	if (TAnomalyMovement* AnomalyMovement = GetComponent<TAnomalyMovement>())
	{
		AnomalyMovement->OnActorTakeArtefact(scan_radius, artefact, actorPos);
	}

	if (TAnomalyElectricCurve* AnomalyElectricCurve = GetComponent<TAnomalyElectricCurve>())
	{
		AnomalyElectricCurve->OnActorTakeArtefact(scan_radius, artefact, actorPos);
	}
}

void CAnomalyZone::UpdateComponents(bool isUpdateCL)
{
	bool IsNeedUpdate = false;
	bool IsNeedScanObjects = false;
	float scan_radius = 0.0f;
	float barier_radius = 0.f;

	TAnomalyElectricCurve* AnomalyElectricCurve = GetComponent<TAnomalyElectricCurve>();
	TAnomalyMovement* AnomalyMovement = GetComponent<TAnomalyMovement>();

	if (AnomalyMovement != nullptr)
	{
		if (AnomalyMovement->IsEnabled())
		{
			IsNeedUpdate = true;
		}

		if (AnomalyMovement->IsNeedScanObjects())
		{
			IsNeedScanObjects = true;
		}

		if (AnomalyMovement->GetScanRadius() > scan_radius)
		{
			scan_radius = AnomalyMovement->GetScanRadius();
		}

		if (AnomalyMovement->GetBarierRadius() > barier_radius)
		{
			barier_radius = AnomalyMovement->GetBarierRadius();
		}
	}

	if (AnomalyElectricCurve != nullptr)
	{
		if (AnomalyElectricCurve->IsEnabled())
		{
			IsNeedUpdate = true;
		}

		if (AnomalyElectricCurve->IsNeedScanObjects())
		{
			IsNeedScanObjects = true;
		}

		if (AnomalyElectricCurve->GetScanRadius() > scan_radius)
		{
			scan_radius = AnomalyElectricCurve->GetScanRadius();
		}

		if (AnomalyElectricCurve->GetBarierRadius() > barier_radius)
		{
			barier_radius = AnomalyElectricCurve->GetBarierRadius();
		}
	}

	if (!IsNeedUpdate)
	{
		return;
	}

	CGameObject* m_best_magnetic_target = nullptr;
	if (IsNeedScanObjects && isUpdateCL && scan_radius > 0.f && barier_radius > 0.f)
	{
		m_best_magnetic_target = ScanObjects(scan_radius, XFORM().c, m_initial_spawn_position, barier_radius);
	}

	if (AnomalyMovement != nullptr)
	{
		AnomalyMovement->Update(m_best_magnetic_target, isUpdateCL);
	}

	if (AnomalyElectricCurve != nullptr)
	{
		AnomalyElectricCurve->Update(isUpdateCL);
	}
	
	if (AnomalyMovement != nullptr && AnomalyMovement->IsEnabled())
	{
		OnMove();
	}

	bool IsNeedActivate = false;
	if (AnomalyMovement != nullptr && AnomalyMovement->AlwaysTheCrow())
	{
		IsNeedActivate = true;
	}

	if (AnomalyElectricCurve != nullptr && AnomalyElectricCurve->AlwaysTheCrow())
	{
		IsNeedActivate = true;
	}

	if (!processing_enabled() && IsNeedActivate)
	{
		processing_activate();
	}
}

xr_vector<CGameObject*> CAnomalyZone::GetSortedByDistanceSpatialObjects(float distance, Fvector centerPos, u64 mask)
{
	static xr_vector<ISpatialShared> spatial;
	spatial.clear();
	spatial.reserve(64);
	g_SpatialSpace->q_sphere(spatial, 0, ESPATIAL_TYPE(mask), centerPos, distance);

	xr_vector<CGameObject*> objects;
	objects.reserve(spatial.size());

	for (auto& sp : spatial) {
		if (!sp)
		{
			continue;
		}

		CObject* obj = sp->dcast_CObject();
		if (obj && !obj->getDestroy() && obj->cast_game_object())
		{
			objects.push_back(obj->cast_game_object());
		}
	}

	if (objects.size() <= 1)
	{
		return objects;
	}


	std::sort(objects.begin(), objects.end(),
		[centerPos](CGameObject* a, CGameObject* b) {
			return a->Position().distance_to(centerPos) < b->Position().distance_to(centerPos);
		});

	for (int pass = 0; pass < 3; ++pass) {
		for (size_t i = 1; i < objects.size() - 1; ++i) {
			float distToPrev = objects[i]->Position().distance_to(objects[i - 1]->Position());
			float distToNext = objects[i]->Position().distance_to(objects[i + 1]->Position());
			float distBetween = objects[i - 1]->Position().distance_to(objects[i + 1]->Position());

			if (distBetween < distToPrev && distBetween < distToNext) {
				std::swap(objects[i], objects[i + 1]);
			}
		}
	}

	lastScannedObjects = objects;
	return objects;
}

CGameObject* CAnomalyZone::ScanObjects(float scanDistance, Fvector scanCenter, Fvector barierCenter, float barierRadius)
{
	u64 mask = (u64)ESPATIAL_TYPE::ACTOR_ALIVE;
	mask |= (u64)ESPATIAL_TYPE::AI_ALIVE;
	mask |= (u64)ESPATIAL_TYPE::AI_DEAD;
	mask |= (u64)ESPATIAL_TYPE::ITEM;

	xr_vector<CGameObject*> objects = GetSortedByDistanceSpatialObjects(scanDistance, scanCenter, mask);

	for (CGameObject* obj : objects)
	{
		if (obj == nullptr)
		{
			continue;
		}

		if (CEntityAlive* entity = obj->cast_entity_alive())
		{
			if (entity->g_Alive() && entity->Position().distance_to(barierCenter) <= barierRadius)
			{
				return obj;
			}
		}
	}

	return nullptr;
}

// called only in "fast-mode"
void CAnomalyZone::UpdateCL() 
{
	UpdateComponents(true);
	inherited::UpdateCL();

	if (m_zone_flags.test(eFastMode))
	{
		UpdateWorkload(Device.dwTimeDelta);
	}
}

// called as usual
void CAnomalyZone::shedule_Update(u32 dt)
{
	PROF_EVENT("CAnomalyZone::shedule_Update");
	m_zone_flags.set(eZoneIsActive, false);

	UpdateComponents(false);

	if (IsEnabled())
	{
		const Fsphere& s = CFORM()->getSphere();
		Fvector	P;
		XFORM().transform_tiny(P,s.P);

		// update
		feel_touch_update(P, s.R);

		//пройтись по всем объектам в зоне
		//и проверить их состояние
		for (SZoneObjectInfo& info : m_ObjectInfoMap)
		{
			CGameObject* pObject = info.object;
			if (!pObject)			
			{
				continue;
			}

			CEntityAlive* pEntityAlive = pObject->cast_entity_alive();

			info.dw_time_in_zone += dt;

			if((!info.small_object && m_iDisableHitTime != -1 && (int)info.dw_time_in_zone > m_iDisableHitTime) || (info.small_object && m_iDisableHitTimeSmall != -1 && (int)info.dw_time_in_zone > m_iDisableHitTimeSmall))
			{
				if(!pEntityAlive || !pEntityAlive->g_Alive())
				{
					info.zone_ignore = true;
				}
			}

			if(m_iDisableIdleTime != -1 && (int)info.dw_time_in_zone > m_iDisableIdleTime)
			{
				if(!pEntityAlive || !pEntityAlive->g_Alive())
				{
					StopObjectIdleParticles(pObject);
				}
			}

			//если есть хотя бы один не дисабленый объект, то
			//зона считается активной
			if(info.zone_ignore == false) 
			{
				m_zone_flags.set(eZoneIsActive,true);
			}
		}

		if(eZoneStateIdle == m_eZoneState)
		{
			CheckForAwaking();
		}

		inherited::shedule_Update(dt);

		// check "fast-mode" border
		float	cam_distance	= Device.vCameraPosition.distance_to(P)-s.R;
		
		if (cam_distance>FASTMODE_DISTANCE && !m_zone_flags.test(eAlwaysFastmode) )	
		{
			o_switch_2_slow();
		}
		else									
		{
			o_switch_2_fast();
		}

		if (!m_zone_flags.test(eFastMode))
		{
			UpdateWorkload(dt);
		}

	};

	UpdateOnOffState();

	if( !IsGameTypeSingle() && Local() )
	{
		if(Device.dwTimeGlobal > m_ttl)
		{
			DestroyObject();
		}
	}
}

void CAnomalyZone::CheckForAwaking()
{
	if(m_zone_flags.test(eZoneIsActive) && eZoneStateIdle ==  m_eZoneState)
	{
		SwitchZoneState(eZoneStateAwaking);
	}
}

void CAnomalyZone::feel_touch_new(CObject* O) 
{
	if (!O || O->getDestroy()) 
	{
		return;
	}

	CGameObject* pGameObject = O->cast_game_object();
	if (!pGameObject)
	{
		return;
	}

	CEntityAlive* pEntityAlive = pGameObject->cast_entity_alive();

	SZoneObjectInfo& object_info = m_ObjectInfoMap.emplace_back();
	object_info.object = pGameObject;

	if (pEntityAlive && pEntityAlive->g_Alive())
	{
		object_info.nonalive_object = false;
	}
	else
	{
		object_info.nonalive_object = true;
	}

	if (pGameObject->Radius() < SMALL_OBJECT_RADIUS)
	{
		object_info.small_object = true;
	}
	else
	{
		object_info.small_object = false;
	}

	if ((object_info.small_object && m_zone_flags.test(eIgnoreSmall)) || (object_info.nonalive_object && m_zone_flags.test(eIgnoreNonAlive)) || (pGameObject->cast_artefact() && m_zone_flags.test(eIgnoreArtefact)))
	{
		object_info.zone_ignore = true;
	}
	else
	{
		object_info.zone_ignore = false;
	}

	enter_Zone(object_info);

	if (IsEnabled())
	{
		PlayEntranceParticles(pGameObject);
		PlayObjectIdleParticles(pGameObject);
	}
};

void CAnomalyZone::feel_touch_delete(CObject* O) 
{
	if (!O || O->getDestroy()) return;
	CGameObject* pGameObject = O->cast_game_object();

	StopObjectIdleParticles(pGameObject);

	SZoneObjectInfo::remove(this, pGameObject);
}

bool CAnomalyZone::feel_touch_contact(CObject* O) 
{
	if (!O || O->getDestroy()) return false;
	CGameObject* pGameObject = O->cast_game_object();
	if (!pGameObject)							return false;
	if (pGameObject->cast_anomaly_zone())		return false;
	if (pGameObject->cast_breakable_object())	return false;
	if (0==PKinematics(O->Visual()))			return false;

	if (O->ID() == ID())
		return		(false);

    if (!pGameObject->IsVisibleForZones())
		return		(false);

	if (!((CCF_Shape*)CFORM())->Contact(O))
		return		(false);

	return			(pGameObject->feel_touch_on_contact(this));
}


float CAnomalyZone::RelativePower(float dist, float nearest_shape_radius)
{
	float radius = effective_radius(nearest_shape_radius);
	float power = (radius<dist) ? 0 : (1.f - m_fAttenuation*(dist/radius)*(dist/radius));
	return (power<0.0f) ? 0.0f : power;
}

float CAnomalyZone::effective_radius(float nearest_shape_radius)
{
	return /*Radius()*/nearest_shape_radius*m_fEffectiveRadius;
}

float CAnomalyZone::Power(float dist, float nearest_shape_radius) 
{
	return  m_fMaxPower * RelativePower(dist, nearest_shape_radius);
}

void CAnomalyZone::PlayIdleParticles(bool bIdleLight)
{
	if (!m_idle_sounds_variants.empty()) 
	{
		StopAllSounds(m_idle_sounds_variants);
		GetRandomSound(m_idle_sounds_variants).play_at_pos(0, Position(), sm_Looped);
	}

	if(*m_sIdleParticles)
	{
		if (!m_pIdleParticles)
		{
			m_pIdleParticles = Particles::Details::Create(m_sIdleParticles.c_str(),false);
			m_pIdleParticles->UpdateParent(XFORM(),zero_vel);
			m_pIdleParticles->Play(false);
		}
	}
	if(bIdleLight)
		StartIdleLight	();
}

void CAnomalyZone::StopIdleParticles(bool bIdleLight)
{
	StopAllSounds(m_idle_sounds_variants);

	if(m_pIdleParticles)
	{
		m_pIdleParticles->Stop(false);
		Particles::Details::Destroy(m_pIdleParticles);
	}

	if(bIdleLight)
		StopIdleLight();
}


void  CAnomalyZone::StartIdleLight	()
{
	if(m_pIdleLight)
	{
		m_pIdleLight->set_range(m_fIdleLightRange);
		Fvector pos = Position();
		pos.y += m_fIdleLightHeight;
		m_pIdleLight->set_position(pos);
		m_pIdleLight->set_active(true);
	}
}
void  CAnomalyZone::StopIdleLight	()
{
	if(m_pIdleLight)
		m_pIdleLight->set_active(false);
}

void CAnomalyZone::UpdateIdleLight	()
{
	if(!m_pIdleLight || !m_pIdleLight->get_active())
		return;

	if (m_pIdleLAnim == nullptr)
	{
		if (!isErrorAnimSend)
		{
			Msg("! Error Cant found light animation in %s", cNameSect_str());
			isErrorAnimSend = true;
		}

		return;
	}


	VERIFY(m_pIdleLAnim);

	int frame = 0;
	u32 clr					= m_pIdleLAnim->CalculateBGR(Device.fTimeGlobal,frame); // возвращает в формате BGR
	Fcolor					fclr;
	fclr.set				((float)color_get_B(clr)/255.f,(float)color_get_G(clr)/255.f,(float)color_get_R(clr)/255.f,1.f);
	
	float range = m_fIdleLightRange + 0.25f*::Random.randF(-1.f,1.f);
	m_pIdleLight->set_range	(range);
	m_pIdleLight->set_color	(fclr);

	Fvector pos		= Position();
	pos.y			+= m_fIdleLightHeight;
	m_pIdleLight->set_position(pos);
}


void CAnomalyZone::PlayBlowoutParticles()
{
	if(!m_sBlowoutParticles) return;

	CParticlesObject* pParticles;
	pParticles	= Particles::Details::Create(*m_sBlowoutParticles,true).get();
	pParticles->UpdateParent(XFORM(),zero_vel);
	pParticles->Play(false);
}

void CAnomalyZone::PlayHitParticles(CGameObject* pObject)
{
	if (!pObject || pObject->getDestroy())
	{
		return;
	}

	if (!m_hit_sounds_variants.empty())
	{
		GetRandomSound(m_hit_sounds_variants).play_at_pos(0, pObject->Position());
	}

	shared_str particle_str = nullptr;

	if(pObject->Radius()<SMALL_OBJECT_RADIUS)
	{
		if(!m_sHitParticlesSmall) return;
		particle_str = m_sHitParticlesSmall;
	}
	else
	{
		if(!m_sHitParticlesBig) return;
		particle_str = m_sHitParticlesBig;
	}

	if (particle_str.size())
	{
		TParticlesPlayer* PPlayer = pObject->GetOrCreateComponent<TParticlesPlayer>();

		u16 play_bone = PPlayer->GetRandomBone();
		if (play_bone != BI_NONE)
		{
			PPlayer->StartParticles(particle_str, play_bone, Fvector().set(0, 1, 0), ID());
		}
	}
}
#include "Bolt.h"
void CAnomalyZone::PlayEntranceParticles(CGameObject* pObject)
{
	if (!pObject || pObject->getDestroy()) 
	{
		return;
	}

	if (!m_entrance_sounds_variants.empty())
	{
		GetRandomSound(m_entrance_sounds_variants).play_at_pos(0, pObject->Position());
	}

	const char* particle_str = nullptr;

	if (pObject->Radius() < SMALL_OBJECT_RADIUS)
	{
		if (!m_sEntranceParticlesSmall)
			return;

		particle_str = m_sEntranceParticlesSmall.c_str();
	}
	else
	{
		if (!m_sEntranceParticlesBig)
			return;

		particle_str = m_sEntranceParticlesBig.c_str();
	}

	Fvector							vel;
	CPhysicsShellHolder* shell_holder = pObject->cast_physics_shell_holder();
	if (shell_holder)
		shell_holder->PHGetLinearVell(vel);
	else
		vel.set(0, 0, 0);

	//выбрать случайную косточку на объекте
	TParticlesPlayer* PPlayer = pObject->GetOrCreateComponent<TParticlesPlayer>();

	u16 play_bone = PPlayer->GetRandomBone();

	if (play_bone != BI_NONE)
	{
		CParticlesObject* pParticles = Particles::Details::Create(particle_str, true).get();
		Fmatrix xform;
		Fvector dir;
		
		if (fis_zero(vel.magnitude()))
		{
			dir.set(0, 1, 0);
		}
		else
		{
			dir.set(vel);
			dir.normalize();
		}

		PPlayer->MakeXFORM(pObject, play_bone, dir, Fvector().set(0, 0, 0), xform);
		pParticles->UpdateParent(xform, vel);
		pParticles->Play(false);
	}

	if (m_zone_flags.test(eBoltEntranceParticles) && pObject->cast_bolt())
	{
		PlayBoltEntranceParticles();
	}
}

u8 CAnomalyZone::PlayEntranceSmallParticles(const Fvector& pos, const Fvector& dir, const Fvector& vel, bool play_effect)
{
	if(m_zone_flags.test(eBulletEntranceAction))
	{
		if (play_effect)
		{
			const char* particles_str = m_sBulletEntranceParticles.size() ? m_sBulletEntranceParticles.c_str() : m_sEntranceParticlesSmall.size() ? m_sEntranceParticlesSmall.c_str() : nullptr;
			if(particles_str)
			{
				if (!m_entrance_sounds_variants.empty())
				{
					GetRandomSound(m_entrance_sounds_variants).play_at_pos(0, pos);
				}


				CParticlesObject* pParticles = Particles::Details::Create(particles_str, true).get();
				Fmatrix xform;
				Fvector::generate_orthonormal_basis(dir, xform.j, xform.i);
				xform.c.set(pos);
				pParticles->UpdateParent(xform, vel);
				pParticles->Play(false);
				
			}
		}
		if (m_zone_flags.test(eBulletRicochet))
			return u8(2);//рикошет по нормали сферы

		if (m_zone_flags.test(eBulletRandom))
			return u8(3);//рикошет в рандомную сторону

		if (m_zone_flags.test(eBulletFliesThrough))
			return u8(4);//пролет пули через зону с эффектами столкновения на кромках

		return u8(1);//эффект и удаление пули при первом контакте с зоной
	}

	return u8(0);
}

void CAnomalyZone::PlayBoltEntranceParticles()
{

	CCF_Shape* Sh		= (CCF_Shape*)CFORM();
	const Fmatrix& XF	= XFORM();
	Fmatrix				PXF;
	xr_vector<CCF_Shape::shape_def>& Shapes = Sh->Shapes();
	Fvector				sP0, sP1, vel;

	CParticlesObject* pParticles = nullptr;

	for (auto& s : Shapes)
	{
		switch (s.type)
		{
		case 0: // sphere
			{
			sP0						= s.data.sphere.P;
			XF.transform_tiny		(sP0);

			
			
			float ki				= 10.0f * s.data.sphere.R;
			float c					= 2.0f * s.data.sphere.R;

			float quant_h			= (PI_MUL_2/float(ki))*c;
			float quant_p			= (PI_DIV_2/float(ki));

			for(float i=0; i<ki; ++i)
			{
				vel.setHP				(	::Random.randF(quant_h/2.0f, quant_h)*i, 
											::Random.randF(quant_p/2.0f, quant_p)*i
										);

				vel.mul					(s.data.sphere.R);

				sP1.add					(sP0, vel);

				PXF.identity			();
				PXF.k.normalize			(vel);
				Fvector::generate_orthonormal_basis(PXF.k, PXF.j, PXF.i);

				PXF.c					= sP1;

				pParticles				= Particles::Details::Create(m_sBoltEntranceParticles.c_str(), true).get();
				pParticles->UpdateParent(PXF,vel);
				pParticles->Play		(false);
			}
			}break;
		case 1: // box
			break;
		}
	}

}

void CAnomalyZone::PlayBulletParticles(Fvector& pos)
{
	if (!m_entrance_sounds_variants.empty())
	{
		GetRandomSound(m_entrance_sounds_variants).play_at_pos(0, pos);
	}

	if(!m_sEntranceParticlesSmall) return;
	
	CParticlesObject* pParticles;
	pParticles = Particles::Details::Create(*m_sEntranceParticlesSmall,true).get();
	
	Fmatrix M;
	M = XFORM();
	M.c.set(pos);

	pParticles->UpdateParent(M,zero_vel);
	pParticles->Play(false);
}

void CAnomalyZone::PlayObjectIdleParticles(CGameObject* pObject)
{
	if (!pObject || pObject->getDestroy())
	{
		return;
	}

	TParticlesPlayer* PPlayer = pObject->GetOrCreateComponent<TParticlesPlayer>();
	shared_str particle_str = nullptr;

	//разные партиклы для объектов разного размера
	if (pObject->Radius() < SMALL_OBJECT_RADIUS)
	{
		if (!m_sIdleObjectParticlesSmall) return;
		particle_str = m_sIdleObjectParticlesSmall;
	}
	else
	{
		if (!m_sIdleObjectParticlesBig) return;
		particle_str = m_sIdleObjectParticlesBig;
	}

	//запустить партиклы на объекте
	PPlayer->StopParticles(particle_str, BI_NONE, true);

	PPlayer->StartParticles(particle_str, Fvector().set(0, 1, 0), ID());
	if (!IsEnabled())
	{
		PPlayer->StopParticles(particle_str, BI_NONE, true);
	}
}

void CAnomalyZone::StopObjectIdleParticles(CGameObject* pObject)
{
	if (!pObject || pObject->getDestroy()) return;
	if (m_zone_flags.test(eIdleObjectParticlesDontStop) && !pObject->cast_actor())
		return;

	TParticlesPlayer* PPlayer = pObject->GetOrCreateComponent<TParticlesPlayer>();

	if (!SZoneObjectInfo::get(this, pObject)) return;

	shared_str particle_str = nullptr;
	//разные партиклы для объектов разного размера
	if (pObject->Radius() < SMALL_OBJECT_RADIUS)
	{
		if (!m_sIdleObjectParticlesSmall) return;
		particle_str = m_sIdleObjectParticlesSmall;
	}
	else
	{
		if (!m_sIdleObjectParticlesBig) return;
		particle_str = m_sIdleObjectParticlesBig;
	}

	PPlayer->StopParticles(particle_str, BI_NONE, true);
}

void	CAnomalyZone::Hit					(SHit* pHDS)
{
	Fmatrix M;
	M.identity();
	M.translate_over	(pHDS->p_in_bone_space);
	M.mulA_43			(XFORM());
	PlayBulletParticles	(M.c);	
}

void CAnomalyZone::StartBlowoutLight		()
{
	if(!m_pLight || m_fLightTime<=0.f) return;
	
	m_fLightTimeLeft = (float)Device.dwTimeGlobal + m_fLightTime*1000.0f;

	m_pLight->set_color(m_LightColor.r, m_LightColor.g, m_LightColor.b);
	m_pLight->set_range(m_fLightRange);
	
	Fvector pos = Position();
	pos.y		+= m_fLightHeight;
	m_pLight->set_position(pos);
	m_pLight->set_active(true);

}

void  CAnomalyZone::StopBlowoutLight		()
{
	m_fLightTimeLeft = 0.f;
	m_pLight->set_active(false);
}

void CAnomalyZone::UpdateBlowoutLight	()
{
	if(m_fLightTimeLeft > (float)Device.dwTimeGlobal)
	{
		float time_k	= m_fLightTimeLeft - (float)Device.dwTimeGlobal;

//		m_fLightTimeLeft -= Device.fTimeDelta;
		clamp(time_k, 0.0f, m_fLightTime*1000.0f);

		float scale		= time_k/(m_fLightTime*1000.0f);
		scale			= powf(scale+EPS_L, 0.15f);
		float r			= m_fLightRange*scale;
		VERIFY(_valid(r));
		m_pLight->set_color(m_LightColor.r*scale, 
							m_LightColor.g*scale, 
							m_LightColor.b*scale);
		m_pLight->set_range(r);

		Fvector pos			= Position();
		pos.y				+= m_fLightHeight;
		m_pLight->set_position(pos);
	}
	else
	{
		StopBlowoutLight();
	}
}

void CAnomalyZone::AffectObjects()
{
	if (m_dwAffectFrameNum == Device.dwFrame)
	{
		return;
	}

	m_dwAffectFrameNum = Device.dwFrame;

	if (Device.dwPrecacheFrame)
	{
		return;
	}

	TAnomalyMovement* AnomalyMovement = GetComponent<TAnomalyMovement>();
	TAnomalyElectricCurve * AnomalyElectricCurve = GetComponent<TAnomalyElectricCurve>();
	
	for (SZoneObjectInfo& info : m_ObjectInfoMap)
	{
		if (!info.object->getDestroy())
		{
			if (AnomalyMovement != nullptr)
			{
				AnomalyMovement->AffectBlast(info.object);
			}

			if (AnomalyElectricCurve != nullptr)
			{
				AnomalyElectricCurve->AffectBlast(info.object);
			}

			Affect(&info);
		}
	}
}

void CAnomalyZone::UpdateBlowout()
{
	if(m_dwBlowoutParticlesTime>=(u32)m_iPreviousStateTime &&  m_dwBlowoutParticlesTime<(u32)m_iStateTime)
	{
		PlayBlowoutParticles();
	}

	if(m_dwBlowoutLightTime>=(u32)m_iPreviousStateTime && m_dwBlowoutLightTime<(u32)m_iStateTime)
	{
		StartBlowoutLight();
	}

	if (m_dwBlowoutSoundTime >= (u32)m_iPreviousStateTime && m_dwBlowoutSoundTime < (u32)m_iStateTime)
	{
		if (!m_blowout_sounds_variants.empty())
		{
			GetRandomSound(m_blowout_sounds_variants).play_at_pos(0, Position());
		}
	}

	if(m_zone_flags.test(eBlowoutWind) && m_dwBlowoutWindTimeStart>=(u32)m_iPreviousStateTime && m_dwBlowoutWindTimeStart<(u32)m_iStateTime)
	{
		StartWind();
	}

	UpdateWind();

	if(m_dwBlowoutExplosionTime>=(u32)m_iPreviousStateTime && m_dwBlowoutExplosionTime<(u32)m_iStateTime)
	{
		AffectObjects();
	}
}

void  CAnomalyZone::OnMove()
{
	if(m_dwLastTimeMoved == 0)
	{
		m_dwLastTimeMoved = Device.dwTimeGlobal;
		m_vPrevPos.set(Position());
	}
	else
	{
		float time_delta	= float(Device.dwTimeGlobal - m_dwLastTimeMoved)/1000.f;
		m_dwLastTimeMoved	= Device.dwTimeGlobal;

		Fvector				vel;
			
		if(fis_zero(time_delta))
			vel = zero_vel;
		else
		{
			vel.sub(Position(), m_vPrevPos);
			vel.div(time_delta);
		}

		if (m_pIdleParticles)
			m_pIdleParticles->UpdateParent(XFORM(), vel);

		if(m_pLight && m_pLight->get_active())
			m_pLight->set_position(Position());

		if(m_pIdleLight && m_pIdleLight->get_active())
			m_pIdleLight->set_position(Position());
     }
}

void	CAnomalyZone::OnEvent (NET_Packet& P, u16 type)
{	
	switch (type)
	{
		case GE_ZONE_STATE_CHANGE:
			{
				u8				S;
				P.r_u8			(S);
				OnStateSwitch	(EZoneState(S));
				break;
			}
	}
	inherited::OnEvent(P, type);
};

void CAnomalyZone::OnStateSwitch	(EZoneState new_state)
{
	if (new_state==eZoneStateDisabled)
		Disable();
	else
		Enable();

	if(new_state==eZoneStateAccumulate)
		PlayAccumParticles();

	if(new_state==eZoneStateAwaking)
		PlayAwakingParticles();

	m_eZoneState			= new_state;
	m_iPreviousStateTime	= m_iStateTime = 0;
};

void CAnomalyZone::SwitchZoneState(EZoneState new_state)
{
	if (OnServer())
	{
		// !!! Just single entry for given state !!!
		NET_Packet		P;
		u_EventGen		(P,GE_ZONE_STATE_CHANGE,ID());
		P.w_u8			(u8(new_state));
		u_EventSend		(P);
	};

	m_iPreviousStateTime = m_iStateTime = 0;
}

bool CAnomalyZone::Enable()
{
	if (IsEnabled()) return false;

	o_switch_2_fast();

	for (SZoneObjectInfo& info : m_ObjectInfoMap)
	{
		CGameObject* pObject = info.object;
		if (!pObject) continue;
		PlayEntranceParticles(pObject);
		PlayObjectIdleParticles(pObject);
	}
	PlayIdleParticles	();
	return				true;
};

bool CAnomalyZone::Disable()
{
	if (!IsEnabled()) return false;
	o_switch_2_slow();

	for (SZoneObjectInfo& info : m_ObjectInfoMap)
	{
		CGameObject* pObject = info.object;
		if (!pObject) 
			continue;

		StopObjectIdleParticles(pObject);
	}
	StopIdleParticles	();
	if (m_actor_effector)
		m_actor_effector->Stop();

	return false;
};

void CAnomalyZone::ZoneEnable()
{
	SwitchZoneState(eZoneStateIdle);
};

void CAnomalyZone::ZoneDisable()
{
	SwitchZoneState(eZoneStateDisabled);
};

void CAnomalyZone::StartWind()
{
	if(m_fDistanceToCurEntity>WIND_RADIUS) return;

	m_zone_flags.set(eBlowoutWindActive, true);

	m_fStoreWindPower = g_pGamePersistent->Environment().wind_strength_factor;
	clamp(g_pGamePersistent->Environment().wind_strength_factor, 0.f, 1.f);
}

void CAnomalyZone::StopWind()
{
	if(!m_zone_flags.test(eBlowoutWindActive)) return;
	m_zone_flags.set(eBlowoutWindActive, false);
	g_pGamePersistent->Environment().wind_strength_factor = m_fStoreWindPower;
}

void CAnomalyZone::UpdateWind()
{
	if(!m_zone_flags.test(eBlowoutWindActive)) return;

	if(m_fDistanceToCurEntity>WIND_RADIUS || m_dwBlowoutWindTimeEnd<(u32)m_iStateTime)
	{
		StopWind();
		return;
	}

	if(m_dwBlowoutWindTimePeak > (u32)m_iStateTime)
	{
		g_pGamePersistent->Environment().wind_strength_factor = m_fBlowoutWindPowerMax + ( m_fStoreWindPower - m_fBlowoutWindPowerMax)*
								float(m_dwBlowoutWindTimePeak - (u32)m_iStateTime)/
								float(m_dwBlowoutWindTimePeak - m_dwBlowoutWindTimeStart);
		clamp(g_pGamePersistent->Environment().wind_strength_factor, 0.f, 1.f);
	}
	else
	{
		g_pGamePersistent->Environment().wind_strength_factor = m_fBlowoutWindPowerMax + (m_fStoreWindPower - m_fBlowoutWindPowerMax)*
			float((u32)m_iStateTime - m_dwBlowoutWindTimePeak)/
			float(m_dwBlowoutWindTimeEnd - m_dwBlowoutWindTimePeak);
		clamp(g_pGamePersistent->Environment().wind_strength_factor, 0.f, 1.f);
	}
}

u32	CAnomalyZone::ef_anomaly_type() const
{
	return	(m_ef_anomaly_type);
}

u32	CAnomalyZone::ef_weapon_type() const
{
	VERIFY	(m_ef_weapon_type != u32(-1));
	return	(m_ef_weapon_type);
}

void CAnomalyZone::CreateHit	(	u16 id_to, 
								u16 id_from, 
								const Fvector& hit_dir, 
								float hit_power, 
								s16 bone_id, 
								const Fvector& pos_in_bone, 
								float hit_impulse, 
								ALife::EHitType hit_type)
{
	if (OnServer())
	{
		if(m_owner_id != u32(-1) )
			id_from	= (u16)m_owner_id;

		NET_Packet			l_P;
		Fvector hdir		= hit_dir;
		SHit Hit			= SHit(hit_power, hdir, this, bone_id, pos_in_bone, hit_impulse, hit_type, 0.0f, false);		
		Hit.GenHeader		(GE_HIT, id_to);
		Hit.whoID			= id_from;
		Hit.weaponID		= this->ID();
		Hit.Write_Packet	(l_P);
		u_EventSend			(l_P);
	};
}

void CAnomalyZone::net_Relcase(CObject* O)
{
	if(O && O->cast_game_object())
		SZoneObjectInfo::remove(this, O->cast_game_object());

	if(O->ID()==m_owner_id)	m_owner_id = u32(-1);

	if(m_actor_effector && m_actor_effector->m_pActor && m_actor_effector->m_pActor->ID() == O->ID())
		m_actor_effector->Stop();

	inherited::net_Relcase(O);
}

void CAnomalyZone::enter_Zone(SZoneObjectInfo& io)
{
	if(m_zone_flags.test(eAffectPickDOF) && Level().CurrentEntity())
	{
		if(io.object->ID()==Level().CurrentEntity()->ID())
			GamePersistent().SetPickableEffectorDOF(true);
	}
}

void CAnomalyZone::exit_Zone	(SZoneObjectInfo& io)
{
	StopObjectIdleParticles(io.object);

	if(m_zone_flags.test(eAffectPickDOF) && Level().CurrentEntity())
	{
		if(io.object->ID()==Level().CurrentEntity()->ID())
			GamePersistent().SetPickableEffectorDOF(false);
	}
}

void CAnomalyZone::PlayAccumParticles()
{
	if(m_sAccumParticles.size())
	{
		CParticlesObject* pParticles;
		pParticles	= Particles::Details::Create(*m_sAccumParticles,true).get();
		pParticles->UpdateParent(XFORM(),zero_vel);
		pParticles->Play(false);
	}
	
	if (!m_accum_sounds_variants.empty())
	{
		GetRandomSound(m_accum_sounds_variants).play_at_pos(0, Position());
	}
}

void CAnomalyZone::PlayAwakingParticles()
{
	if(m_sAwakingParticles.size())
	{
		CParticlesObject* pParticles;
		pParticles	= Particles::Details::Create(*m_sAwakingParticles,true).get();
		pParticles->UpdateParent(XFORM(),zero_vel);
		pParticles->Play(false);
	}

	if (!m_awaking_sounds_variants.empty())
	{
		GetRandomSound(m_awaking_sounds_variants).play_at_pos(0, Position());
	}
}

void CAnomalyZone::UpdateOnOffState()
{
	if(!m_zone_flags.test(eUseOnOffTime)) return;
	
	bool dest_state;
	u32 t = (Device.dwTimeGlobal-m_StartTime+m_TimeShift) % (m_TimeToEnable+m_TimeToDisable);
	if	(t < m_TimeToEnable)
	{
		dest_state=true;
	}else
	if(t >=(m_TimeToEnable+m_TimeToDisable) ) 
	{
		dest_state=true;
	}else
	{
		dest_state=false;
		VERIFY(t<(m_TimeToEnable+m_TimeToDisable));
	}

	if( (eZoneStateDisabled==m_eZoneState) && dest_state)
	{
		GoEnabledState		();
	}else
	if( (eZoneStateIdle==m_eZoneState) && !dest_state)
	{
		GoDisabledState			();
	}
}

void CAnomalyZone::GoDisabledState()
{
	//switch to disable	
	NET_Packet P;
	u_EventGen		(P,GE_ZONE_STATE_CHANGE,ID());
	P.w_u8			(u8(eZoneStateDisabled));
	u_EventSend		(P);

	for (SZoneObjectInfo& info : m_ObjectInfoMap)
		exit_Zone(info);
	
	m_ObjectInfoMap.clear		();
	feel_touch.clear			();
}

void CAnomalyZone::GoEnabledState()
{
		//switch to idle	
		NET_Packet P;
		u_EventGen		(P,GE_ZONE_STATE_CHANGE,ID());
		P.w_u8			(u8(eZoneStateIdle));
		u_EventSend		(P);
}

bool CAnomalyZone::feel_touch_on_contact	(CObject *O)
{
	if ((SpatialComponent->spatial.type & ESPATIAL_TYPE::VISIBLEFORAI) == ESPATIAL_TYPE::NONE)
		return			(false);

	return				(inherited::feel_touch_on_contact(O));
}

bool CAnomalyZone::AlwaysTheCrow()
{
	TAnomalyMovement* AnomalyMovement = GetComponent<TAnomalyMovement>();
	TAnomalyElectricCurve* AnomalyElectricCurve = GetComponent<TAnomalyElectricCurve>();

	if (AnomalyMovement != nullptr && AnomalyMovement->AlwaysTheCrow())
	{
		return true;
	}

	if (AnomalyElectricCurve != nullptr && AnomalyElectricCurve->AlwaysTheCrow())
	{
		return true;
	}

	bool b_idle = ZoneState()==eZoneStateIdle || ZoneState()==eZoneStateDisabled;
 	if(!b_idle || (m_zone_flags.test(eAlwaysFastmode) && IsEnabled()) )
		return true;
 	else
 		return inherited::AlwaysTheCrow();
}

void CAnomalyZone::CalcDistanceTo(const Fvector& P, float& dist, float& radius)
{
	R_ASSERT			(CFORM()->Type()==cftShape);
	CCF_Shape* Sh		= (CCF_Shape*)CFORM();

	dist				= P.distance_to(Position());
	float sr			= CFORM()->getSphere().R;
	//quick test
	if(Sh->Shapes().size()==1)
	{
		radius			= sr;
		return;
	}
/*
	//2nd quick test
	Fvector				SC;
	float				dist2;
	XF.transform_tiny	(SC,CFORM()->getSphere().P);
	dist2				= P.distance_to(SC);
	if(dist2>sr)
	{
		radius		= sr;
		return;
	}
*/
	//full test
	const Fmatrix& XF	= XFORM();
	xr_vector<CCF_Shape::shape_def>& Shapes = Sh->Shapes();
	CCF_Shape::shape_def* nearest_s = nullptr;
	float nearest = flt_max;


	Fvector sP;

	xr_vector<CCF_Shape::shape_def>::iterator it = Shapes.begin();
	xr_vector<CCF_Shape::shape_def>::iterator it_e = Shapes.end();
	for(;it!=it_e;++it)
	{
		CCF_Shape::shape_def& s = *it;
		float d = 0.0f;
		switch (s.type)
		{
		case 0: // sphere
			sP = s.data.sphere.P;
			break;
		case 1: // box
			sP = s.data.box.c;
			break;
		}

		XF.transform_tiny(sP);
		d = P.distance_to(sP);
		if(d<nearest)
		{
			nearest		= d;
			nearest_s	= &s;
		}
	}
	R_ASSERT(nearest_s);
	
	dist	= nearest;

	if(nearest_s->type==0)
		radius	= nearest_s->data.sphere.R;
	else
	{
		float r1 = nearest_s->data.box.i.magnitude();
		float r2 = nearest_s->data.box.j.magnitude();
		float r3 = nearest_s->data.box.k.magnitude();
		radius = std::max(r1,r2);
		radius = std::max(radius,r3);
	}

}

// Lain: added Start/Stop idle light calls
void CAnomalyZone::o_switch_2_fast				()
{
	if (m_zone_flags.test(eFastMode))		return	;
	m_zone_flags.set(eFastMode, true);
	StartIdleLight();
	processing_activate			();
}

void CAnomalyZone::o_switch_2_slow				()
{
	if (!m_zone_flags.test(eFastMode))	return	;
	m_zone_flags.set(eFastMode, false);
	if ( !light_in_slow_mode() )
	{
		StopIdleLight();
	}
	processing_deactivate		();
}

void CAnomalyZone::save							(NET_Packet &output_packet)
{
	inherited::save			(output_packet);
	output_packet.w_u8		(static_cast<u8>(m_eZoneState));
}

void CAnomalyZone::load							(IReader &input_packet)
{
	inherited::load			(input_packet);	

	CAnomalyZone::EZoneState temp = static_cast<CAnomalyZone::EZoneState>(input_packet.r_u8());

	if (temp == eZoneStateDisabled)
		m_eZoneState = eZoneStateDisabled;
	else
		m_eZoneState = eZoneStateIdle;
}