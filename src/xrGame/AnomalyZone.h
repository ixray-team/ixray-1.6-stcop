#pragma once

#include "space_restrictor.h"
#include "../xrEngine/Feel_Touch.h"
#include "../xrScripts/script_export_space.h"
#include "ElectricCurve.h"
#include "RandomSoundEmmiter.h"

class CActor;
class CLAItem;
class CParticlesObject;
class CZoneEffector;
struct SZoneObjectInfo;
#define SMALL_OBJECT_RADIUS 0.6f

class CAnomalyZone :		public CSpaceRestrictor,
						public Feel::Touch
{
private:
    typedef	CSpaceRestrictor inherited;
	bool isErrorAnimSend = false;

public:
	CZoneEffector*		m_actor_effector;

protected:
	bool m_bVolumetricBlowout    = true;
	float m_fVolumetricQuality   = 0.f;
	float m_fVolumetricDistance  = 0.f;
	float m_fVolumetricIntensity = 0.f;

public:

						CAnomalyZone						();
	virtual				~CAnomalyZone					();

	virtual		bool	net_Spawn						(CSE_Abstract* DC);
	virtual		void	net_Import						(NET_Packet& P);
	virtual		void	net_Export						(NET_Packet& P);
	virtual		void	Load							(const char* section);
	virtual		void	net_Destroy						();

	virtual		void	save							(NET_Packet &output_packet);
	virtual		void	load							(IReader &input_packet);
	
	virtual		void	UpdateCL						();
	virtual		void	UpdateWorkload					(u32 dt);
	virtual		void	shedule_Update					(u32 dt);
	virtual		void	enter_Zone						(SZoneObjectInfo& io);
	virtual		void	exit_Zone						(SZoneObjectInfo& io);
	virtual		void	feel_touch_new					(CObject* O);
	virtual		void	feel_touch_delete				(CObject* O);
	virtual		bool	feel_touch_contact				(CObject* O);
	virtual		bool	feel_touch_on_contact			(CObject* O);
				
				float	effective_radius				(float nearest_shape_radius);
	virtual		void	net_Relcase						(CObject* O);
	virtual		void	OnEvent							(NET_Packet& P, u16 type);

				float	GetMaxPower						()							{return m_fMaxPower;}
				void	SetMaxPower						(float p)					{m_fMaxPower = p;}
	//вычисление силы хита в зависимости от расстояния до центра зоны
	//относительный размер силы (от 0 до 1)
				float	RelativePower					(float dist, float nearest_shape_radius);
	//абсолютный размер
				float	Power							(float dist, float nearest_shape_radius);

	xr_vector<SZoneObjectInfo>& GetObjectInfoMap() { return m_ObjectInfoMap; }

	virtual CAnomalyZone* cast_anomaly_zone() { return this; }

	//различные состояния в которых может находиться зона
	typedef enum {
		eZoneStateIdle = 0,		//состояние зоны, когда внутри нее нет активных объектов
		eZoneStateAwaking,		//пробуждение зоны (объект попал в зону)
		eZoneStateBlowout,		//выброс
        eZoneStateAccumulate,	//накапливание энергии, после выброса
		eZoneStateDisabled,
		eZoneStateMax
	} EZoneState;

	IC ALife::EHitType		GetHitType()	{	return m_eHitTypeBlowout; }
	void UpdateSoundsPosition(xr_vector<ref_sound>& soundsArray, const Fvector& pos);
private:
	void DestroySoundsArray(xr_vector<ref_sound>& soundsArray);
	void ParseRandomSounds(const char* section, const char* soundPrefix, xr_vector<ref_sound>& soundsArray);
	ICF ref_sound& GetRandomSound(xr_vector<ref_sound>& soundsArray) { return soundsArray[::Random.randI(soundsArray.size())]; }

	void StopAllSounds(xr_vector<ref_sound>& soundsArray);

protected:
	enum EZoneFlags{
		eIgnoreNonAlive			=(1<<0),
		eIgnoreSmall			=(1<<1),
		eIgnoreArtefact			=(1<<2),
		eZoneIsActive			=(1<<3),
		eBlowoutWind			=(1<<4),
		eBlowoutLight			=(1<<5),
		eIdleLight				=(1<<6),
		eBlowoutWindActive		=(1<<7),
		eUseOnOffTime			=(1<<8),
		eIdleLightVolumetric	=(1<<9),
		eIdleLightShadow		=(1<<10),
		eAlwaysFastmode			=(1<<11),
		eFastMode				=(1<<12),
		eIdleObjectParticlesDontStop=(1<<13),
		eAffectPickDOF			=(1<<14),
		eIdleLightR1			=(1<<15),
		eBoltEntranceParticles	=(1<<16),
		eUseSecondaryHit		=(1<<17),
		eBulletEntranceAction	=(1<<18),
		eBulletFliesThrough		=(1<<19),
		eBulletRicochet			=(1<<20),
		eBulletRandom			=(1<<21),
	};
	u32					m_owner_id;
	u32					m_ttl;
	Flags32				m_zone_flags;

	//максимальная сила заряда зоны
	float				m_fMaxPower;
	//сила постоянного небольшого демеджа (для огненных и ядовитых мин)
	float				m_fSecondaryHitPower;

	//линейный коэффициент затухания в зависимости от расстояния
	float				m_fAttenuation;
	//процент удара зоны, который пойдет на физический импульс	
	float				m_fHitImpulseScale;
	//размер радиуса в процентах от оригинального, 
	//где действует зона
	float				m_fEffectiveRadius;

	//тип наносимого хита
	ALife::EHitType		m_eHitTypeBlowout;
	EZoneState			m_eZoneState;

	//текущее время пребывания зоны в определенном состоянии 
	int					m_iStateTime;
	int					m_iPreviousStateTime;
	
	u32					m_TimeToDisable;
	u32					m_TimeToEnable;
	u32					m_TimeShift;
	u32					m_StartTime;

	//массив с временами, сколько каждое состояние должно 
	//длиться (если 0, то мгновенно -1 - бесконечность, 
	//-2 - вообще не должно вызываться)
	typedef	svector<int, eZoneStateMax>					StateTimeSVec;
	StateTimeSVec		m_StateTime;

	virtual		void		SwitchZoneState				(EZoneState new_state);
	virtual		void		OnStateSwitch				(EZoneState new_state);
	virtual		void		CheckForAwaking				();
	//обработка зоны в различных состояниях
	virtual		bool		IdleState					();
	virtual		bool		AwakingState				();
	virtual		bool		BlowoutState				();
	virtual		bool		AccumulateState				();

	virtual		void		UpdateSecondaryHit			() {};

	virtual		bool		Enable						();
	virtual		bool		Disable						();
				void		UpdateOnOffState			();
	virtual		void		GoEnabledState				();
	virtual		void		GoDisabledState				();
public:
				bool		IsEnabled					()	{return m_eZoneState != eZoneStateDisabled; };
				void		ZoneEnable					();	
				void		ZoneDisable					();
	EZoneState				ZoneState					() {return m_eZoneState;}
protected:


	//воздействие зоной на объект
	virtual		void		Affect						(SZoneObjectInfo* O)  {}

	//воздействовать на все объекты в зоне
	void					AffectObjects				();

	u32						m_dwAffectFrameNum;	

	//параметры для выброса, с какой задержкой 
	//включать эффекты и логику
	u32						m_dwBlowoutParticlesTime;
	u32						m_dwBlowoutLightTime;
	u32						m_dwBlowoutSoundTime;
	u32						m_dwBlowoutExplosionTime;
	void					UpdateBlowout				();
	
	//ветер
	u32						m_dwBlowoutWindTimeStart;
	u32						m_dwBlowoutWindTimePeak;
	u32						m_dwBlowoutWindTimeEnd;
	//сила ветра (увеличение текущего) (0,1) когда в аномалию попадает актер
	float					m_fBlowoutWindPowerMax;
	float					m_fStoreWindPower;
				
	void					StartWind					();
	void					StopWind					();
	void					UpdateWind					();


	//время, через которое, зона перестает реагировать 
	//на объект мертвый объект (-1 если не указано)
	int						m_iDisableHitTime;
	//тоже самое но для маленьких объектов
	int						m_iDisableHitTimeSmall;
	int						m_iDisableIdleTime;

	////////////////////////////////
	// имена партиклов зоны
	//обычное состояние зоны
	shared_str				m_sIdleParticles;
	//выброс зоны
	shared_str				m_sBlowoutParticles;
	bool					m_bBlowoutOnce;
	shared_str				m_sAccumParticles;
	shared_str				m_sAwakingParticles;


	//появление большого и мальнекого объекта в зоне
	shared_str				m_sEntranceParticlesSmall;
	shared_str				m_sEntranceParticlesBig;
	//поражение большого и мальнекого объекта в зоне
	shared_str				m_sHitParticlesSmall;
	shared_str				m_sHitParticlesBig;
	//нахождение большого и мальнекого объекта в зоне
	shared_str				m_sIdleObjectParticlesSmall;
	shared_str				m_sIdleObjectParticlesBig;
	shared_str				m_sBoltEntranceParticles;
	shared_str				m_sBulletEntranceParticles;

	xr_vector<ref_sound>	m_idle_sounds_variants;
	xr_vector<ref_sound>	m_awaking_sounds_variants;
	xr_vector<ref_sound>	m_accum_sounds_variants;
	xr_vector<ref_sound>	m_blowout_sounds_variants;
	xr_vector<ref_sound>	m_hit_sounds_variants;
	xr_vector<ref_sound>	m_entrance_sounds_variants;


	//объект партиклов обычного состояния зоны
	xr_shared_ptr<CParticlesObject>		m_pIdleParticles;

	//////////////////////////////
	//подсветка аномалии

	//подсветка idle состояния
	ref_light				m_pIdleLight;
	Fcolor					m_IdleLightColor;
	float					m_fIdleLightRange;
	float					m_fIdleLightHeight;
	CLAItem*				m_pIdleLAnim;

	void					StartIdleLight				();
	void					StopIdleLight				();
	void					UpdateIdleLight				();


	//подсветка выброса
	ref_light				m_pLight;
	float					m_fLightRange;
	Fcolor					m_LightColor;
	float					m_fLightTime;
	float					m_fLightTimeLeft;
	float					m_fLightHeight;

	void					StartBlowoutLight			();
	void					StopBlowoutLight			();
	void					UpdateBlowoutLight			();

	//список партиклов для объетов внутри зоны
	xr_vector<SZoneObjectInfo> m_ObjectInfoMap;

	void					CreateHit					(	u16 id_to, 
															u16 id_from, 
															const Fvector& hit_dir, 
															float hit_power, 
															s16 bone_id, 
															const Fvector& pos_in_bone, 
															float hit_impulse, 
															ALife::EHitType hit_type);
		

	virtual	void	Hit					(SHit* pHDS);


	//для визуализации зоны
		virtual	void		PlayIdleParticles			(bool bIdleLight=true);
		virtual	void		StopIdleParticles			(bool bIdleLight=true);
				void		PlayAccumParticles			();
				void		PlayAwakingParticles		();
				void		PlayBlowoutParticles		();
				void		PlayEntranceParticles		(CGameObject* pObject);
				void		PlayBulletParticles			(Fvector& pos );
				void		PlayBoltEntranceParticles	();

				void		PlayHitParticles			(CGameObject* pObject);

				void		PlayObjectIdleParticles		(CGameObject* pObject);
				void		StopObjectIdleParticles		(CGameObject* pObject);

	virtual		bool		IsVisibleForZones			() { return false;}

	//обновление, если зона передвигается
	virtual		void		OnMove						();
	Fvector					m_vPrevPos;
	u32						m_dwLastTimeMoved;

	//FFx0001++
	bool m_use_electric_curve = false;
	bool m_cascade_curves = false;
	bool m_cascade_curves_by_anomalies = false;
	shared_str m_electric_curve_particle_path;
	xr_vector<SElectricCurve> m_electric_curves;
	u8 m_max_count_electric_curves = 1;
	float max_trace_curve_distance = 15.f;
	float m_max_curve_damage = 0.005f;
	float m_max_curve_impulse = 0.012f;

	xr_vector<CRandomSoundEmmiter*> m_snd_emmiter_electric_core_target_damage;
	xr_vector<CRandomSoundEmmiter*> m_snd_emmiter_electric_core_loop;

	xr_vector<CRandomSoundEmmiter*> m_snd_emmiter_electric_curve_start;
	xr_vector<CRandomSoundEmmiter*> m_snd_emmiter_electric_curve_loop;
	xr_vector<CRandomSoundEmmiter*> m_snd_emmiter_electric_curve_end;

	bool m_use_movement = false;
	float max_processing_distance = 200.f;
	bool draw_dbg = false;
	bool m_use_movement_always_mode = false;
	bool m_use_movement_magnetic_on_inside_alive_mode = false;
	float movement_magnetic_on_inside_alive_mode_speed = 0.f;
	bool m_use_movement_magnetic_on_take_artefacts_mode = false;
	float m_timer_magnetic_on_take_artefacts = 0.f;
	float m_max_timer_magnetic_on_take_artefacts = 0.f;
	float movement_magnetic_on_take_artefacts_mode_speed = 0.f;
	float m_movement_speed = 0.f;
	float m_movement_radius = 0.f;
	Fvector m_initial_spawn_position;
	Fvector m_target_position;
	float animTime = 0.0f;
	float blastTimeProcessing = 0.0f;
	float max_blastTimeProcessing = 0.0f;
	CGameObject* lastDamagedObject = nullptr;
	xr_vector<CGameObject*> lastScannedObjects;

	//расстояние от зоны до текущего актера
	float					m_fDistanceToCurEntity;
protected:
	u32						m_ef_anomaly_type;
	u32						m_ef_weapon_type;
public:
	void					CalcDistanceTo				(const Fvector& P, float& dist, float& radius);
	virtual u32				ef_anomaly_type				() const;
	virtual u32				ef_weapon_type				() const;
	virtual	bool			register_schedule			() const {return true;}
	u8						PlayEntranceSmallParticles	(const Fvector& pos, const Fvector& dir, const Fvector& vel, bool play_effect = true);

	void					MoveToFromDelta	(Fvector newPos, float speed);
	Fvector					GetLVPos(Fvector newPos);
	CGameObject*			ScanObjects(float distance, Fvector scanCenter, Fvector barierCenter, float barierRadius);
	void					OnActorTakeArtefact(float scan_radius, CArtefact* art, Fvector actorPos);
	void					UpdateElectricCurves(CGameObject* firstObject);
	void					UpdateMovement(bool isUpdateCL);
	void					OnBlastElectricCurvesProcessing(CGameObject* obj);
	void					OnBlastElectricCurvesUpdate(CGameObject* obj);
	xr_vector<CGameObject*> GetSortedByDistanceSpatialObjects(float distance, Fvector centerPos, u64 mask);
	void					AffectCurveDamade(CGameObject* obj);
	// optimization FAST/SLOW mode
public:	
	virtual bool			AlwaysTheCrow				();
	void					o_switch_2_fast				();
	void					o_switch_2_slow				();

// Lain: adde
private:
	virtual bool            light_in_slow_mode () { return true; }
	DECLARE_SCRIPT_REGISTER_FUNCTION
};

//информация о объекте, находящемся в зоне
struct SZoneObjectInfo
{
	SZoneObjectInfo() :object(NULL), zone_ignore(false), dw_time_in_zone(0), f_time_affected(Device.fTimeGlobal), small_object(false), nonalive_object(false) {}
	CGameObject* object;
	bool					small_object;
	bool					nonalive_object;
	//игнорирование объекта в зоне
	bool					zone_ignore;
	//присоединенные партиклы
	xr_vector<CParticlesObject*>	particles_vector;
	//время прибывания в зоне
	u32						dw_time_in_zone;
	float					f_time_affected;

	static xr_vector<SZoneObjectInfo>::iterator find(CAnomalyZone* zone, CGameObject* GO)
	{
		xr_vector<SZoneObjectInfo>& zone_objects_map = zone->GetObjectInfoMap();
		return std::find(zone_objects_map.begin(), zone_objects_map.end(), GO);
	}

	static bool get(CAnomalyZone* zone, CGameObject* GO)
	{
		xr_vector<SZoneObjectInfo>& zone_objects_map = zone->GetObjectInfoMap();
		return std::find(zone_objects_map.begin(), zone_objects_map.end(), GO)!= zone_objects_map.end();
	}

	static void remove(CAnomalyZone* zone, CGameObject* GO)
	{
		xr_vector<SZoneObjectInfo>& zone_objects_map = zone->GetObjectInfoMap();

		if (zone_objects_map.empty()) return;

		xr_vector<SZoneObjectInfo>::iterator it = std::find(zone_objects_map.begin(), zone_objects_map.end(), GO);

		if(it!= zone_objects_map.end())
		{
			zone->exit_Zone(*it);
			zone_objects_map.erase(it);
		}
	}

	bool operator == (const CGameObject* O) const { return object == O; }
};
