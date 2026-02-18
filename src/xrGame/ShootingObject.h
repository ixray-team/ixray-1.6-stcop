//////////////////////////////////////////////////////////////////////
// ShootingObject.h: интерфейс для семейства стреляющих объектов 
//					 (оружие и осколочные гранаты) 	
//					 обеспечивает набор хитов, звуков рикошетп
//////////////////////////////////////////////////////////////////////

#pragma once

#include "alife_space.h"
#include "../xrEngine/Render.h"
#include "anticheat_dumpable_object.h"
#include "../xrPhysics/DamageSource.h"

class CCartridge;
class CParticlesObject;
class IRender_Sector;

#define WEAPON_MATERIAL_NAME "objects\\bullet"

class CShootingObject : public IAnticheatDumpable, public IDamageSource
{
protected:
	CShootingObject();
	virtual ~CShootingObject() = default;

	void	reload(const char* section) {};
	void	Load(const char* section);

private:
	float		m_air_resistance_factor;
//////////////////////////////////////////////////////////////////////////
// Fire Params
//////////////////////////////////////////////////////////////////////////
protected:

	enum
	{
		eDefaultFire = 0,
		eSilencerFire = 1,
		eGlauncherFire = 2,
	} fire_mode = eDefaultFire;

	virtual void			LoadFireParams		(const char* section); 		//сила выстрела
	virtual bool			SendHitAllowed		(CObject* pUser);
	virtual void			FireBullet			(const Fvector& pos, 
        										const Fvector& dir, 
												float fire_disp,
												const CCartridge& cartridge,
												ALife::_OBJECT_ID parent_id,
												ALife::_OBJECT_ID weapon_id,
												bool send_hit);
	void					SetBulletSpeed(float new_speed) {m_fStartBulletSpeed = new_speed;}
	float					GetBulletSpeed()				{return m_fStartBulletSpeed;}

	virtual void			FireStart			();
	virtual void			FireEnd				();
public:
	IC bool					IsWorking			()	const	{return bWorking;}
	virtual bool			ParentMayHaveAimBullet()		{return false;}
	virtual bool			ParentIsActor()					{return false;}

	float getFireDistance(void) const { return fireDistance; }
	void setFireDistance(float value);
	float getFireDispersionBase(void) const { return fireDispersionBase; }
	void setFireDispersionBase(float value);
	float getStartBulletSpeed(void) const { return m_fStartBulletSpeed; }
	void setStartBulletSpeed(float value);
	float getRPM(void) const { return fOneShotTime; }
	void setRPM(float value);

	virtual float getHitImpulse() const { return fHitImpulse; }
	virtual void setHitImpulse(float value);
	virtual const Fvector4& getHitPower() const { return fvHitPower; }
	virtual void setHitPower(const Fvector4& vec);
	virtual const Fvector4& getHitPowerCritical() const { return fvHitPowerCritical; }
	virtual void setHitPowerCritical(const Fvector4& vec);

protected:
	// Weapon fires now
	bool					bWorking;

	float					fOneShotTime = 0.0f;
	float					fOneShotTimeSaved = 0.0f;
	Fvector4				fvHitPower;
	Fvector4				fvHitPowerCritical;
	float					fHitImpulse;

	//скорость вылета пули из ствола
	float					m_fStartBulletSpeed;
	//максимальное расстояние стрельбы
	float					fireDistance;

	//рассеивание во время стрельбы
	float					fireDispersionBase;

	//счетчик времени, затрачиваемого на выстрел
	float					fShotTimeCounter;

	struct SilencerKoeffs // value *= koef;
	{
		float	hit_power;
		float	hit_impulse;
		float	bullet_speed;
		float	fire_dispersion;
		float	cam_dispersion;
		float	cam_disper_inc;
		float   attached_recoil;

		SilencerKoeffs() { Reset(); }
		IC void Reset()
		{
			hit_power       = 1.0f;
			hit_impulse     = 1.0f;
			bullet_speed    = 1.0f;
			fire_dispersion = 1.0f;
			cam_dispersion  = 1.0f;
			cam_disper_inc  = 1.0f;
			attached_recoil = 1.0f;
		}
	};// SilencerKoeffs
	SilencerKoeffs		m_silencer_koef;

public:
	SilencerKoeffs		cur_silencer_koef;

protected:
	Fcolor					light_base_color;
	float					light_base_range;
	Fcolor					light_build_color;
	float					light_build_range;
	ref_light				light_render;
	float					light_var_color;
	float					light_var_range;
	float					light_lifetime;
	u32						light_frame;
	float					light_time;
	bool					m_bLightShotEnabled;
protected:
	void					Light_Start			();
	void					Light_Render		(const Fvector& P);

			void			LoadLights			(const char* section, const char* prefix);
			void			RenderLight			();
			void			UpdateEffects		();
			void			DestroyEffects		();
			void			StopLight			();
	virtual bool			IsHudModeNow		() { return false; };
protected:
	virtual const Fvector&	get_CurrentFirePoint()	{ return zero_vel; };
	virtual const Fvector&	get_CurrentFirePoint2() { return get_CurrentFirePoint(); };
	virtual const Fvector&	get_CurrentShellPoint()	{ return get_CurrentFirePoint(); };
	virtual const Fmatrix&	get_ParticlesXFORM()	{ return Fidentity; };

			void			LoadParticle		(const char* section, const char* line, xr_shared_ptr<CParticlesObject>& particle);

			void			StartFlameParticle();
			void			StartSmokeParticle(const Fvector& parent_vel);
			void			StartShellParticle(const Fvector& parent_vel);
public:
	Fvector					vLoadedShellPoint;
	float					m_fPredBulletTime;
	float					m_fTimeToAim;
	bool					m_bUseAimBullet;
protected:

	shared_str						m_sShellParticles;

	xr_shared_ptr<CParticlesObject> m_pSmokeParticles;
	xr_shared_ptr<CParticlesObject> m_pFlameParticles;

	xr_shared_ptr<CParticlesObject> m_pSmokeSilencerParticles;
	xr_shared_ptr<CParticlesObject> m_pFlameSilencerParticles;
	xr_shared_ptr<CParticlesObject> m_pFlameGlaucherParticles;

	ALife::_OBJECT_ID initiator_id;

public:
	virtual void DumpActiveParams(shared_str const& section_name, CInifile& dst_ini) const;

	void SetInitiator(ALife::_OBJECT_ID id) override { initiator_id = id; }
	ALife::_OBJECT_ID Initiator() override { return initiator_id; }
	IDamageSource* cast_IDamageSource() override { return this; }
};
