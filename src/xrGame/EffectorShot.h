// EffectorShot.h: interface for the CCameraShotEffector class.
//
//////////////////////////////////////////////////////////////////////

#pragma once

#include "CameraEffector.h"
#include "../xrEngine/CameraManager.h"
#include "Actor.h"
#include "CameraRecoil.h"

class CWeapon;

class CWeaponShotEffector
{
protected:
	CameraRecoil	current_recoil;

	float			m_angle_vert;
	float			m_angle_horz;

	float			m_prev_angle_vert;
	float			m_prev_angle_horz;


	float           m_accumulated_time = 0.0f;


	// Пружинная система
	float			m_target_angle_vert;
	float			m_target_angle_horz;
	float			m_velocity_vert;
	float			m_velocity_horz;
	float			m_return_target_vert;      // позиция предпоследнего выстрела (куда возвращаемся)
	float			m_return_target_horz;

	// Флаг использования паттерна
	bool			m_using_pattern; 

	float			m_delta_vert;
	float			m_delta_horz;

	int				m_shot_numer;
	bool			m_shot_end;

	bool			m_actived;
	bool			m_single_shot = false;

private:
	CRandom			m_Random;
	s32				m_LastSeed;

public:
	CWeaponShotEffector();
	virtual		~CWeaponShotEffector() {};

	void SetCustomRecoil(const CameraRecoil& custom_recoil) {
		current_recoil = custom_recoil;
	}

	void	Initialize(const CameraRecoil& cam_recoil);
	void	Reset();

	IC	bool	IsActive() { return m_actived; }
	IC	void	StopShoting() { m_shot_end = true; }
	IC	bool	IsSingleShot() { return m_single_shot; }
	void	SetSingleShoot(bool Single) { m_single_shot = Single; };

	void	Update();

	void	SetRndSeed(s32 Seed);

	void	Shot(CWeapon* weapon);
	void	Shot2Legacy(float angle);
	void    ShotFromPattern(float pattern_x, float pattern_y);

	void	GetDeltaAngle(Fvector& angle);
	void	GetLastDelta(Fvector& delta_angle);
	void	ChangeHP(float* pitch, float* yaw);



protected:
	void SpringPhysics(float dt, float spring_stiffness, float damping);
	void Relax(float dt);
    void UpdateSpringRecoil(float dt);
 
};

class CCameraShotEffector : public CWeaponShotEffector, public CEffectorCam
{
protected:
	CActor* m_pActor;
public:
	//-					CCameraShotEffector	(float max_angle, float relax_speed, float max_angle_horz, float step_angle_horz, float angle_frac);
	CCameraShotEffector();
	virtual			~CCameraShotEffector();

	virtual bool	ProcessCam(SCamEffectorInfo& info);
	virtual void	SetActor(CActor* pActor) { m_pActor = pActor; };

	virtual CCameraShotEffector* cast_effector_shot() { return this; }
	u16				m_WeaponID;
};