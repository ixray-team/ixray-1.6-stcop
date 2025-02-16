// EffectorZoomInertion.h: инерция(покачивания) оружия в режиме 
//						   приближения	
//////////////////////////////////////////////////////////////////////

#pragma once

#include "CameraEffector.h"
#include "../cameramanager.h"
#include "WeaponMagazined.h"

class CEffectorZoomInertion : public CEffectorCam
{
	bool	m_enabled;
	float	m_fDispRadius;
	float	m_fOldDispRadius;

	Fvector	m_vCurrentPoint;
	Fvector	m_vLastPoint;
	Fvector	m_vTargetPoint;

	Fvector	m_vOldCameraDir;

	u32		m_dwTimePassed;
	float	m_fTimeCameraMove;
	bool	m_bCameraMoving;

	//параметры настройки эффектора
	float	m_fCameraSpeedThreshold;
	float	m_fDispEpsilon;
	float	m_fDispMin;
	float	m_fDispHorzCoef;
	float	m_fZoomAimingDispK;
	//время через которое эффектор меняет направление движения
	u32		m_dwDeltaTime;
	u32		m_dwCenterDeltaTime;

	CRandom		m_Random;

	void			CalcNextPoint		();
	void			LoadParams			(LPCSTR Section, LPCSTR Prefix);
	IC		bool	IsFirstUpdate		() { return m_dwTimePassed == (u32)-1; }
	IC		bool	IsCameraMoving		() { return m_bCameraMoving; }
	
			template <class T>
			T		LoadParam			(LPCSTR section, LPCSTR prefix, LPCSTR param, T devFal) = delete;

			template <>
			float	LoadParam			(LPCSTR section, LPCSTR prefix, LPCSTR param, float devFal);
			template <>
			u32		LoadParam			(LPCSTR section, LPCSTR prefix, LPCSTR param, u32 devFal);
			void	ApplyPoint			(Fvector& dest, const Fvector& point);
			bool	UpdateCameraMoved	(const Fvector& currentDir, bool& justStopped);

public:
	CEffectorZoomInertion				();
	virtual ~CEffectorZoomInertion		();

			void	Load				();
			void	SetParams			(float disp);

	virtual BOOL	ProcessCam			(SCamEffectorInfo& info);
	virtual	void	SetRndSeed			(s32 Seed) { m_Random.seed(Seed); };
	virtual	void	Init				(CWeaponMagazined*	pWeapon);
	virtual	void	Enable				(bool flag, float rotateTime = 0.f);
	virtual	bool	Enabled				() { return m_enabled; }

	virtual CEffectorZoomInertion*		cast_effector_zoom_inertion	()	{return this;}
};