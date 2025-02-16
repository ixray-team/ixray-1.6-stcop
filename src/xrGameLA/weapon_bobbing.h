////////////////////////////////////////////////////////////////////////////
//	Module 		: weapon_bobbing.h
//	Created 	: 04/09/2011
//  Modified 	: 04/09/2011
//	Author		: lost alpha
//	Description : weapon bobbing
////////////////////////////////////////////////////////////////////////////
#pragma once

#define BOBBING_SECT "wpn_bobbing_effector"

#define CROUCH_FACTOR	0.75f
#define SPEED_REMINDER	5.f 

class CWeaponBobbing
{
	public:
		CWeaponBobbing();
		virtual ~CWeaponBobbing();
		void Load();
		void Update(Fmatrix &m);
		void CheckState();

	private:
		float	fTime;
		Fvector	vAngleAmplitude;
		float	fYAmplitude;
		float	fSpeed;

		u32		dwMState;
		float	fReminderFactor;
		bool	is_limping;
		bool	m_bZoomMode;

		float	m_fAmplitudeRun;
		float	m_fAmplitudeWalk;
		float	m_fAmplitudeLimp;

		float	m_fSpeedRun;
		float	m_fSpeedWalk;
		float	m_fSpeedLimp;
};