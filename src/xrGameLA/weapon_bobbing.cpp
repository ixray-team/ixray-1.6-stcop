////////////////////////////////////////////////////////////////////////////
//	Module 		: weapon_bobbing.cpp
//	Created 	: 04/09/2011
//  Modified 	: 04/09/2011
//	Author		: lost alpha
//	Description : weapon bobbing
////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "weapon_bobbing.h"
#include "actor.h"

CWeaponBobbing::CWeaponBobbing()
{
	Load();
}

CWeaponBobbing::~CWeaponBobbing()
{
}

void CWeaponBobbing::Load()
{
	fTime			= 0;
	fReminderFactor	= 0;
	is_limping		= false;

	m_fAmplitudeRun		= pSettings->r_float(BOBBING_SECT, "run_amplitude");
	m_fAmplitudeWalk	= pSettings->r_float(BOBBING_SECT, "walk_amplitude");
	m_fAmplitudeLimp	= pSettings->r_float(BOBBING_SECT, "limp_amplitude");

	m_fSpeedRun			= pSettings->r_float(BOBBING_SECT, "run_speed");
	m_fSpeedWalk		= pSettings->r_float(BOBBING_SECT, "walk_speed");
	m_fSpeedLimp		= pSettings->r_float(BOBBING_SECT, "limp_speed");
}

void CWeaponBobbing::CheckState()
{
	dwMState		= Actor()->MovingState();
	is_limping		= Actor()->IsLimping();
	m_bZoomMode		= Actor()->IsZoomAimingMode();
	fTime			+= Device.fTimeDelta;
}

void CWeaponBobbing::Update(Fmatrix &m)
{
	CheckState();
	if (dwMState&ACTOR_DEFS::mcAnyMove)
	{
		if (fReminderFactor < 1.f)
			fReminderFactor += SPEED_REMINDER * Device.fTimeDelta;
		else						
			fReminderFactor = 1.f;
	}
	else
	{
		if (fReminderFactor > 0.f)
			fReminderFactor -= SPEED_REMINDER * Device.fTimeDelta;
		else			
			fReminderFactor = 0.f;
	}
	if (!fsimilar(fReminderFactor, 0))
	{
		Fvector dangle;
		Fmatrix		R, mR;
		float k		= ((dwMState & ACTOR_DEFS::mcCrouch) ? CROUCH_FACTOR : 1.f);

		float A, ST;

		if (isActorAccelerated(dwMState, m_bZoomMode))
		{
			A	= m_fAmplitudeRun * k;
			ST	= m_fSpeedRun * fTime * k;
		}
		else if (is_limping)
		{
			A	= m_fAmplitudeLimp * k;
			ST	= m_fSpeedLimp * fTime * k;
		}
		else
		{
			A	= m_fAmplitudeWalk * k;
			ST	= m_fSpeedWalk * fTime * k;
		}
	
		float _sinA	= _abs(_sin(ST) * A) * fReminderFactor;
		float _cosA	= _cos(ST) * A * fReminderFactor;

		m.c.y		+=	_sinA;
		dangle.x	=	_cosA;
		dangle.z	=	_cosA;
		dangle.y	=	_sinA;

		
		R.setHPB(dangle.x, dangle.y, dangle.z);
		
		mR.mul		(m, R);
		
		m.k.set(mR.k);
		m.j.set(mR.j);
	}
}