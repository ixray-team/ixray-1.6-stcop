#ifndef _EFFECTOR_BOBBING_H
#define _EFFECTOR_BOBBING_H
#pragma once

#include "CameraEffector.h"
#include "../xrEngine/cameramanager.h"

class CEffectorBobbing : public CEffectorCam  
{
	float	fTime;
	Fvector	vAngleAmplitude;
	float	fYAmplitude;
	float	fSpeed;

	u32		dwMState;
	float	fReminderFactor;
	bool	is_limping;
	bool	m_bZoomMode;

	float	m_fAmplitudeDelta;

	float	fOldPhase;
	float	fOldFreq;
	float	fOldAmp;

	struct EffectorType
	{
		float Default = 0.f;
		float Zoom = 0.f;
	};

	struct EffectorData
	{
		EffectorType Amplitude;
		EffectorType Speed;
	};

	EffectorData Run;
	EffectorData Walk;
	EffectorData Limp;
	EffectorData Crouch;
	EffectorData SlowCrouch;
	EffectorData Sprint;

public:
			CEffectorBobbing	();
	virtual ~CEffectorBobbing	();
	virtual void	SelectBobbingParams(bool zoom_mode, bool is_limping, float& old_phase, float& old_freq, float& old_amp, u32 mstate, float time, float& amp, float& st);
	virtual BOOL	ProcessCam	(SCamEffectorInfo& info);
	void	SetState			(u32 st, bool limping, bool ZoomMode);
};

#endif //_EFFECTOR_BOBBING_H
