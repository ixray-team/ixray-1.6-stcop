#pragma once

#include "CameraEffector.h"
#include "../xrEngine/CameraManager.h"

class CEffectorBobbing final : public CEffectorCam
{
	float fTime = 0.0f;

	u32 dwMState = 0;
	float fReminderFactor = 0.0f;
	bool is_limping = false;
	bool m_bZoomMode = false;

	float m_fAmplitudeDelta = 1.0f;

	float fOldPhase = 0.0f;
	float fOldFreq = 0.0f;
	float fOldAmp = 0.0f;

	struct EffectorType
	{
		float Default = 0.0f;
		float Zoom = 0.0f;
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
	CEffectorBobbing();
	virtual ~CEffectorBobbing() = default;
	void SelectBobbingParams(bool zoom_mode, bool is_limping, float& old_phase, float& old_freq, float& old_amp, u32 mstate, float time, float& amp, float& st);
	virtual bool ProcessCam(SCamEffectorInfo& info) override;
	void SetState(u32 st, bool limping, bool ZoomMode);
};