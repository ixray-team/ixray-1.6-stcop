#pragma once

#include "FlashlightInterference.h"

class CActor;

extern int g_FlashlightBeam;

class FlashlightBeam
{
public:
	void Load(const char* Section);
	void SetLegacy(const Fcolor& LegacyColor, float LegacyRange, float LegacyCone);

	bool IsActive() const { return g_FlashlightBeam != 0 && Configured; }
	bool IsSwayEnabled() const { return IsActive() && SwayEnabled; }
	bool IsEyeAnchored() const { return IsActive() && EyeAnchored; }
	bool HasEyeShadow() const { return EyeShadow; }
	Fvector GetEyePosition() const;

	bool AdjustFocus(int Direction);
	static bool IsFocusKeyHeld();

	void UpdateLight(IRender_Light* Light, const Fvector& ProbePosition, const Fcolor* AnimatedColor, float TimeDelta);
	float GetInterference() const { return InterferenceLevel; }
	Fcolor GetBaseColor(const Fcolor* AnimatedColor) const;

	void Reset();
	Fvector2 UpdateDirection(CActor* Actor, float TimeDelta);

private:
	void StepLag(float& Angle, float& Velocity, float Target, float TimeDelta) const;
	void UpdateWalk(CActor* Actor, float TimeDelta);
	void UpdateHands(CActor* Actor, float TimeDelta);
	float GetAimFactor(CActor* Actor) const;

	bool Configured = false;
	LightBeamParams Beam;
	bool ColorOverride = false;
	Fcolor Color = { 1.0f, 1.0f, 1.0f, 1.0f };
	float Range = 0.0f;
	float SpotAngle = 0.0f;
	bool EyeAnchored = false;
	bool EyeShadow = false;
	Fvector EyeOffset = { 0.0f, 0.0f, 0.0f };

	Fcolor LegacyColor = { 1.0f, 1.0f, 1.0f, 1.0f };
	float LegacyRange = 0.0f;
	float LegacyCone = 0.0f;

	bool FocusEnabled = false;
	float FocusStep = 0.2f;
	float FocusSpeed = 6.0f;
	float FocusTarget = 0.5f;
	float Focus = 0.5f;
	Fvector2 FocusSpotAngle = { 0.0f, 0.0f };
	Fvector2 FocusRange = { 0.0f, 0.0f };
	Fvector2 FocusIntensity = { 1.0f, 1.0f };
	Fvector2 FocusReferenceDistance = { 0.0f, 0.0f };
	Fvector2 FocusBeamSize = { 0.0f, 0.0f };

	FlashlightInterference Interference;
	float InterferenceLevel = 1.0f;

	bool SwayEnabled = false;
	float LagFrequency = 10.0f;
	float LagMaxAngle = 0.0f;
	float WalkSpeed = 1.5f;
	float RunSpeed = 5.0f;
	float WalkAmplitude = 0.0f;
	float RunAmplitude = 0.0f;
	float WalkFrequency = 0.9f;
	float RunFrequency = 1.25f;
	float VerticalRatio = 0.5f;
	float FadeTime = 0.35f;
	float HandsFactor = 0.35f;
	float HandsMaxAngle = 0.0f;
	float HandsSmoothTime = 0.12f;
	float HandsBaselineTime = 0.8f;
	float AimDamping = 0.7f;

	bool Initialized = false;
	Fvector2 Angles = { 0.0f, 0.0f };
	Fvector2 AngularVelocity = { 0.0f, 0.0f };
	float Phase = 0.0f;
	float Amplitude = 0.0f;
	float Frequency = 0.0f;
	const void* HandsItem = nullptr;
	Fvector2 HandsBaseline = { 0.0f, 0.0f };
	Fvector2 HandsOffset = { 0.0f, 0.0f };
};
