#pragma once
#include "DescRegistry.h"
#include "../InertionData.h"
#include "../player_hud.h"

struct SHudYPRParams
{
	float m_fHudYawInertiaK = 0.0f;
	float m_fHudPitchInertiaK = 0.0f;
	float m_fHudRollInertiaK = 0.0f;
	float m_fHudInertiaSpeed = 10.0f;
};

struct SHudJitterParams
{
	float pos_amplitude = 0.0f;
	float rot_amplitude = 0.0f;
	float stop_time = 0.0f;
};

struct SHudItemDesc final
{
	using Registry = TDescRegistry<SHudItemDesc>;

	float m_fHudFov = 0.0f;
	float m_fHudFovFactor = 1.0f;
	float m_fLookOutSpeedKoef = 1.0f;
	float m_fLookOutAmplK = 1.0f;

	SHudYPRParams BaseYPRParams;
	SHudYPRParams ZoomYPRParams;

	InertionData Inertion;
	SHudJitterParams m_jitter_params;

	bool m_bDisableBore = false;
	bool m_bBlendMovement = false;
	SBlendParams m_sMovementBlendParams[EMovementLayers::eLayersCount];

	float ControllerTime = 0.0f;
	bool ProhibitSuicide = true;

	void Load(const shared_str& HudSection);
};
