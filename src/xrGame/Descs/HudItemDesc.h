#pragma once
#include "DescRegistry.h"
#include "../InertionData.h"
#include "../player_hud.h"

XREFLECT(IniRead)
struct SHudYPRParams
{
	XPROPERTY(IniReadExist, Name="yaw_inertia_k")
	float m_fHudYawInertiaK = 0.0f;
	XPROPERTY(IniReadExist, Name="pitch_inertia_k")
	float m_fHudPitchInertiaK = 0.0f;
	XPROPERTY(IniReadExist, Name="roll_inertia_k")
	float m_fHudRollInertiaK = 0.0f;
	XPROPERTY(IniReadExist, Name="inertia_speed")
	float m_fHudInertiaSpeed = 10.0f;
};

struct SHudJitterParams
{
	float pos_amplitude = 0.0f;
	float rot_amplitude = 0.0f;
	float stop_time = 0.0f;
};

XREFLECT(IniRead)
struct SHudItemDesc final
{
	using Registry = TDescRegistry<SHudItemDesc>;

	XPROPERTY(IniReadExist, Name="hud_fov")
	float m_fHudFov = 0.0f;
	XPROPERTY(IniReadExist, Name="hud_fov_factor")
	float m_fHudFovFactor = 1.0f;
	XPROPERTY(IniReadExist, Name="lookout_speed_koef")
	float m_fLookOutSpeedKoef = 1.0f;
	XPROPERTY(IniReadExist, Name="lookout_ampl_k")
	float m_fLookOutAmplK = 1.0f;

	XPROPERTY(IniRead, Prefix="hud_")
	SHudYPRParams BaseYPRParams;
	XPROPERTY(IniRead, Prefix="hud_zoom_")
	SHudYPRParams ZoomYPRParams;

	InertionData Inertion;
	SHudJitterParams m_jitter_params;

	XPROPERTY(IniReadExist, Name="disable_bore")
	bool m_bDisableBore = false;
	XPROPERTY(IniReadExist, Name="use_blending_movement")
	bool m_bBlendMovement = false;
	SBlendParams m_sMovementBlendParams[EMovementLayers::eLayersCount];

	XPROPERTY(IniReadExist, Name="controller_time")
	float ControllerTime = 0.0f;
	XPROPERTY(IniReadExist, Name="prohibit_suicide")
	bool ProhibitSuicide = true;

	void Load(const shared_str& HudSection);
};
