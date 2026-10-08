#include "StdAfx.h"
#include "HudItemDesc.h"

void SHudItemDesc::Load(const shared_str& HudSection)
{
	m_fHudFov = READ_IF_EXISTS(pSettings, r_float, HudSection, "hud_fov", 0.0f);
	m_fHudFovFactor = READ_IF_EXISTS(pSettings, r_float, HudSection, "hud_fov_factor", 1.0f);

	m_fLookOutSpeedKoef = READ_IF_EXISTS(pSettings, r_float, HudSection, "lookout_speed_koef", 1.0f);
	m_fLookOutAmplK = READ_IF_EXISTS(pSettings, r_float, HudSection, "lookout_ampl_k", 1.0f);

	BaseYPRParams.m_fHudYawInertiaK = READ_IF_EXISTS(pSettings, r_float, HudSection, "hud_yaw_inertia_k", 0.0f);
	BaseYPRParams.m_fHudPitchInertiaK = READ_IF_EXISTS(pSettings, r_float, HudSection, "hud_pitch_inertia_k", 0.0f);
	BaseYPRParams.m_fHudRollInertiaK = READ_IF_EXISTS(pSettings, r_float, HudSection, "hud_roll_inertia_k", 0.0f);
	BaseYPRParams.m_fHudInertiaSpeed = READ_IF_EXISTS(pSettings, r_float, HudSection, "hud_inertia_speed", 10.0f);

	ZoomYPRParams.m_fHudYawInertiaK = READ_IF_EXISTS(pSettings, r_float, HudSection, "hud_zoom_yaw_inertia_k", 0.0f);
	ZoomYPRParams.m_fHudPitchInertiaK = READ_IF_EXISTS(pSettings, r_float, HudSection, "hud_zoom_pitch_inertia_k", 0.0f);
	ZoomYPRParams.m_fHudRollInertiaK = READ_IF_EXISTS(pSettings, r_float, HudSection, "hud_zoom_roll_inertia_k", 0.0f);
	ZoomYPRParams.m_fHudInertiaSpeed = READ_IF_EXISTS(pSettings, r_float, HudSection, "hud_zoom_inertia_speed", 10.0f);

	Inertion.PitchOffsetR = READ_IF_EXISTS(pSettings, r_float, HudSection, "inertion_pitch_offset_r", PITCH_OFFSET_R);
	Inertion.PitchOffsetD = READ_IF_EXISTS(pSettings, r_float, HudSection, "inertion_pitch_offset_d", PITCH_OFFSET_D);
	Inertion.PitchOffsetN = READ_IF_EXISTS(pSettings, r_float, HudSection, "inertion_pitch_offset_n", PITCH_OFFSET_N);
	Inertion.OriginOffset = READ_IF_EXISTS(pSettings, r_float, HudSection, "inertion_origin_offset", ORIGIN_OFFSET);
	Inertion.TendtoSpeed = READ_IF_EXISTS(pSettings, r_float, HudSection, "inertion_tendto_speed", TENDTO_SPEED);

	m_jitter_params.pos_amplitude = READ_IF_EXISTS(pSettings, r_float, "gunslinger_base", "base_jitter_pos_amplitude", 0.001f);
	m_jitter_params.rot_amplitude = READ_IF_EXISTS(pSettings, r_float, "gunslinger_base", "base_jitter_rot_amplitude", 0.1f);
	m_jitter_params.pos_amplitude = READ_IF_EXISTS(pSettings, r_float, HudSection, "jitter_pos_amplitude", m_jitter_params.pos_amplitude);
	m_jitter_params.rot_amplitude = READ_IF_EXISTS(pSettings, r_float, HudSection, "jitter_rot_amplitude", m_jitter_params.rot_amplitude);
	m_jitter_params.stop_time = floor(READ_IF_EXISTS(pSettings, r_float, HudSection, "jitter_stop_time", 3.0f) * 1000.f);

	m_bDisableBore = READ_IF_EXISTS(pSettings, r_bool, HudSection, "disable_bore", false);

	pSettings->read_if_exists<bool>(m_bBlendMovement, HudSection, "use_blending_movement");
	pSettings->read_if_exists<float>(ControllerTime, HudSection, "controller_time");
	pSettings->read_if_exists<bool>(ProhibitSuicide, HudSection, "prohibit_suicide");

	if (m_bBlendMovement)
	{
		m_sMovementBlendParams[EMovementLayers::eWalk].Load(HudSection, "anim_blend_walk");
		m_sMovementBlendParams[EMovementLayers::eWalkSlow].Load(HudSection, "anim_blend_walk_slow");
		m_sMovementBlendParams[EMovementLayers::eCrouch].Load(HudSection, "anim_blend_crouch");
		m_sMovementBlendParams[EMovementLayers::eCrouchSlow].Load(HudSection, "anim_blend_crouch_slow");
		m_sMovementBlendParams[EMovementLayers::eSprint].Load(HudSection, "anim_blend_sprint");
		m_sMovementBlendParams[EMovementLayers::eIdle].Load(HudSection, "anim_blend_idle");
		m_sMovementBlendParams[EMovementLayers::eIdleAim].Load(HudSection, "anim_blend_idle_aim");
		m_sMovementBlendParams[EMovementLayers::eAimWalk].Load(HudSection, "anim_blend_aim_walk");
	}
}
