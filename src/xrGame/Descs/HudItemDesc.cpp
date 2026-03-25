#include "StdAfx.h"
#include "HudItemDesc.h"

void SHudItemDesc::Load(const shared_str& HudSection)
{
	if (!HudSection.size())
	{
		return;
	}
	m_fHudFov = pSettings->read_if_exists<float>(HudSection, "hud_fov", 0.0f);
	m_fHudFovFactor = pSettings->read_if_exists<float>(HudSection, "hud_fov_factor", 1.0f);

	m_fLookOutSpeedKoef = pSettings->read_if_exists<float>(HudSection, "lookout_speed_koef", 1.0f);
	m_fLookOutAmplK = pSettings->read_if_exists<float>(HudSection, "lookout_ampl_k", 1.0f);

	BaseYPRParams.m_fHudYawInertiaK = pSettings->read_if_exists<float>(HudSection, "hud_yaw_inertia_k", 0.0f);
	BaseYPRParams.m_fHudPitchInertiaK = pSettings->read_if_exists<float>(HudSection, "hud_pitch_inertia_k", 0.0f);
	BaseYPRParams.m_fHudRollInertiaK = pSettings->read_if_exists<float>(HudSection, "hud_roll_inertia_k", 0.0f);
	BaseYPRParams.m_fHudInertiaSpeed = pSettings->read_if_exists<float>(HudSection, "hud_inertia_speed", 10.0f);

	ZoomYPRParams.m_fHudYawInertiaK = pSettings->read_if_exists<float>(HudSection, "hud_zoom_yaw_inertia_k", 0.0f);
	ZoomYPRParams.m_fHudPitchInertiaK = pSettings->read_if_exists<float>(HudSection, "hud_zoom_pitch_inertia_k", 0.0f);
	ZoomYPRParams.m_fHudRollInertiaK = pSettings->read_if_exists<float>(HudSection, "hud_zoom_roll_inertia_k", 0.0f);
	ZoomYPRParams.m_fHudInertiaSpeed = pSettings->read_if_exists<float>(HudSection, "hud_zoom_inertia_speed", 10.0f);

	Inertion.PitchOffsetR = pSettings->read_if_exists<float>(HudSection, "inertion_pitch_offset_r", PITCH_OFFSET_R);
	Inertion.PitchOffsetD = pSettings->read_if_exists<float>(HudSection, "inertion_pitch_offset_d", PITCH_OFFSET_D);
	Inertion.PitchOffsetN = pSettings->read_if_exists<float>(HudSection, "inertion_pitch_offset_n", PITCH_OFFSET_N);
	Inertion.OriginOffset = pSettings->read_if_exists<float>(HudSection, "inertion_origin_offset", ORIGIN_OFFSET);
	Inertion.TendtoSpeed = pSettings->read_if_exists<float>(HudSection, "inertion_tendto_speed", TENDTO_SPEED);

	m_jitter_params.pos_amplitude = pSettings->read_if_exists<float>("gunslinger_base", "base_jitter_pos_amplitude", 0.001f);
	m_jitter_params.rot_amplitude = pSettings->read_if_exists<float>("gunslinger_base", "base_jitter_rot_amplitude", 0.1f);
	m_jitter_params.pos_amplitude = pSettings->read_if_exists<float>(HudSection, "jitter_pos_amplitude", m_jitter_params.pos_amplitude);
	m_jitter_params.rot_amplitude = pSettings->read_if_exists<float>(HudSection, "jitter_rot_amplitude", m_jitter_params.rot_amplitude);
	m_jitter_params.stop_time = floor(pSettings->read_if_exists<float>(HudSection, "jitter_stop_time", 3.0f) * 1000.f);

	m_bDisableBore = pSettings->read_if_exists<bool>(HudSection, "disable_bore", false);

	pSettings->r_bool_nullable(HudSection, "use_blending_movement", m_bBlendMovement);
	pSettings->r_float_nullable(HudSection, "controller_time", ControllerTime);
	pSettings->r_bool_nullable(HudSection, "prohibit_suicide", ProhibitSuicide);

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
		m_sMovementBlendParams[EMovementLayers::eStrafeLeft].Load(HudSection, "anim_blend_strafe_left");
		m_sMovementBlendParams[EMovementLayers::eStrafeRight].Load(HudSection, "anim_blend_strafe_right");
	}
}
