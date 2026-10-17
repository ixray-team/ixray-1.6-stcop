#include "StdAfx.h"
#include "HudItemDesc.h"

void SHudItemDesc::Load(const shared_str& HudSection)
{
	if (!HudSection.size())
	{
		return;
	}
	XReflect::ReadIni(*this, HudSection);

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
