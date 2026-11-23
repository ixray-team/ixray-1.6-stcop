#include "StdAfx.h"
#include "FlashlightBeam.h"
#include "Actor.h"
#include "Inventory.h"
#include "HudItem.h"
#include "player_hud.h"
#include "CharacterPhysicsSupport.h"
#include "../xrEngine/CameraBase.h"
#include "../xrEngine/xr_input.h"
#include "../xrEngine/xr_level_controller.h"

int g_FlashlightBeam = 1;

namespace FlashlightBeamMath
{
	float Smooth(float TimeDelta, float Time)
	{
		return Time > EPS ? 1.0f - std::exp(-TimeDelta / Time) : 1.0f;
	}

	float SmoothStep(float Edge0, float Edge1, float Value)
	{
		const float T = clampr((Value - Edge0) / std::max(Edge1 - Edge0, EPS), 0.0f, 1.0f);
		return T * T * (3.0f - 2.0f * T);
	}

	float ClampAngle(float Value, float Limit)
	{
		return clampr(Value, -Limit, Limit);
	}
}

void FlashlightBeam::Load(const char* Section)
{
	Beam.Enabled = pSettings->read_if_exists<bool>(Section, "beam_enabled", false);
	Beam.CoreSize = pSettings->read_if_exists<float>(Section, "beam_core_size", Beam.CoreSize);
	Beam.BeamSize = pSettings->read_if_exists<float>(Section, "beam_size", Beam.BeamSize);
	Beam.EdgeSoftness = pSettings->read_if_exists<float>(Section, "beam_edge_softness", Beam.EdgeSoftness);
	Beam.SpillLevel = pSettings->read_if_exists<float>(Section, "beam_spill", Beam.SpillLevel);
	Beam.CoreGain = pSettings->read_if_exists<float>(Section, "beam_core_gain", Beam.CoreGain);
	Beam.RingLevel = pSettings->read_if_exists<float>(Section, "beam_ring", Beam.RingLevel);
	Beam.Intensity = pSettings->read_if_exists<float>(Section, "beam_intensity", Beam.Intensity);
	Beam.EdgeWarmth = pSettings->read_if_exists<float>(Section, "beam_edge_warmth", Beam.EdgeWarmth);
	Beam.ReferenceDistance = pSettings->read_if_exists<float>(Section, "beam_reference_distance", Beam.ReferenceDistance);
	Beam.NearBoostMax = pSettings->read_if_exists<float>(Section, "beam_near_boost_max", Beam.NearBoostMax);

	ColorOverride = pSettings->line_exist(Section, "beam_color");
	if (ColorOverride)
	{
		Color = pSettings->r_fcolor(Section, "beam_color");
	}

	Range = pSettings->read_if_exists<float>(Section, "beam_range", 0.0f);
	SpotAngle = deg2rad(pSettings->read_if_exists<float>(Section, "beam_spot_angle", 0.0f));
	EyeAnchored = pSettings->read_if_exists<bool>(Section, "beam_eye_anchor", false);
	EyeOffset = pSettings->read_if_exists<Fvector>(Section, "beam_eye_offset", EyeOffset);
	EyeShadow = pSettings->read_if_exists<bool>(Section, "beam_eye_shadow", false);

	SwayEnabled = pSettings->read_if_exists<bool>(Section, "sway_enabled", false);
	LagFrequency = pSettings->read_if_exists<float>(Section, "sway_lag_frequency", LagFrequency);
	LagMaxAngle = deg2rad(pSettings->read_if_exists<float>(Section, "sway_lag_max_angle", 5.0f));
	WalkSpeed = pSettings->read_if_exists<float>(Section, "sway_walk_speed", WalkSpeed);
	RunSpeed = pSettings->read_if_exists<float>(Section, "sway_run_speed", RunSpeed);
	WalkAmplitude = deg2rad(pSettings->read_if_exists<float>(Section, "sway_walk_amplitude", 0.5f));
	RunAmplitude = deg2rad(pSettings->read_if_exists<float>(Section, "sway_run_amplitude", 1.1f));
	WalkFrequency = pSettings->read_if_exists<float>(Section, "sway_walk_frequency", WalkFrequency);
	RunFrequency = pSettings->read_if_exists<float>(Section, "sway_run_frequency", RunFrequency);
	VerticalRatio = pSettings->read_if_exists<float>(Section, "sway_vertical_ratio", VerticalRatio);
	FadeTime = pSettings->read_if_exists<float>(Section, "sway_fade_time", FadeTime);
	HandsFactor = pSettings->read_if_exists<float>(Section, "sway_hands_factor", HandsFactor);
	HandsMaxAngle = deg2rad(pSettings->read_if_exists<float>(Section, "sway_hands_max_angle", 2.0f));
	HandsSmoothTime = pSettings->read_if_exists<float>(Section, "sway_hands_smooth_time", HandsSmoothTime);
	HandsBaselineTime = pSettings->read_if_exists<float>(Section, "sway_hands_baseline_time", HandsBaselineTime);
	AimDamping = pSettings->read_if_exists<float>(Section, "sway_aim_damping", AimDamping);

	RunSpeed = std::max(RunSpeed, WalkSpeed + 0.1f);

	FocusEnabled = pSettings->read_if_exists<bool>(Section, "beam_focus", false);
	FocusStep = 1.0f / float(std::max(pSettings->read_if_exists<int>(Section, "beam_focus_steps", 5), 1));
	FocusSpeed = pSettings->read_if_exists<float>(Section, "beam_focus_speed", FocusSpeed);
	FocusTarget = clampr(pSettings->read_if_exists<float>(Section, "beam_focus_default", 0.5f), 0.0f, 1.0f);
	Focus = FocusTarget;

	const float SpotAngleDegrees = rad2deg(SpotAngle);
	FocusSpotAngle = pSettings->read_if_exists<Fvector2>(Section, "beam_focus_spot_angle", Fvector2{ SpotAngleDegrees, SpotAngleDegrees });
	FocusSpotAngle.mul(PI / 180.0f);
	FocusRange = pSettings->read_if_exists<Fvector2>(Section, "beam_focus_range", Fvector2{ Range, Range });
	FocusIntensity = pSettings->read_if_exists<Fvector2>(Section, "beam_focus_intensity", Fvector2{ 1.0f, 1.0f });
	FocusReferenceDistance = pSettings->read_if_exists<Fvector2>(Section, "beam_focus_reference_distance", Fvector2{ Beam.ReferenceDistance, Beam.ReferenceDistance });
	FocusBeamSize = pSettings->read_if_exists<Fvector2>(Section, "beam_focus_size", Fvector2{ Beam.BeamSize, Beam.BeamSize });

	Interference.Load(Section);

	Configured = Beam.Enabled || SwayEnabled || EyeAnchored;
	Reset();
}

void FlashlightBeam::SetLegacy(const Fcolor& NewColor, float NewRange, float NewCone)
{
	LegacyColor = NewColor;
	LegacyRange = NewRange;
	LegacyCone = NewCone;
}

Fcolor FlashlightBeam::GetBaseColor(const Fcolor* AnimatedColor) const
{
	if (IsActive() && ColorOverride)
	{
		return Color;
	}

	return AnimatedColor != nullptr ? *AnimatedColor : LegacyColor;
}

bool FlashlightBeam::AdjustFocus(int Direction)
{
	if (!IsActive() || !FocusEnabled || Direction == 0)
	{
		return false;
	}

	FocusTarget = clampr(FocusTarget + (Direction > 0 ? FocusStep : -FocusStep), 0.0f, 1.0f);
	return true;
}

bool FlashlightBeam::IsFocusKeyHeld()
{
	const EGameActions Id = Hotkeys::Find("flashlight_focus");
	if (Id == kNOTBINDED)
	{
		return false;
	}

	const bool Gamepad = pInput->GetControllerMode();
	for (int Index = 0; Index < 2; ++Index)
	{
		const int Dik = get_action_dik(Id, Index);
		if (Dik > 0 && (Gamepad ? pInput->iGetAsyncGamepadKeyState(Dik) : pInput->iGetAsyncKeyState(Dik)))
		{
			return true;
		}
	}

	return false;
}

void FlashlightBeam::UpdateLight(IRender_Light* Light, const Fvector& ProbePosition, const Fcolor* AnimatedColor, float TimeDelta)
{
	InterferenceLevel = Interference.Update(ProbePosition, TimeDelta);

	if (Light == nullptr)
	{
		return;
	}

	if (TimeDelta > 0.0f)
	{
		Focus += (FocusTarget - Focus) * FlashlightBeamMath::Smooth(TimeDelta, FocusSpeed > EPS ? 1.0f / FocusSpeed : 0.0f);
	}

	LightBeamParams Params = Beam;
	float TargetRange = LegacyRange;
	float TargetCone = LegacyCone;

	if (IsActive())
	{
		TargetRange = Range > 0.0f ? Range : TargetRange;
		TargetCone = SpotAngle > 0.0f ? SpotAngle : TargetCone;

		if (FocusEnabled)
		{
			const float FocusCone = FocusSpotAngle.x + (FocusSpotAngle.y - FocusSpotAngle.x) * Focus;
			const float FocusDistance = FocusRange.x + (FocusRange.y - FocusRange.x) * Focus;
			TargetCone = FocusCone > 0.0f ? FocusCone : TargetCone;
			TargetRange = FocusDistance > 0.0f ? FocusDistance : TargetRange;
			Params.Intensity *= FocusIntensity.x + (FocusIntensity.y - FocusIntensity.x) * Focus;
			Params.ReferenceDistance = FocusReferenceDistance.x + (FocusReferenceDistance.y - FocusReferenceDistance.x) * Focus;
			Params.BeamSize = FocusBeamSize.x + (FocusBeamSize.y - FocusBeamSize.x) * Focus;
		}
	}
	else
	{
		Params.Enabled = false;
	}

	Light->SetBeam(Params);

	if (TargetRange > 0.0f)
	{
		Light->set_range(TargetRange);
	}

	if (TargetCone > 0.0f)
	{
		Light->set_cone(std::min(TargetCone, deg2rad(120.0f)));
	}

	Fcolor FinalColor = GetBaseColor(AnimatedColor);
	FinalColor.mul_rgb(InterferenceLevel);
	Light->set_color(FinalColor);
}

Fvector FlashlightBeam::GetEyePosition() const
{
	Fvector Position = Device.vCameraPosition;
	Position.mad(Device.vCameraRight, EyeOffset.x);
	Position.mad(Device.vCameraTop, EyeOffset.y);
	Position.mad(Device.vCameraDirection, EyeOffset.z);
	return Position;
}

void FlashlightBeam::Reset()
{
	Initialized = false;
	AngularVelocity.set(0.0f, 0.0f);
	Amplitude = 0.0f;
	Frequency = WalkFrequency;
	HandsItem = nullptr;
	HandsOffset.set(0.0f, 0.0f);
}

void FlashlightBeam::StepLag(float& Angle, float& Velocity, float Target, float TimeDelta) const
{
	const float Offset = angle_normalize_signed(Angle - Target);
	const float Decay = std::exp(-LagFrequency * TimeDelta);
	const float Impulse = (Velocity + LagFrequency * Offset) * TimeDelta;

	Velocity = (Velocity - LagFrequency * Impulse) * Decay;
	float NewOffset = (Offset + Impulse) * Decay;

	if (std::abs(NewOffset) > LagMaxAngle)
	{
		NewOffset = FlashlightBeamMath::ClampAngle(NewOffset, LagMaxAngle);
		Velocity = 0.0f;
	}

	Angle = angle_normalize_signed(Target + NewOffset);
}

void FlashlightBeam::UpdateWalk(CActor* Actor, float TimeDelta)
{
	float Speed = 0.0f;
	if (CCharacterPhysicsSupport* Support = Actor->character_physics_support())
	{
		if (CPHMovementControl* Movement = Support->movement())
		{
			const Fvector Velocity = Movement->GetVelocity();
			Speed = _sqrt(Velocity.x * Velocity.x + Velocity.z * Velocity.z);
		}
	}

	const float MoveWeight = FlashlightBeamMath::SmoothStep(0.2f, WalkSpeed * 0.6f, Speed);
	const float RunWeight = FlashlightBeamMath::SmoothStep(WalkSpeed, RunSpeed, Speed);
	const float TargetAmplitude = MoveWeight * (WalkAmplitude + (RunAmplitude - WalkAmplitude) * RunWeight);
	const float TargetFrequency = WalkFrequency + (RunFrequency - WalkFrequency) * RunWeight;
	const float Blend = FlashlightBeamMath::Smooth(TimeDelta, FadeTime);

	Amplitude += (TargetAmplitude - Amplitude) * Blend;
	Frequency += (TargetFrequency - Frequency) * Blend;
	Phase = std::fmod(Phase + PI_MUL_2 * Frequency * TimeDelta, PI_MUL_2);
}

void FlashlightBeam::UpdateHands(CActor* Actor, float TimeDelta)
{
	CHudItem* HudItem = nullptr;
	if (PIItem Item = Actor->inventory().ActiveItem())
	{
		HudItem = Item->cast_hud_item();
	}

	attachable_hud_item* Data = HudItem != nullptr && HudItem->GetHUDmode() ? HudItem->HudItemData() : nullptr;
	Fvector2 Target = { 0.0f, 0.0f };

	if (Data != nullptr && HandsFactor > 0.0f)
	{
		float ItemH = 0.0f, ItemP = 0.0f, CameraH = 0.0f, CameraP = 0.0f;
		Data->m_item_transform.k.getHP(ItemH, ItemP);
		Device.vCameraDirection.getHP(CameraH, CameraP);

		const Fvector2 Raw = { angle_normalize_signed(ItemH - CameraH), angle_normalize_signed(ItemP - CameraP) };

		if (HandsItem != Data)
		{
			HandsItem = Data;
			HandsBaseline = Raw;
		}

		const float BaselineBlend = FlashlightBeamMath::Smooth(TimeDelta, HandsBaselineTime);
		HandsBaseline.x += angle_normalize_signed(Raw.x - HandsBaseline.x) * BaselineBlend;
		HandsBaseline.y += angle_normalize_signed(Raw.y - HandsBaseline.y) * BaselineBlend;

		Target.x = FlashlightBeamMath::ClampAngle(angle_normalize_signed(Raw.x - HandsBaseline.x), HandsMaxAngle);
		Target.y = FlashlightBeamMath::ClampAngle(angle_normalize_signed(Raw.y - HandsBaseline.y), HandsMaxAngle);
	}
	else
	{
		HandsItem = nullptr;
	}

	const float Blend = FlashlightBeamMath::Smooth(TimeDelta, HandsSmoothTime);
	HandsOffset.x += (Target.x - HandsOffset.x) * Blend;
	HandsOffset.y += (Target.y - HandsOffset.y) * Blend;
}

float FlashlightBeam::GetAimFactor(CActor* Actor) const
{
	PIItem Item = Actor->inventory().ActiveItem();
	CHudItem* HudItem = Item != nullptr ? Item->cast_hud_item() : nullptr;
	return HudItem != nullptr ? clampr(HudItem->GetAimFactor(), 0.0f, 1.0f) : 0.0f;
}

Fvector2 FlashlightBeam::UpdateDirection(CActor* Actor, float TimeDelta)
{
	CCameraBase* Camera = Actor->cam_Active();
	const Fvector2 Target = { -Camera->yaw, -Camera->pitch };

	if (!Initialized)
	{
		Initialized = true;
		Angles = Target;
		AngularVelocity.set(0.0f, 0.0f);
	}

	const float Step = clampr(TimeDelta, 0.0f, 0.05f);
	if (Step > 0.0f)
	{
		StepLag(Angles.x, AngularVelocity.x, Target.x, Step);
		StepLag(Angles.y, AngularVelocity.y, Target.y, Step);
		UpdateWalk(Actor, Step);
		UpdateHands(Actor, Step);
	}

	const float Damping = 1.0f - clampr(AimDamping, 0.0f, 1.0f) * GetAimFactor(Actor);

	Fvector2 Offset;
	Offset.x = Amplitude * std::sin(Phase) + HandsOffset.x * HandsFactor;
	Offset.y = Amplitude * VerticalRatio * std::sin(2.0f * Phase) + HandsOffset.y * HandsFactor;
	Offset.mul(Damping);

	return { Angles.x + Offset.x, Angles.y + Offset.y };
}
