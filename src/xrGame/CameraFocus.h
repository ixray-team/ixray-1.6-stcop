#pragma once

#include <cmath>

struct CameraFocusParams
{
	float Fov = 35.f;
	float BlendIn = 1.f;
	float HoldTime = -1.f;
	float BlendOut = 0.5f;
	float TrackingSpeed = 8.f;
	bool LockInput = true;
	bool Track = true;
	bool RestoreDirection = false;

	bool IsValid() const
	{
		return std::isfinite(Fov) && Fov >= 5.f && Fov <= 170.f && std::isfinite(BlendIn) && BlendIn >= 0.f && std::isfinite(HoldTime) && (HoldTime >= 0.f || HoldTime == -1.f) && std::isfinite(BlendOut) && BlendOut >= 0.f && std::isfinite(TrackingSpeed) && TrackingSpeed > 0.f;
	}
};

// Pure transition state; the actor owns target lifetime and camera/input integration.
class CameraFocus final
{
public:
	enum class Phase
	{
		Idle,
		Entering,
		Holding,
		Leaving
	};

	CameraFocus() = default;
	~CameraFocus() = default;

	bool Start(const CameraFocusParams& NewParams, float CameraYaw, float CameraPitch, float CameraFov)
	{
		if (!NewParams.IsValid() || !std::isfinite(CameraYaw) || !std::isfinite(CameraPitch) || !std::isfinite(CameraFov) || CameraFov < 5.f || CameraFov > 170.f)
		{
			return false;
		}
		// Replacing a focus preserves the original return direction.
		if (!IsActive())
		{
			OriginalYaw = CameraYaw;
			OriginalPitch = CameraPitch;
		}
		Params = NewParams;
		Yaw = CameraYaw;
		Pitch = CameraPitch;
		Fov = StartFov = CameraFov;
		Elapsed = 0.f;
		CurrentPhase = Phase::Entering;
		return true;
	}

	void Stop(float Duration = -1.f)
	{
		if (!IsActive() || CurrentPhase == Phase::Leaving || !std::isfinite(Duration) || (Duration < 0.f && Duration != -1.f))
		{
			return;
		}
		ExitDuration = Duration == -1.f ? Params.BlendOut : Duration;
		StartFov = Fov;
		ExitYaw = Yaw;
		ExitPitch = Pitch;
		Elapsed = 0.f;
		CurrentPhase = Phase::Leaving;
		if (ExitDuration == 0.f)
		{
			if (Params.RestoreDirection)
			{
				Yaw = OriginalYaw;
				Pitch = OriginalPitch;
			}
			Reset();
		}
	}

	void Reset()
	{
		CurrentPhase = Phase::Idle;
		Elapsed = 0.f;
	}

	void Update(float Delta, float TargetYaw, float TargetPitch, float NaturalFov)
	{
		if (!IsActive() || !std::isfinite(Delta) || Delta < 0.f)
		{
			return;
		}
		// Consume remaining frame time across phase boundaries, including zero durations.
		while (IsActive())
		{
			if (CurrentPhase == Phase::Entering)
			{
				const float Step = Smaller(Delta, Params.BlendIn - Elapsed);
				const float Previous = Progress(Elapsed, Params.BlendIn);
				Elapsed += Step;
				Delta -= Step;
				const float Blend = Progress(Elapsed, Params.BlendIn);
				const float TurnBlend = Previous < 1.f ? (Blend - Previous) / (1.f - Previous) : 1.f;
				TurnTo(TargetYaw, TargetPitch, TurnBlend);
				Fov = StartFov + (Params.Fov - StartFov) * Blend;
				if (Elapsed < Params.BlendIn)
				{
					return;
				}
				CurrentPhase = Phase::Holding;
				Elapsed = 0.f;
			}
			else if (CurrentPhase == Phase::Holding)
			{
				const float Step = Params.HoldTime < 0.f ? Delta : Smaller(Delta, Params.HoldTime - Elapsed);
				TurnTo(TargetYaw, TargetPitch, 1.f - std::exp(-Params.TrackingSpeed * Step));
				Fov = Params.Fov;
				Elapsed += Step;
				Delta -= Step;
				if (Params.HoldTime < 0.f || Elapsed < Params.HoldTime)
				{
					return;
				}
				Stop();
			}
			else
			{
				Elapsed += Delta;
				const float Blend = Progress(Elapsed, ExitDuration);
				Fov = StartFov + (NaturalFov - StartFov) * Blend;
				if (Params.RestoreDirection)
				{
					Yaw = ExitYaw + AngleDelta(OriginalYaw, ExitYaw) * Blend;
					Pitch = ExitPitch + AngleDelta(OriginalPitch, ExitPitch) * Blend;
				}
				if (Elapsed >= ExitDuration)
				{
					Reset();
				}
				return;
			}
		}
	}

	bool IsActive() const { return CurrentPhase != Phase::Idle; }
	bool IsInputLocked() const { return IsActive() && Params.LockInput; }
	Phase GetPhase() const { return CurrentPhase; }
	float GetYaw() const { return Yaw; }
	float GetPitch() const { return Pitch; }
	float GetFov() const { return Fov; }

private:
	static float Smaller(float First, float Second) { return First < Second ? First : Second; }
	static float AngleDelta(float Target, float Source) { return std::remainder(Target - Source, 6.2831853071795864769f); }
	static float Progress(float Time, float Duration)
	{
		const float Linear = Duration > 0.f ? Smaller(Time / Duration, 1.f) : 1.f;
		return Linear * Linear * (3.f - 2.f * Linear);
	}
	void TurnTo(float TargetYaw, float TargetPitch, float Blend)
	{
		Yaw += AngleDelta(TargetYaw, Yaw) * Blend;
		Pitch += AngleDelta(TargetPitch, Pitch) * Blend;
	}

	CameraFocusParams Params;
	Phase CurrentPhase = Phase::Idle;
	float Elapsed = 0.f;
	float ExitDuration = 0.f;
	float OriginalYaw = 0.f;
	float OriginalPitch = 0.f;
	float ExitYaw = 0.f;
	float ExitPitch = 0.f;
	float StartFov = 0.f;
	float Yaw = 0.f;
	float Pitch = 0.f;
	float Fov = 0.f;
};
