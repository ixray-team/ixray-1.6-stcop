#pragma once
#include "../New/SoundShader.h"

// Doppler shift of a 3D voice: changes its playback rate from the listener and voice velocities.
// The mixer glides the rate over each block. Voices only; 2D voices drift back to 1, on a bus it does nothing
XREFLECT(SoundShader, Name="doppler")
class SoundDopplerShader : public ISoundShader
{
	XSOUND_GENERATED_BODY()

public:
	// Scale of the velocities, multiplied by snd_doppler
	XPROPERTY(SoundParam, Name="amount", Min=0, Max=10)
	float Amount = 1.0f;

	// Speed of sound, m/s
	XPROPERTY(SoundParam, Name="speed", Min=50, Max=2000)
	float SpeedOfSound = 343.0f;

	// Seconds the shift takes to follow a velocity change, on top of the mixer velocity smoothing (80 ms)
	XPROPERTY(SoundParam, Name="smooth", Min=0, Max=1)
	float Smooth = 0.03f;

	XPROPERTY(SoundParam, Name="min_pitch", Min=0.25, Max=1)
	float MinPitch = 0.5f;

	XPROPERTY(SoundParam, Name="max_pitch", Min=1, Max=4)
	float MaxPitch = 2.0f;

	void Reset() override;
	void Process(float** Data, u32 Frames, const SoundShaderContext& Context) override;

private:
	// 0 until the first block: the voice starts at its shift instead of gliding in from 1
	float Current = 0.0f;
};
