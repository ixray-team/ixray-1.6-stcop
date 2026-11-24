#pragma once
#include "../New/SoundShader.h"
#include "SoundBiquad.h"

// Resonant low-pass, 12 dB/oct per slope step. Cutoff changes glide over a block;
// at XRay::Sound::LowPass::MaxCutoffHz it is bypassed
XREFLECT(SoundShader, Name="low_pass")
class SoundLowPassShader : public ISoundShader
{
	XSOUND_GENERATED_BODY()

public:
	// Min / Max match XRay::Sound::LowPass::MinCutoffHz / MaxCutoffHz
	XPROPERTY(SoundParam, Name="cutoff", Min=20, Max=20000)
	float CutoffHz = 1000.0f;

	// 0.5 soft (two one-pole stages), 0.7071 flat, higher resonates
	XPROPERTY(SoundParam, Name="q", Min=0.3, Max=12)
	float Q = 0.5f;

	// dB/oct: 12, 24, 36 or 48
	XPROPERTY(SoundParam, Name="slope", Min=12, Max=48)
	float Slope = 12.0f;

	void Reset() override;
	void Process(float** Data, u32 Frames, const SoundShaderContext& Context) override;

private:
	XRay::Sound::Biquad::Cascade Filter{XRay::Sound::Biquad::Cascade::Kind::LowPass};
};
