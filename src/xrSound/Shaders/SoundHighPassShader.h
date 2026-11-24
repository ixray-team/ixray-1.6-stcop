#pragma once
#include "../New/SoundShader.h"
#include "SoundBiquad.h"

// Resonant high-pass, 12 dB/oct per slope step. Bypassed at XRay::Sound::LowPass::MinCutoffHz
XREFLECT(SoundShader, Name="high_pass")
class SoundHighPassShader : public ISoundShader
{
	XSOUND_GENERATED_BODY()

public:
	XPROPERTY(SoundParam, Name="cutoff", Min=20, Max=20000)
	float CutoffHz = 200.0f;

	XPROPERTY(SoundParam, Name="q", Min=0.3, Max=12)
	float Q = 0.7071f;

	XPROPERTY(SoundParam, Name="slope", Min=12, Max=48)
	float Slope = 12.0f;

	void Reset() override;
	void Process(float** Data, u32 Frames, const SoundShaderContext& Context) override;

private:
	XRay::Sound::Biquad::Cascade Filter{XRay::Sound::Biquad::Cascade::Kind::HighPass};
};
