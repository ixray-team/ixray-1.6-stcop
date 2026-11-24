#pragma once
#include "../New/SoundShader.h"
#include "SoundBiquad.h"

// Walkie-talkie: narrow band-pass, saturation, hiss and crackle
XREFLECT(SoundShader, Name="radio")
class SoundRadioShader : public ISoundShader
{
	XSOUND_GENERATED_BODY()

public:
	// Band-pass edges
	XPROPERTY(SoundParam, Name="low_cut", Min=20, Max=4000)
	float LowCutHz = 400.0f;

	XPROPERTY(SoundParam, Name="high_cut", Min=500, Max=16000)
	float HighCutHz = 3200.0f;

	// Saturation amount, 1 is clean
	XPROPERTY(SoundParam, Name="drive", Min=1, Max=30)
	float Drive = 4.0f;

	// Constant hiss level
	XPROPERTY(SoundParam, Name="noise", Min=0, Max=0.5)
	float Noise = 0.015f;

	// Static bursts: density and loudness
	XPROPERTY(SoundParam, Name="crackle", Min=0, Max=1)
	float Crackle = 0.2f;

	// Stereo collapse, 1 is mono
	XPROPERTY(SoundParam, Name="mono", Min=0, Max=1)
	float Mono = 1.0f;

	XPROPERTY(SoundParam, Name="gain", Min=0, Max=4)
	float Gain = 0.8f;

	// Dry/wet
	XPROPERTY(SoundParam, Name="mix", Min=0, Max=1)
	float Mix = 1.0f;

	SoundRadioShader();

	void Reset() override;
	void OnParamsChanged() override;
	void Process(float** Data, u32 Frames, const SoundShaderContext& Context) override;

private:
	static constexpr u32 StageCount = 2;

	float Random();

	XRay::Sound::Biquad::Coefficients HighPassCoeffs;
	XRay::Sound::Biquad::Coefficients LowPassCoeffs;
	XRay::Sound::Biquad::State HighPassState[SND_CHANNEL_COUNT][StageCount];
	XRay::Sound::Biquad::State LowPassState[SND_CHANNEL_COUNT][StageCount];
	float DriveNorm = 1.0f;
	u32 Seed = 0x9E3779B9u;
	u32 CrackleFrames = 0;
	float CrackleLevel = 0.0f;
};
