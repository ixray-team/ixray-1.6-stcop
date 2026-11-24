#pragma once
#include "../New/SoundShader.h"
#include "SoundBiquad.h"

// Waveshaper with a tone filter after it
XREFLECT(SoundShader, Name="distortion")
class SoundDistortionShader : public ISoundShader
{
	XSOUND_GENERATED_BODY()

public:
	// Input gain into the shaper, 1 is clean
	XPROPERTY(SoundParam, Name="drive", Min=1, Max=50)
	float Drive = 4.0f;

	// 0 soft (tanh), 1 hard clip, 2 tube (asymmetric), 3 fold
	XPROPERTY(SoundParam, Name="mode", Min=0, Max=3)
	float Mode = 0.0f;

	// Low-pass after the shaper, 20000 is off
	XPROPERTY(SoundParam, Name="tone", Min=200, Max=20000)
	float ToneHz = 20000.0f;

	XPROPERTY(SoundParam, Name="gain", Min=0, Max=4)
	float Gain = 1.0f;

	// Dry/wet
	XPROPERTY(SoundParam, Name="mix", Min=0, Max=1)
	float Mix = 1.0f;

	void Reset() override;
	void Process(float** Data, u32 Frames, const SoundShaderContext& Context) override;

private:
	// 0 until the first block
	float CurrentDrive = 0.0f;
	float DcIn[SND_CHANNEL_COUNT] = {};
	float DcOut[SND_CHANNEL_COUNT] = {};
	float Wet[SND_CHANNEL_COUNT][XRay::Sound::Shader::AutomationStep] = {};
	XRay::Sound::Biquad::Cascade Tone{XRay::Sound::Biquad::Cascade::Kind::LowPass};
};
