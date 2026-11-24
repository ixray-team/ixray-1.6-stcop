#pragma once
#include "../New/SoundShader.h"
#include "SoundBiquad.h"

// Three bell (peaking) bands and an output gain. A band at 0 dB is skipped
XREFLECT(SoundShader, Name="eq")
class SoundEqShader : public ISoundShader
{
	XSOUND_GENERATED_BODY()

public:
	XPROPERTY(SoundParam, Name="freq1", Min=20, Max=20000)
	float Freq1 = 250.0f;

	XPROPERTY(SoundParam, Name="gain1", Min=-24, Max=24)
	float Gain1 = 0.0f;

	XPROPERTY(SoundParam, Name="q1", Min=0.1, Max=18)
	float Q1 = 1.0f;

	XPROPERTY(SoundParam, Name="freq2", Min=20, Max=20000)
	float Freq2 = 1000.0f;

	XPROPERTY(SoundParam, Name="gain2", Min=-24, Max=24)
	float Gain2 = 0.0f;

	XPROPERTY(SoundParam, Name="q2", Min=0.1, Max=18)
	float Q2 = 1.0f;

	XPROPERTY(SoundParam, Name="freq3", Min=20, Max=20000)
	float Freq3 = 4000.0f;

	XPROPERTY(SoundParam, Name="gain3", Min=-24, Max=24)
	float Gain3 = 0.0f;

	XPROPERTY(SoundParam, Name="q3", Min=0.1, Max=18)
	float Q3 = 1.0f;

	// dB
	XPROPERTY(SoundParam, Name="output", Min=-24, Max=24)
	float Output = 0.0f;

	void Reset() override;
	void Process(float** Data, u32 Frames, const SoundShaderContext& Context) override;

private:
	static constexpr u32 BandCount = 3;

	struct Band
	{
		// 0 until the band is engaged
		float Freq = 0.0f;
		float Gain = 0.0f;
		float Q = 1.0f;
		XRay::Sound::Biquad::Coefficients Coeffs;
		XRay::Sound::Biquad::State States[SND_CHANNEL_COUNT];
	};

	void ProcessBand(Band& Filter, float** Data, u32 Frames, float Freq, float Gain, float Q);

	Band Bands[BandCount];
	// Linear, 0 until the first block
	float OutputGain = 0.0f;
};
