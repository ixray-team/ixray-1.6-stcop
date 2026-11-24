#pragma once
#include "../New/SoundShader.h"

// Opto-style downward compressor with makeup gain. Also the "compressor" bus effect (master, zone buses, configs):
// the first five parameters keep their order and ranges
XREFLECT(SoundShader, Name="compressor")
class SoundCompressorShader : public ISoundShader
{
	XSOUND_GENERATED_BODY()

public:
	// Seconds
	XPROPERTY(SoundParam, Name="attack", Min=0, Max=1)
	float Attack = 0.0001f;

	// Seconds
	XPROPERTY(SoundParam, Name="release", Min=0, Max=5)
	float Release = 0.1f;

	// dB
	XPROPERTY(SoundParam, Name="threshold", Min=-60, Max=0)
	float Threshold = -20.0f;

	XPROPERTY(SoundParam, Name="ratio", Min=1, Max=20)
	float Ratio = 2.0f;

	// Amount of the gain reduction, 0 bypasses
	XPROPERTY(SoundParam, Name="mix", Min=0, Max=1)
	float Mix = 1.0f;

	// dB after the compression
	XPROPERTY(SoundParam, Name="makeup", Min=0, Max=24)
	float Makeup = 0.0f;

	void Reset() override;
	void Process(float** Data, u32 Frames, const SoundShaderContext& Context) override;

private:
	float Envelope[SND_CHANNEL_COUNT] = {};
	// Linear, 0 until the first block
	float MakeupGain = 0.0f;
};
