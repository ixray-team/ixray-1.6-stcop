#include "SoundCompressorShader.h"
#include "SoundShaderUtils.h"
#include "../New/SoundDSP.h"

using namespace XRay::Sound::ShaderUtils;

void SoundCompressorShader::Reset()
{
	for (float& Value : Envelope)
	{
		Value = FLT_EPSILON;
	}
	MakeupGain = 0.0f;
}

void SoundCompressorShader::Process(float** Data, u32 Frames, const SoundShaderContext& Context)
{
	if (Frames == 0)
	{
		return;
	}

	if (Mix > 0.0f)
	{
		DSP_Compressor(Attack, Release, Threshold, Ratio, Data, Mix, Frames, Envelope);
	}

	const float TargetGain = DbToGain(Makeup);
	const float BeginGain = MakeupGain != 0.0f ? MakeupGain : TargetGain;
	MakeupGain = TargetGain;
	if (BeginGain == 1.0f && TargetGain == 1.0f)
	{
		return;
	}

	const float Step = (TargetGain - BeginGain) / (float)Frames;
	for (u32 Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
	{
		float CurrentGain = BeginGain;
		for (u32 Frame = 0; Frame < Frames; Frame++)
		{
			CurrentGain += Step;
			Data[Channel][Frame] *= CurrentGain;
		}
	}
}
