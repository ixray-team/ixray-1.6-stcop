#include "SoundRadioShader.h"
#include "SoundShaderUtils.h"

using namespace XRay::Sound::ShaderUtils;

namespace
{
	constexpr float RadioQ = 0.7071f;

	// Expected crackle bursts per second at crackle = 1
	constexpr float CrackleRate = 6.0f;
	constexpr u32 CrackleMinFrames = SND_SAMPLERATE / 1000;
	constexpr u32 CrackleMaxFrames = SND_SAMPLERATE / 40;

	float SoftClip(float Value)
	{
		return std::tanh(Value);
	}
}

SoundRadioShader::SoundRadioShader()
{
	OnParamsChanged();
}

float SoundRadioShader::Random()
{
	// xorshift32, uniform in [0, 1)
	Seed ^= Seed << 13;
	Seed ^= Seed >> 17;
	Seed ^= Seed << 5;
	return (float)(Seed >> 8) * (1.0f / 16777216.0f);
}

void SoundRadioShader::Reset()
{
	for (u32 Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
	{
		for (u32 Stage = 0; Stage < StageCount; Stage++)
		{
			HighPassState[Channel][Stage] = {};
			LowPassState[Channel][Stage] = {};
		}
	}

	CrackleFrames = 0;
	CrackleLevel = 0.0f;
}

void SoundRadioShader::OnParamsChanged()
{
	const float HighCut = std::max(HighCutHz, LowCutHz + 100.0f);
	HighPassCoeffs = XRay::Sound::Biquad::HighPass(LowCutHz, RadioQ);
	LowPassCoeffs = XRay::Sound::Biquad::LowPass(HighCut, RadioQ);

	// Full-scale input stays at full scale after the saturation
	DriveNorm = 1.0f / SoftClip(Drive);
}

void SoundRadioShader::Process(float** Data, u32 Frames, const SoundShaderContext& Context)
{
	if (Mix <= 0.0f)
	{
		return;
	}

	// A bus that only renders a tail gets no hiss, otherwise the radio would never fall silent
	const bool AddNoise = Context.HasInput;
	const float CrackleChance = Crackle * CrackleRate / (float)SND_SAMPLERATE;
	const float InvChannels = 1.0f / (float)SND_CHANNEL_COUNT;

	for (u32 Frame = 0; Frame < Frames; Frame++)
	{
		float Interference = 0.0f;
		if (AddNoise)
		{
			Interference = (Random() * 2.0f - 1.0f) * Noise;

			if (CrackleFrames == 0 && CrackleChance > 0.0f && Random() < CrackleChance)
			{
				CrackleFrames = CrackleMinFrames + (u32)(Random() * (float)(CrackleMaxFrames - CrackleMinFrames));
				CrackleLevel = (0.1f + 0.4f * Random()) * Crackle;
			}

			if (CrackleFrames > 0)
			{
				// Sparse impulses sound like static rather than plain hiss
				if (Random() < 0.25f)
				{
					Interference += (Random() * 2.0f - 1.0f) * CrackleLevel;
				}
				CrackleFrames--;
			}
		}

		float MonoSample = 0.0f;
		for (u32 Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
		{
			MonoSample += Data[Channel][Frame];
		}
		MonoSample *= InvChannels;

		for (u32 Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
		{
			const float Dry = Data[Channel][Frame];
			float Wet = lerp(Dry, MonoSample, Mono);

			for (u32 Stage = 0; Stage < StageCount; Stage++)
			{
				Wet = XRay::Sound::Biquad::Process(HighPassCoeffs, HighPassState[Channel][Stage], Wet);
			}

			Wet = SoftClip(Wet * Drive) * DriveNorm + Interference;

			for (u32 Stage = 0; Stage < StageCount; Stage++)
			{
				Wet = XRay::Sound::Biquad::Process(LowPassCoeffs, LowPassState[Channel][Stage], Wet);
			}

			Data[Channel][Frame] = lerp(Dry, Wet * Gain, Mix);
		}
	}
}
