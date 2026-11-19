#pragma once
#include "SoundMeta.h"

namespace XRay::Sound::LowPass
{
	// Cutoff at or above this value means "no filtering"
	constexpr float MaxCutoffHz = 20000.0f;
	constexpr float MinCutoffHz = 20.0f;

	// One-pole smoothing coefficient for a cutoff at the mixer sample rate
	inline float AlphaFromCutoff(float CutoffHz)
	{
		if (CutoffHz >= MaxCutoffHz)
		{
			return 1.0f;
		}

		const float Cutoff = std::max(CutoffHz, MinCutoffHz);
		return 1.0f - std::exp(-2.0f * PI * Cutoff / (float)SND_SAMPLERATE);
	}

	// Maps a 0..1 factor to a cutoff in log space: 0 -> MinHz, 1 -> MaxHz
	inline float CutoffFromFactor(float Factor, float MinHz, float MaxHz = MaxCutoffHz)
	{
		Factor = std::clamp(Factor, 0.0f, 1.0f);
		return std::exp(std::log(MinHz) + (std::log(MaxHz) - std::log(MinHz)) * Factor);
	}
}

// Self-contained 12 dB/oct low-pass (two cascaded one-pole stages) for planar float buffers.
// Cutoff changes are ramped across one Process call, so it is safe to retune every block.
template <u32 ChannelCount = SND_CHANNEL_COUNT>
class SoundLowPassFilter
{
public:
	void Reset()
	{
		Alpha = 1.0f;
		for (auto& Channel : State)
		{
			Channel[0] = 0.0f;
			Channel[1] = 0.0f;
		}
	}

	bool IsBypassed() const
	{
		return Alpha >= 1.0f;
	}

	void Process(float** Data, u32 Frames, float CutoffHz)
	{
		const float TargetAlpha = XRay::Sound::LowPass::AlphaFromCutoff(CutoffHz);
		const float BeginAlpha = Alpha;
		Alpha = TargetAlpha;

		if (Frames == 0 || (BeginAlpha >= 1.0f && TargetAlpha >= 1.0f))
		{
			// Fully open: drop history so the next engage starts clean
			for (auto& Channel : State)
			{
				Channel[0] = 0.0f;
				Channel[1] = 0.0f;
			}
			return;
		}

		const float Step = (TargetAlpha - BeginAlpha) / (float)Frames;
		for (u32 Channel = 0; Channel < ChannelCount; Channel++)
		{
			float* Samples = Data[Channel];
			float Stage0 = State[Channel][0];
			float Stage1 = State[Channel][1];
			float CurrentAlpha = BeginAlpha;
			for (u32 Idx = 0; Idx < Frames; Idx++)
			{
				CurrentAlpha += Step;
				Stage0 += CurrentAlpha * (Samples[Idx] - Stage0);
				Stage1 += CurrentAlpha * (Stage0 - Stage1);
				Samples[Idx] = Stage1;
			}
			State[Channel][0] = Stage0;
			State[Channel][1] = Stage1;
		}
	}

private:
	float State[ChannelCount][2] = {};
	float Alpha = 1.0f;
};
