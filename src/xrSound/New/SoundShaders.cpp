#include "SoundShaders.h"

namespace XRay::Sound::Biquad
{
	namespace
	{
		struct Angle
		{
			float Cos;
			float Alpha;
		};

		Angle MakeAngle(float CutoffHz, float Q)
		{
			const float Nyquist = (float)SND_SAMPLERATE * 0.5f;
			const float Cutoff = std::clamp(CutoffHz, 10.0f, Nyquist * 0.95f);
			const float Omega = 2.0f * PI * Cutoff / (float)SND_SAMPLERATE;
			return {std::cos(Omega), std::sin(Omega) / (2.0f * std::max(Q, 0.05f))};
		}

		Coefficients Normalize(float B0, float B1, float B2, float A0, float A1, float A2)
		{
			const float InvA0 = 1.0f / A0;
			return {B0 * InvA0, B1 * InvA0, B2 * InvA0, A1 * InvA0, A2 * InvA0};
		}
	}

	Coefficients LowPass(float CutoffHz, float Q)
	{
		const Angle W = MakeAngle(CutoffHz, Q);
		const float B1 = 1.0f - W.Cos;
		return Normalize(B1 * 0.5f, B1, B1 * 0.5f, 1.0f + W.Alpha, -2.0f * W.Cos, 1.0f - W.Alpha);
	}

	Coefficients HighPass(float CutoffHz, float Q)
	{
		const Angle W = MakeAngle(CutoffHz, Q);
		const float B1 = 1.0f + W.Cos;
		return Normalize(B1 * 0.5f, -B1, B1 * 0.5f, 1.0f + W.Alpha, -2.0f * W.Cos, 1.0f - W.Alpha);
	}
}

namespace
{
	// One-pole smoothing coefficient for a cutoff at the mixer sample rate
	float LowPassAlpha(float CutoffHz)
	{
		if (CutoffHz >= XRay::Sound::LowPass::MaxCutoffHz)
		{
			return 1.0f;
		}

		const float Cutoff = std::max(CutoffHz, XRay::Sound::LowPass::MinCutoffHz);
		return 1.0f - std::exp(-2.0f * PI * Cutoff / (float)SND_SAMPLERATE);
	}
}

void SoundLowPassShader::Reset()
{
	Alpha = 1.0f;
	memset(State, 0, sizeof(State));
}

void SoundLowPassShader::Process(float** Data, u32 Frames, const SoundShaderContext& Context)
{
	const float TargetAlpha = LowPassAlpha(CutoffHz);
	const float BeginAlpha = Alpha;
	Alpha = TargetAlpha;

	if (Frames == 0 || (BeginAlpha >= 1.0f && TargetAlpha >= 1.0f))
	{
		// Fully open: drop history so the next engage starts clean
		memset(State, 0, sizeof(State));
		return;
	}

	const float Step = (TargetAlpha - BeginAlpha) / (float)Frames;
	for (u32 Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
	{
		float* Samples = Data[Channel];
		float Stage0 = State[Channel][0];
		float Stage1 = State[Channel][1];
		float CurrentAlpha = BeginAlpha;
		for (u32 Frame = 0; Frame < Frames; Frame++)
		{
			CurrentAlpha += Step;
			Stage0 += CurrentAlpha * (Samples[Frame] - Stage0);
			Stage1 += CurrentAlpha * (Stage0 - Stage1);
			Samples[Frame] = Stage1;
		}
		State[Channel][0] = Stage0;
		State[Channel][1] = Stage1;
	}
}

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
