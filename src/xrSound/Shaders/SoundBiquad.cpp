#include "SoundBiquad.h"
#include "SoundShaderUtils.h"
#include "../New/SoundMixer.h"

using namespace XRay::Sound::ShaderUtils;

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

	Coefficients Peak(float FreqHz, float Q, float GainDb)
	{
		const Angle W = MakeAngle(FreqHz, Q);
		const float A = std::pow(10.0f, GainDb / 40.0f);
		return Normalize(1.0f + W.Alpha * A, -2.0f * W.Cos, 1.0f - W.Alpha * A, 1.0f + W.Alpha / A, -2.0f * W.Cos, 1.0f - W.Alpha / A);
	}

	namespace
	{
		constexpr float ButterworthQ = 0.70710678f;

		// Q of stage Index in a Butterworth cascade of Count biquads
		float StageQ(u32 Index, u32 Count)
		{
			const float Angle = PI * (float)(2 * Index + 1) / (float)(4 * Count);
			return 1.0f / (2.0f * std::cos(Angle));
		}
	}

	bool Cascade::IsBypassed(float CutoffHz) const
	{
		return FilterKind == Kind::LowPass ? CutoffHz >= XRay::Sound::LowPass::MaxCutoffHz : CutoffHz <= XRay::Sound::LowPass::MinCutoffHz;
	}

	void Cascade::Reset()
	{
		StageCount = 0;
		CurrentHz = 0.0f;
		CurrentQ = ButterworthQ;
		memset(States, 0, sizeof(States));
	}

	void Cascade::UpdateCoefficients(float CutoffHz, float Q)
	{
		const float Resonance = Q / ButterworthQ;
		for (u32 Stage = 0; Stage < StageCount; Stage++)
		{
			// Only the last stage resonates, otherwise high Q values would multiply
			const float Scale = Stage + 1 == StageCount ? Resonance : std::min(Resonance, 1.0f);
			const float StageQValue = StageQ(Stage, StageCount) * Scale;
			Coeffs[Stage] = FilterKind == Kind::LowPass ? LowPass(CutoffHz, StageQValue) : HighPass(CutoffHz, StageQValue);
		}
	}

	void Cascade::Process(float** Data, u32 Frames, float CutoffHz, float Q, u32 Stages)
	{
		const float EdgeHz = FilterKind == Kind::LowPass ? XRay::Sound::LowPass::MaxCutoffHz : XRay::Sound::LowPass::MinCutoffHz;
		const float TargetHz = std::clamp(CutoffHz, XRay::Sound::LowPass::MinCutoffHz, XRay::Sound::LowPass::MaxCutoffHz);
		Stages = std::clamp(Stages, 1u, MaxStages);

		if (Frames == 0)
		{
			return;
		}

		if (CurrentHz == 0.0f && IsBypassed(TargetHz))
		{
			// Fully open: drop history so the next engage starts clean
			Reset();
			return;
		}

		// Engaging: glide in from the open edge
		const float BeginHz = CurrentHz != 0.0f ? CurrentHz : EdgeHz;
		const float BeginQ = CurrentHz != 0.0f ? CurrentQ : Q;

		if (Stages != StageCount)
		{
			for (u32 Stage = StageCount; Stage < MaxStages; Stage++)
			{
				for (u32 Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
				{
					States[Channel][Stage] = {};
				}
			}
			StageCount = Stages;
		}

		ForEachPiece(Frames, [&](u32 Offset, u32 Count, float Factor)
		{
			UpdateCoefficients(LogLerp(BeginHz, TargetHz, Factor), lerp(BeginQ, Q, Factor));
			for (u32 Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
			{
				float* Samples = Data[Channel] + Offset;
				for (u32 Stage = 0; Stage < StageCount; Stage++)
				{
					State& Filter = States[Channel][Stage];
					const Coefficients& StageCoeffs = Coeffs[Stage];
					for (u32 Frame = 0; Frame < Count; Frame++)
					{
						Samples[Frame] = Biquad::Process(StageCoeffs, Filter, Samples[Frame]);
					}
				}
			}
		});

		CurrentHz = TargetHz;
		CurrentQ = Q;

		if (IsBypassed(TargetHz))
		{
			// Reached the open edge: the next block bypasses
			Reset();
		}
	}
}
