#pragma once
#include "../New/SoundShader.h"

namespace XRay::Sound::Biquad
{
	struct Coefficients
	{
		float B0 = 1.0f;
		float B1 = 0.0f;
		float B2 = 0.0f;
		float A1 = 0.0f;
		float A2 = 0.0f;
	};

	struct State
	{
		float Z1 = 0.0f;
		float Z2 = 0.0f;
	};

	// RBJ cookbook filters at the mixer sample rate
	Coefficients LowPass(float CutoffHz, float Q);
	Coefficients HighPass(float CutoffHz, float Q);
	// Bell (peaking) band: GainDb at FreqHz, Q sets the width
	Coefficients Peak(float FreqHz, float Q, float GainDb);

	// Transposed direct form II
	inline float Process(const Coefficients& Coeffs, State& Filter, float Input)
	{
		const float Output = Coeffs.B0 * Input + Filter.Z1;
		Filter.Z1 = Coeffs.B1 * Input - Coeffs.A1 * Output + Filter.Z2;
		Filter.Z2 = Coeffs.B2 * Input - Coeffs.A2 * Output;
		return Output;
	}

	// Cascade of up to MaxStages biquads (12 dB/oct each) with Butterworth stage Q values scaled by the resonance.
	// Cutoff and Q changes glide over the processed block in log space, coefficients follow every AutomationStep frames
	class Cascade
	{
	public:
		static constexpr u32 MaxStages = 4;

		enum class Kind : u8
		{
			LowPass,
			HighPass
		};

		explicit Cascade(Kind FilterKind) : FilterKind(FilterKind)
		{
		}

		void Reset();
		// Q 0.7071 is flat (Butterworth), lower is softer, higher peaks at the cutoff. Stages 1..MaxStages.
		// A low-pass at or above MaxCutoffHz and a high-pass at or below MinCutoffHz are bypassed
		void Process(float** Data, u32 Frames, float CutoffHz, float Q, u32 Stages);

	private:
		bool IsBypassed(float CutoffHz) const;
		void UpdateCoefficients(float CutoffHz, float Q);

		Kind FilterKind;
		u32 StageCount = 0;
		// 0 until the first engaged block
		float CurrentHz = 0.0f;
		float CurrentQ = 0.7071f;
		Coefficients Coeffs[MaxStages];
		State States[SND_CHANNEL_COUNT][MaxStages];
	};
}
