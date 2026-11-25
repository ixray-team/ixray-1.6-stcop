#include "SoundRoomReverb.h"
#include "SoundEffect.h"
#include "../Shaders/SoundBiquad.h"

namespace
{
	namespace Biquad = XRay::Sound::Biquad;

	// 16 lines keep the modal density high enough for long tails; fewer lines ring like a metal pipe
	constexpr u32 LineCount = 16;
	constexpr u32 LineFrames[LineCount] = {997, 1087, 1163, 1249, 1361, 1447, 1549, 1667, 1801, 1931, 2081, 2237, 2411, 2591, 2789, 3001};
	constexpr u32 LineSize = 4096;
	// Slow line length modulation smears the remaining resonances (about 1.5 cents of pitch drift)
	constexpr float ModDepth = 12.0f;
	constexpr float ModRate[LineCount] = {0.1300f, 0.3321f, 0.2905f, 0.9700f, 0.8484f, 0.3797f, 0.2222f, 0.4342f, 0.1486f, 0.2540f, 0.4964f, 0.7420f, 0.1943f, 0.5676f, 0.1699f, 0.6489f};

	// Input diffuser: each step delays the 16 channels by different amounts, shuffles them, flips some
	// polarities and mixes them with a Hadamard matrix. It has no feedback, so unlike all-pass chains it does not ring.
	constexpr u32 DiffuserSteps = 4;
	constexpr u32 DiffuserSize[DiffuserSteps] = {1024, 512, 256, 128};
	constexpr u32 DiffuserFrames[DiffuserSteps][LineCount] =
	{
		{507, 482, 128, 830, 648, 59, 555, 802, 234, 358, 703, 278, 723, 427, 20, 202},
		{345, 209, 225, 397, 75, 291, 415, 96, 327, 187, 6, 255, 34, 374, 149, 124},
		{45, 29, 208, 63, 145, 134, 98, 79, 114, 94, 157, 172, 200, 1, 26, 186},
		{54, 39, 25, 81, 73, 101, 92, 2, 85, 105, 9, 29, 45, 14, 68, 61}
	};
	constexpr u8 DiffuserShuffle[DiffuserSteps][LineCount] =
	{
		{0, 5, 10, 7, 8, 6, 3, 14, 4, 2, 12, 15, 1, 13, 11, 9},
		{9, 5, 10, 15, 2, 0, 8, 12, 4, 3, 14, 1, 13, 11, 7, 6},
		{11, 12, 0, 6, 8, 7, 2, 5, 10, 14, 4, 3, 15, 9, 13, 1},
		{0, 2, 5, 10, 14, 4, 12, 8, 7, 6, 3, 1, 11, 15, 9, 13}
	};
	constexpr float DiffuserSign[DiffuserSteps][LineCount] =
	{
		{1.0f, -1.0f, -1.0f, 1.0f, -1.0f, 1.0f, -1.0f, 1.0f, 1.0f, -1.0f, 1.0f, -1.0f, 1.0f, -1.0f, 1.0f, -1.0f},
		{-1.0f, 1.0f, 1.0f, -1.0f, -1.0f, 1.0f, 1.0f, 1.0f, -1.0f, 1.0f, 1.0f, 1.0f, -1.0f, 1.0f, -1.0f, -1.0f},
		{1.0f, -1.0f, 1.0f, -1.0f, -1.0f, -1.0f, 1.0f, 1.0f, 1.0f, 1.0f, -1.0f, -1.0f, -1.0f, -1.0f, -1.0f, 1.0f},
		{1.0f, -1.0f, 1.0f, -1.0f, 1.0f, -1.0f, 1.0f, 1.0f, 1.0f, -1.0f, -1.0f, 1.0f, 1.0f, -1.0f, -1.0f, 1.0f}
	};
	constexpr u32 DiffuserLatency = 830 + 415 + 208 + 105;
	// Channel buffers of every step live in one array: step offsets in frames
	constexpr u32 DiffuserOffset[DiffuserSteps] = {0, LineCount * 1024, LineCount * (1024 + 512), LineCount * (1024 + 512 + 256)};
	constexpr u32 DiffuserStorage = LineCount * (1024 + 512 + 256 + 128);
	constexpr u32 ReflectionSize = 8192;
	constexpr u32 RoomSurfaces = 6;
	constexpr float SpeedOfSound = 343.0f;
	// Upper edge of the 1 kHz octave band: the bands above it decay with decay * decay_hf * air_hf
	constexpr float CrossoverHz = 1414.2f;
	// Upper edge of the 8 kHz octave band: the Resonance spectral reverb had no tail above it
	constexpr float BandLimitHz = 11314.0f;
	constexpr float InvSqrtLines = 0.25f;
	// Keeps the feedback network and filters out of denormals while the tail fades out
	constexpr float AntiDenormal = 1e-18f;
	constexpr float MinDecay = 0.05f;
	constexpr float MaxDecay = 27.2f;
	// Resonance Audio rendered no tail for a band below this RT60 at 44.1 kHz (0.15 s at 48 kHz)
	constexpr float SilentDecay = 0.1633f;
	// Decay changes glide over this time, like the Resonance reverb node did
	constexpr float UpdateSeconds = 1.0f;

	static_assert(SND_SAMPLERATE == 44100, "Line lengths and the level table are tuned for 44.1 kHz");
	static_assert(SND_CHANNEL_COUNT == 2, "The reverb renders a stereo tail");
	static_assert(LineCount == 16, "Mix assumes a 16x16 Hadamard matrix");
	static_assert(LineFrames[LineCount - 1] + (u32)ModDepth + 3 < LineSize, "Delay buffers are too short");

	// Input gain that gives the tail the white-noise level of the Resonance spectral reverb with the same RT60.
	// Measured offline: the Resonance level curve divided by the band-limited level of this network with unit input gain.
	constexpr u32 LevelPoints = 15;
	constexpr float LevelDecay[LevelPoints] = {0.17f, 0.2f, 0.3f, 0.5f, 0.75f, 1.0f, 1.5f, 2.0f, 3.0f, 4.0f, 6.0f, 8.0f, 12.0f, 16.0f, 25.0f};
	constexpr float LevelGain[LevelPoints] = {47.5981f, 38.9005f, 26.6982f, 20.8133f, 19.4857f, 19.4605f, 20.7923f, 22.3103f, 25.2632f, 27.4049f, 31.2844f, 34.8658f, 40.1979f, 44.2174f, 51.3827f};

	enum RoomReverbParam : u32
	{
		ReverbGain,
		ReverbSize,
		ReverbCutoff,
		ReverbDiffusion,
		ReverbReflections,
		ReverbDecay,
		ReverbDecayHf,
		ReverbAirHf
	};

	const SoundEffectParam RoomReverbParams[] =
	{
		{"gain", 0.005f, 0.0f, 1.0f},
		{"size", 10.0f, 1.0f, 100.0f},
		{"cutoff", 5000.0f, 200.0f, 20000.0f},
		{"diffusion", 1.0f, 0.0f, 1.0f},
		{"reflections", 0.05f, 0.0f, 1.0f},
		{"decay", 1.49f, 0.1f, 20.0f},
		{"decay_hf", 0.83f, 0.1f, 2.0f},
		{"air_hf", 0.994f, 0.0f, 1.0f}
	};

	float TailInputGain(float Decay)
	{
		if (Decay < SilentDecay)
		{
			return 0.0f;
		}

		if (Decay <= LevelDecay[0])
		{
			return LevelGain[0];
		}

		// Log-log interpolation between the measured points
		for (u32 Point = 1; Point < LevelPoints; Point++)
		{
			if (Decay <= LevelDecay[Point])
			{
				const float Factor = std::log(Decay / LevelDecay[Point - 1]) / std::log(LevelDecay[Point] / LevelDecay[Point - 1]);
				return LevelGain[Point - 1] * std::pow(LevelGain[Point] / LevelGain[Point - 1], Factor);
			}
		}

		return LevelGain[LevelPoints - 1];
	}

	float OnePoleAlpha(float CutoffHz)
	{
		const float Cutoff = std::clamp(CutoffHz, 10.0f, (float)SND_SAMPLERATE * 0.45f);
		return 1.0f - std::exp(-2.0f * PI * Cutoff / (float)SND_SAMPLERATE);
	}

	// Unnormalized 16x16 Sylvester-Hadamard transform, in place
	void Hadamard(float* Values)
	{
		for (u32 Span = 1; Span < LineCount; Span *= 2)
		{
			for (u32 Start = 0; Start < LineCount; Start += Span * 2)
			{
				for (u32 Index = Start; Index < Start + Span; Index++)
				{
					const float A = Values[Index];
					const float B = Values[Index + Span];
					Values[Index] = A + B;
					Values[Index + Span] = A - B;
				}
			}
		}
	}

	class CRoomReverb
	{
	public:
		CRoomReverb()
		{
			CrossoverAlpha = OnePoleAlpha(CrossoverHz);
			BandLimit[0] = Biquad::LowPass(BandLimitHz, 0.5412f);
			BandLimit[1] = Biquad::LowPass(BandLimitHz, 1.3066f);

			// Modulation phases spread evenly over the lines; the oscillators are rotating phasors
			for (u32 Line = 0; Line < LineCount; Line++)
			{
				const float Phase = 2.0f * PI * (float)Line / (float)LineCount;
				const float Step = 2.0f * PI * ModRate[Line] / (float)SND_SAMPLERATE;
				ModCos[Line] = std::cos(Phase);
				ModSin[Line] = std::sin(Phase);
				ModStepCos[Line] = std::cos(Step);
				ModStepSin[Line] = std::sin(Step);
			}
		}

		void SetParams(const float* Params, bool Immediate)
		{
			const float Blocks = Immediate ? 0.0f : UpdateSeconds * (float)SND_SAMPLERATE / (float)SND_BLOCKSIZE;
			LowDecay.Set(std::clamp(Params[ReverbDecay], MinDecay, MaxDecay), Blocks);
			HighDecay.Set(std::clamp(Params[ReverbDecay] * Params[ReverbDecayHf] * Params[ReverbAirHf], MinDecay, MaxDecay), Blocks);

			// Listener in the centre of a cube room: all six first-order images arrive together
			const float Distance = Params[ReverbSize] * 0.5f + 1.0f;
			ReflectionDelay = std::min((u32)(Distance / SpeedOfSound * (float)SND_SAMPLERATE), ReflectionSize - 1);
			ReflectionGain = (float)RoomSurfaces * Params[ReverbDiffusion] / Distance * Params[ReverbReflections];
			ReflectionAlpha = OnePoleAlpha(Params[ReverbCutoff]);
		}

		u32 GetTailFrames() const
		{
			const float Decay = std::max({LowDecay.Current, LowDecay.Target, HighDecay.Current, HighDecay.Target});
			const u32 Tail = Decay < SilentDecay ? 0 : (u32)(Decay * (float)SND_SAMPLERATE) + LineFrames[LineCount - 1] + (u32)ModDepth + DiffuserLatency;
			return Tail + ReflectionDelay + SND_BLOCKSIZE;
		}

		// Data is the stereo bus input; it is replaced with the wet signal scaled by TargetGain
		void Process(float** Data, float TargetGain)
		{
			LowDecay.Advance();
			HighDecay.Advance();

			// Keeps the rotating phasors on the unit circle against float drift
			for (u32 Line = 0; Line < LineCount; Line++)
			{
				const float Norm = 1.0f / std::sqrt(ModCos[Line] * ModCos[Line] + ModSin[Line] * ModSin[Line]);
				ModCos[Line] *= Norm;
				ModSin[Line] *= Norm;
			}

			float LowFeedback[LineCount];
			float HighFeedback[LineCount];
			for (u32 Line = 0; Line < LineCount; Line++)
			{
				const float Frames = -3.0f * (float)LineFrames[Line] / (float)SND_SAMPLERATE;
				LowFeedback[Line] = std::pow(10.0f, Frames / LowDecay.Current);
				HighFeedback[Line] = std::pow(10.0f, Frames / HighDecay.Current);
			}

			const float TargetLow = TailInputGain(LowDecay.Current);
			const float TargetHigh = TailInputGain(HighDecay.Current);
			if (!IsStarted)
			{
				LowInputGain = TargetLow;
				HighInputGain = TargetHigh;
				OutputGain = TargetGain;
				IsStarted = true;
			}

			float* Left = Data[0];
			float* Right = Data[1];
			const float Step = 1.0f / (float)SND_BLOCKSIZE;
			for (u32 Frame = 0; Frame < SND_BLOCKSIZE; Frame++)
			{
				const float Factor = (float)(Frame + 1) * Step;
				const float Mono = 0.5f * (Left[Frame] + Right[Frame]);

				// Two-band input gain: the per-band level calibration
				InputLowPass += CrossoverAlpha * (Mono - InputLowPass);
				float Input = lerp(LowInputGain, TargetLow, Factor) * InputLowPass + lerp(HighInputGain, TargetHigh, Factor) * (Mono - InputLowPass) + AntiDenormal;

				// Multichannel diffuser: 16 decorrelated copies of the input feed the lines
				float Diffused[LineCount];
				for (u32 Line = 0; Line < LineCount; Line++)
				{
					Diffused[Line] = Input;
				}

				for (u32 Step = 0; Step < DiffuserSteps; Step++)
				{
					const u32 Mask = DiffuserSize[Step] - 1;
					float* Buffer = Diffusers + DiffuserOffset[Step];
					float Delayed[LineCount];
					for (u32 Line = 0; Line < LineCount; Line++)
					{
						float* Channel = Buffer + Line * DiffuserSize[Step];
						Channel[DiffuserPos & Mask] = Diffused[Line];
						Delayed[Line] = Channel[(DiffuserPos - DiffuserFrames[Step][Line]) & Mask];
					}

					for (u32 Line = 0; Line < LineCount; Line++)
					{
						Diffused[Line] = Delayed[DiffuserShuffle[Step][Line]] * DiffuserSign[Step][Line];
					}

					Hadamard(Diffused);
					for (u32 Line = 0; Line < LineCount; Line++)
					{
						Diffused[Line] *= InvSqrtLines;
					}
				}

				DiffuserPos++;

				// Feedback delay network with modulated line lengths and a crossover absorption filter per line
				float Taps[LineCount];
				float WetLeft = 0.0f;
				float WetRight = 0.0f;
				for (u32 Line = 0; Line < LineCount; Line++)
				{
					// First-order all-pass interpolation of the fractional delay: unlike polynomial interpolation it has
					// no high-frequency loss, which would shorten the treble decay on every pass through the loop.
					// The fraction stays in [0.5, 1.5) so the coefficient stays small and well away from the pole at -1.
					const float Delay = (float)LineFrames[Line] + ModDepth * ModSin[Line];
					const u32 Whole = (u32)(Delay - 0.5f);
					const float Frac = Delay - (float)Whole;
					const float Coeff = (1.0f - Frac) / (1.0f + Frac);
					const float* Buffer = Lines[Line];
					const float Near = Buffer[(LinePos - Whole) & (LineSize - 1)];
					const float Far = Buffer[(LinePos - Whole - 1) & (LineSize - 1)];
					float Sample = Coeff * (Near - Interpolated[Line]) + Far;
					Interpolated[Line] = Sample;

					const float Cos = ModCos[Line];
					ModCos[Line] = Cos * ModStepCos[Line] - ModSin[Line] * ModStepSin[Line];
					ModSin[Line] = ModSin[Line] * ModStepCos[Line] + Cos * ModStepSin[Line];

					Absorption[Line] += CrossoverAlpha * (Sample - Absorption[Line]);
					Sample = LowFeedback[Line] * Absorption[Line] + HighFeedback[Line] * (Sample - Absorption[Line]);
					Taps[Line] = Sample;

					// Orthogonal sign patterns decorrelate the two outputs
					WetLeft += (Line & 1) ? -Sample : Sample;
					WetRight += (Line & 2) ? -Sample : Sample;
				}

				Hadamard(Taps);
				for (u32 Line = 0; Line < LineCount; Line++)
				{
					Lines[Line][LinePos] = Taps[Line] * InvSqrtLines + Diffused[Line];
				}

				LinePos = (LinePos + 1) & (LineSize - 1);

				for (u32 Stage = 0; Stage < 2; Stage++)
				{
					WetLeft = Biquad::Process(BandLimit[Stage], BandState[0][Stage], WetLeft);
					WetRight = Biquad::Process(BandLimit[Stage], BandState[1][Stage], WetRight);
				}

				// Early reflections: the low-passed input, delayed by the wall distance
				ReflectionLowPass += ReflectionAlpha * (Mono - ReflectionLowPass);
				Reflections[ReflectionPos] = ReflectionLowPass;
				const float Early = ReflectionGain * Reflections[(ReflectionPos - ReflectionDelay) & (ReflectionSize - 1)];
				ReflectionPos = (ReflectionPos + 1) & (ReflectionSize - 1);

				const float Gain = lerp(OutputGain, TargetGain, Factor);
				Left[Frame] = (WetLeft + Early) * Gain;
				Right[Frame] = (WetRight + Early) * Gain;
			}

			LowInputGain = TargetLow;
			HighInputGain = TargetHigh;
			OutputGain = TargetGain;
		}

	private:
		struct Ramp
		{
			float Current = 0.0f;
			float Target = 0.0f;
			float Step = 0.0f;

			// Blocks 0 jumps to the value
			void Set(float Value, float Blocks)
			{
				Target = Value;
				if (Blocks <= 0.0f)
				{
					Current = Value;
					Step = 0.0f;
				}
				else
				{
					Step = (Target - Current) / Blocks;
				}
			}

			void Advance()
			{
				Current = std::abs(Target - Current) <= std::abs(Step) ? Target : Current + Step;
			}
		};

		float Lines[LineCount][LineSize] = {};
		float Absorption[LineCount] = {};
		float Diffusers[DiffuserStorage] = {};
		// Last output of each line's all-pass interpolator
		float Interpolated[LineCount] = {};
		float ModCos[LineCount] = {};
		float ModSin[LineCount] = {};
		float ModStepCos[LineCount] = {};
		float ModStepSin[LineCount] = {};
		float Reflections[ReflectionSize] = {};
		u32 LinePos = 0;
		u32 DiffuserPos = 0;
		u32 ReflectionPos = 0;

		float CrossoverAlpha = 0.0f;
		float InputLowPass = 0.0f;
		Biquad::Coefficients BandLimit[2];
		Biquad::State BandState[SND_CHANNEL_COUNT][2];

		Ramp LowDecay;
		Ramp HighDecay;
		// Values reached at the end of the last block; the next block ramps from them
		float LowInputGain = 0.0f;
		float HighInputGain = 0.0f;
		float OutputGain = 0.0f;
		bool IsStarted = false;

		u32 ReflectionDelay = 0;
		float ReflectionGain = 0.0f;
		float ReflectionAlpha = 0.0f;
		float ReflectionLowPass = 0.0f;
	};

	// Effect states are zero-filled raw memory, so the reverb lives on the heap
	struct RoomReverbState
	{
		CRoomReverb* Reverb;
	};
}

bool Snd_RoomReverbProc(void* StatePtr, SoundEffectOp Op, void* Arg)
{
	RoomReverbState* State = (RoomReverbState*)StatePtr;
	switch (Op)
	{
	case SoundEffectOp::Describe:
		*(SoundEffectDesc*)Arg = {SoundEffectScope::Bus, sizeof(RoomReverbState), (u32)std::size(RoomReverbParams), RoomReverbParams};
		return true;
	case SoundEffectOp::Create:
		State->Reverb = new CRoomReverb();
		State->Reverb->SetParams(((SoundEffectCreate*)Arg)->Params, true);
		return true;
	case SoundEffectOp::Update:
		if (State->Reverb != nullptr)
		{
			State->Reverb->SetParams((const float*)Arg, false);
		}
		return true;
	case SoundEffectOp::Destroy:
		xr_delete(State->Reverb);
		return true;
	case SoundEffectOp::Tail:
		*(u32*)Arg = State->Reverb != nullptr ? State->Reverb->GetTailFrames() : 0;
		return true;
	case SoundEffectOp::Process:
	{
		SoundEffectProcess* Process = (SoundEffectProcess*)Arg;
		if (State->Reverb == nullptr)
		{
			memset(Process->Data[0], 0, SND_BLOCKSIZE * sizeof(float));
			memset(Process->Data[1], 0, SND_BLOCKSIZE * sizeof(float));
			return true;
		}

		State->Reverb->Process(Process->Data, Process->Params[ReverbGain]);
		return true;
	}
	default:
		return false;
	}
}
