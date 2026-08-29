#include "stdafx.h"
#include "SoundConvolution.h"
#include "SoundMixer.h"

// Applies a 2nd-order peaking ("bell") EQ to each channel of an IR. RBJ audio
// EQ cookbook. gain_db < 0 cuts the band; q sets the bandwidth (lower = wider).
static void ApplyBellEQ(xr_vector<xr_vector<float>>& IR, float center_hz, float q, float gain_db)
{
	const float Fs = (float)SND_SAMPLERATE;
	const float A = std::pow(10.0f, gain_db / 40.0f);
	const float W0 = 2.0f * PI * center_hz / Fs;
	const float Cosw0 = std::cos(W0);
	const float Sinw0 = std::sin(W0);
	const float Alpha = Sinw0 / (2.0f * q);

	const float B0 = 1.0f + Alpha * A;
	const float B1 = -2.0f * Cosw0;
	const float B2 = 1.0f - Alpha * A;
	const float A0 = 1.0f + Alpha / A;
	const float A1 = -2.0f * Cosw0;
	const float A2 = 1.0f - Alpha / A;

	const float Nb0 = B0 / A0;
	const float Nb1 = B1 / A0;
	const float Nb2 = B2 / A0;
	const float Na1 = A1 / A0;
	const float Na2 = A2 / A0;

	for (u32 Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
	{
		float X1 = 0.0f, X2 = 0.0f, Y1 = 0.0f, Y2 = 0.0f;
		for (float& S : IR[Channel])
		{
			const float y = Nb0 * S + Nb1 * X1 + Nb2 * X2 - Na1 * Y1 - Na2 * Y2;
			X2 = X1;
			X1 = S;
			Y2 = Y1;
			Y1 = y;
			S = y;
		}
	}
}

static float* AlignedAlloc(size_t Count)
{
	return (float*)pffft_aligned_malloc(Count * sizeof(float));
}

static void AlignedFree(float* Part)
{
	if (Part)
	{
		pffft_aligned_free(Part);
	}
}

CConvolutionReverb::~CConvolutionReverb()
{
	Free();
}

void CConvolutionReverb::Free()
{
	for (auto& Channel : IrFFT)
	{
		for (float* Part : Channel)
		{
			AlignedFree(Part);
		}
	}
	IrFFT.clear();

	for (float* Part : AccVec)
	{
		AlignedFree(Part);
	}
	AccVec.clear();

	AlignedFree(InTime);
	AlignedFree(InputFFT);
	AlignedFree(Prod);
	AlignedFree(Temp);
	AlignedFree(Work);
	InTime = InputFFT = Prod = Temp = Work = nullptr;

	if (Setup)
	{
		pffft_destroy_setup(Setup);
		Setup = nullptr;
	}

	NumPartitions = 0;
	AccLen = 0;
	IsValid = false;
}

// Normalizes each channel by ENERGY (||IR||2 = target) instead of peak: with
// unit Energy the convolved tail'S RMS matches the dry signal'S RMS, which is
// what a dry/wet volume mix requires. A peak-normalized diffuse tail has a
// much lower RMS than the dry signal, so full-wet sounds far too quiet.
static void NormalizeIREnergy(xr_vector<xr_vector<float>>& IR, float target = 1.0f)
{
	for (u32 Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
	{
		float Energy = 0.0f;
		for (float S : IR[Channel])
		{
			Energy += S * S;
		}
		if (Energy > 1e-12f)
		{
			const float Norm = target / std::sqrt(Energy);
			for (float& S : IR[Channel])
			{
				S *= Norm;
			}
		}
	}
}

bool CConvolutionReverb::LoadWav(const char* Path)
{
	xr_vector<xr_vector<float>> ChannelAudio;
	u32 SampleRate = 0;
	u16 NumChannels = 0;
	XRay::Sound::Mixer::LoadImpulseResponse(Path, ChannelAudio, SampleRate, NumChannels);

	if (ChannelAudio.empty() || ChannelAudio[0].empty() || NumChannels == 0)
	{
		return false;
	}

	IrFrames = (u32)ChannelAudio[0].size();
	IrRate = SampleRate;

	const float Ratio = (float)SampleRate / (float)SND_SAMPLERATE;

	xr_vector<xr_vector<float>> IR;
	IR.resize(SND_CHANNEL_COUNT);

	for (u32 Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
	{
		const xr_vector<float>& Src = ChannelAudio[Channel % NumChannels];
		xr_vector<float>& Dest = IR[Channel];

		if (std::fabs(Ratio - 1.0f) < 1e-3f)
		{
			Dest = Src;
		}
		else
		{
			const u32 OutLen = (u32)(((uint64_t)Src.size() * SND_SAMPLERATE) / SampleRate);
			Dest.resize(OutLen);

			for (u32 Key = 0; Key < OutLen; Key++)
			{
				const double Pos = (double)Key * (double)SampleRate / (double)SND_SAMPLERATE;
				const u32 I0 = (u32)Pos;

				if (I0 >= Src.size() - 1)
				{
					Dest[Key] = Src.back();
					continue;
				}

				const u32 Il = I0 + 1;
				const float Frac = (float)(Pos - (double)I0);

				Dest[Key] = Src[I0] * (1.0f - Frac) + Src[Il] * Frac;
			}
		}
	}

	IrFrames = (u32)IR[0].size();
	IrRate = SND_SAMPLERATE;

	// Energy normalization: keeps the wet tail as loud as the dry signal in a
	// dry/wet mix and balances L/R for stereo IRs.
	NormalizeIREnergy(IR);

	// Far Field: quieter tail (-6 dB) to match the perceived level.
	if (IsFar)
	{
		const float FarGain = 0.5f;
		for (u32 Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
		{
			for (float& S : IR[Channel])
			{
				S *= FarGain;
			}
		}
	}

	return FinalizeIR(IR);
}

static u32 ProceduralRandom(u32& State)
{
	State ^= State << 13;
	State ^= State >> 17;
	State ^= State << 5;
	return State;
}

static float ProceduralNoise(u32& State)
{
	return ((float)(ProceduralRandom(State) & 0x00FFFFFF) / (float)0x007FFFFF) - 1.0f;
}

// Synthesizes the indoor (near) impulse response algorithmically: discrete
// early reflections within a short window plus a one-pole low-passed noise
// tail with exponential decay. A low-mid "boom" bell keeps indoor gunshots
// from sounding thin.
static void SynthesizeIndoorIR(xr_vector<xr_vector<float>>& IR)
{
	const float Fs = (float)SND_SAMPLERATE;
	const float RT60 = 0.35f;
	const float Len = 0.5f;
	const float ErrWindow = 0.025f;
	const u32 ErCount = 9;
	const float LpfHz = 4200.0f;
	const float TailGain = 1.0f;

	const u32 LocalFrames = (u32)(Len * Fs);
	const float DecayRate = 6.907755278982137f / RT60; // ln(2^10)/RT60 -> -60 dB
	const float LpfAlpha = 1.0f - std::exp(-2.0f * 3.14159265358979323846f * LpfHz / Fs);

	u32 Seeds[SND_CHANNEL_COUNT] = {0x9E3779B9u, 0x85CA8F5Du};

	IR.resize(SND_CHANNEL_COUNT);
	for (u32 Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
	{
		IR[Channel].assign(LocalFrames, 0.0f);

		float LpState = 0.0f;
		for (u32 Key = 0; Key < LocalFrames; Key++)
		{
			const float Time = (float)Key / Fs;
			LpState += LpfAlpha * (ProceduralNoise(Seeds[Channel]) - LpState);
			IR[Channel][Key] = LpState * std::exp(-DecayRate * Time);
		}

		// Early reflections: deterministic pseudo-random delays inside the
		// window, exponentially decreasing Gain, alternating polarity.
		for (u32 Key = 0; Key < ErCount; Key++)
		{
			const float Jitter = 0.85f + 0.3f * ((float)(ProceduralRandom(Seeds[Channel]) & 0xFF) / 255.0f);
			const float Delay = ErrWindow * ((float)(Key + 1) / (float)ErCount) * Jitter + (Channel == 1 ? 0.0012f : -0.0009f); // slight L/R offset for width
			const u32 Idx = (u32)(std::max(Delay, 0.004f) * Fs);
			if (Idx >= LocalFrames)
			{
				break;
			}
			const float Gain = std::pow(0.72f, (float)(Key + 1)) * ((Key & 1) ? -1.0f : 1.0f);
			IR[Channel][Idx] += Gain;
		}

		for (float& S : IR[Channel])
		{
			S *= TailGain;
		}
	}

	ApplyBellEQ(IR, 120.0f, 0.9f, 3.5f);
	NormalizeIREnergy(IR, 2.0f);
}

bool CConvolutionReverb::InitializeProcedural(float WetGain, ReverbField Field)
{
	Free();
	Wet = WetGain;
	IsFar = (Field == ReverbField::Far);

	xr_vector<xr_vector<float>> IR;
	SynthesizeIndoorIR(IR);
	IrFrames = (u32)IR[0].size();
	IrRate = SND_SAMPLERATE;

	if (!FinalizeIR(IR))
	{
		Free();
		return false;
	}

	IsValid = true;
	return true;
}

bool CConvolutionReverb::FinalizeIR(xr_vector<xr_vector<float>>& IR)
{
	Setup = pffft_new_setup((int)FFT_SIZE, PFFFT_REAL);
	if (!Setup)
	{
		return false;
	}

	InTime = AlignedAlloc(FFT_SIZE);
	InputFFT = AlignedAlloc(FFT_SIZE);
	Prod = AlignedAlloc(FFT_SIZE);
	Temp = AlignedAlloc(FFT_SIZE);
	Work = AlignedAlloc(FFT_SIZE);

	if (!InTime || !InputFFT || !Prod || !Temp || !Work)
	{
		return false;
	}

	NumPartitions = std::max(1u, (u32)((IR[0].size() + BLOCK - 1) / BLOCK));
	AccLen = (NumPartitions + 1) * BLOCK;

	IrFFT.resize(SND_CHANNEL_COUNT);
	AccVec.resize(SND_CHANNEL_COUNT);

	for (u32 Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
	{
		IrFFT[Channel].resize(NumPartitions);

		AccVec[Channel] = AlignedAlloc(AccLen);
		if (!AccVec[Channel])
		{
			return false;
		}

		memset(AccVec[Channel], 0, AccLen * sizeof(float));

		for (u32 Part = 0; Part < NumPartitions; Part++)
		{
			float* Seg = AlignedAlloc(FFT_SIZE);
			if (!Seg)
			{
				return false;
			}

			memset(Seg, 0, FFT_SIZE * sizeof(float));

			const u32 Start = Part * BLOCK;

			if (Start < IR[Channel].size())
			{
				const u32 Count = std::min(BLOCK, (u32)IR[Channel].size() - Start);
				memcpy(Seg, IR[Channel].data() + Start, Count * sizeof(float));
			}

			pffft_transform(Setup, Seg, Seg, Work, PFFFT_FORWARD);

			IrFFT[Channel][Part] = Seg;
		}
	}

	return true;
}

bool CConvolutionReverb::Initialize(const char* ir_path, float WetGain, ReverbField Field)
{
	Free();
	Wet = WetGain;
	IsFar = (Field == ReverbField::Far);

	if (!LoadWav(ir_path))
	{
		Free();
		return false;
	}

	IsValid = true;
	return true;
}

void CConvolutionReverb::Process(float** Input, float** output, u32 LocalFrames)
{
	if (!IsValid || LocalFrames != BLOCK)
	{
		return;
	}

	const float InvN = 1.0f / (float)FFT_SIZE;

	for (u32 Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
	{
		memcpy(InTime, Input[Channel], BLOCK * sizeof(float));
		memset(InTime + BLOCK, 0, BLOCK * sizeof(float));
		pffft_transform(Setup, InTime, InputFFT, Work, PFFFT_FORWARD);

		float* Acc = AccVec[Channel];

		// Overlap-add: each IR partition contributes a 2*BLOCK linear
		// convolution placed at offset Part*BLOCK.
		for (u32 Part = 0; Part < NumPartitions; Part++)
		{
			memset(Prod, 0, FFT_SIZE * sizeof(float));
			pffft_zconvolve_accumulate(Setup, InputFFT, IrFFT[Channel][Part], Prod, 1.0f);
			pffft_transform(Setup, Prod, InTime, Work, PFFFT_BACKWARD);

			float* Dest = Acc + Part * BLOCK;
			for (u32 Key = 0; Key < FFT_SIZE; Key++)
			{
				Dest[Key] += InTime[Key] * InvN;
			}
		}

		// Emit the first BLOCK samples as the wet tail, scaled by the Gain.
		for (u32 Key = 0; Key < BLOCK; Key++)
		{
			output[Channel][Key] += Acc[Key] * Wet;
		}

		// Shift the accumulation buffer left by BLOCK for the next frame.
		memmove(Acc, Acc + BLOCK, (AccLen - BLOCK) * sizeof(float));
		memset(Acc + (AccLen - BLOCK), 0, BLOCK * sizeof(float));
	}
}
