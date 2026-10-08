#include "stdafx.h"
#include "SoundConvolution.h"
#include "SoundMixer.h"

// RBJ audio EQ cookbook peaking filter.
static void ApplyBellEQ(xr_vector<xr_vector<float>>& IR, float CenterHz, float Q, float GainDb)
{
	const float Fs = (float)SND_SAMPLERATE;
	const float A = std::pow(10.0f, GainDb / 40.0f);
	const float W0 = 2.0f * PI * CenterHz / Fs;
	const float Cosw0 = std::cos(W0);
	const float Alpha = std::sin(W0) / (2.0f * Q);

	const float A0 = 1.0f + Alpha / A;
	const float B0 = (1.0f + Alpha * A) / A0;
	const float B1 = (-2.0f * Cosw0) / A0;
	const float B2 = (1.0f - Alpha * A) / A0;
	const float A1 = (-2.0f * Cosw0) / A0;
	const float A2 = (1.0f - Alpha / A) / A0;

	for (xr_vector<float>& Channel : IR)
	{
		float X1 = 0.0f, X2 = 0.0f, Y1 = 0.0f, Y2 = 0.0f;
		for (float& S : Channel)
		{
			const float Y = B0 * S + B1 * X1 + B2 * X2 - A1 * Y1 - A2 * Y2;
			X2 = X1;
			X1 = S;
			Y2 = Y1;
			Y1 = Y;
			S = Y;
		}
	}
}

static void NormalizeIREnergy(xr_vector<xr_vector<float>>& IR)
{
	for (xr_vector<float>& Channel : IR)
	{
		double Energy = 0.0;
		for (float S : Channel)
		{
			Energy += (double)S * S;
		}

		if (Energy > 1e-12)
		{
			const float Norm = (float)(1.0 / std::sqrt(Energy));
			for (float& S : Channel)
			{
				S *= Norm;
			}
		}
	}
}

// Schroeder backward integration: drop the tail once the remaining energy is below -90 dB.
static void TrimIRTail(xr_vector<xr_vector<float>>& IR)
{
	size_t Length = 0;
	for (const xr_vector<float>& Channel : IR)
	{
		double Total = 0.0;
		for (float S : Channel)
		{
			Total += (double)S * S;
		}

		const double Threshold = Total * 1e-9;
		double Remaining = 0.0;
		size_t End = Channel.size();
		while (End > 0)
		{
			Remaining += (double)Channel[End - 1] * Channel[End - 1];
			if (Remaining > Threshold)
			{
				break;
			}
			--End;
		}

		Length = std::max(Length, End);
	}

	for (xr_vector<float>& Channel : IR)
	{
		Channel.resize(std::max<size_t>(Length, 1));
	}
}

static u32 XorShift32(u32& State)
{
	State ^= State << 13;
	State ^= State >> 17;
	State ^= State << 5;
	return State;
}

static float WhiteNoise(u32& State)
{
	return ((float)(XorShift32(State) & 0x00FFFFFF) / (float)0x007FFFFF) - 1.0f;
}

// Small room: sparse early reflections over a low-passed, exponentially decaying noise tail,
// decorrelated between channels, with a low-mid bell so indoor gunshots keep their body.
static void SynthesizeIndoorIR(xr_vector<xr_vector<float>>& IR)
{
	constexpr float RT60 = 0.35f;
	constexpr float Length = 0.5f;
	constexpr float EarlyWindow = 0.025f;
	constexpr u32 EarlyCount = 9;
	constexpr float LowPassHz = 4200.0f;

	const float Fs = (float)SND_SAMPLERATE;
	const u32 Frames = (u32)(Length * Fs);
	const float DecayRate = 6.907755279f / RT60;
	const float LowPassAlpha = 1.0f - std::exp(-2.0f * PI * LowPassHz / Fs);
	const float StereoOffset[SND_CHANNEL_COUNT] = {-0.0009f, 0.0012f};
	u32 Seeds[SND_CHANNEL_COUNT] = {0x9E3779B9u, 0x85CA8F5Du};

	IR.resize(SND_CHANNEL_COUNT);
	for (u32 Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
	{
		xr_vector<float>& Dest = IR[Channel];
		Dest.assign(Frames, 0.0f);

		float LowPass = 0.0f;
		for (u32 Key = 0; Key < Frames; Key++)
		{
			LowPass += LowPassAlpha * (WhiteNoise(Seeds[Channel]) - LowPass);
			Dest[Key] = LowPass * std::exp(-DecayRate * (float)Key / Fs);
		}

		for (u32 Key = 0; Key < EarlyCount; Key++)
		{
			const float Jitter = 0.85f + 0.3f * ((float)(XorShift32(Seeds[Channel]) & 0xFF) / 255.0f);
			const float Delay = EarlyWindow * ((float)(Key + 1) / (float)EarlyCount) * Jitter + StereoOffset[Channel];
			const u32 Index = (u32)(std::max(Delay, 0.004f) * Fs);
			if (Index >= Frames)
			{
				break;
			}

			const float Gain = std::pow(0.72f, (float)(Key + 1));
			Dest[Index] += (Key & 1) ? -Gain : Gain;
		}
	}

	ApplyBellEQ(IR, 120.0f, 0.9f, 3.5f);
}

CConvolutionReverb::~CConvolutionReverb()
{
	Free();
}

void CConvolutionReverb::Free()
{
	pffft_aligned_free(IrSpectra);
	pffft_aligned_free(Fdl);
	pffft_aligned_free(InputWindow);
	pffft_aligned_free(Spectrum);
	pffft_aligned_free(TimeOut);
	pffft_aligned_free(Work);
	IrSpectra = Fdl = InputWindow = Spectrum = TimeOut = Work = nullptr;

	if (Setup != nullptr)
	{
		pffft_destroy_setup(Setup);
		Setup = nullptr;
	}

	Valid = false;
	IrFrames = 0;
	NumPartitions = 0;
	FdlHead = 0;
	TailBlocksLeft = 0;
}

void CConvolutionReverb::Reset()
{
	if (!Valid)
	{
		return;
	}

	memset(Fdl, 0, (size_t)SND_CHANNEL_COUNT * NumPartitions * FFT_SIZE * sizeof(float));
	memset(InputWindow, 0, (size_t)SND_CHANNEL_COUNT * FFT_SIZE * sizeof(float));
	FdlHead = 0;
	TailBlocksLeft = 0;
}

bool CConvolutionReverb::Initialize(const char* IrPath)
{
	Free();

	xr_vector<xr_vector<float>> Source;
	u32 SampleRate = 0;
	u16 NumChannels = 0;
	XRay::Sound::Mixer::LoadImpulseResponse(IrPath, Source, SampleRate, NumChannels);

	if (NumChannels == 0 || Source.empty() || Source[0].empty())
	{
		Msg("! [Sound] Can't load impulse response '%s'", IrPath);
		return false;
	}

	R_ASSERT(SampleRate == SND_SAMPLERATE);

	xr_vector<xr_vector<float>> IR(SND_CHANNEL_COUNT);
	for (u32 Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
	{
		IR[Channel] = Source[Channel % NumChannels];
	}

	return Build(IR);
}

bool CConvolutionReverb::InitializeIndoor()
{
	Free();

	xr_vector<xr_vector<float>> IR;
	SynthesizeIndoorIR(IR);
	return Build(IR);
}

bool CConvolutionReverb::Build(xr_vector<xr_vector<float>>& IR)
{
	TrimIRTail(IR);
	NormalizeIREnergy(IR);

	IrFrames = (u32)IR[0].size();
	NumPartitions = (IrFrames + BLOCK - 1) / BLOCK;

	const size_t SpectraSize = (size_t)SND_CHANNEL_COUNT * NumPartitions * FFT_SIZE * sizeof(float);
	const size_t BufferSize = FFT_SIZE * sizeof(float);

	Setup = pffft_new_setup((int)FFT_SIZE, PFFFT_REAL);
	IrSpectra = (float*)pffft_aligned_malloc(SpectraSize);
	Fdl = (float*)pffft_aligned_malloc(SpectraSize);
	InputWindow = (float*)pffft_aligned_malloc(SND_CHANNEL_COUNT * BufferSize);
	Spectrum = (float*)pffft_aligned_malloc(BufferSize);
	TimeOut = (float*)pffft_aligned_malloc(BufferSize);
	Work = (float*)pffft_aligned_malloc(BufferSize);

	if (Setup == nullptr || IrSpectra == nullptr || Fdl == nullptr || InputWindow == nullptr || Spectrum == nullptr || TimeOut == nullptr || Work == nullptr)
	{
		Free();
		return false;
	}

	for (u32 Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
	{
		for (u32 Part = 0; Part < NumPartitions; Part++)
		{
			const u32 Start = Part * BLOCK;
			const u32 Count = std::min(BLOCK, IrFrames - Start);

			memset(TimeOut, 0, BufferSize);
			memcpy(TimeOut, IR[Channel].data() + Start, Count * sizeof(float));

			float* Dest = IrSpectra + ((size_t)Channel * NumPartitions + Part) * FFT_SIZE;
			pffft_transform(Setup, TimeOut, Dest, Work, PFFFT_FORWARD);
		}
	}

	Valid = true;
	Reset();
	return true;
}

void CConvolutionReverb::Process(float** Input, float** Output, bool HasInput)
{
	if (!Valid)
	{
		return;
	}

	if (HasInput)
	{
		TailBlocksLeft = NumPartitions;
	}
	else if (TailBlocksLeft == 0)
	{
		return;
	}
	else
	{
		--TailBlocksLeft;
	}

	const float InvN = 1.0f / (float)FFT_SIZE;
	FdlHead = (FdlHead + NumPartitions - 1) % NumPartitions;

	for (u32 Channel = 0; Channel < SND_CHANNEL_COUNT; Channel++)
	{
		float* Window = InputWindow + (size_t)Channel * FFT_SIZE;
		memcpy(Window, Window + BLOCK, BLOCK * sizeof(float));
		if (HasInput)
		{
			memcpy(Window + BLOCK, Input[Channel], BLOCK * sizeof(float));
		}
		else
		{
			memset(Window + BLOCK, 0, BLOCK * sizeof(float));
		}

		float* ChannelFdl = Fdl + (size_t)Channel * NumPartitions * FFT_SIZE;
		const float* ChannelIr = IrSpectra + (size_t)Channel * NumPartitions * FFT_SIZE;

		pffft_transform(Setup, Window, ChannelFdl + (size_t)FdlHead * FFT_SIZE, Work, PFFFT_FORWARD);

		memset(Spectrum, 0, FFT_SIZE * sizeof(float));
		for (u32 Part = 0; Part < NumPartitions; Part++)
		{
			const u32 Index = (FdlHead + Part) % NumPartitions;
			pffft_zconvolve_accumulate(Setup, ChannelFdl + (size_t)Index * FFT_SIZE, ChannelIr + (size_t)Part * FFT_SIZE, Spectrum, InvN);
		}

		pffft_transform(Setup, Spectrum, TimeOut, Work, PFFFT_BACKWARD);

		// Overlap-save: the first half is circular-convolution garbage.
		float* Dest = Output[Channel];
		const float* Src = TimeOut + BLOCK;
		for (u32 Key = 0; Key < BLOCK; Key++)
		{
			Dest[Key] += Src[Key];
		}
	}
}
