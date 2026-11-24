#include "SoundAnalyzer.h"
#include <pffft.h>

SoundSpectrumAnalyzer::SoundSpectrumAnalyzer()
{
	Input = (float*)pffft_aligned_malloc(MaxFftSize * sizeof(float));
	Output = (float*)pffft_aligned_malloc(MaxFftSize * sizeof(float));
	Work = (float*)pffft_aligned_malloc(MaxFftSize * sizeof(float));
}

SoundSpectrumAnalyzer::~SoundSpectrumAnalyzer()
{
	pffft_aligned_free(Input);
	pffft_aligned_free(Output);
	pffft_aligned_free(Work);
	if (Setup != nullptr)
	{
		pffft_destroy_setup(Setup);
	}
}

bool SoundSpectrumAnalyzer::Prepare(u32 FftSize)
{
	if (FftSize < MinFftSize || FftSize > MaxFftSize || (FftSize & (FftSize - 1)) != 0)
	{
		return false;
	}

	if (Setup != nullptr && Size == FftSize)
	{
		return true;
	}

	if (Setup != nullptr)
	{
		pffft_destroy_setup(Setup);
	}

	Setup = pffft_new_setup((int)FftSize, PFFFT_REAL);
	Size = Setup != nullptr ? FftSize : 0;

	float Sum = 0.0f;
	for (u32 Frame = 0; Frame < FftSize; Frame++)
	{
		Window[Frame] = 0.5f - 0.5f * std::cos(2.0f * PI * (float)Frame / (float)(FftSize - 1));
		Sum += Window[Frame];
	}

	// A full scale sine reads 0 dB
	WindowGain = Sum > 0.0f ? 2.0f / Sum : 1.0f;
	return Setup != nullptr;
}

float SoundSpectrumAnalyzer::BinMagnitude(float Bin) const
{
	const u32 Last = Size / 2;
	Bin = std::clamp(Bin, 1.0f, (float)Last);
	const u32 Low = (u32)Bin;
	const u32 High = std::min(Low + 1, Last);
	const float Fraction = Bin - (float)Low;
	return Magnitudes[Low] + (Magnitudes[High] - Magnitudes[Low]) * Fraction;
}

bool SoundSpectrumAnalyzer::Analyze(const float* Samples, u32 FftSize, float* BandsDb, u32 BandCount, float MinHz, float MaxHz)
{
	if (Input == nullptr || Output == nullptr || Work == nullptr || BandCount == 0 || !Prepare(FftSize))
	{
		return false;
	}

	for (u32 Frame = 0; Frame < FftSize; Frame++)
	{
		Input[Frame] = Samples[Frame] * Window[Frame];
	}

	// Ordered real output: [0] DC, [1] Nyquist, then re / im pairs
	pffft_transform_ordered(Setup, Input, Output, Work, PFFFT_FORWARD);
	const u32 Last = FftSize / 2;
	Magnitudes[0] = std::abs(Output[0]) * WindowGain * 0.5f;
	Magnitudes[Last] = std::abs(Output[1]) * WindowGain * 0.5f;
	for (u32 Bin = 1; Bin < Last; Bin++)
	{
		const float Re = Output[Bin * 2];
		const float Im = Output[Bin * 2 + 1];
		Magnitudes[Bin] = std::sqrt(Re * Re + Im * Im) * WindowGain;
	}

	const float BinHz = (float)SND_SAMPLERATE / (float)FftSize;
	MaxHz = std::clamp(MaxHz, 1.0f, (float)SND_SAMPLERATE * 0.5f);
	MinHz = std::clamp(MinHz, 1.0f, MaxHz);
	const float LogMin = std::log(MinHz);
	const float LogStep = (std::log(MaxHz) - LogMin) / (float)BandCount;
	for (u32 Band = 0; Band < BandCount; Band++)
	{
		const float BeginBin = std::exp(LogMin + LogStep * (float)Band) / BinHz;
		const float EndBin = std::exp(LogMin + LogStep * (float)(Band + 1)) / BinHz;

		// Narrow (low) bands interpolate at their center, wide ones take the loudest bin
		float Magnitude = BinMagnitude((BeginBin + EndBin) * 0.5f);
		const u32 FirstBin = std::max((u32)std::ceil(BeginBin), 1u);
		const u32 LastBin = std::min((u32)EndBin, Last);
		for (u32 Bin = FirstBin; Bin <= LastBin; Bin++)
		{
			Magnitude = std::max(Magnitude, Magnitudes[Bin]);
		}

		BandsDb[Band] = 20.0f * std::log10(std::max(Magnitude, 1e-9f));
	}

	return true;
}
