#pragma once
#include "SoundMeta.h"

struct PFFFT_Setup;

// Magnitude spectrum of a block (pffft, Hann window), for debug views. Not thread safe
class SoundSpectrumAnalyzer
{
public:
	static constexpr u32 MinFftSize = 256;
	static constexpr u32 MaxFftSize = SND_SCOPE_FRAMES;

	SoundSpectrumAnalyzer();
	~SoundSpectrumAnalyzer();
	SoundSpectrumAnalyzer(const SoundSpectrumAnalyzer&) = delete;
	SoundSpectrumAnalyzer& operator=(const SoundSpectrumAnalyzer&) = delete;

	// Samples: FftSize frames, a power of two in [MinFftSize, MaxFftSize].
	// BandsDb: BandCount log-spaced bands in [MinHz, MaxHz], dB of a full scale sine
	bool Analyze(const float* Samples, u32 FftSize, float* BandsDb, u32 BandCount, float MinHz, float MaxHz);

private:
	bool Prepare(u32 FftSize);
	float BinMagnitude(float Bin) const;

	PFFFT_Setup* Setup = nullptr;
	u32 Size = 0;
	float* Input = nullptr;
	float* Output = nullptr;
	float* Work = nullptr;
	float Window[MaxFftSize] = {};
	float Magnitudes[MaxFftSize / 2 + 1] = {};
	float WindowGain = 1.0f;
};
