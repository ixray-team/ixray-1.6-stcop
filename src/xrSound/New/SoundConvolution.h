/**************************************************************************************
 * Copyright (C) 2025 Anton Kovalev (vertver)
 * New Sound Engine
 ***************************************************************************************
 * Source code is licensed under the following terms:
 *
 * 1. IX-Ray Team License
 *    Non-exclusive, royalty-free, perpetual license is hereby granted to:
 *      - ForserX   (https://github.com/ForserX)
 *      - Drombeys  (https://github.com/Drombeys)
 *      - v2v3v4    (https://github.com/v2v3v4)
 *
 *    Permitted rights:
 *      - Copy, modify, merge, publish and distribute this Software
 *        and its documentation.
 *
 * 2. Public Access License
 *    Non-exclusive, "access-view-study" rights granted to everyone else.
 *
 *    Permitted rights:
 *      - Private copying is allowed, provided that no distribution occurs.
 *      - Public cloning (i.e. "forking") is allowed, but any source code
 *        modification or binary redistribution is prohibited.
 *
 * Usage of this Software beyond the rights granted above is strictly prohibited.
 *
 * The above copyright notice and this license text must be included in all
 * copies or substantial portions of the Software.
 **************************************************************************************/
#pragma once

#include "SoundMeta.h"
#include <pffft.h>

// Uniformly partitioned convolution (UPOLS): overlap-save with a frequency-domain delay line.
// The IR is energy-normalized, so the wet output has the same RMS as the input and the
// wet/dry balance is set entirely by the caller's send/dry gains.
class CConvolutionReverb
{
public:
	CConvolutionReverb() = default;
	~CConvolutionReverb();

	CConvolutionReverb(const CConvolutionReverb&) = delete;
	CConvolutionReverb& operator=(const CConvolutionReverb&) = delete;

	bool Initialize(const char* IrPath);
	bool InitializeIndoor();
	void Free();

	bool IsValid() const { return Valid; }
	void GetIRInfo(u32& Frames, u32& SampleRate) const { Frames = IrFrames; SampleRate = SND_SAMPLERATE; }

	// Accumulates the wet signal of SND_BLOCKSIZE planar frames into Output. Must be called
	// every block: with HasInput == false it only renders the remaining tail and then idles.
	void Process(float** Input, float** Output, bool HasInput);

	void Reset();

private:
	bool Build(xr_vector<xr_vector<float>>& IR);

	static constexpr u32 BLOCK = SND_BLOCKSIZE;
	static constexpr u32 FFT_SIZE = SND_BLOCKSIZE * 2;

	bool Valid = false;
	u32 IrFrames = 0;
	u32 NumPartitions = 0;
	u32 FdlHead = 0;
	u32 TailBlocksLeft = 0;

	PFFFT_Setup* Setup = nullptr;

	// [Channel][Partition][FFT_SIZE], pffft internal (unordered) spectrum layout.
	float* IrSpectra = nullptr;
	float* Fdl = nullptr;

	// [Channel][FFT_SIZE]: previous + current input block (overlap-save window).
	float* InputWindow = nullptr;

	float* Spectrum = nullptr;
	float* TimeOut = nullptr;
	float* Work = nullptr;
};
