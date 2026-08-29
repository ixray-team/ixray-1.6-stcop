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

// Which impulse response slot this processor represents; drives per-field
// shaping (EQ / level) baked into the IR at load time. IndoorNear uses an
// algorithmically synthesized impulse response (no asset required).
enum class ReverbField { Far, IndoorNear };

// Partitioned convolution reverb (overlap-add) driven by an externally
// recorded impulse response (e.g. a noise burst captured in a real space).
// Resonance Audio's public API only exposes parametric RT60 reverb, so this
// lives in the core mixer and is fed by a dedicated "shooting" reverb send.
class CConvolutionReverb
{
public:
    CConvolutionReverb() = default;
    ~CConvolutionReverb();

    // Loads a PCM/FP WAV impulse response. Returns true on success.
    bool Initialize(const char* ir_path, float wet_gain, ReverbField field = ReverbField::Far);

    // Synthesizes an impulse response algorithmically (indoor rooms).
    bool InitializeProcedural(float wet_gain, ReverbField field);
    bool Valid() const { return IsValid; }
    void SetWetGain(float gain) { Wet = gain; }

    // Convolves `input` (SND_CHANNEL_COUNT planar buffers of SND_BLOCKSIZE
    // samples) and accumulates the wet tail into `output`.
    void Process(float** input, float** output, u32 frames);

    bool IsLoaded() const { return IsValid; }
    void GetIRInfo(u32& frames, u32& sample_rate) const { frames = IrFrames; sample_rate = IrRate; }

	void Free();

private:
    bool LoadWav(const char* path);

    // Shared tail: per-field shaping, FFT setup and partitioned IR spectra.
    bool FinalizeIR(xr_vector<xr_vector<float>>& ir);

    bool IsValid = false;
    float Wet = 1.0f;
    bool IsFar = false;

    u32 IrFrames = 0;
    u32 IrRate = 0;

    PFFFT_Setup* Setup = nullptr;

    // FFT size is twice the block size so circular convolution of two
    // BLOCK-length segments equals their linear convolution without aliasing.
    static constexpr u32 FFT_SIZE = SND_BLOCKSIZE * 2;
    static constexpr u32 BLOCK = SND_BLOCKSIZE;

    u32 NumPartitions = 0;
    u32 AccLen = 0;

    // Per-channel IR spectra: [channel][partition] -> FFT_SIZE floats (aligned).
    xr_vector<xr_vector<float*>> IrFFT;

    // Per-channel overlap accumulation buffers (time domain).
    xr_vector<float*> AccVec;

    // Scratch buffers (aligned).
    float* InTime = nullptr;   // FFT_SIZE
    float* InputFFT = nullptr; // FFT_SIZE
    float* Prod = nullptr;      // FFT_SIZE
    float* Temp = nullptr;       // FFT_SIZE
    float* Work = nullptr;      // FFT_SIZE
};
