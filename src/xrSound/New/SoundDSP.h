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
#include "../xrCore/vector.h"
#include "../xrCore/_matrix.h"
#include "SoundMixerInternal.h"

// linear -> dB conversion
inline double lin2dB(double lin)
{
	return log(lin) * 8.6858896380650365530225783783321;		// 20 / ln( 10 )
}

// dB -> linear conversion
inline double dB2lin(double dB)
{
	return exp(dB * 0.11512925464970228420089957273422);		// ln( 10 ) / 20
}

struct dsp_stuff
{
	f32 Dt;
	f32* Panning;
	const Fvector* CameraPosition;
	const Fvector* CameraDirection;
	const Fvector* CameraNormal;
	const Fvector* ObjPosition;
};

void DSP_CalculateRelativePosition(const dsp_stuff& stuff, Fvector& out_pos, float& out_distance);
void DSP_SpatialProcess(float** buffer, const Fvector& distances, const dsp_stuff& stuff, bool disable_attenuation);
void DSP_ResampleBuffer(float** input, float** output, float history[SND_CHANNEL_COUNT][SND_RESAMPLING_QUALITY+1], u32 input_frames, u32 output_frames); // requires +1 sample of tail
void DSP_Compressor(float attack_ms, float release_ms, float threshold_db, float ratio, float** data, float drywet, u32 frames, float envelope[SND_CHANNEL_COUNT]);
void DSP_MixBuffer(float** mix_buffer, float** data, float begin_factor, float end_factor, u32 frames);
void DSP_MixBufferPanning(float** mix_buffer, float** data, float begin_factor, float end_factor, float left, float right, u32 frames);