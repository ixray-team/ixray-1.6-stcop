/**************************************************************************************
* Copyright (C) 2026 Anton Kovalev (vertver)
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
#include "SoundMixerInternal.h"

inline double
lin2dB(double lin)
{
    return log(lin) * 8.6858896380650365530225783783321;
}

inline double
dB2lin(double db)
{
    return exp(db * 0.11512925464970228420089957273422);
}

typedef struct _dsp_spatial_desc {
    float* panning;
    const Fvector* camera_position;
    const Fvector* camera_direction;
    const Fvector* camera_normal;
    const Fvector* camera_velocity;
    const Fvector* obj_position;
    const Fvector* obj_velocity;
    float* doppler;
} dsp_spatial_desc;

void DSP_CalculateRelativePosition(const dsp_spatial_desc* desc, Fvector* out_pos, float* out_distance);
void DSP_Doppler(const dsp_spatial_desc* desc, float distance);
float DSP_Attenuation(const Fvector* distances, float distance, float power);
void DSP_SpatialProcess(float** buffer, const Fvector* distances, const dsp_spatial_desc* desc);
void DSP_ResampleBuffer(float** input, float** output, float* phase, u32 input_frames, u32 output_frames);
void DSP_Compressor(float attack_ms, float release_ms, float threshold_db, float ratio, float** data, float drywet, u32 frames, float* envelope);
void DSP_MixBuffer(float** mix_buffer, float** data, float begin_factor, float end_factor, float left, float right, u32 frames);
