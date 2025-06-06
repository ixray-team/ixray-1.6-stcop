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
#include "SoundDSP.h"
#include <Sound.h>

static float
sinc_func(float x)
{
    if (x == 0.0f) return 1.0f;
    float pi_x = M_PI * x;
    return sinf(pi_x) / pi_x;
}

void
DSP_CalculateRelativePosition(const Fvector& P, const Fvector& D, const Fvector& N, const Fvector& obj_pos, Fvector& out_pos, float& out_distance)
{
    // Direction vector
    Fvector pos = obj_pos; pos.sub(P);
    if (fis_zero(pos.x) && fis_zero(pos.y) && fis_zero(pos.z)) {
        out_distance = EPS;
    } else {
        out_distance = pos.magnitude();
    }

    // Look at matrix
    Fmatrix m; m.build_camera_dir(P, D, N);

    // Transform only position without w component
    m.transform_tiny_noadd(out_pos, pos);
    out_pos.normalize_safe();
}

void 
DSP_SpatialProcess(float** buffer, const Fvector& distances, const Fvector& P, const Fvector& D, const Fvector& N, const Fvector& obj_pos)
{   
    // LH coordinates
    Fvector pos;
    float distance;
    Fvector speaker_l = Fvector(-1, 0, 0.5);
    Fvector speaker_r = Fvector(1, 0, 0.5);
    
    DSP_CalculateRelativePosition(P, D, N, obj_pos, pos, distance);

    // Attenuation
    distance = std::clamp(distance, distances.x, distances.y);
    float att = distances.x / (psSoundRolloff * distance);
    att *= 1.0f - std::clamp(std::max(distance - distances.x, 0.0f) / (distances.y-distances.x), 0.0f, 1.0f);
    att = std::clamp(att, 0.f, 1.f);

    // Panning
    float lc = att * ((speaker_l.dotproduct(pos) + 1.0f) * 0.5f);
    float rc = att * ((speaker_r.dotproduct(pos) + 1.0f) * 0.5f);
    for (size_t i = 0; i < SND_BLOCKSIZE; i++) {
        buffer[0][i] *= lc;
    }
    for (size_t i = 0; i < SND_BLOCKSIZE; i++) {
        buffer[1][i] *= rc;
    }
}

void
DSP_ResampleBuffer(float** input, float** output, float history[SND_CHANNEL_COUNT][SND_RESAMPLING_QUALITY+1], u32 input_frames, u32 output_frames)
{
    float ratio = (float)input_frames / (float)output_frames;

    for (size_t i = 0; i < SND_CHANNEL_COUNT; i++) {
        history[i][0] = fmodf(history[i][0], 1.0f);
    }

    for (size_t k = 0; k < SND_CHANNEL_COUNT; k++) {
        for (u32 i = 0; i < output_frames; ++i) {
            float& phase = history[k][0];

            u32 idx0 = (u32)phase;
            u32 idx1 = idx0 + 1;
            //R_ASSERT(idx1 < input_frames);

            float delta = phase - (float)idx0;
            float sample0 = input[k][idx0];
            float sample1 = input[k][idx1];

            float sample = lerp(sample0, sample1, delta);
            output[k][i] += sample;
            phase += ratio;
        }
    }
}

void 
DSP_MixBuffer(float** mix_buffer, float** data, float begin_factor, float end_factor, u32 frames)
{
    for (size_t ch = 0; ch < SND_CHANNEL_COUNT; ch++) {
        for (size_t k = 0; k < frames; k++) {
            float factor = lerp(begin_factor, end_factor, (float)(k) / (float)(frames - 1));
            float mix_sample = mix_buffer[ch][k];
            float sample = data[ch][k];
            mix_buffer[ch][k] += sample * factor;
        }
    }
}

void 
DSP_Compressor(float attack_ms, float release_ms, float threshold_db, float ratio, float** data, float drywet, u32 frames, float envelope[SND_CHANNEL_COUNT])
{
    float lin_attack = attack_ms == 0.0f ? 0.0 : (f32)exp(-1.0 / ((float)SND_SAMPLERATE * attack_ms));
    float lin_release = release_ms == 0.0f ? 0.0 : (f32)exp(-1.0 / ((float)SND_SAMPLERATE * release_ms));
    ratio = (1.0f - 1.0f / (ratio));

    for (size_t ch = 0; ch < SND_CHANNEL_COUNT; ch++) {
        for (size_t k = 0; k < frames; k++) {
            float& sample = data[ch][k];

            float temp = lin2dB(sample + FLT_EPSILON);
            float over_db = temp - threshold_db;
            if (over_db < 0.0f) over_db = 0.0f;
            over_db += FLT_EPSILON; 

            float theta = (over_db > envelope[ch]) ? lin_attack : lin_release;
            envelope[ch] = over_db + theta * (envelope[ch] - over_db);

            float p_vart = envelope[ch] - FLT_EPSILON;
            if (p_vart > 0.0f) p_vart -= envelope[ch] * envelope[ch] * 0.001f; // opto pseudo curve
            float gain = 0.0f - p_vart * ratio;

            float comp = lerp(1.0f, dB2lin(gain), drywet);
            sample *= comp;
        }
    }

}