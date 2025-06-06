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

void
DSP_CalculateRelativePosition(const dsp_stuff& stuff, Fvector& out_pos, float& out_distance)
{
    // Direction vector
    Fvector pos = *stuff.obj_position; pos.sub(*stuff.camera_position);
    if (fis_zero(pos.x) && fis_zero(pos.y) && fis_zero(pos.z)) {
        out_distance = EPS;
    } else {
        out_distance = pos.magnitude();
    }

    // Look at matrix
    Fmatrix m; m.build_camera_dir(*stuff.camera_position, *stuff.camera_direction, *stuff.camera_normal);

    // Transform only position without w component
    m.transform_tiny_noadd(out_pos, pos);
    out_pos.normalize_safe();
}

static constexpr float SND_BACK_ATTENUATION = 0.3f;

void
DSP_SpatialProcess(float** buffer, const Fvector& distances, const dsp_stuff& stuff, bool disable_attenuation)
{
    // LH coordinates
    Fvector pos;
    float distance;

    DSP_CalculateRelativePosition(stuff, pos, distance);

    // Broken ogg-comments give us zero/inverted ranges
    float min_distance = std::max(distances.x, EPS_S);
    float max_distance = std::max(distances.y, min_distance + EPS_S);

    // Panning level
    float pl = std::min(distance / min_distance, 1.0f);

    // Attenuation
    distance = std::clamp(distance, min_distance, max_distance);
    float att = 1.0f;

    if (!disable_attenuation) {
        att = min_distance / (psSoundRolloff * distance);
        att = powf(att, 1.3f);
        att *= 1.0f - std::clamp(std::max(distance - min_distance, 0.0f) / (max_distance - min_distance), 0.0f, 1.0f);
        att = std::clamp(att, 0.f, 1.f);
    }

    float pan_angle = (std::clamp(pos.x, -1.0f, 1.0f) + 1.0f) * PI_DIV_4;
    float lc = cosf(pan_angle);
    float rc = sinf(pan_angle);

    float back_gain = 1.0f - SND_BACK_ATTENUATION * std::clamp(-pos.z, 0.0f, 1.0f);
    lc *= back_gain;
    rc *= back_gain;

    lc = lerp(1.0f, lc, std::min(distance, 1.0f) / 1.0f);
    rc = lerp(1.0f, rc, std::min(distance, 1.0f) / 1.0f);
    //volume_lerp(stuff.panning[0], lc, 10.0f, stuff.dt);
    //volume_lerp(stuff.panning[1], rc, 10.0f, stuff.dt);

    float sample_dt = 1.0f / (float)SND_SAMPLERATE;

    for (size_t i = 0; i < SND_BLOCKSIZE; i++) {
        buffer[0][i] *= att * (stuff.panning[0] * pl);
        volume_lerp(stuff.panning[0], lc, 10.0f, sample_dt);
    }

    for (size_t i = 0; i < SND_BLOCKSIZE; i++) {
        buffer[1][i] *= att * (stuff.panning[1] * pl);
        volume_lerp(stuff.panning[1], rc, 10.0f, sample_dt);
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

            // input_frames is the last valid index: the caller decodes input_frames+1 frames
            u32 idx0 = std::min((u32)phase, input_frames);
            u32 idx1 = std::min(idx0 + 1, input_frames);

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
            float sample = data[ch][k];
            mix_buffer[ch][k] += sample * factor;
        }
    }
}

void 
DSP_MixBufferPanning(float** mix_buffer, float** data, float begin_factor, float end_factor, float left, float right, u32 frames)
{
    float factors[SND_CHANNEL_COUNT] = { left, right };
    for (size_t ch = 0; ch < SND_CHANNEL_COUNT; ch++) {
        for (size_t k = 0; k < frames; k++) {
            float factor = lerp(begin_factor, end_factor, (float)(k) / (float)(frames - 1)) * factors[ch];
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

            float temp = lin2dB(std::abs(sample) + FLT_EPSILON);
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