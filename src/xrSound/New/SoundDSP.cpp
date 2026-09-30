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
#include "SoundDSP.h"
#include <Sound.h>

#define SND_BACK_ATTENUATION (0.3f)
#define SND_SPEED_OF_SOUND (343.0f)
#define SND_DOPPLER_SMOOTH (4.0f)

void
DSP_CalculateRelativePosition(const dsp_spatial_desc* desc, Fvector* out_pos, float* out_distance)
{
    Fvector dt = *desc->obj_position;
    dt.sub(*desc->camera_position);
    *out_distance = (fis_zero(dt.x) && fis_zero(dt.y) && fis_zero(dt.z)) ? EPS : dt.magnitude();

    Fmatrix look_at;
    look_at.build_camera_dir(*desc->camera_position, *desc->camera_direction, *desc->camera_normal);
    look_at.transform_tiny_noadd(*out_pos, dt);
    out_pos->normalize_safe();
}

void
DSP_Doppler(const dsp_spatial_desc* desc, float distance)
{
    float target = 1.0f;
    if (distance > EPS_S) {
        Fvector to_listener;
        to_listener.sub(*desc->camera_position, *desc->obj_position).mul(1.0f / distance);

        float begin = to_listener.dotproduct(*desc->camera_velocity) * psSoundDoppler;
        float end = to_listener.dotproduct(*desc->obj_velocity) * psSoundDoppler;
        target = std::clamp((SND_SPEED_OF_SOUND - begin) / std::max(SND_SPEED_OF_SOUND - end, 1.0f), 0.5f, 2.0f);
    }

    Snd_VolumeLerp(desc->doppler, target, SND_DOPPLER_SMOOTH, (float)SND_BLOCKSIZE / (float)SND_SAMPLERATE);
}

float
DSP_Attenuation(const Fvector* distances, float distance, float power)
{
    float min_distance = std::max(distances->x, EPS_S);
    float max_distance = std::max(distances->y, min_distance);
    distance = std::clamp(distance, min_distance, max_distance);

    float attenuation = powf(min_distance / (psSoundRolloff * distance), power);
    attenuation *= 1.0f - std::clamp(std::max(distance - min_distance, 0.0f) / (max_distance - min_distance), 0.0f, 1.0f);
    return std::clamp(attenuation, 0.0f, 1.0f);
}

void
DSP_SpatialProcess(float** buffer, const Fvector* distances, const dsp_spatial_desc* desc)
{
    Fvector pos;
    float distance;
    DSP_CalculateRelativePosition(desc, &pos, &distance);
    DSP_Doppler(desc, distance);

    float min_distance = std::max(distances->x, EPS_S);
    float panning_level = std::min(distance / min_distance, 1.0f);
    float attenuation = DSP_Attenuation(distances, distance, 1.3f);
    float near_mix = std::min(std::clamp(distance, min_distance, std::max(distances->y, min_distance + EPS_S)), 1.0f);

    float angle = (std::clamp(pos.x, -1.0f, 1.0f) + 1.0f) * PI_DIV_4;
    float atten_gain = 1.0f - SND_BACK_ATTENUATION * std::clamp(-pos.z, 0.0f, 1.0f);
    float lc = lerp(1.0f, cosf(angle) * atten_gain, near_mix);
    float rc = lerp(1.0f, sinf(angle) * atten_gain, near_mix);

    float t = 1.0f / (float)SND_SAMPLERATE;
    for (u32 frame_idx = 0; frame_idx < SND_BLOCKSIZE; frame_idx++) {
        buffer[0][frame_idx] *= attenuation * (desc->panning[0] * panning_level);
        Snd_VolumeLerp(&desc->panning[0], lc, 10.0f, t);

        buffer[1][frame_idx] *= attenuation * (desc->panning[1] * panning_level);
        Snd_VolumeLerp(&desc->panning[1], rc, 10.0f, t);
    }
}

void
DSP_ResampleBuffer(float** input, float** output, float* phase, u32 input_frames, u32 output_frames)
{
    float ratio = (float)input_frames / (float)output_frames;
    for (u32 channel_idx = 0; channel_idx < SND_CHANNEL_COUNT; channel_idx++) {
        phase[channel_idx] = fmodf(phase[channel_idx], 1.0f);
        for (u32 frame_idx = 0; frame_idx < output_frames; frame_idx++) {
            u32 idx0 = std::min((u32)phase[channel_idx], input_frames);
            u32 idx1 = std::min(idx0 + 1, input_frames);
            output[channel_idx][frame_idx] += lerp(input[channel_idx][idx0], input[channel_idx][idx1], phase[channel_idx] - (float)idx0);
            phase[channel_idx] += ratio;
        }
    }
}

void
DSP_MixBuffer(float** mix_buffer, float** data, float begin_factor, float end_factor, float left, float right, u32 frames)
{
    float channel_factors[SND_CHANNEL_COUNT] = { left, right };
    for (u32 channel_idx = 0; channel_idx < SND_CHANNEL_COUNT; channel_idx++) {
        for (u32 frame_idx = 0; frame_idx < frames; frame_idx++) {
            float factor = lerp(begin_factor, end_factor, (float)frame_idx / (float)(frames - 1)) * channel_factors[channel_idx];
            mix_buffer[channel_idx][frame_idx] += data[channel_idx][frame_idx] * factor;
        }
    }
}

void
DSP_Compressor(float attack_ms, float release_ms, float threshold_db, float ratio, float** data, float drywet, u32 frames, float* envelope)
{
    float attack = (attack_ms == 0.0f) ? 0.0f : (float)exp(-1.0 / ((float)SND_SAMPLERATE * attack_ms));
    float release = (release_ms == 0.0f) ? 0.0f : (float)exp(-1.0 / ((float)SND_SAMPLERATE * release_ms));
    ratio = 1.0f - 1.0f / ratio;

    for (u32 channel_idx = 0; channel_idx < SND_CHANNEL_COUNT; channel_idx++) {
        for (u32 frame_idx = 0; frame_idx < frames; frame_idx++) {
            float* sample = &data[channel_idx][frame_idx];
            float over_db = std::max((float)lin2dB(fabsf(*sample) + FLT_EPSILON) - threshold_db, 0.0f) + FLT_EPSILON;

            float* current_envelope = &envelope[channel_idx];
			*current_envelope = over_db + ((over_db > *current_envelope) ? attack : release) * (*current_envelope - over_db);

            float comp = *current_envelope - FLT_EPSILON;
            if (comp > 0.0f) {
				comp -= *current_envelope * *current_envelope * 0.001f;
            }

            *sample *= lerp(1.0f, (float)dB2lin(0.0f - comp * ratio), drywet);
        }
    }
}
