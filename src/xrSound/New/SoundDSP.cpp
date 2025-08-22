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

// Clamp helper
static inline float clampf(float v, float lo, float hi) {
    return (v < lo) ? lo : (v > hi ? hi : v);
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

#if 0
// Define EAX-like constraints and constants
const float EAX_DECAY_TIME_MIN = 0.1f;
const float EAX_DECAY_TIME_MAX = 20.0f;
const float EAX_DECAY_HF_RATIO_MIN = 0.1f;
const float EAX_DECAY_HF_RATIO_MAX = 2.0f;

// Helper function to convert millibels to a linear gain factor
float mb_to_gain(float mb) {
    return std::pow(10.0f, mb / 2000.0f);
}

// Simple comb filter (no internal LP filter needed with this structure)
void process_comb_filter(float* buffer, u32 buffer_size, u32 & offset, float input, float* output, float gain) 
{
    float delayed_sample = buffer[offset];
    float feedback_sample = input + delayed_sample * gain;
    *output = feedback_sample;

    buffer[offset] = feedback_sample;
    offset = (offset + 1) % buffer_size;
}

// Simple all-pass filter
void process_all_pass_filter(float* buffer, u32 buffer_size, u32 & offset, float input, float* output, float gain)
{
    float delayed_sample = buffer[offset];
    float output_sample = -input + delayed_sample + gain * delayed_sample;
    buffer[offset] = input + gain * delayed_sample;
    *output = output_sample;
    offset = (offset + 1) % buffer_size;
}

void DSP_AlgorithmicReverb(sound_reberb_state& state, sound_reverb_settings& settings, float** input, float** output, u32 frames) 
{
    // EAX-like parameter clamping and conversion
    float decay_time = std::clamp(settings.decay_time, 0.1f, 20.0f);
    float decay_hf_ratio = std::clamp(settings.decay_hf_ratio, 0.1f, 2.0f);
    float reverb_gain = mb_to_gain(settings.reverb);
    float reflections_gain = mb_to_gain(settings.reflections);
    float room_gain = mb_to_gain(settings.room);

    // Feedback gains
    float late_reverb_feedback_gain = std::pow(10.0f, -3.0f / decay_time);
    float ap_feedback_gain = std::pow(late_reverb_feedback_gain, decay_hf_ratio);

    // **Corrected:** Prime number base delays scaled by decay_time
    const u32 comb_delays[8] = {
        static_cast<u32>(SND_SAMPLERATE * 0.0297f * decay_time),
        static_cast<u32>(SND_SAMPLERATE * 0.0371f * decay_time),
        static_cast<u32>(SND_SAMPLERATE * 0.0411f * decay_time),
        static_cast<u32>(SND_SAMPLERATE * 0.0513f * decay_time),
        static_cast<u32>(SND_SAMPLERATE * 0.0617f * decay_time),
        static_cast<u32>(SND_SAMPLERATE * 0.0701f * decay_time),
        static_cast<u32>(SND_SAMPLERATE * 0.0793f * decay_time),
        static_cast<u32>(SND_SAMPLERATE * 0.0887f * decay_time)
    };

    const u32 ap_delays[8] = {
        static_cast<u32>(SND_SAMPLERATE * 0.005f),
        static_cast<u32>(SND_SAMPLERATE * 0.0017f),
        static_cast<u32>(SND_SAMPLERATE * 0.0011f),
        static_cast<u32>(SND_SAMPLERATE * 0.0031f),
        static_cast<u32>(SND_SAMPLERATE * 0.0023f),
        static_cast<u32>(SND_SAMPLERATE * 0.0039f),
        static_cast<u32>(SND_SAMPLERATE * 0.0047f),
        static_cast<u32>(SND_SAMPLERATE * 0.0059f)
    };


    // Early reflections parameters
    const u32 reflections_base_index = 16;
    const u32 reflections_delay_frames = static_cast<u32>(settings.reflections_delay * SND_SAMPLERATE);

    // Late reverb pre-delay
    const u32 reverb_pre_delay_frames = static_cast<u32>(settings.reverb_delay * SND_SAMPLERATE);

    for (u32 frame = 0; frame < frames; ++frame) {
        float mono_input = 0.0f;
        for (u32 channel = 0; channel < SND_CHANNEL_COUNT; ++channel) {
            mono_input += input[channel][frame];
        }
        mono_input /= SND_CHANNEL_COUNT;

        // 1. Process Early Reflections (Parallel with Late Reverb)
        float reflections_output = 0.0f;
        process_comb_filter(
            state.lines[reflections_base_index].buffer,
            reflections_delay_frames,
            state.lines[reflections_base_index].offset,
            mono_input,
            &reflections_output,
            0.0f
        );

        // 2. Process Late Reverb
        // Apply late reverb pre-delay
        float pre_delayed_input = 0.0f;
        process_comb_filter(
            state.lines[reflections_base_index + 1].buffer, // Use a new delay line for pre-delay
            reverb_pre_delay_frames,
            state.lines[reflections_base_index + 1].offset,
            mono_input,
            &pre_delayed_input,
            0.0f
        );

        float ap_input = pre_delayed_input;

        // All-pass filters first to diffuse the signal
        float ap_output_sum = 0.0f;
        for (u32 i = 8; i < 16; ++i) {
            float ap_output = 0.0f;
            process_all_pass_filter(
                state.lines[i].buffer,
                ap_delays[i-8],
                state.lines[i].offset,
                ap_input,
                &ap_output,
                late_reverb_feedback_gain
            );
            ap_output_sum += ap_output;
        }

        // Summed output of all-pass filters feeds into comb filters
        float comb_input = ap_output_sum;

        float comb_output_sum = 0.0f;
        for (u32 i = 0; i < 8; ++i) {
            float comb_output = 0.0f;
            process_comb_filter(
                state.lines[i].buffer,
                comb_delays[i],
                state.lines[i].offset,
                comb_input,
                &comb_output,
                late_reverb_feedback_gain
            );
            comb_output_sum += comb_output;
        }

        // 3. Final Mix
        float wet_reverb_signal = comb_output_sum * reverb_gain;
        float wet_reflections_signal = reflections_output * reflections_gain;
        float dry_signal = mono_input;

        for (u32 channel = 0; channel < SND_CHANNEL_COUNT; ++channel) {
            output[channel][frame] =
                wet_reflections_signal +
                wet_reverb_signal;
            output[channel][frame] *= room_gain * 0.1;
        }
    }
}
#else

float mb_to_gain(float mb) {
    return std::pow(10.0f, mb / 2000.0f);
}

void clamp_eax_settings(sound_reverb_settings& settings)
{
    // EAX parameter ranges in mB (millibels), seconds, and meters.
    settings.room = std::max(-10000.0f, std::min(0.0f, settings.room));
    settings.room_hf = std::max(-10000.0f, std::min(0.0f, settings.room_hf));
    settings.room_rolloff_factor = std::max(0.0f, std::min(10.0f, settings.room_rolloff_factor));
    settings.decay_time = std::max(0.1f, std::min(20.0f, settings.decay_time));
    settings.decay_hf_ratio = std::max(0.1f, std::min(2.0f, settings.decay_hf_ratio));
    settings.reflections = std::max(-10000.0f, std::min(1000.0f, settings.reflections));
    settings.reflections_delay = std::max(0.0f, std::min(0.3f, settings.reflections_delay));
    settings.reverb = std::max(-10000.0f, std::min(2000.0f, settings.reverb));
    settings.reverb_delay = std::max(0.0f, std::min(0.1f, settings.reverb_delay));
    settings.environment_size = std::max(1.0f, std::min(100.0f, settings.environment_size));
    settings.environment_diffusion = std::max(0.0f, std::min(1.0f, settings.environment_diffusion));
    settings.air_absorption_hf = std::max(-100.0f, std::min(0.0f, settings.air_absorption_hf));
}

typedef struct {
    float z1_lp;   // last output of lowpass
    float z1_hp;   // last input for highpass
} dsp_filter_state;

static inline float iir_lowpass(float x, float cutoff, float samplerate, dsp_filter_state* st) {
    float rc = 1.0f / (2.0f * 3.1415926f * cutoff);
    float dt = 1.0f / samplerate;
    float alpha = dt / (rc + dt);
    st->z1_lp = st->z1_lp + alpha * (x - st->z1_lp);
    return st->z1_lp;
}

static inline float iir_highpass(float x, float cutoff, float samplerate, dsp_filter_state* st) {
    float rc = 1.0f / (2.0f * 3.1415926f * cutoff);
    float dt = 1.0f / samplerate;
    float alpha = rc / (rc + dt);
    float y = alpha * (st->z1_hp + x - st->z1_lp);
    st->z1_hp = y;
    st->z1_lp = x;
    return y;
}


void DSP_AlgorithmicReverb(sound_reberb_state& state, sound_reverb_settings& settings, float** input, float** output, unsigned int frames)
{
    // First, clamp the settings to EAX limits to ensure valid parameter ranges.
    clamp_eax_settings(settings);

    const float hf_decay_coeff = 0.5f;// 1.0f - settings.air_absorption_hf;
    const float decay_gain = std::pow(10.0f, (-3.0f * settings.decay_time) / (settings.decay_time * SND_SAMPLERATE / 1000.0f));
    const float decay_hf_gain = decay_gain * settings.decay_hf_ratio;
    const float allpass_gain = 0.7f; // A typical fixed value for a simple all-pass filter.
    const float reflections_gain = mb_to_gain(settings.reflections);
    const float reverb_gain = mb_to_gain(settings.reverb);

    const unsigned int comb_delays[4] = {
        (unsigned int)(0.0297f * SND_SAMPLERATE),
        (unsigned int)(0.0371f * SND_SAMPLERATE),
        (unsigned int)(0.0411f * SND_SAMPLERATE),
        (unsigned int)(0.0437f * SND_SAMPLERATE)
    };

    const unsigned int allpass_delays[2] = {
        (unsigned int)(0.005f * SND_SAMPLERATE),
        (unsigned int)(0.0017f * SND_SAMPLERATE)
    };

    for (unsigned int n = 0; n < frames; ++n) {
        for (unsigned int c = 0; c < SND_CHANNEL_COUNT; ++c) {
            float in_sample = input[c][n];
            float early_ref_sample = 0.0f;
            float late_reverb_sample = 0.0f;

            unsigned int early_ref_line_idx = c;
            sound_reverb_line_state& early_ref_line = state.lines[early_ref_line_idx];

            unsigned int write_pos = early_ref_line.offset;
            early_ref_line.buffer[write_pos] = in_sample;

            unsigned int read_pos_offset = (unsigned int)(settings.reflections_delay * SND_SAMPLERATE);
            if (read_pos_offset >= early_ref_line.frames) {
                read_pos_offset = early_ref_line.frames - 1;
            }
            unsigned int read_pos = (write_pos + early_ref_line.frames - read_pos_offset) % early_ref_line.frames;

            early_ref_sample = early_ref_line.buffer[read_pos] * reflections_gain;
            early_ref_line.iir_state = (1.0f - hf_decay_coeff) * early_ref_sample + hf_decay_coeff * early_ref_line.iir_state;
            early_ref_sample = early_ref_line.iir_state;

            float late_reverb_input = early_ref_sample;

            float comb_out_sum = 0.0f;
            for (unsigned int i = 2; i < 6; ++i) {
                sound_reverb_line_state& line = state.lines[i];
                unsigned int delay_frames = comb_delays[i - 2];

                if (line.frames == 0 || line.frames < delay_frames) {
                    continue; // Skip this line if not properly sized
                }

                unsigned int comb_read_pos = (line.offset + line.frames - delay_frames) % line.frames;
                float delayed_sample = line.buffer[comb_read_pos];

                // Apply IIR low-pass filter to the feedback
                // y(n) = (1-a) * x(n) + a * y(n-1)
                line.iir_state = (1.0f - hf_decay_coeff) * delayed_sample + hf_decay_coeff * line.iir_state;

                float feedback_sample = line.iir_state * decay_hf_gain;
                float new_input = late_reverb_input + feedback_sample;
                line.buffer[line.offset] = new_input;
                comb_out_sum += delayed_sample;
            }

            float allpass_input = comb_out_sum;
            for (unsigned int i = 6; i < 8; ++i) {
                sound_reverb_line_state& line = state.lines[i];
                unsigned int delay_frames = allpass_delays[i - 6];

                if (line.frames == 0 || line.frames < delay_frames) {
                    continue; // Skip this line if not properly sized
                }

                // Read from the delay buffer
                unsigned int ap_read_pos = (line.offset + line.frames - delay_frames) % line.frames;
                float delayed_sample = line.buffer[ap_read_pos];

                // All-pass filter equation: y(n) = G*x(n) + x(n-k) - G*y(n-k)
                float new_input = allpass_input - allpass_gain * delayed_sample;
                line.buffer[line.offset] = new_input;
                allpass_input = delayed_sample + allpass_gain * new_input;
            }

            late_reverb_sample = allpass_input;
            float wet_signal = late_reverb_sample * reverb_gain;
            output[c][n] = wet_signal;

            early_ref_line.offset = (early_ref_line.offset + 1) % early_ref_line.frames;
            for (unsigned int i = 2; i < 8; ++i)
            {
                state.lines[i].offset = (state.lines[i].offset + 1) % state.lines[i].frames;
            }
        }
    }
}

#endif

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
    att = powf(att, 1.3f);
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