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
#include "SoundMixer.h"
#include "SoundMixerInternal.h"
#include "SoundSource.h"
#include "SoundBackend.h"
#include "SoundDSP.h"

#include "../Sound.h"
#include "../SoundRender.h"
#include "../ai_sounds.h"
#include "../Plugins/ResonanceAudio.h"

#include <pffft.h>

#define ENGINE_API
#include "../xrEngine/xr_object.h"

#define SND_SLOT_COUNT (512)
#define SND_MAX_PITCH (4)
#define SND_MAX_VELOCITY (100.0f)
#define SND_OCC_RAY_COUNT (12)
#define SND_OCC_MAX_RANGE (1000.0f)
#define SND_GOLDEN_ANGLE (2.39996322972865332f)
#define SND_ZONE_IDLE_MS (3000)
#define SND_ZONE_REVERB_GAIN (0.010f * 0.5f)
#define SND_SLOT_RESERVE (4096)
#define SND_ZONE_RESERVE (512)

using namespace XRay::Sound;

enum class sound_command_id : u8 {
    invalid,
    play,
    pause,
    stop,
    destroy,
    stop_all,
    pause_all,
    resume_all,
    update_parameter,
    set_volume,
    set_panning
};

enum class sound_occlusion : u8 {
    none,
    active,
    occluded
};

struct sound_command_desc {
    u32 slot;
    sound_command_id id;
    u16 argument;
    ref_sound* sound;
    double values[3];
    shared_str name;
    sound_source_state* source;
    float occ;
    bool has_occ;
};

struct sound_probe_desc {
    float occ;
    float indoor;
    u32 zone_idx;
    u8 pending_flags;
    bool has_pending_flags;
    bool has_occ;
    bool has_indoor;
    bool has_zone;
};

struct sound_mixer_state {
    xrSRWLock render_lock;
    xrSRWLock update_lock;
    xrSRWLock manage_lock;
    xrSRWLock sounds_lock;
    xrCriticalSection play_lock;

    float time_factor = 1.0f;
    float master_volume = 0.0f;
    float effect_volume = 0.0f;
    float music_volume = 0.0f;
    float shooting_volume = 0.0f;
    float compression = 0.0f;
    float compressor_envelope[SND_CHANNEL_COUNT] = { FLT_EPSILON, FLT_EPSILON };

    Fvector P, D, N;
    Fvector listener_velocity;

    xr_vector<u32> free_slots;
    xr_vector<u32> free_hrtf_slots;
    xr_vector<sound_command_desc> cmd;
    xr_vector<sound_command_desc> cmd_local;
    xr_vector<sound_probe_desc> probes;
    Fvector occ_cache[3];
    xr_vector<sound_slot_state> slots;
    xr_hash_set<ref_sound*> sounds;
    xr_vector<sound_zone_desc> zones;

    float buses[SND_BUS_COUNT][SND_CHANNEL_COUNT][SND_BLOCKSIZE];
    float read_buffer[SND_CHANNEL_COUNT][(SND_BLOCKSIZE + 1) * 10];

    std::atomic_bool editor_zone = false;
    bool hrtf_enabled = false;
};

static sound_mixer_state mixer_state = {};

static void
Snd_UnwrapPointer(float (*data)[SND_BLOCKSIZE], float** out_buffer)
{
    for (u32 channel_idx = 0; channel_idx < SND_CHANNEL_COUNT; channel_idx++) {
        out_buffer[channel_idx] = data[channel_idx];
    }
}

static void
Snd_GrowSlots()
{
    xrSRWLockGuard render_guard(mixer_state.render_lock, false);
    xrSRWLockGuard manage_guard(mixer_state.manage_lock, false);
    xrSRWLockGuard update_guard(mixer_state.update_lock, false);

    size_t old_size = mixer_state.slots.size();
    size_t new_size = std::max((size_t)SND_SLOT_COUNT, old_size * 2);

    mixer_state.slots.resize(new_size);
    mixer_state.free_slots.reserve(new_size);

    SND_STAT_ADD(snd_stats.possible_free_count, (int)(new_size - old_size));
    for (size_t slot_idx = old_size; slot_idx < new_size; slot_idx++) {
        mixer_state.slots[slot_idx].is_free = true;
        mixer_state.free_slots.push_back((u32)slot_idx + 1);
    }
}

static void
Snd_AcquireHrtfSlot(u32 slot_idx)
{
    PROF_EVENT("Sound: AcquireHRTFSlot");
    if (!mixer_state.hrtf_enabled || !psSoundFlags.is(ss_HRTF)) {
        return;
    }

    sound_slot_state* slot = &mixer_state.slots[slot_idx - 1];
    if ((slot->flags & (u16)Mixer::Flags::Spatial) == 0) {
        return;
    }

	if (slot->hrtf_slot != 0) {
		return;
	}

	if (mixer_state.free_hrtf_slots.empty()) {
		return;
	}

    slot->hrtf_slot = mixer_state.free_hrtf_slots.back();
    mixer_state.free_hrtf_slots.pop_back();
    Resonance_ResetHrtfSlot(slot->hrtf_slot - 1);
}

static void
Snd_ReleaseHrtfSlot(u32 slot_idx)
{
    sound_slot_state* slot = &mixer_state.slots[slot_idx - 1];
    if (slot->hrtf_slot == 0) {
        return;
    }

    Resonance_FreeHrtfSlot(slot->hrtf_slot - 1);
    mixer_state.free_hrtf_slots.push_back(slot->hrtf_slot);
    slot->hrtf_slot = 0;
}

static void
Snd_NewState(u32 slot_idx, Mixer::State state)
{
    if (slot_idx == 0) {
        return;
    }

    sound_slot_state* slot = &mixer_state.slots[slot_idx - 1];
    slot->prev_state = slot->state;
    slot->state = state;
    slot->fake_state = state;
}

static sound_occlusion
Snd_SlotOcclusion(u32 slot_idx, const sound_source_state* source, float* out_occ_volume)
{
    PROF_EVENT("Sound: SlotOcclusion");
    sound_slot_state* slot = &mixer_state.slots[slot_idx - 1];
    if (slot->state != Mixer::State::Playing) {
        return sound_occlusion::none;
    }

    if (source->desc.channels_count != 1) {
        return sound_occlusion::active;
    }
	
	if ((slot->flags & (u32)Mixer::Flags::Spatial) == 0) {
        return sound_occlusion::active;
	}

    Fvector* pos = &slot->parameters[(u32)Mixer::ParameterId::Position];
    Fvector* distances = &slot->parameters[(u32)Mixer::ParameterId::DistanceRange];
    if (mixer_state.P.distance_to(*pos) > distances->y) {
        if (out_occ_volume != nullptr) {
            *out_occ_volume = 0.0f;
        }

        return sound_occlusion::none;
    }

    if (out_occ_volume == nullptr) {
        return sound_occlusion::active;
    }

    *out_occ_volume = std::clamp(::Sound->get_occlusion_to(mixer_state.P, *pos), 0.0f, 1.0f);
    return (*out_occ_volume < 0.01f) ? sound_occlusion::occluded : sound_occlusion::active;
}

static u32
Snd_ReadSlotData(u32 slot_idx, sound_source_state* source, float** data, u32 frames)
{
    sound_slot_state* slot = &mixer_state.slots[slot_idx - 1];
    u32 position = slot->position;
    u32 left = frames;
    u32 wait_cycles = 0;

    while (left && position < source->desc.frames_total) {
        float* dst[SND_CHANNEL_COUNT];
        for (u32 channel_idx = 0; channel_idx < SND_CHANNEL_COUNT; channel_idx++) {
            dst[channel_idx] = &data[channel_idx][frames - left];
        }

        u32 copied = Snd_CopyCached(source, position, dst, left);
        if (copied) {
            left -= copied;
            position += copied;
            wait_cycles = 0;
            continue;
        }

        PROF_EVENT("Decode OGG Wait");
        SND_STAT_ADD(snd_stats.render_cache_miss, 1u);
        Snd_QueueDecode(&slot->sound_name, position);
        if (wait_cycles++ > 4096) {
            break;
        }

        std::this_thread::yield();
    }

    return frames - left;
}

static void
Snd_ReadSlot(u32 slot_idx, sound_source_state* source, float** data, u32 frames_count)
{
    sound_slot_state* slot = &mixer_state.slots[slot_idx - 1];
    if (source->desc.frames_total == 0) {
        Snd_NewState(slot_idx, Mixer::State::Stopped);
        return;
    }

    u32 left = frames_count;
    while (left) {
        float* offset_data[SND_CHANNEL_COUNT];
        for (u32 channel_idx = 0; channel_idx < SND_CHANNEL_COUNT; channel_idx++) {
            offset_data[channel_idx] = &data[channel_idx][frames_count - left];
        }

        u32 read_frames = Snd_ReadSlotData(slot_idx, source, offset_data, left);
        left -= read_frames;
        slot->position = std::min(slot->position + read_frames, source->desc.frames_total);

        if (slot->position < source->desc.frames_total) {
            if (read_frames == 0) {
                Snd_NewState(slot_idx, Mixer::State::Stopped);
                break;
            }

            continue;
        }

        slot->position = 0;
        if ((slot->flags & (u32)Mixer::Flags::Looped) == 0) {
            Snd_NewState(slot_idx, Mixer::State::Stopped);
            break;
        }
    }
}

static void
Snd_ProcessSlot(u32 slot_idx, sound_source_state* source, float** data)
{
    sound_slot_state* slot = &mixer_state.slots[slot_idx - 1];
    float pitch = slot->parameters[(u32)Mixer::ParameterId::Pitch].x;

    u32 output_frames = SND_BLOCKSIZE;
    float ratio = std::clamp(pitch * slot->doppler * mixer_state.time_factor, 0.0f, (float)SND_MAX_PITCH);
    u32 input_frames = std::max((u32)((float)output_frames * ratio), 1u);

    for (u32 channel_idx = 0; channel_idx < SND_CHANNEL_COUNT; channel_idx++) {
        memset(mixer_state.read_buffer[channel_idx], 0, (input_frames + 1) * sizeof(float));
    }

    if ((slot->flags & (u16)Mixer::Flags::Intro) || fis_zero(1.0f - ratio)) {
        Snd_ReadSlot(slot_idx, source, data, SND_BLOCKSIZE);
        return;
    }

    float* read_buffer[SND_CHANNEL_COUNT];
    for (u32 channel_idx = 0; channel_idx < SND_CHANNEL_COUNT; channel_idx++) {
        read_buffer[channel_idx] = mixer_state.read_buffer[channel_idx];
    }

    Snd_ReadSlot(slot_idx, source, read_buffer, input_frames + 1);
    if (slot->position > 0) {
        slot->position -= 1;
    }

    DSP_ResampleBuffer(read_buffer, data, slot->resample_state, input_frames, output_frames);
}

static void
Snd_PrecacheRenderCallback()
{
    PROF_EVENT("Sound: Precache Stage");
    xrSRWLockGuard manage_guard(mixer_state.manage_lock, true);

    static u64 counter = 0;
    static u64 timestamp = Snd_GetTimestamp();

    u64 now = Snd_GetTimestamp();
    float dt = (float)((double)(now - timestamp) / 1000000000.0);

    snd_stats.frame_time_micros = (u32)((now - timestamp) / 1000);
    timestamp = now;

    if (counter % 100 == 0) {
        SND_STAT_SET(snd_stats.cache_hit_count, 0u);
        SND_STAT_SET(snd_stats.cache_miss_count, 0u);
    }

    for (size_t slot_idx = 0; slot_idx < mixer_state.slots.size(); slot_idx++) {
        PROF_EVENT("Sound: Slot");
        sound_slot_state* slot = &mixer_state.slots[slot_idx];
        u32 slot_id = (u32)slot_idx + 1;
        if (slot->source == nullptr) {
            Snd_ReleaseHrtfSlot(slot_id);
            continue;
        }

        if (Snd_SlotOcclusion(slot_id, slot->source, nullptr) != sound_occlusion::none) {
            Snd_AcquireHrtfSlot(slot_id);
            if (!Snd_HasCacheLine(slot->source, slot->position)) {
                PROF_EVENT("Sound: QueueDecode");
                Snd_QueueDecode(&slot->sound_name, slot->position);
            }
        } else {
            Snd_ReleaseHrtfSlot(slot_id);
        }

        if (slot->state == Mixer::State::Delay) {
            slot->delay -= dt;
            if (slot->delay <= 0.0f) {
                Snd_NewState(slot_id, Mixer::State::Playing);
            }
        }
    }

    snd_stats.precache_time_micros = (u32)((Snd_GetTimestamp() - timestamp) / 1000);
    counter++;
}

static Fvector
Snd_Velocity(const Fvector* from, const Fvector* to, float dt)
{
    Fvector out; out.set(0.0f, 0.0f, 0.0f);

    if (dt > EPS_S) {
        out.sub(*to, *from).mul(1.0f / dt);
    } if (out.square_magnitude() > SND_MAX_VELOCITY * SND_MAX_VELOCITY) {
        out.set(0.0f, 0.0f, 0.0f);
    }

    return out;
}

static float
Snd_OcclusionFactor(const Fvector* pos)
{
    PROF_EVENT("Sound: Indoor Hemi");

    CDB::MODEL* env_model = ::Sound->get_geometry_env();
    CDB::COLLIDER* collider = ::Sound->get_geometry_db();
    if (env_model == nullptr || collider == nullptr) {
        return 0.0f;
    }


    float occluded = 0.0f;
    for (u32 ray_idx = 0; ray_idx < SND_OCC_RAY_COUNT; ray_idx++) {
        float height = (float)(ray_idx + 1) / (float)(SND_OCC_RAY_COUNT + 1);
        float radius = sqrtf(std::max(1.0f - height * height, 0.0f));
        float angle = (float)ray_idx * SND_GOLDEN_ANGLE;

        Fvector dir = { cosf(angle) * radius, height, sinf(angle) * radius };
        collider->ray_options(CDB::OPT_ONLYNEAREST);
        collider->ray_query(env_model, *pos, dir, SND_OCC_MAX_RANGE);
        if (collider->r_count()) {
            occluded += 1.0f;
        }
    }


    float t = std::clamp((occluded / (float)SND_OCC_RAY_COUNT - 0.1f) / 0.6f, 0.0f, 1.0f);
    return t * t * (3.0f - 2.0f * t);
}

static void
Snd_RenderSlot(u32 slot_idx, sound_source_state* source, float** process_buffer)
{
    sound_slot_state* slot = &mixer_state.slots[slot_idx - 1];

    float occ_volume = 1.0f;
    sound_occlusion occ = Snd_SlotOcclusion(slot_idx, source, &occ_volume);
    if (occ == sound_occlusion::none) {
        slot->position = std::min(slot->position + SND_BLOCKSIZE, source->desc.frames_total);
        if (slot->position == source->desc.frames_total && (slot->flags & (u16)Mixer::Flags::Looped) == 0) {
            Snd_NewState(slot_idx, Mixer::State::Stopped);
        }

        return;
    }

    for (u32 channel_idx = 0; channel_idx < SND_CHANNEL_COUNT; channel_idx++) {
        memset(process_buffer[channel_idx], 0, SND_BLOCKSIZE * sizeof(float));
    }

    Snd_ProcessSlot(slot_idx, source, process_buffer);

    Fvector* pos = &slot->parameters[(u32)Mixer::ParameterId::Position];
    Fvector* distances = &slot->parameters[(u32)Mixer::ParameterId::DistanceRange];
    Fvector* volumes = &slot->parameters[(u32)Mixer::ParameterId::VolumePerChannel];
    bool is_music = (slot->flags & (u16)Mixer::Flags::Intro) != 0;
    if ((slot->flags & (u16)Mixer::Flags::NoOCC) == 0 || occ != sound_occlusion::occluded) {
        occ_volume = 1.0f;
    }

    float begin_factor = 1.0f;
    float end_factor = 1.0f;
    if (slot->stopping_position != (u32)-1) {
        u32 stopping_total = source->desc.frames_total - slot->stopping_position;
        if (stopping_total > 1 && slot->position >= slot->stopping_position) {
            u32 begin_offset = slot->position - slot->stopping_position;
            u32 samples_count = is_music ? SND_BLOCKSIZE : (u32)((float)SND_BLOCKSIZE * mixer_state.time_factor);
            u32 end_offset = std::min(begin_offset + samples_count, source->desc.frames_total - 1);

            begin_factor = std::clamp(1.0f - ((float)begin_offset / (float)(stopping_total - 1)), 0.0f, 1.0f);
            end_factor = std::clamp(1.0f - ((float)end_offset / (float)(stopping_total - 1)), 0.0f, 1.0f);
        }
    }

    float mix_volume = mixer_state.effect_volume;
    if (slot->flags & (u16)Mixer::Flags::Music) {
        mix_volume = mixer_state.music_volume;
    } else if (slot->flags & (u16)Mixer::Flags::Shooting) {
        mix_volume = mixer_state.shooting_volume;
    }

    float final_volume = occ_volume * (volumes->x * volumes->y * volumes->z) * mix_volume;
    begin_factor *= final_volume;
    end_factor *= final_volume;

    float left_panning = slot->parameters[(u32)Mixer::ParameterId::Panning].x;
    float right_panning = slot->parameters[(u32)Mixer::ParameterId::Panning].y;

    if (!is_music && source->desc.channels_count == 1) {
        PROF_EVENT("Slot Spatial");

        if (slot->flags & (u32)Mixer::Flags::Spatial) {
            dsp_spatial_desc spatial = {
                slot->panning, &mixer_state.P, &mixer_state.D, &mixer_state.N,
                &mixer_state.listener_velocity, pos, &slot->velocity, &slot->doppler
            };

			bool not_shooting = !(slot->flags & (u32)Mixer::Flags::Shooting);
            if (psSoundFlags.is(ss_HRTF) && mixer_state.hrtf_enabled && not_shooting && slot->hrtf_slot != 0) {
                Fvector relative_pos;
                float distance;
                DSP_CalculateRelativePosition(&spatial, &relative_pos, &distance);
                DSP_Doppler(&spatial, distance);

                Resonance_ProcessHrtf(slot->hrtf_slot - 1, process_buffer, &relative_pos);

                float attenuation = DSP_Attenuation(distances, std::max(distance, 0.1f), 2.0f);
                for (u32 channel_idx = 0; channel_idx < SND_CHANNEL_COUNT; channel_idx++) {
                    for (u32 frame_idx = 0; frame_idx < SND_BLOCKSIZE; frame_idx++) {
                        process_buffer[channel_idx][frame_idx] *= attenuation;
                    }
                }
            } else {
                DSP_SpatialProcess(process_buffer, distances, &spatial);
            }
        }

        if (mixer_state.editor_zone) {
            slot->zone_idx = 1;
        }

        if (slot->zone_idx && slot->zone_idx <= mixer_state.zones.size()) {
            sound_zone_desc* zone = &mixer_state.zones[slot->zone_idx - 1];
            zone->use_count++;
            zone->last_use_ms = Snd_Milliseconds();

            float* buffer[SND_CHANNEL_COUNT];
			Snd_UnwrapPointer(zone->data, buffer);

            float fade = std::clamp(slot->indoor_factor, 0.0f, 1.0f);
			DSP_MixBuffer(buffer, process_buffer, begin_factor * fade, end_factor * fade, left_panning, right_panning, SND_BLOCKSIZE);
        }
    }

    float* bus_buffer[SND_CHANNEL_COUNT];
    Snd_UnwrapPointer(mixer_state.buses[is_music ? SND_BUS_MUSIC : SND_BUS_EFFECTS], bus_buffer);
    DSP_MixBuffer(bus_buffer, process_buffer, begin_factor, end_factor, left_panning, right_panning, SND_BLOCKSIZE);
}

static void
Snd_MixerRenderCallback(float* buffer)
{
    PROF_EVENT("Sound: Render Stage");

    xrSRWLockGuard render_guard(mixer_state.render_lock, true);

    SND_STAT_SET(snd_stats.render_cache_miss, 0u);

    u64 timestamp = Snd_GetTimestamp();

    memset(buffer, 0, SND_BLOCKSIZE * SND_CHANNEL_COUNT * sizeof(float));
    memset(mixer_state.buses, 0, sizeof(mixer_state.buses));

    static float process_data[SND_CHANNEL_COUNT][SND_BLOCKSIZE] = {};
    float* process_buffer[SND_CHANNEL_COUNT];
    Snd_UnwrapPointer(process_data, process_buffer);

    for (size_t zone_idx = 0; zone_idx < mixer_state.zones.size(); zone_idx++) {
        mixer_state.zones[zone_idx].use_count = 0;
        memset(mixer_state.zones[zone_idx].data, 0, sizeof(mixer_state.zones[zone_idx].data));
    }

    for (size_t slot_idx = 0; slot_idx < mixer_state.slots.size(); slot_idx++) {
        PROF_EVENT("Slot Render");
        sound_slot_state* slot = &mixer_state.slots[slot_idx];
        if (slot->state != Mixer::State::Playing) {
            continue;
        }

        if (slot->source == nullptr) {
            Snd_NewState((u32)slot_idx + 1, Mixer::State::Stopped);
            continue;
        }

        Snd_RenderSlot((u32)slot_idx + 1, slot->source, process_buffer);
    }

    float* reverb_buffer[SND_CHANNEL_COUNT];
    Snd_UnwrapPointer(mixer_state.buses[SND_BUS_REVERB], reverb_buffer);

    if (psSoundFlags.is(ss_EFX)) {
        for (size_t zone_idx = 0; zone_idx < mixer_state.zones.size(); zone_idx++) {
            sound_zone_desc* zone = &mixer_state.zones[zone_idx];

			bool outdated = (zone->last_use_ms + SND_ZONE_IDLE_MS) < Snd_Milliseconds();
            if (zone->use_count == 0 && outdated) {
                continue;
            }

            PROF_EVENT("Reverb rendering");
            float* zone_buffer[SND_CHANNEL_COUNT];
            Snd_UnwrapPointer(zone->data, zone_buffer);

            Resonance_ProcessZone(zone, zone_buffer, process_buffer, reverb_buffer);

            float reverb_gain = std::clamp(zone->settings.reverb, 0.0f, 1.0f) * SND_ZONE_REVERB_GAIN;
            DSP_MixBuffer(reverb_buffer, process_buffer, reverb_gain, reverb_gain, 1.0f, 1.0f, SND_BLOCKSIZE);
        }
    }

    {
        PROF_EVENT("Sound Mixing");
        float* master_buffer[SND_CHANNEL_COUNT];
        Snd_UnwrapPointer(mixer_state.buses[SND_BUS_MASTER], master_buffer);

        for (u32 bus_idx = SND_BUS_MASTER + 1; bus_idx < SND_BUS_COUNT; bus_idx++) {
            float* bus_buffer[SND_CHANNEL_COUNT];
            Snd_UnwrapPointer(mixer_state.buses[bus_idx], bus_buffer);
            DSP_MixBuffer(master_buffer, bus_buffer, 1.0f, 1.0f, 1.0f, 1.0f, SND_BLOCKSIZE);
        }

        DSP_Compressor(0.0001f, 0.100f, -20.0f, 2.0f, master_buffer, mixer_state.compression, SND_BLOCKSIZE, mixer_state.compressor_envelope);

        for (u32 frame_idx = 0; frame_idx < SND_BLOCKSIZE; frame_idx++) {
            for (u32 channel_idx = 0; channel_idx < SND_CHANNEL_COUNT; channel_idx++) {
                buffer[frame_idx * SND_CHANNEL_COUNT + channel_idx] = std::clamp(master_buffer[channel_idx][frame_idx], -1.0f, 1.0f) * mixer_state.master_volume;
            }
        }
    }

    snd_stats.render_time_micros = (u32)((Snd_GetTimestamp() - timestamp) / 1000);
}

static void
Snd_DestroyInternal(u32 slot_idx)
{
    if (slot_idx == 0) {
        return;
    }

    sound_slot_state* slot = &mixer_state.slots[slot_idx - 1];
    if (slot->is_free) {
        return;
    }

    Snd_ReleaseHrtfSlot(slot_idx);
    if (slot->source != nullptr) {
        Snd_ReleaseSource(&slot->sound_name);
        slot->source = nullptr;
    }

    slot->sound_name.clear();
    memset(slot->parameters, 0, sizeof(slot->parameters));
    memset(slot->resample_state, 0, sizeof(slot->resample_state));
    slot->position = 0;
    slot->stopping_position = (u32)-1;
    slot->flags = 0;
    slot->state = Mixer::State::Stopped;
    slot->prev_state = Mixer::State::Stopped;
    slot->fake_state = Mixer::State::Stopped;
    slot->is_free = true;
    mixer_state.free_slots.push_back(slot_idx);
}

void
Mixer::Initialize()
{
    mixer_state.slots.clear();
    mixer_state.free_slots.clear();
    mixer_state.cmd.clear();
    mixer_state.cmd_local.clear();
    mixer_state.slots.reserve(SND_SLOT_RESERVE);
    mixer_state.zones.reserve(SND_ZONE_RESERVE);
    mixer_state.cmd.reserve(256);
    mixer_state.cmd_local.reserve(256);
    Snd_GrowSlots();
    Snd_InitSources();

    mixer_state.hrtf_enabled = Resonance_Initialize();
    mixer_state.free_hrtf_slots.resize(SND_HRTF_SLOT_COUNT);
    for (u32 hrtf_idx = 0; hrtf_idx < SND_HRTF_SLOT_COUNT; hrtf_idx++) {
        mixer_state.free_hrtf_slots[hrtf_idx] = hrtf_idx + 1;
    }

    Backend::Initialize(Snd_MixerRenderCallback, Snd_PrecacheRenderCallback);
}

void
Mixer::Shutdown()
{
    Backend::Shutdown();
    for (size_t slot_idx = 0; slot_idx < mixer_state.slots.size(); slot_idx++) {
        sound_slot_state* slot = &mixer_state.slots[slot_idx];
        if (slot->source != nullptr) {
            Snd_ReleaseSource(&slot->sound_name);
            slot->source = nullptr;
        }
    }

    Snd_ShutdownSources();

    Resonance_Shutdown();
    mixer_state.hrtf_enabled = false;
    mixer_state.free_hrtf_slots.clear();
    mixer_state.slots.clear();
    mixer_state.free_slots.clear();
    mixer_state.cmd.clear();
    mixer_state.cmd_local.clear();
}

void
Mixer::Update(void* event_handler, float time_factor, float volume, float eff_volume, float mus_volume, float shooting_volume, float compression, Fvector P, Fvector D, Fvector N)
{
    PROF_EVENT("Sound: Update Stage");
    sound_event* handler = (sound_event*)event_handler;

    static u64 timestamp = Snd_GetTimestamp();
    float dt = (float)((Snd_GetTimestamp() - timestamp) / 1000000) * 0.001f;
    timestamp = Snd_GetTimestamp();

    {
        xrCriticalSectionGuard play_guard(mixer_state.play_lock);
        mixer_state.cmd_local.clear();
        mixer_state.cmd.swap(mixer_state.cmd_local);
    }

    size_t slot_count = mixer_state.slots.size();
    mixer_state.probes.resize(slot_count);
    memset(mixer_state.probes.data(), 0, slot_count * sizeof(sound_probe_desc));

    for (size_t cmd_idx = 0; cmd_idx < mixer_state.cmd_local.size(); cmd_idx++) {
        sound_command_desc* cmd = &mixer_state.cmd_local[cmd_idx];
        if (cmd->slot == 0 || cmd->slot > slot_count) {
            continue;
        }

        sound_probe_desc* probe = &mixer_state.probes[cmd->slot - 1];
        if (cmd->id == sound_command_id::play) {
            xr_string cmd_name = cmd->name.c_str();

            cmd->source = Snd_AcquireSource(&cmd_name);
            probe->pending_flags = (u8)cmd->argument;
            probe->has_pending_flags = true;
        } else if (cmd->id == sound_command_id::update_parameter) {
			if (cmd->argument == (u16)ParameterId::Position) {
				u8 slot_flags = probe->has_pending_flags ? probe->pending_flags : mixer_state.slots[cmd->slot - 1].flags;

				if (slot_flags & (u16)Flags::Spatial) {
					Fvector cmd_pos = { (float)cmd->values[0], (float)cmd->values[1], (float)cmd->values[2] };
					cmd->occ = ::Sound->get_occlusion(cmd_pos, 0.2f, mixer_state.occ_cache);
					cmd->has_occ = true;
				}
			}
        }
    }

    for (size_t slot_idx = 0; slot_idx < slot_count; slot_idx++) {
        sound_slot_state* slot = &mixer_state.slots[slot_idx];
        sound_probe_desc* probe = &mixer_state.probes[slot_idx];

        if (((slot->flags & (u16)Flags::NoFeedback) && slot->state == State::Stopped)) {
            continue;
        }

		if ((slot->flags & ((u16)Flags::Intro | (u16)Flags::NoOCC)) != 0) {
			continue;
		}

		if (slot->state != State::Playing) {
			continue;
		}

        Fvector pos = (slot->flags & (u16)Flags::Spatial) ? slot->parameters[(u32)ParameterId::Position] : mixer_state.P;
        if (slot->flags & (u16)Flags::Shooting) {
            probe->indoor = Snd_OcclusionFactor(&pos);
            probe->has_indoor = true;
        }

        if (P.distance_to(pos) > slot->parameters[(u32)ParameterId::DistanceRange].y) {
            continue;
        }

        probe->occ = ::Sound->get_occlusion(pos, 0.2f, mixer_state.occ_cache);
        probe->has_occ = true;
        probe->has_zone = true;

        CDB::MODEL* env_model = ::Sound->get_geometry_env();
        if (env_model == nullptr) {
            continue;
        }

        CDB::COLLIDER* collider = ::Sound->get_geometry_db();
        Fvector down = { 0.0f, -1.0f, 0.0f };

        collider->ray_options(CDB::OPT_ONLYNEAREST);
        collider->ray_query(env_model, pos, down, 1000.0f);
        if (collider->r_count()) {
            CDB::TRI* tri = &env_model->get_tris()[collider->r_begin()->id];
            R_ASSERT(tri->dummy < mixer_state.zones.size());
            probe->zone_idx = tri->dummy + 1;
        }
    }

    mixer_state.render_lock.AcquireExclusive();
    mixer_state.manage_lock.AcquireExclusive();
    mixer_state.update_lock.AcquireExclusive();
    mixer_state.sounds_lock.AcquireShared();

    mixer_state.time_factor = std::clamp(time_factor, 0.1f, 10.0f);
    mixer_state.compression = compression;
    mixer_state.master_volume = volume;
    mixer_state.effect_volume = eff_volume;
    mixer_state.music_volume = mus_volume;
    mixer_state.shooting_volume = shooting_volume;

    mixer_state.listener_velocity = Snd_Velocity(&mixer_state.P, &P, dt);
    mixer_state.P = P;
    mixer_state.D = D;
    mixer_state.N = N;

    for (auto& sound : mixer_state.sounds) {
        if (sound == nullptr || !sound->slot() || sound->_g_object() == nullptr || !sound->unique_id()) {
            continue;
        }

        sound_slot_state* slot = &mixer_state.slots[sound->slot() - 1];
        if (slot->fake_state != State::Playing || slot->state != State::Playing) {
            continue;
        }

        if ((slot->flags & (u16)Flags::Spatial) && (slot->flags & (u16)Flags::NoPosUpdate) == 0) {
            slot->parameters[(u32)ParameterId::Position] = ((IRenderable*)sound->_g_object())->renderable.xform.c;
        }

        if (sound->_p == nullptr || sound->_p->g_type == 0) {
            continue;
        }

        sound->TimeToPropagade -= dt;
        if (sound->TimeToPropagade > 0.0f) {
            continue;
        }

        sound->TimeToPropagade = s_f_def_event_pulse;
        if (handler == nullptr) {
            continue;
        }

        const Fvector* distances = &slot->parameters[(u32)ParameterId::DistanceRange];
        float range = std::min(distances->z, distances->z * slot->parameters[(u32)ParameterId::VolumePerChannel].y);
        if (range >= 0.1f) {
            handler(sound->_p, range);
        }
    }

    for (size_t slot_idx = 0; slot_idx < slot_count; slot_idx++) {
        sound_slot_state* slot = &mixer_state.slots[slot_idx];
        sound_probe_desc* probe = &mixer_state.probes[slot_idx];
        if ((slot->flags & (u16)Flags::NoFeedback) && slot->state == State::Stopped) {
            Snd_DestroyInternal((u32)slot_idx + 1);
            SND_STAT_ADD(snd_stats.possible_free_count, 1);
            continue;
        }

        if (slot->flags & (u16)Flags::Spatial) {
            const Fvector* slot_pos = &slot->parameters[(u32)ParameterId::Position];
            slot->velocity = Snd_Velocity(&slot->prev_position, slot_pos, dt);
            slot->prev_position = *slot_pos;
        }

        if (probe->has_indoor) {
            slot->indoor_factor += (probe->indoor - slot->indoor_factor) * std::clamp(dt * 3.0f, 0.0f, 1.0f);
        } if (probe->has_occ) {
            Snd_VolumeLerp(&slot->parameters[(u32)ParameterId::VolumePerChannel].z, probe->occ, 1.0f, dt);
        } if (probe->has_zone) {
            slot->zone_idx = probe->zone_idx;
        }
    }

    for (size_t cmd_idx = 0; cmd_idx < mixer_state.cmd_local.size(); cmd_idx++) {
        const sound_command_desc* cmd = &mixer_state.cmd_local[cmd_idx];
        sound_slot_state* slot = (cmd->slot != 0) ? &mixer_state.slots[cmd->slot - 1] : nullptr;

        switch (cmd->id) {
            case sound_command_id::play: {
                ref_sound* sound = (cmd->sound != nullptr && mixer_state.sounds.contains(cmd->sound)) ? cmd->sound : nullptr;
                u16 play_flags = cmd->argument;

                xr_string cmd_name = cmd->name.c_str();
                bool is_same_file = (slot->source != nullptr && slot->sound_name == cmd_name);
                if (!is_same_file && slot->source != nullptr) {
                    Snd_ReleaseSource(&slot->sound_name);
                    slot->source = nullptr;
                    slot->sound_name.clear();
                }

                sound_source_state* source = cmd->source;
                if (source == nullptr) {
                    Snd_NewState(cmd->slot, State::Stopped);
                    break;
                }

                if (is_same_file) {
                    Snd_ReleaseSource(&cmd_name);
                } else {
                    slot->source = source;
                    slot->sound_name = cmd_name;
                }

                memset(slot->parameters, 0, sizeof(slot->parameters));
                memset(slot->resample_state, 0, sizeof(slot->resample_state));
                slot->parameters[(u32)ParameterId::VolumePerChannel].set(source->desc.volume, 1.0f, 1.0f);
                slot->parameters[(u32)ParameterId::DistanceRange].set(source->desc.min_distance, source->desc.max_distance, source->desc.max_ai_distance);
                slot->parameters[(u32)ParameterId::Pitch].set(1.0f, 1.0f, 1.0f);
                slot->parameters[(u32)ParameterId::Panning].set(1.0f, 1.0f, 1.0f);
                slot->position = 0;
                slot->stopping_position = (u32)-1;
                slot->flags = (u8)play_flags;

                Snd_QueueDecode(&slot->sound_name, 0);

                if ((slot->flags & (u16)Flags::Spatial) && sound != nullptr && sound->_g_object() != nullptr) {
                    slot->parameters[(u32)ParameterId::Position] = ((IRenderable*)sound->_g_object())->renderable.xform.c;
                }

                slot->doppler = 1.0f;
                slot->velocity.set(0.0f, 0.0f, 0.0f);
                slot->prev_position = slot->parameters[(u32)ParameterId::Position];

                float ai_range = std::min(source->desc.max_ai_distance, source->desc.max_ai_distance * source->desc.volume);
                CObject* object = (sound != nullptr) ? sound->_g_object() : nullptr;
                if (handler != nullptr && ai_range >= 0.1f && object != nullptr && sound->_p != nullptr) {
                    int game_type = sound->_p->g_type;
                    if (game_type == (int)sg_SourceType) {
                        game_type = (int)source->desc.game_type;
                        sound->_p->g_type = game_type;
                    }

                    if (play_flags & (u16)Flags::NoFeedback) {
                        ref_sound_data_ptr data_ptr = new ref_sound_data();
                        data_ptr->slot = cmd->slot;
                        data_ptr->g_type = game_type;
                        data_ptr->g_object = object;
                        data_ptr->dont_destroy_slot = true;
                        data_ptr->fn_attached[0] = source->desc.path;
                        handler(data_ptr, ai_range);
                    } else {
                        handler(sound->_p, ai_range);
                    }
                }

                if (cmd->values[0] != 0.0) {
                    slot->delay = (float)cmd->values[0];
                    Snd_NewState(cmd->slot, State::Delay);
                } else {
                    Snd_NewState(cmd->slot, State::Playing);
                }
            } break;

            case sound_command_id::pause: {
                Snd_NewState(cmd->slot, State::Paused);
            } break;

            case sound_command_id::stop: {
                if (cmd->argument) {
                    slot->flags &= ~((u8)Flags::Looped);
                    slot->stopping_position = slot->position;
                } else {
                    Snd_NewState(cmd->slot, State::Stopped);
                    slot->position = 0;
                    slot->stopping_position = (u32)-1;
                }
            } break;

            case sound_command_id::destroy: {
                if (slot->state != State::Delay) {
                    Snd_DestroyInternal(cmd->slot);
                }
            } break;

            case sound_command_id::stop_all: {
                for (size_t slot_idx = 0; slot_idx < mixer_state.slots.size(); slot_idx++) {
                    sound_slot_state* other = &mixer_state.slots[slot_idx];
                    if (other->sound_name.size() && other->state != State::Stopped) {
                        other->position = 0;
                        other->stopping_position = (u32)-1;
                        other->prev_state = other->state;
                        other->state = State::Stopped;
                        other->fake_state = State::Stopped;
                    }
                }
            } break;

            case sound_command_id::pause_all: {
                for (size_t slot_idx = 0; slot_idx < mixer_state.slots.size(); slot_idx++) {
                    sound_slot_state* other = &mixer_state.slots[slot_idx];
                    if (other->sound_name.size() && other->state != State::Stopped && other->state != State::Paused) {
                        other->prev_state = other->state;
                        other->state = State::Paused;
                    }
                }
            } break;

            case sound_command_id::resume_all: {
                for (size_t slot_idx = 0; slot_idx < mixer_state.slots.size(); slot_idx++) {
                    sound_slot_state* other = &mixer_state.slots[slot_idx];
                    if (other->state == State::Paused && other->prev_state != State::Paused) {
                        other->state = other->prev_state;
                        other->prev_state = State::Paused;
                    }
                }
            } break;

            case sound_command_id::update_parameter: {
                slot->parameters[(u32)cmd->argument].set((float)cmd->values[0], (float)cmd->values[1], (float)cmd->values[2]);
                if (cmd->has_occ) {
                    Snd_VolumeLerp(&slot->parameters[(u32)ParameterId::VolumePerChannel].z, cmd->occ, 1.0f, dt);
                }
            } break;

            case sound_command_id::set_volume: {
                slot->parameters[(u32)ParameterId::VolumePerChannel].y = (float)cmd->values[0];
            } break;

            case sound_command_id::set_panning: {
                slot->parameters[(u32)ParameterId::Panning].x = (float)cmd->values[0];
                slot->parameters[(u32)ParameterId::Panning].y = (float)cmd->values[1];
            } break;
        }
    }

    mixer_state.cmd_local.clear();
    snd_stats.update_time_micros = (u32)((Snd_GetTimestamp() - timestamp) / 1000);

    mixer_state.sounds_lock.ReleaseShared();
    mixer_state.update_lock.ReleaseExclusive();
    mixer_state.manage_lock.ReleaseExclusive();
    mixer_state.render_lock.ReleaseExclusive();
}

void
Mixer::StopAll()
{
    xrCriticalSectionGuard guard(mixer_state.play_lock);
    sound_command_desc command = { .id = sound_command_id::stop_all };
    mixer_state.cmd.push_back(command);
}

void
Mixer::PauseAll()
{
    xrCriticalSectionGuard guard(mixer_state.play_lock);
    sound_command_desc command = { .id = sound_command_id::pause_all };
    mixer_state.cmd.push_back(command);
}

void
Mixer::ResumeAll()
{
    xrCriticalSectionGuard guard(mixer_state.play_lock);
    sound_command_desc command = { .id = sound_command_id::resume_all };
    mixer_state.cmd.push_back(command);
}

void
Mixer::DereferenceObjects(CObject** object, int count)
{
    xrSRWLockGuard sounds_guard(mixer_state.sounds_lock, false);

    for (auto& sound : mixer_state.sounds) {
        if (sound == nullptr || sound->_p == nullptr) {
            continue;
        }

        for (int object_idx = 0; object_idx < count; object_idx++) {
            if (object[object_idx] == sound->_g_object()) {
                sound->_p->g_object = nullptr;
            }
        }
    }
}

u32
Mixer::Create()
{
    for (;;) {
        {
            xrSRWLockGuard update_guard(mixer_state.update_lock);
            if (!mixer_state.free_slots.empty()) {
                u32 slot_idx = mixer_state.free_slots.back();
                mixer_state.free_slots.pop_back();
                mixer_state.slots[slot_idx - 1].is_free = false;
                SND_STAT_ADD(snd_stats.possible_free_count, -1);
                return slot_idx;
            }
        }

        Snd_GrowSlots();
    }
}

void
Mixer::Destroy(u32 slot_id)
{
    if (slot_id == 0 || mixer_state.slots[slot_id - 1].state == State::Delay) {
        return;
    }

    mixer_state.slots[slot_id - 1].fake_state = State::Stopped;

    xrCriticalSectionGuard guard(mixer_state.play_lock);
    sound_command_desc command = { .slot = slot_id, .id = sound_command_id::destroy };
    mixer_state.cmd.push_back(command);
    SND_STAT_ADD(snd_stats.possible_free_count, 1);
}

void
Mixer::Play(u32 slot_id, u16 flags, ref_sound* sound, double delay)
{
    xrCriticalSectionGuard guard(mixer_state.play_lock);
    if (slot_id == 0 || sound == nullptr || sound->_p == nullptr || sound->_p->fn_attached[0] == nullptr) {
        return;
    }

    mixer_state.slots[slot_id - 1].fake_state = State::Playing;

    sound_command_desc command = { .slot = slot_id, .id = sound_command_id::play, .argument = flags, .sound = sound, .values = { delay }, .name = sound->_p->fn_attached[0] };
    mixer_state.cmd.push_back(command);
}

void
Mixer::PlayNoFeedback(u16 flags, ref_sound* sound, double delay, float* pitch, float* volume, Fvector* distance, Fvector* pos)
{
    u32 slot_id = Create();
    mixer_state.slots[slot_id - 1].state = State::Paused;
    mixer_state.slots[slot_id - 1].fake_state = State::Playing;

    CSound_params params = sound->_p->get_params();
    Fvector distances = { params.min_distance, params.max_distance, params.max_ai_distance };
    if (sound->slot()) {
        pitch = (pitch != nullptr) ? pitch : &params.freq;
        distance = (distance != nullptr) ? distance : &distances;
        pos = (pos != nullptr) ? pos : &params.position;
        volume = (volume != nullptr) ? volume : &params.volume;
    }

    sound_command_desc commands[5] = {};
    u32 command_count = 0;
    commands[command_count++] = { .slot = slot_id, .id = sound_command_id::play, .argument = flags, .sound = sound, .values = { delay }, .name = sound->_p->fn_attached[0] };
    if (pitch != nullptr) {
        commands[command_count++] = { .slot = slot_id, .id = sound_command_id::update_parameter, .argument = (u16)ParameterId::Pitch, .values = { *pitch, 1.0, 1.0 } };
    } if (distance != nullptr) {
        commands[command_count++] = { .slot = slot_id, .id = sound_command_id::update_parameter, .argument = (u16)ParameterId::DistanceRange, .values = { distance->x, distance->y, distance->z } };
    } if (pos != nullptr) {
        commands[command_count++] = { .slot = slot_id, .id = sound_command_id::update_parameter, .argument = (u16)ParameterId::Position, .values = { pos->x, pos->y, pos->z } };
    } if (volume != nullptr) {
        commands[command_count++] = { .slot = slot_id, .id = sound_command_id::set_volume, .values = { std::clamp((double)*volume, 0.0, 1.0) } };
    }

    xrCriticalSectionGuard guard(mixer_state.play_lock);
    for (u32 command_idx = 0; command_idx < command_count; command_idx++) {
        mixer_state.cmd.push_back(commands[command_idx]);
    }
}

void
Mixer::Pause(u32 slot_id)
{
    if (slot_id == 0) {
        return;
    }

    xrCriticalSectionGuard guard(mixer_state.play_lock);
    mixer_state.slots[slot_id - 1].fake_state = State::Paused;
    sound_command_desc command = { .slot = slot_id, .id = sound_command_id::pause };
    mixer_state.cmd.push_back(command);
}

void
Mixer::Stop(u32 slot_id, bool is_deferred)
{
    if (slot_id == 0 || mixer_state.slots[slot_id - 1].state == State::Delay) {
        return;
    }

    if (!is_deferred) {
        mixer_state.slots[slot_id - 1].fake_state = State::Stopped;
    }

    xrCriticalSectionGuard guard(mixer_state.play_lock);
    sound_command_desc command = { .slot = slot_id, .id = sound_command_id::stop, .argument = is_deferred };
    mixer_state.cmd.push_back(command);
}

void
Mixer::UpdateParameter(u32 slot_id, ParameterId parameter, Fvector value)
{
    if (slot_id == 0) {
        return;
    }

    xrCriticalSectionGuard guard(mixer_state.play_lock);
    sound_command_desc command = { .slot = slot_id, .id = sound_command_id::update_parameter, .argument = (u16)parameter, .values = { value.x, value.y, value.z } };
    mixer_state.cmd.push_back(command);
}

void
Mixer::SetVolume(u32 slot_id, double volume)
{
    if (slot_id == 0) {
        return;
    }

    xrCriticalSectionGuard guard(mixer_state.play_lock);
    sound_command_desc command = { .slot = slot_id, .id = sound_command_id::set_volume, .values = { std::clamp(volume, 0.0, 1.0) } };
    mixer_state.cmd.push_back(command);
}

void
Mixer::SetPanning(u32 slot_id, double left, double right)
{
    if (slot_id == 0) {
        return;
    }

    xrCriticalSectionGuard guard(mixer_state.play_lock);
    sound_command_desc command = { .slot = slot_id, .id = sound_command_id::set_panning, .values = { std::clamp(left, 0.0, 1.0), std::clamp(right, 0.0, 1.0) } };
    mixer_state.cmd.push_back(command);
}

xr_vector<sound_slot_state>&
Mixer::GetSlots()
{
    return mixer_state.slots;
}

xrSRWLock&
Mixer::GetRenderMutex()
{
    return mixer_state.render_lock;
}

xrSRWLock&
Mixer::GetUpdateMutex()
{
    return mixer_state.update_lock;
}

xrSRWLock&
Mixer::GetManageMutex()
{
    return mixer_state.manage_lock;
}

sound_stats*
Mixer::GetStats()
{
    return &snd_stats;
}

float
Mixer::GetPlaytime(u32 slot_id)
{
    if (slot_id == 0) {
        return 0.0f;
    }

    return (float)mixer_state.slots[slot_id - 1].position / (float)SND_SAMPLERATE;
}

float
Mixer::GetDuration(u32 slot_id)
{
    xrSRWLockGuard guard(snd_source_lock, true);
    if (slot_id == 0) {
        return 0.0f;
    }

    sound_source_state* source = Snd_LookupSource(&mixer_state.slots[slot_id - 1].sound_name);
    return (source != nullptr) ? (float)source->desc.frames_total / (float)SND_SAMPLERATE : 0.0f;
}

bool
Mixer::SlotIsRelated(u32 slot_id)
{
    if (slot_id == 0) {
        return false;
    }

    const xr_string* name = &mixer_state.slots[slot_id - 1].sound_name;
    sound_source_state* source = Snd_FindSource(name);
    if (source == nullptr) {
        return false;
    }

    bool is_related = Snd_SlotOcclusion(slot_id, source, nullptr) != sound_occlusion::none;
    Snd_ReleaseSource(name);
    return is_related;
}

u32
Mixer::GetGameType(u32 slot_id)
{
    xrSRWLockGuard guard(snd_source_lock, true);
    if (slot_id == 0) {
        return 0;
    }

    sound_source_state* source = Snd_LookupSource(&mixer_state.slots[slot_id - 1].sound_name);
    return (source != nullptr) ? source->desc.game_type : 0;
}

u32
Mixer::GetFlags(u32 slot_id)
{
    return (slot_id == 0) ? 0 : mixer_state.slots[slot_id - 1].flags;
}

Mixer::State
Mixer::GetState(u32 slot_id)
{
    return (slot_id == 0) ? State::Stopped : mixer_state.slots[slot_id - 1].fake_state;
}

Fvector*
Mixer::GetParameters(u32 slot_id)
{
    return (slot_id == 0) ? nullptr : mixer_state.slots[slot_id - 1].parameters;
}

void
Mixer::AddEditorZone(sound_zone_desc* zone)
{
	mixer_state.editor_zone = true;
    ResetZones();
    AddZone(zone);
}

void
Mixer::AddZone(sound_zone_desc* zone)
{
    Resonance_InitZone(zone);

    xrSRWLockGuard render_guard(mixer_state.render_lock);
    mixer_state.zones.push_back(*zone);
}

void
Mixer::ResetZones()
{
    xrSRWLockGuard render_guard(mixer_state.render_lock);

    for (size_t zone_idx = 0; zone_idx < mixer_state.zones.size(); zone_idx++) {
        Resonance_ReleaseZone(&mixer_state.zones[zone_idx]);
    }

    mixer_state.zones.clear();
}

xr_vector<sound_zone_desc>&
Mixer::GetZones()
{
    return mixer_state.zones;
}

ref_sound::ref_sound()
{
    xrSRWLockGuard sounds_guard(mixer_state.sounds_lock, false);
    mixer_state.sounds.insert(this);
}

ref_sound::~ref_sound()
{
    xrSRWLockGuard sounds_guard(mixer_state.sounds_lock, false);
    mixer_state.sounds.erase(this);
}
