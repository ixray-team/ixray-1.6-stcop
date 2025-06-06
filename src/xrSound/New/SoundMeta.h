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

#define SND_CHANNEL_COUNT (2)
#define SND_SAMPLERATE 44100
#define SND_BLOCKSIZE (1 << 10)
#define SND_REBERB_BUFFER_SIZE (SND_SAMPLERATE*8)
#define SND_REBERB_LINE_COUNT (8*2+2)

#define SND_BUS_MASTER 0
#define SND_BUS_REVERB 1
#define SND_BUS_EFFECTS 2
#define SND_BUS_MUSIC 3
#define SND_BUS_LAST SND_BUS_MUSIC
#define SND_BUS_COUNT (SND_BUS_LAST+1)

typedef void(*audio_render_callback)(float*);
typedef void(*audio_precache_callback)();

#ifndef IPL_PHONON_H
typedef struct _IPLReflectionEffect_t {} *IPLReflectionEffect;
#endif

#ifndef DISABLE_RESONANCE_AUDIO
namespace vraudio
{
    struct ResonanceAudioApi;
}
#endif


struct ref_sound;
class CObject;

namespace XRay::Sound::Mixer
{
    enum class Flags : u8
    {
        None = 0,
        Looped = (1 << 0),
        Spatial = (1 << 1),
        Intro = (1 << 2),
        NoPosUpdate = (1 << 3),
        NoFeedback = (1 << 4),
        NoOCC = (1 << 5),
        Music = (1 << 6),
        Shooting = (1 << 7)
    };

    enum class State : u8
    {
        Stopped,
        Playing,
        Delay,
        Paused
    };

    enum class ParameterId : u16
    {
        VolumePerChannel,
        DistanceRange,
        Pitch,
        Position,
        Panning,
        Count
    };
}

struct sound_stats
{
    int possible_free_count;
    u32 update_time_micros;
    u32 frame_time_micros;
    u32 precache_time_micros;
    u32 render_time_micros;
    u32 cache_lines_total;
    u32 cache_lines_free;
    u32 cache_miss_count;
    u32 cache_hit_count;

#ifdef DEBUG_DRAW
    float channel_volumes[SND_CHANNEL_COUNT];
    float spectral_data[SND_BLOCKSIZE];
#endif
};

struct sound_source_public
{
    u8 channels_count;
    u8 reserved0;
    u16 game_type;
    u32 data_size;
    xr_atomic_u32 ref_count;
    u32 frames_total;

    float volume;
    float min_distance;
    float max_distance;
    float max_ai_distance;

    shared_str name;
    shared_str path;
};

struct sound_reverb_settings
{
    float room;
    float room_hf;
    float room_rolloff_factor;
    float decay_time;
    float decay_hf_ratio;
    float reflections;
    float reflections_delay;
    float reverb;
    float reverb_delay;
    float environment_size;
    float environment_diffusion;
    float air_absorption_hf;
};

struct sound_reverb_line_state
{
    u32 offset;
    u32 frames;
    float* buffer;
    float iir_state;
};

struct sound_reberb_state
{
#ifndef DISABLE_STEAM_AUDIO
    IPLReflectionEffect effect[SND_CHANNEL_COUNT] = { 0 };
#endif
#ifndef DISABLE_RESONANCE_AUDIO
    vraudio::ResonanceAudioApi* ra_context;
    int buffer;
#endif
};

struct sound_zone_params
{
    float data[SND_CHANNEL_COUNT][SND_BLOCKSIZE];
	float compressor_envelope[SND_CHANNEL_COUNT][2] = {{FLT_EPSILON, FLT_EPSILON}, {FLT_EPSILON, FLT_EPSILON}};
    u32 version;
    u32	environment;
    u32 use_count;
    u64 last_use_ms;
    Fvector min;
    Fvector max;
    Fvector center;
    Fvector size;
    shared_str name;
    sound_reverb_settings settings;
    sound_reberb_state state;
};