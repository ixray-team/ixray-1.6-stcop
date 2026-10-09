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
#define SND_BUS_COUNT (64)
#define SND_BUS_EFFECT_COUNT (8)
#define SND_VOICE_EFFECT_COUNT (4)
#define SND_VOICE_EFFECT_STATE_SIZE (64)
#define SND_EFFECT_COUNT (32)
#define SND_EFFECT_PARAM_COUNT (16)

typedef void(*audio_render_callback)(float*);
typedef void(*audio_precache_callback)();

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
		NoOCC = (1 << 5)
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

enum class SoundEffectOp : u8
{
	Describe,
	Create,
	Destroy,
	Reset,
	Update,
	Process,
	Tail
};

enum class SoundEffectScope : u8
{
	Voice,
	Bus
};

struct SoundEffectParam
{
	const char* Name;
	float Default;
	float Min;
	float Max;
};

struct SoundEffectDesc
{
	SoundEffectScope Scope;
	u32 StateSize;
	u32 ParamCount;
	const SoundEffectParam* Params;
};

struct SoundEffectListener
{
	Fvector Position;
	Fvector Direction;
	Fvector Normal;
	Fvector Velocity;
};

struct SoundEffectVoice
{
	Fvector Position;
	Fvector Velocity;
	Fvector Distances;
	float* Doppler;
	float Gain;
};

struct SoundEffectCreate
{
	const float* Params;
	const char* Resource;
};

struct SoundEffectProcess
{
	float** Data;
	const float* Params;
	const SoundEffectListener* Listener;
	SoundEffectVoice* Voice;
	bool HasInput;
};

typedef bool (*SoundEffectProc)(void* State, SoundEffectOp Op, void* Arg);

struct SoundEffectEntry
{
	shared_str Name;
	SoundEffectProc Proc = nullptr;
	SoundEffectDesc Desc = {};
};

struct SoundBusEffect
{
	u8 Effect = 0;
	void* State = nullptr;
	shared_str Resource;
	float Params[SND_EFFECT_PARAM_COUNT] = {};
};

struct SoundBus
{
	shared_str Name;
	u32 Output = 0;
	u32 Depth = 0;
	float Volume = 1.0f;
	float UserVolume = 1.0f;
	float Gain = -1.0f;
	float ZoneSend = 0.0f;
	float DirectRatio = 0.0f;
	u32 FarSend = 0;
	u32 IndoorSend = 0;
	float Peak[SND_CHANNEL_COUNT] = {};
	u32 TailFrames = 0;
	u32 EffectCount = 0;
	u32 VoiceEffectCount = 0;
	SoundBusEffect Effects[SND_BUS_EFFECT_COUNT];
	SoundBusEffect VoiceEffects[SND_VOICE_EFFECT_COUNT];
	bool IsUsed = false;
	bool IsGenerated = false;
	bool HasInput = false;
	float Data[SND_CHANNEL_COUNT][SND_BLOCKSIZE] = {};
};

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
	u32 render_cache_miss;

#ifdef DEBUG_DRAW
	float channel_volumes[SND_CHANNEL_COUNT];
#endif
};

struct sound_source_desc
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
	u32 bus;

	shared_str name;
	shared_str path;
};

struct sound_config
{
	float volume;
	float min_distance;
	float max_distance;
	float max_ai_distance;
	u32 game_type;
	u32 bus;
	shared_str file;
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

struct sound_zone_params
{
	u32 version;
	u32	environment;
	u32 bus;
	Fvector min;
	Fvector max;
	Fvector center;
	Fvector size;
	shared_str name;
	sound_reverb_settings settings;
};
