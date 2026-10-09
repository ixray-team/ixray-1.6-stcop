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

#define SND_RESAMPLING_QUALITY 3

IC void	volume_lerp(float& c, float t, float s, float dt)
{
    float diff = t - c;
    float diff_a = std::abs(diff);
    if (diff_a < EPS_S) return;
    float mot = s * dt;
    if (mot > diff_a) mot = diff_a;
    c += (diff / diff_a) * mot;
}

struct sound_slot_state
{
    XRay::Sound::Mixer::State prev_state;
    XRay::Sound::Mixer::State state;
    XRay::Sound::Mixer::State fake_state;
    u8 flags;
    u32 bus;
    u32 zone_idx;
    u32 position;
    u32 stopping_position;
    f32 history[SND_CHANNEL_COUNT][SND_RESAMPLING_QUALITY + 1];
    f32 delay = 0.f;
    xr_string sound_name;
    Fvector parameters[(u32)XRay::Sound::Mixer::ParameterId::Count];
    Fvector prev_position = {};
    Fvector velocity = {};
    f32 doppler = 1.0f;
    f32 fade_volume = 1.0f;

	f32 ReverbDryGain = -1.0f;
	f32 IndoorFactor = 0.0f;
	bool IndoorFactorValid = false;
	u32 SendBus = 0;
	f32 SendGain = 0.0f;
	u8 VoiceEffects[SND_VOICE_EFFECT_COUNT] = {};
	alignas(16) u8 VoiceEffectState[SND_VOICE_EFFECT_COUNT][SND_VOICE_EFFECT_STATE_SIZE] = {};
};

namespace XRay::Sound::Mixer
{
    XRSOUND_API SoundBus* GetBuses();
    XRSOUND_API const SoundEffectEntry* GetEffect(u8 id);
    XRSOUND_API u32 GetEffectCount();
    XRSOUND_API u32 CreateBus(const char* name);
    XRSOUND_API void DeleteBus(u32 bus);
    XRSOUND_API void SetBusValue(u32 bus, const char* key, const char* value);
    XRSOUND_API void SetBusEffectParam(u32 bus, bool voice, u32 effect, u32 param, float value);
    XRSOUND_API const char* GetBusValue(u32 bus, const char* key);
    XRSOUND_API bool SaveBus(u32 bus);
    XRSOUND_API void AddEditorZone(sound_zone_params& params);
    XRSOUND_API void AddZone(sound_zone_params& params);
    XRSOUND_API void ResetZones();
    const XRSOUND_API xr_vector<sound_zone_params>& GetZones();
    XRSOUND_API xr_vector<sound_slot_state>& GetSlots();
    XRSOUND_API xrSRWLock& GetUpdateMutex();
    XRSOUND_API xrSRWLock& GetManageMutex();
}