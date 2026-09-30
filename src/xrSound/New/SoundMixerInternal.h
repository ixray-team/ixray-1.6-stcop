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
#include "SoundMeta.h"

typedef struct _sound_source_state sound_source_state;

struct sound_slot_state {
    XRay::Sound::Mixer::State prev_state;
    XRay::Sound::Mixer::State state;
    XRay::Sound::Mixer::State fake_state;
    u8 flags;
    u32 zone_idx;
    u32 position;
    u32 stopping_position;
    u32 hrtf_slot;
    float resample_state[SND_CHANNEL_COUNT];
    float panning[SND_CHANNEL_COUNT];
    float delay = 0.0f;
    xr_string sound_name;
    Fvector parameters[(u32)XRay::Sound::Mixer::ParameterId::Count];
    Fvector prev_position = {};
    Fvector velocity = {};
    float doppler = 1.0f;
    float indoor_factor = 0.0f;
    sound_source_state* source = NULL;
    bool is_free = false;
};

IC void
Snd_VolumeLerp(float* current, float target, float speed, float dt)
{
    float diff = target - *current;
    float diff_abs = fabsf(diff);
    if (diff_abs < EPS_S) {
        return;
    }

    *current += (diff / diff_abs) * std::min(speed * dt, diff_abs);
}

namespace XRay::Sound::Mixer 
{
    XRSOUND_API void AddEditorZone(sound_zone_desc* zone);
    XRSOUND_API void AddZone(sound_zone_desc* zone);
    XRSOUND_API void ResetZones();
    XRSOUND_API xr_vector<sound_zone_desc>& GetZones();
    XRSOUND_API xr_vector<sound_slot_state>& GetSlots();
    XRSOUND_API xrSRWLock& GetRenderMutex();
    XRSOUND_API xrSRWLock& GetUpdateMutex();
    XRSOUND_API xrSRWLock& GetManageMutex();
}
