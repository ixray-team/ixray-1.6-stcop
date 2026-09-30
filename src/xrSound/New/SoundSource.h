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

#include <atomic>

#define SND_CACHE_ENTRY_COUNT (32)
#define SND_STAT_ADD(field, delta) std::atomic_ref<decltype(field)>(field).fetch_add(delta, std::memory_order_relaxed)
#define SND_STAT_SET(field, value) std::atomic_ref<decltype(field)>(field).store(value, std::memory_order_relaxed)

typedef struct _sound_source_state {
    sound_source_desc desc = {};
    OggVorbis_File file = {};
    IReader* reader = NULL;
    u8* data = NULL;
    u32 cache_lines[SND_CACHE_ENTRY_COUNT] = {};
    bool is_ready = false;
    bool is_loading = false;
} sound_source_state;

extern xrSRWLock snd_source_lock;
extern sound_stats snd_stats;

IC u64
Snd_GetTimestamp()
{
    return std::chrono::high_resolution_clock::now().time_since_epoch().count();
}

IC u32
Snd_Milliseconds()
{
    return (u32)(Snd_GetTimestamp() / 1000000);
}

void Snd_InitSources();
void Snd_ShutdownSources();
sound_source_state* Snd_LookupSource(const xr_string* name);
sound_source_state* Snd_FindSource(const xr_string* name);
sound_source_state* Snd_AcquireSource(const xr_string* name);
void Snd_ReleaseSource(const xr_string* name);
void Snd_QueueDecode(const xr_string* name, u32 position);
bool Snd_HasCacheLine(sound_source_state* source, u32 position);
u32 Snd_CopyCached(sound_source_state* source, u32 position, float** out_data, u32 frames);
