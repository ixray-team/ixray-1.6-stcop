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
#include <chrono>

#define SND_CACHE_ENTRY_COUNT (32)

#define SND_STAT_ADD(Field, Delta) std::atomic_ref<std::remove_cvref_t<decltype(Field)>>(Field).fetch_add((Delta), std::memory_order_relaxed)
#define SND_STAT_SET(Field, Value) std::atomic_ref<std::remove_cvref_t<decltype(Field)>>(Field).store((Value), std::memory_order_relaxed)

struct SoundSourceState
{
	sound_source_desc Desc = {};
	OggVorbis_File File = {};
	IReader* Reader = nullptr;
	u8* Data = nullptr;
	u32 CacheLines[SND_CACHE_ENTRY_COUNT] = {};
	// OggVorbis_File is not thread safe: serializes the decode thread and on-the-fly decode in the render thread
	xrCriticalSection DecodeLock;
	bool IsReady = false;
	bool IsLoading = false;
};

extern xrSRWLock g_SoundSourceLock;
extern sound_stats g_SoundStats;

IC u64 Snd_GetTimestamp()
{
	return std::chrono::high_resolution_clock::now().time_since_epoch().count();
}

IC u32 Snd_Milliseconds()
{
	return (u32)(Snd_GetTimestamp() / 1000000);
}

void Snd_InitSources();
void Snd_ShutdownSources();

// Caller must hold g_SoundSourceLock (shared or exclusive). Does not add a reference.
SoundSourceState* Snd_LookupSource(const xr_string* Name);

SoundSourceState* Snd_FindSource(const xr_string* Name);
SoundSourceState* Snd_AcquireSource(const xr_string* Name);
void Snd_ReleaseSource(const xr_string* Name);
// Async: hand a decode request to the decode thread
void Snd_QueueDecode(const xr_string* Name, u32 Position);
// Async: make sure the current position and the read-ahead window are cached (or queued)
void Snd_PrefetchSource(const SoundSourceState* Source, const xr_string* Name, u32 Position, bool IsLooped);
// Sync: decode the cache line for Position on the calling thread. Caller must hold a source reference
bool Snd_DecodeNow(SoundSourceState* Source, u32 Position);
bool Snd_HasCacheLine(const SoundSourceState* Source, u32 Position);
u32 Snd_CopyCached(const SoundSourceState* Source, u32 Position, float** OutData, u32 Frames);
