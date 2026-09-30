#pragma once

#include "New/SoundMeta.h"

#ifndef DISABLE_RESONANCE_AUDIO
bool Resonance_Initialize();
void Resonance_Shutdown();
void Resonance_InitZone(sound_zone_desc* zone);
void Resonance_ReleaseZone(sound_zone_desc* zone);
void Resonance_ProcessZone(sound_zone_desc* zone, float** reverb_buffer, float** process_buffer, float** bus_buffer);
void Resonance_ResetHrtfSlot(u32 slot_idx);
void Resonance_FreeHrtfSlot(u32 slot_idx);
void Resonance_ProcessHrtf(u32 slot_idx, float** data, const Fvector* relative_direction);
#else
#define Resonance_Initialize() false
#define Resonance_Shutdown() ((void)0)
#define Resonance_InitZone(zone) ((void)0)
#define Resonance_ReleaseZone(zone) ((void)0)
#define Resonance_ProcessZone(zone, reverb_buffer, process_buffer, bus_buffer) ((void)0)
#define Resonance_ResetHrtfSlot(slot_idx) ((void)0)
#define Resonance_FreeHrtfSlot(slot_idx) ((void)0)
#define Resonance_ProcessHrtf(slot_idx, data, relative_direction) ((void)0)
#endif
