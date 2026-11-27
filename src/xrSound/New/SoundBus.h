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
#include "SoundShader.h"

void Snd_InitBuses();
void Snd_ShutdownBuses();

u32 Snd_ReserveBus(const char* Name);
u32 Snd_FindBus(const char* Name);
u32 Snd_GetMasterBus();
SoundBus* Snd_GetBus(u32 BusIdx);
SoundBus* Snd_GetBuses();
SoundShaderChain* Snd_GetBusShaders(u32 BusIdx);
void Snd_SetBusParam(u32 BusIdx, const char* Effect, const char* Param, float Value);

u32 Snd_CreateBus(const char* Name);
void Snd_DeleteBus(u32 BusIdx);
void Snd_SetBusValue(u32 BusIdx, const char* Key, const char* Value);
void Snd_SetBusEffectParam(u32 BusIdx, bool IsVoice, u32 EffectIdx, u32 ParamIdx, float Value);
const char* Snd_GetBusValue(u32 BusIdx, const char* Key);
bool Snd_SaveBus(u32 BusIdx);

// Runtime buses: Acquire counts owners (a config bus of that name is returned without one), the last Release frees the bus
u32 Snd_AcquireDynamicBus(const char* Name, u32 Output);
// True when the bus was freed
bool Snd_ReleaseDynamicBus(u32 BusIdx);
// Overrides the output of a bus until Target 0; a route that makes a loop is dropped
void Snd_RouteBus(u32 BusIdx, u32 Target);
void Snd_GetDynamicBuses(xr_vector<SoundDynamicBusInfo>& Out);
// Drops the records of released dynamic buses
void Snd_ForgetDynamicBuses();

void Snd_BeginBuses();
// VoiceFlags (Mixer::Flags): NoBusShaders, or a 2D voice on a bus with a spatial-only shader, skips the runtime shaders
bool Snd_SendToBus(u32 BusIdx, float** Data, float BeginFactor, float EndFactor, float Left, float Right, u8 VoiceFlags);
void Snd_RenderBuses(float* Output, float MuteVolume);
// Copies the last Frames (up to SND_SCOPE_FRAMES) of one channel, SND_SCOPE_MID for (L + R) / 2,
// and keeps the capture on for about a second
bool Snd_ReadBusScope(u32 BusIdx, u32 Channel, float* Out, u32 Frames);

void Snd_AssignZoneBus(sound_zone_params* Zone);
void Snd_AddZone(sound_zone_params* Params, bool IsEditor);
void Snd_ResetZones();
void Snd_SwapZones(SoundZoneVector& Zones);
u32 Snd_FindZone(const Fvector& Position);
u32 Snd_GetZoneBus(u32 ZoneIdx);
const xr_vector<sound_zone_params>& Snd_GetZones();
