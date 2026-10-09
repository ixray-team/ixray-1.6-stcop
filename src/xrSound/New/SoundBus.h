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

void Snd_InitBuses();
void Snd_ShutdownBuses();

u32 Snd_ReserveBus(const char* Name);
u32 Snd_FindBus(const char* Name);
u32 Snd_GetMasterBus();
SoundBus* Snd_GetBus(u32 BusIdx);
SoundBus* Snd_GetBuses();
void Snd_SetBusParam(u32 BusIdx, const char* Effect, const char* Param, float Value);

u32 Snd_CreateBus(const char* Name);
void Snd_DeleteBus(u32 BusIdx);
void Snd_SetBusValue(u32 BusIdx, const char* Key, const char* Value);
void Snd_SetBusEffectParam(u32 BusIdx, bool IsVoice, u32 EffectIdx, u32 ParamIdx, float Value);
const char* Snd_GetBusValue(u32 BusIdx, const char* Key);
bool Snd_SaveBus(u32 BusIdx);

void Snd_BeginBuses();
bool Snd_SendToBus(u32 BusIdx, float** Data, float BeginFactor, float EndFactor, float Left, float Right);
void Snd_RenderBuses(float* Output, float MuteVolume);

void Snd_AddZone(sound_zone_params* Params, bool IsEditor);
void Snd_ResetZones();
u32 Snd_FindZone(const Fvector& Position);
u32 Snd_GetZoneBus(u32 ZoneIdx);
const xr_vector<sound_zone_params>& Snd_GetZones();
