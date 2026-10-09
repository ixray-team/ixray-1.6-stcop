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

void Snd_InitEffects();
u8 Snd_RegisterEffect(const char* Name, SoundEffectProc Proc);
u8 Snd_FindEffect(const char* Name);
const SoundEffectEntry* Snd_GetEffect(u8 Id);
u32 Snd_GetEffectCount();
bool Snd_CallEffect(u8 Id, void* State, SoundEffectOp Op, void* Arg);

void Snd_ResetEffect(SoundBusEffect* Effect, u8 Id);
bool Snd_SetEffectParam(SoundBusEffect* Effect, const char* Name, float Value);
bool Snd_CreateEffect(SoundBusEffect* Effect);
void Snd_DestroyEffect(SoundBusEffect* Effect);

struct SoundSpatialState
{
	float Panning[SND_CHANNEL_COUNT];
	bool IsPanned;
};

struct SoundSpatialPosition
{
	Fvector Local;
	float Distance;
	float MinDistance;
	float MaxDistance;
	float Gain;
};

void Snd_SpatialLocate(SoundEffectProcess* Process, float DopplerScale, SoundSpatialPosition* OutPosition);
float Snd_DistanceAttenuation(const SoundSpatialPosition* Position, float Power);
void Snd_SpatialPan(SoundSpatialState* State, SoundEffectProcess* Process, const SoundSpatialPosition* Position, float Rolloff, float BackAttenuation);

bool Snd_SpatialProc(void* State, SoundEffectOp Op, void* Arg);
bool Snd_CompressorProc(void* State, SoundEffectOp Op, void* Arg);
bool Snd_ConvolutionProc(void* State, SoundEffectOp Op, void* Arg);
