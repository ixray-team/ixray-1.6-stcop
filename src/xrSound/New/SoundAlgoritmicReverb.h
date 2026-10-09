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

void Snd_ShutdownReverb();

// Zone under the given point (1-based), 0 if there is none
u32 Snd_FindReverbZone(const Fvector& Position);

// Render thread: clear zone sends before the slots are rendered
void Snd_BeginReverbBlock();

// Render thread: accumulate a slot into its zone send
void Snd_SendToReverbZone(u32 ZoneIdx, float** Data, float BeginFactor, float EndFactor, float Left, float Right);

// Render thread: process active zones and mix them into the bus. ScratchBuffer is overwritten
void Snd_RenderReverbZones(float** ScratchBuffer, float** BusBuffer);
