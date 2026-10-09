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

struct SoundConfigSection
{
	CInifile::Items Items;
	shared_str File;
	bool IsDeleted = false;
};

using SoundConfig = xr_hash_map<shared_str, SoundConfigSection>;

void Snd_LoadConfig();
void Snd_UnloadConfig();
const SoundConfig& Snd_GetConfig();
const SoundConfigSection* Snd_FindConfig(const char* Name);
SoundConfigSection* Snd_EditConfig(const char* Name);
void Snd_SetConfigValue(SoundConfigSection* Section, const shared_str& Key, const shared_str& Value);
bool Snd_SaveConfig(const char* Name);
const char* Snd_ConfigValue(const SoundConfigSection* Section, const char* Key);
