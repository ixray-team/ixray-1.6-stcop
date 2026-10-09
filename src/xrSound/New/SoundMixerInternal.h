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

enum class SoundReverbFlags : u8
{
	None = 0,
	Algoritmic = (1 << 0),
	Convolution = (1 << 1)
};

struct SoundSubmix
{
	// Submix fader. The effective gain is Volume * master volume
	float Volume = 1.0f;
	u8 ReverbFlags = (u8)SoundReverbFlags::None;
	bool AllowHrtf = true;
	float Bus[SND_CHANNEL_COUNT][SND_BLOCKSIZE] = {};

	bool HasReverb(SoundReverbFlags Flag) const
	{
		return (ReverbFlags & (u8)Flag) != 0;
	}
};

struct sound_slot_state
{
    XRay::Sound::Mixer::State prev_state;
    XRay::Sound::Mixer::State state;
    XRay::Sound::Mixer::State fake_state;
    u8 flags;
    SoundSubmixId SubmixId = SoundSubmixId::Effects;
    u32 zone_idx;
    u32 position;
    u32 stopping_position;
    u32 hrtf_slot;
    f32 history[SND_CHANNEL_COUNT][SND_RESAMPLING_QUALITY + 1];
    f32 panning[SND_CHANNEL_COUNT];
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
};

namespace XRay::Sound::Mixer
{
    XRSOUND_API void AddEditorZone(sound_zone_params& params);
    XRSOUND_API void AddZone(sound_zone_params& params);
    XRSOUND_API void ResetZones();
    const XRSOUND_API xr_vector<sound_zone_params>& GetZones();
    XRSOUND_API xr_vector<sound_slot_state>& GetSlots();
    XRSOUND_API xrSRWLock& GetUpdateMutex();
    XRSOUND_API xrSRWLock& GetManageMutex();
}