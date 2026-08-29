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

namespace XRay::Sound::Mixer
{
    XRSOUND_API void Initialize();
    XRSOUND_API void Shutdown();
    XRSOUND_API void Update(void* event_handler, float time_factor, float volume, float eff_volume, float mus_volume, float shooting_volume, float compression, const Fmatrix& mtx, Fvector P, Fvector D, Fvector N);
    XRSOUND_API void StopAll();
    XRSOUND_API void PauseAll();
    XRSOUND_API void ResumeAll();
    XRSOUND_API void DereferenceObjects(CObject** object, int count);
    XRSOUND_API sound_stats* GetStats();
    XRSOUND_API u32 GetSourceCount();
    XRSOUND_API const sound_source_public* GetSource(u32 index);

    // Non-scheduled stuff
    XRSOUND_API u32 Create();
    XRSOUND_API void Destroy(u32 slot);

    // Scheduled stuff
    XRSOUND_API void Play(u32 slot, u16 flags, ref_sound* sound, double delay);
    XRSOUND_API void PlayNoFeedback(u16 flags, ref_sound* sound, CObject* obj, double delay, float* pitch, float* volume, Fvector* distance, Fvector* pos);
    XRSOUND_API void Pause(u32 slot);
    XRSOUND_API void Stop(u32 slot, bool deferred);
    XRSOUND_API void UpdateParameter(u32 slot, ParameterId parameter, Fvector value);
    XRSOUND_API void SetVolume(u32 slot, double volume);
    XRSOUND_API void SetPanning(u32 slot, double left, double right);

    XRSOUND_API bool SlotIsRelated(u32 slot);
    XRSOUND_API u32 GetGameType(u32 slot);
    XRSOUND_API u32 GetFlags(u32 slot);
    XRSOUND_API float GetPlaytime(u32 slot);
    XRSOUND_API float GetDuration(u32 slot);
    XRSOUND_API State GetState(u32 slot);
    XRSOUND_API Fvector* GetParameters(u32 slot);

	XRSOUND_API void LoadImpulseResponse(const char* name, xr_vector<xr_vector<float>>& ch_audio, u32& sample_rate, u16& num_channels);
}
