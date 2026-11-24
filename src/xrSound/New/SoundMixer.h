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
#include "SoundShader.h"

namespace XRay::Sound::LowPass
{
	// Cutoff at or above this value means "no filtering"
	constexpr float MaxCutoffHz = 20000.0f;
	constexpr float MinCutoffHz = 20.0f;

	// Maps a 0..1 factor to a cutoff in log space: 0 -> MinHz, 1 -> MaxHz
	inline float CutoffFromFactor(float Factor, float MinHz, float MaxHz = MaxCutoffHz)
	{
		Factor = std::clamp(Factor, 0.0f, 1.0f);
		return std::exp(std::log(MinHz) + (std::log(MaxHz) - std::log(MinHz)) * Factor);
	}
}

namespace XRay::Sound::Mixer
{
    XRSOUND_API void Initialize();
    XRSOUND_API void Shutdown();
    XRSOUND_API void Update(void* event_handler, float time_factor, float mute_volume, float compression, Fvector P, Fvector D, Fvector N);
    XRSOUND_API void StopAll();
    XRSOUND_API void PauseAll();
    XRSOUND_API void ResumeAll();
    XRSOUND_API void DereferenceObjects(CObject** object, int count);
    XRSOUND_API sound_stats* GetStats();

    // Sound shaders (SoundShader.h). Changes are queued until the next Update.
    // Voice shaders stay on the slot across plays; play_no_feedback copies them
    XRSOUND_API void AddShader(u32 slot, SoundShaderType type);
    XRSOUND_API void RemoveShader(u32 slot, SoundShaderType type);
    XRSOUND_API void ClearShaders(u32 slot);
    XRSOUND_API void SetShaderParam(u32 slot, SoundShaderType type, const char* param, float value);
    // Parameter automation, also queued. A ramp moves the parameter from its current value to value over seconds
    // (0 sets it at once); an LFO swings it by depth (parameter units) around its value, rate or depth 0 stops it.
    // A direct SetShaderParam drops the ramp and keeps the LFO. Automation time runs while the voice renders.
    // StopShaderAutomation keeps the current value; param nullptr or "" stops every parameter of the shader
    XRSOUND_API void RampShaderParam(u32 slot, SoundShaderType type, const char* param, float value, float seconds, Shader::Curve curve = Shader::Curve::Linear);
    XRSOUND_API void SetShaderLfo(u32 slot, SoundShaderType type, const char* param, float rate_hz, float depth, Shader::LfoShape shape = Shader::LfoShape::Sine);
    XRSOUND_API void StopShaderAutomation(u32 slot, SoundShaderType type, const char* param = nullptr);
    // Bus shaders run before the bus effects. Add / Remove count owners, the shader stays while one is left.
    // spatial_only: 2D voices skip the shaders of a bus that has such a shader; remove with the same value
    XRSOUND_API void AddBusShader(u32 bus, SoundShaderType type, bool spatial_only = false);
    XRSOUND_API void RemoveBusShader(u32 bus, SoundShaderType type, bool spatial_only = false);
    XRSOUND_API void ClearBusShaders(u32 bus);
    XRSOUND_API void SetBusShaderParam(u32 bus, SoundShaderType type, const char* param, float value);
    // Same as the voice automation above; time runs while the bus renders (input or tail)
    XRSOUND_API void RampBusShaderParam(u32 bus, SoundShaderType type, const char* param, float value, float seconds, Shader::Curve curve = Shader::Curve::Linear);
    XRSOUND_API void SetBusShaderLfo(u32 bus, SoundShaderType type, const char* param, float rate_hz, float depth, Shader::LfoShape shape = Shader::LfoShape::Sine);
    XRSOUND_API void StopBusShaderAutomation(u32 bus, SoundShaderType type, const char* param = nullptr);
    // Read under GetUpdateMutex (shared); nullptr for an unused bus
    XRSOUND_API const SoundShaderChain* GetBusShaders(u32 bus);

    // Dynamic buses: runtime only, never saved. Acquire adds an owner and returns the bus (a config bus of that name
    // is returned as is), Release drops one. The last Release frees the bus and drops every route into it. output 0 = master
    XRSOUND_API u32 AcquireDynamicBus(const char* name, u32 output = 0);
    // Dynamic bus "fx:<shader>" with one instance of the shader: the voices and buses routed into it share that instance
    XRSOUND_API u32 AcquireShaderBus(SoundShaderType type, u32 output = 0);
    XRSOUND_API void ReleaseDynamicBus(u32 bus);
    // Queued. Mixes the whole bus into target instead of its output; target 0 restores the config output
    XRSOUND_API void RouteBus(u32 bus, u32 target);
    // Queued. Mixes the voice into bus instead of its own bus (sends and voice effects stay); 0 restores.
    // The next play resets it, ref_sound::set_route_bus keeps a route across plays
    XRSOUND_API void RouteVoice(u32 slot, u32 bus);

    // General volume (snd_volume_eff) and bus faders. Pointers stay valid for the whole process
    XRSOUND_API float* GetMasterVolume();
    XRSOUND_API float* GetBusVolume(const char* name);
    XRSOUND_API u32 FindBus(const char* name);
    // "" for an unused bus
    XRSOUND_API const char* GetBusName(u32 bus);
    // Spectrum of one channel (0 left, 1 right, SND_SCOPE_MID for (L + R) / 2) of the bus output, after its effects
    // and fader: band_count log-spaced bands in [min_hz, max_hz], in dB of a full scale sine. fft_size: a power of two
    // in [256, SND_SCOPE_FRAMES]. The first call starts capturing the bus, it stops about a second after the last one.
    // Debug views only; false for an unused bus
    XRSOUND_API bool GetBusSpectrum(u32 bus, u32 channel, float* bands_db, u32 band_count, float min_hz = 20.0f, float max_hz = 20000.0f, u32 fft_size = SND_BLOCKSIZE * 2);
    // Every dynamic bus created so far, released ones included (debug views)
    XRSOUND_API void GetDynamicBuses(xr_vector<SoundDynamicBusInfo>& buses);
    // Drops the released ones from that list
    XRSOUND_API void ForgetDynamicBuses();
    XRSOUND_API u32 RegisterEffect(const char* name, SoundEffectProc proc);
    XRSOUND_API void ExportConfig();
    XRSOUND_API void ReloadSources();
    XRSOUND_API bool GetSoundConfig(const char* name, sound_config* config);
    XRSOUND_API void SetSoundConfig(const char* name, sound_config* config);
    XRSOUND_API bool SaveSoundConfig(const char* name);
    XRSOUND_API void GetLoadedSources(xr_vector<shared_str>& names);
    XRSOUND_API u32 GetSourceCount();
    XRSOUND_API const sound_source_desc* GetSource(u32 index);

    // Non-scheduled stuff
    XRSOUND_API u32 Create();
    XRSOUND_API void Destroy(u32 slot);

    // Scheduled stuff
    // route: bus the voice is mixed into instead of its own (RouteVoice)
    XRSOUND_API void Play(u32 slot, u16 flags, ref_sound* sound, double delay, u32 bus = 0, u32 route = 0);
    XRSOUND_API void PlayNoFeedback(u16 flags, ref_sound* sound, CObject* obj, double delay, float* pitch, float* volume, Fvector* distance, Fvector* pos, u32 bus = 0, u32 route = 0);
    XRSOUND_API void Pause(u32 slot);
    XRSOUND_API void Stop(u32 slot, bool deferred);
    XRSOUND_API void Stop(u32 slot, bool deferred, float fade_seconds);
    XRSOUND_API void UpdateParameter(u32 slot, ParameterId parameter, Fvector value);
    XRSOUND_API void SetVolume(u32 slot, double volume);
    XRSOUND_API void SetPanning(u32 slot, double left, double right);

    XRSOUND_API bool SlotIsRelated(u32 slot);
    XRSOUND_API u32 GetGameType(u32 slot);
    XRSOUND_API u32 GetFlags(u32 slot);
    XRSOUND_API float GetPlaytime(u32 slot);
    XRSOUND_API float GetDuration(u32 slot);
    XRSOUND_API float GetDuration(const char* name);
    XRSOUND_API State GetState(u32 slot);
    XRSOUND_API Fvector* GetParameters(u32 slot);
}
