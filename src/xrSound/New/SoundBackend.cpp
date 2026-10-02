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
#include "SoundBackend.h"

#include <atomic>

struct sound_backend_state {
    std::atomic<bool> is_running;
    std::atomic<bool> is_stopping;
    u64 read_position;
    u64 write_position;
    SDL_AudioStream* stream;
    SDL_AudioDeviceID device;
    ThreadID sound_thread;
    float* buffer;
    float* output_buffer;
    audio_render_callback render_callback;
    audio_precache_callback precache_callback;
};

static sound_backend_state backend_state;

static void
Snd_Initialize()
{
    SDL_AudioSpec spec = {};
    spec.channels = SND_CHANNEL_COUNT;
    spec.format = SDL_AUDIO_F32;
    spec.freq = SND_SAMPLERATE;

    backend_state.device = SDL_OpenAudioDevice(SDL_AUDIO_DEVICE_DEFAULT_PLAYBACK, &spec);
    backend_state.stream = SDL_CreateAudioStream(&spec, &spec);

    R_ASSERT2(backend_state.stream, make_string<const char*>("Couldn't create audio stream: %s", SDL_GetError()));
    SDL_BindAudioStream(backend_state.device, backend_state.stream);

    backend_state.is_running = true;
    backend_state.buffer = xr_alloc<float>(SND_BLOCKSIZE * SND_CHANNEL_COUNT);
    backend_state.output_buffer = xr_alloc<float>(SND_BLOCKSIZE * SND_CHANNEL_COUNT);
    memset(backend_state.buffer, 0, SND_BLOCKSIZE * SND_CHANNEL_COUNT * sizeof(float));
    memset(backend_state.output_buffer, 0, SND_BLOCKSIZE * SND_CHANNEL_COUNT * sizeof(float));
    R_ASSERT2(SDL_ResumeAudioDevice(backend_state.device), make_string<const char*>("Couldn't resume audio stream: %s", SDL_GetError()));
}

static void
Snd_Shutdown()
{
    xr_free(backend_state.buffer);
    xr_free(backend_state.output_buffer);
    SDL_DestroyAudioStream(backend_state.stream);
    SDL_CloseAudioDevice(backend_state.device);
}

static void
Snd_ThreadProc(void*)
{
    PROF_THREAD("Sound Thread");

    Snd_Initialize();
    while (!backend_state.is_stopping) {
        PROF_EVENT("Sound: WASAPI update");
        u8* output = (u8*)backend_state.output_buffer;

        backend_state.precache_callback();

        u32 queued_frames = SDL_GetAudioStreamQueued(backend_state.stream) / (sizeof(float) * SND_CHANNEL_COUNT);
        while (queued_frames >= SND_BLOCKSIZE) {
            Sleep(1);
            queued_frames = SDL_GetAudioStreamQueued(backend_state.stream) / (sizeof(float) * SND_CHANNEL_COUNT);
        }

        u32 required_frames = SND_BLOCKSIZE;
        u64 last_frames = backend_state.write_position - backend_state.read_position;
        while (last_frames < required_frames) {
            float* buffer_data = &backend_state.buffer[(backend_state.read_position % SND_BLOCKSIZE) * SND_CHANNEL_COUNT];
            memcpy(output, buffer_data, last_frames * SND_CHANNEL_COUNT * sizeof(float));

            backend_state.read_position += last_frames;
            required_frames -= (u32)last_frames;
            output += last_frames * SND_CHANNEL_COUNT * sizeof(float);

            buffer_data = &backend_state.buffer[(backend_state.read_position % SND_BLOCKSIZE) * SND_CHANNEL_COUNT];
            backend_state.render_callback(buffer_data);
            backend_state.write_position += SND_BLOCKSIZE;

            last_frames = backend_state.write_position - backend_state.read_position;
        }

        if (required_frames > 0) {
            float* buffer_data = &backend_state.buffer[(backend_state.read_position % SND_BLOCKSIZE) * SND_CHANNEL_COUNT];
            memcpy(output, buffer_data, required_frames * SND_CHANNEL_COUNT * sizeof(float));
            R_ASSERT(SDL_PutAudioStreamData(backend_state.stream, output, SND_BLOCKSIZE * (sizeof(float) * SND_CHANNEL_COUNT)));
            backend_state.read_position += required_frames;
        }
    }

    Snd_Shutdown();
}

void
XRay::Sound::Backend::Initialize(audio_render_callback render_callback, audio_precache_callback precache_callback)
{
    if (backend_state.is_running) {
        return;
    }

    backend_state.is_stopping = false;
    backend_state.render_callback = render_callback;
    backend_state.precache_callback = precache_callback;
    backend_state.sound_thread = thread_spawn(Snd_ThreadProc, "Sound Backend Thread", 0, NULL);
}

void
XRay::Sound::Backend::ChangeDevice(u32 device_id)
{
    if (!backend_state.is_running) {
        return;
    }

    SDL_AudioDeviceID old_device = SDL_GetAudioStreamDevice(backend_state.stream);
    if (old_device == device_id) {
        return;
    }

    SDL_PauseAudioDevice(old_device);

    SDL_AudioSpec spec = {};
    spec.channels = SND_CHANNEL_COUNT;
    spec.format = SDL_AUDIO_F32;
    spec.freq = SND_SAMPLERATE;
    SDL_AudioDeviceID new_device = SDL_OpenAudioDevice(device_id, &spec);

    SDL_UnbindAudioStream(backend_state.stream);
    if (!SDL_BindAudioStream(new_device, backend_state.stream)) {
        Msg("!Error change device: %s", SDL_GetError());
        SDL_BindAudioStream(old_device, backend_state.stream);
        SDL_ResumeAudioDevice(old_device);
        SDL_CloseAudioDevice(new_device);
        return;
    }

    SDL_CloseAudioDevice(old_device);
}

void
XRay::Sound::Backend::Shutdown()
{
    backend_state.is_stopping = true;
    if (backend_state.sound_thread) {
        Platform::JoinThread(backend_state.sound_thread);
        backend_state.sound_thread = 0;
    }
    backend_state.is_running = false;
}
