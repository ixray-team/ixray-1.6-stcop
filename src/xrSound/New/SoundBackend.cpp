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
    // Device switch requested by the game thread; applied by the mixer thread so the
    // frame thread never blocks inside CoreAudio (SDL_OpenAudioDevice can stall for
    // seconds in the HAL while a device starts).
    std::atomic<u32> pending_device;
    std::atomic<bool> pending_valid;
    u64 read_position;
    u64 write_position;
    SDL_AudioStream* stream;
    SDL_AudioDeviceID device;
    xrCriticalSection device_lock;
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

    // Open outside device_lock: CoreAudio can block for seconds while starting the
    // device IO thread, and the game thread must never wait on that.
    SDL_AudioDeviceID device = SDL_OpenAudioDevice(SDL_AUDIO_DEVICE_DEFAULT_PLAYBACK, &spec);
    SDL_AudioStream* stream = SDL_CreateAudioStream(&spec, &spec);

    R_ASSERT2(stream, make_string<const char*>("Couldn't create audio stream: %s", SDL_GetError()));
    if (device == 0) {
        Msg("! Couldn't open audio device; continuing without audio output: %s", SDL_GetError());
    } else if (!SDL_BindAudioStream(device, stream) ||
        !SDL_ResumeAudioDevice(device)) {
        Msg("! Couldn't start audio device; continuing without audio output: %s", SDL_GetError());
        SDL_UnbindAudioStream(stream);
        SDL_CloseAudioDevice(device);
        device = 0;
    }

    float* buffer = xr_alloc<float>(SND_BLOCKSIZE * SND_CHANNEL_COUNT);
    float* output_buffer = xr_alloc<float>(SND_BLOCKSIZE * SND_CHANNEL_COUNT);
    memset(buffer, 0, SND_BLOCKSIZE * SND_CHANNEL_COUNT * sizeof(float));
    memset(output_buffer, 0, SND_BLOCKSIZE * SND_CHANNEL_COUNT * sizeof(float));

    xrCriticalSectionGuard deviceGuard(backend_state.device_lock);
    backend_state.device = device;
    backend_state.stream = stream;
    backend_state.buffer = buffer;
    backend_state.output_buffer = output_buffer;
    backend_state.is_running = true;
}

static void
Snd_Shutdown()
{
    xrCriticalSectionGuard deviceGuard(backend_state.device_lock);
    backend_state.is_running = false;
    xr_free(backend_state.buffer);
    xr_free(backend_state.output_buffer);
    SDL_DestroyAudioStream(backend_state.stream);
    if (backend_state.device != 0) {
        SDL_CloseAudioDevice(backend_state.device);
    }
    backend_state.stream = nullptr;
    backend_state.device = 0;
}

static void
Snd_ApplyPendingDevice()
{
    if (!backend_state.pending_valid.exchange(false)) {
        return;
    }

    const u32 target = backend_state.pending_device.load();

    SDL_AudioDeviceID old_device = 0;
    {
        xrCriticalSectionGuard deviceGuard(backend_state.device_lock);
        old_device = backend_state.device;
    }

    if (old_device == target) {
        return;
    }

    SDL_AudioSpec spec = {};
    spec.channels = SND_CHANNEL_COUNT;
    spec.format = SDL_AUDIO_F32;
    spec.freq = SND_SAMPLERATE;

    // Runs on the mixer thread: a stuck CoreAudio open only stalls audio, never a frame.
    SDL_AudioDeviceID new_device = SDL_OpenAudioDevice(target, &spec);
    if (new_device == 0) {
        Msg("! Couldn't open audio device: %s", SDL_GetError());
        return;
    }

    // Detach from the old device first: SDL_BindAudioStream fails when the stream is
    // still bound, which used to leave the stream unbound (silence) and the mixer
    // spinning forever on a queue nobody consumes.
    if (old_device != 0) {
        SDL_PauseAudioDevice(old_device);
    }
    SDL_UnbindAudioStream(backend_state.stream);

    if (!SDL_BindAudioStream(new_device, backend_state.stream) || !SDL_ResumeAudioDevice(new_device)) {
        Msg("! Error change device: %s", SDL_GetError());
        SDL_UnbindAudioStream(backend_state.stream);
        if (old_device != 0) {
            SDL_BindAudioStream(old_device, backend_state.stream);
            SDL_ResumeAudioDevice(old_device);
        }
        SDL_CloseAudioDevice(new_device);
        return;
    }

    {
        xrCriticalSectionGuard deviceGuard(backend_state.device_lock);
        backend_state.device = new_device;
    }

    if (old_device != 0) {
        SDL_CloseAudioDevice(old_device);
    }
}

static void
Snd_ThreadProc(void*)
{
    PROF_THREAD("Sound Thread");

    Snd_Initialize();
    while (!backend_state.is_stopping) {
        Snd_ApplyPendingDevice();
        PROF_EVENT("Sound: WASAPI update");
        u8* output = (u8*)backend_state.output_buffer;

        backend_state.precache_callback();

        u32 queued_frames = 0;
        {
            xrCriticalSectionGuard deviceGuard(backend_state.device_lock);
            if (backend_state.device != 0) {
                queued_frames = SDL_GetAudioStreamQueued(backend_state.stream) / (sizeof(float) * SND_CHANNEL_COUNT);
            }
        }
        while (queued_frames >= SND_BLOCKSIZE && !backend_state.is_stopping) {
            Sleep(1);
            xrCriticalSectionGuard deviceGuard(backend_state.device_lock);
            queued_frames = backend_state.device != 0 ?
                SDL_GetAudioStreamQueued(backend_state.stream) / (sizeof(float) * SND_CHANNEL_COUNT) : 0;
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
            bool silentOutput = false;
            {
                xrCriticalSectionGuard deviceGuard(backend_state.device_lock);
                silentOutput = backend_state.device == 0;
                if (!silentOutput) {
                    R_ASSERT(SDL_PutAudioStreamData(backend_state.stream, output, SND_BLOCKSIZE * (sizeof(float) * SND_CHANNEL_COUNT)));
                }
            }
            backend_state.read_position += required_frames;
            if (silentOutput) {
                // Keep mixer commands and AI sound events progressing without a playback device.
                Sleep(1000 * SND_BLOCKSIZE / SND_SAMPLERATE);
            }
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
    // Called from the frame thread. Defer the actual switch to the mixer thread:
    // SDL_OpenAudioDevice can block for seconds inside CoreAudio, which would freeze
    // the whole engine and (with device_lock held) deadlock against the mixer.
    backend_state.pending_device.store(device_id);
    backend_state.pending_valid.store(true);
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
