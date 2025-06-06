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
#include "SoundBackend.h"

#define MEM_ALIGN(size, align) ((size + align -1) & (~(uintptr_t)(align - 1)))

struct sound_backend_state
{
    u32 is_running;
    u32 device_frame_count;
    u32 buffer_frame_count;
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

static sound_backend_state backend;

static void
Snd_Initialize()
{
    SDL_AudioSpec spec = { };
    spec.channels = SND_CHANNEL_COUNT;
    spec.format = SDL_AUDIO_F32;
    spec.freq = SND_SAMPLERATE;

    backend.device = SDL_OpenAudioDevice(SDL_AUDIO_DEVICE_DEFAULT_PLAYBACK, &spec);
    backend.stream = SDL_CreateAudioStream(&spec, &spec);

    R_ASSERT2(backend.stream, make_string<const char*>("Couldn't create audio stream: %s", SDL_GetError()));
    SDL_BindAudioStream(backend.device, backend.stream);

    backend.is_running = true;
    backend.buffer_frame_count = std::max((u32)SND_BLOCKSIZE, (u32)MEM_ALIGN(backend.device_frame_count, SND_BLOCKSIZE));
    backend.buffer = xr_alloc<float>(backend.buffer_frame_count * SND_CHANNEL_COUNT);
    backend.output_buffer = xr_alloc<float>(backend.buffer_frame_count * SND_CHANNEL_COUNT);
    memset(backend.buffer, 0, backend.buffer_frame_count * SND_CHANNEL_COUNT * sizeof(float));
    memset(backend.output_buffer, 0, backend.buffer_frame_count * SND_CHANNEL_COUNT * sizeof(float));
    R_ASSERT2(SDL_ResumeAudioDevice(backend.device),
        make_string<const char*>("Couldn't resume audio stream: %s", SDL_GetError()));
}

static void
Snd_Shutdown()
{
    xr_free(backend.buffer);
    xr_free(backend.output_buffer);
    SDL_DestroyAudioStream(backend.stream);
    SDL_CloseAudioDevice(backend.device);
}

static void
Snd_ThreadProc(void* data)
{
    PROF_THREAD("Sound Thread");

    Snd_Initialize();
    while (backend.is_running) {
        PROF_EVENT("Sound: WASAPI update");
        u32 required_frames = 0;
        u8* buffer = (u8*)backend.output_buffer;

        backend.precache_callback();

        required_frames = SDL_GetAudioStreamQueued(backend.stream) / (sizeof(float)*SND_CHANNEL_COUNT);
        while (required_frames >= SND_BLOCKSIZE) {
            Sleep(1);
            required_frames = SDL_GetAudioStreamQueued(backend.stream) / (sizeof(float) * SND_CHANNEL_COUNT);
        }

        required_frames = SND_BLOCKSIZE;

        u64 last_frames = backend.write_position - backend.read_position;
        while (last_frames < required_frames && required_frames > 0) {
            float* buffer_data = &backend.buffer[(backend.read_position % SND_BLOCKSIZE) * SND_CHANNEL_COUNT];
            memcpy(buffer, buffer_data, last_frames * SND_CHANNEL_COUNT * sizeof(float));

            backend.read_position += last_frames;
            required_frames -= last_frames;
            buffer += last_frames * SND_CHANNEL_COUNT * sizeof(float);

            buffer_data = &backend.buffer[(backend.read_position % SND_BLOCKSIZE) * SND_CHANNEL_COUNT];
            backend.render_callback(buffer_data);
            backend.write_position += SND_BLOCKSIZE;

            last_frames = backend.write_position - backend.read_position;
        }

        if (required_frames > 0) {
            float* buffer_data = &backend.buffer[(backend.read_position % SND_BLOCKSIZE) * SND_CHANNEL_COUNT];
            memcpy(buffer, buffer_data, required_frames * SND_CHANNEL_COUNT * sizeof(float));
            R_ASSERT(SDL_PutAudioStreamData(backend.stream, buffer, SND_BLOCKSIZE * (sizeof(float) * SND_CHANNEL_COUNT)));
            backend.read_position += required_frames;
        }
    }

    Snd_Shutdown();
}

void XRay::Sound::Backend::Initialize(audio_render_callback render_callback, audio_precache_callback precache_callback)
{
    if (backend.is_running) {
        return;
    }

    backend.render_callback = render_callback;
    backend.precache_callback = precache_callback;
    backend.sound_thread = thread_spawn(Snd_ThreadProc, "Sound Backend Thread", 0, NULL);
}

void XRay::Sound::Backend::ChangeDevice(u32 DeviceID)
{
    if (!backend.is_running) {
        return;
    }
    
    u32 OldDeviceID = SDL_GetAudioStreamDevice(backend.stream);
    if (OldDeviceID == DeviceID)
    {
        return;
    }

    SDL_PauseAudioDevice(OldDeviceID);

    SDL_AudioSpec spec = { };
    spec.channels = SND_CHANNEL_COUNT;
    spec.format = SDL_AUDIO_F32;
    spec.freq = SND_SAMPLERATE;
    SDL_AudioDeviceID NewDeviceLogicalID = SDL_OpenAudioDevice(DeviceID, &spec);

    SDL_UnbindAudioStream(backend.stream);
    if (!SDL_BindAudioStream(NewDeviceLogicalID, backend.stream))
    {
        Msg("!Error change device: %s", SDL_GetError());
        SDL_BindAudioStream(OldDeviceID, backend.stream);
        return;
    }

    SDL_CloseAudioDevice(OldDeviceID);
}

void XRay::Sound::Backend::Shutdown()
{
    backend.is_running = false;
    Platform::WaitForSingleObject(backend.sound_thread);
}
