#include "stdafx.h"
#include "ResonanceAudio.h"
#include "../New/SoundDSP.h"

#include "../../3rd-party/resonance-audio/resonance_audio/api/binaural_surround_renderer.h"
#include "../../3rd-party/resonance-audio/resonance_audio/api/resonance_audio_api.h"

#define RESONANCE_ZONE_MAX (1024)

typedef struct _resonance_zone_state {
    std::atomic<vraudio::ResonanceAudioApi*> api{ nullptr };
    int buffer = 0;
    float compressor_envelope[SND_CHANNEL_COUNT][2] = { { FLT_EPSILON, FLT_EPSILON }, { FLT_EPSILON, FLT_EPSILON } };
} resonance_zone_state;

typedef struct _resonance_hrtf_state {
    vraudio::ResonanceAudioApi* api = NULL;
    vraudio::ResonanceAudioApi::SourceId source_id = vraudio::ResonanceAudioApi::kInvalidSourceId;
} resonance_hrtf_state;

typedef struct _resonance_state_struct {
    resonance_zone_state zones[RESONANCE_ZONE_MAX];
    xr_vector<resonance_hrtf_state> hrtf_slots;
    xr_vector<resonance_hrtf_state> hrtf_free;
} resonance_state_struct;

static resonance_state_struct resonance_state = {};

static float
Resonance_DbToLinear(float db)
{
    return powf(10.0f, db / 20.0f);
}

static void
Resonance_DestroyHrtfList(xr_vector<resonance_hrtf_state>* list)
{
    for (size_t slot_idx = 0; slot_idx < list->size(); slot_idx++) {
        resonance_hrtf_state* slot = &(*list)[slot_idx];
        if (slot->api != NULL && slot->source_id != vraudio::ResonanceAudioApi::kInvalidSourceId) {
            slot->api->DestroySource(slot->source_id);
        }

        delete slot->api;
    }

    list->clear();
}

bool
Resonance_Initialize()
{
    resonance_state.hrtf_slots.resize(SND_HRTF_SLOT_COUNT);
    return true;
}

void
Resonance_Shutdown()
{
    Resonance_DestroyHrtfList(&resonance_state.hrtf_slots);
    Resonance_DestroyHrtfList(&resonance_state.hrtf_free);

    for (u32 zone_idx = 0; zone_idx < RESONANCE_ZONE_MAX; zone_idx++) {
        delete resonance_state.zones[zone_idx].api.exchange(nullptr);
    }
}

void
Resonance_InitZone(sound_zone_desc* zone)
{
    const float hf_reference = 5000.0f;

    vraudio::ReflectionProperties reflection = {};
    vraudio::ReverbProperties reverb = {};

    reflection.room_dimensions[0] = zone->size.x;
    reflection.room_dimensions[1] = zone->size.y;
    reflection.room_dimensions[2] = zone->size.z;
    reflection.room_position[0] = zone->center.x;
    reflection.room_position[1] = zone->center.y;
    reflection.room_position[2] = zone->center.z;
    reflection.room_rotation[0] = 0.0f;
    reflection.room_rotation[1] = 0.0f;
    reflection.room_rotation[2] = 0.0f;
    reflection.room_rotation[3] = 1.0f;

    float hf_db = std::clamp(zone->settings.room_hf, -10000.0f, 0.0f);
    reflection.cutoff_frequency = std::clamp(hf_reference * Resonance_DbToLinear(hf_db), 200.0f, 20000.0f);

    float diffusion = std::clamp(zone->settings.environment_diffusion, 0.0f, 1.0f);
    for (u32 coefficient_idx = 0; coefficient_idx < std::size(reflection.coefficients); coefficient_idx++) {
        reflection.coefficients[coefficient_idx] = diffusion;
    }

    reflection.gain = Resonance_DbToLinear(std::clamp(zone->settings.reflections, -10000.0f, 0.0f));
    reverb.gain = Resonance_DbToLinear(std::clamp(zone->settings.reverb, -10000.0f, 0.0f));

    float base_rt60 = std::clamp(zone->settings.decay_time, 0.1f, 20.0f);
    float hf_ratio = std::clamp(zone->settings.decay_hf_ratio, 0.1f, 4.0f);
    float hf_scale = powf(10.0f, -std::max(0.0f, zone->settings.air_absorption_hf) / 20.0f);
    for (u32 band_idx = 0; band_idx < 9; band_idx++) {
        float rt60 = (band_idx >= 6) ? base_rt60 * hf_ratio * hf_scale : base_rt60;
        reverb.rt60_values[band_idx] = std::clamp(rt60, 0.05f, 120.0f);
    }

    u32 zone_idx = 0;
    while (zone_idx < RESONANCE_ZONE_MAX && resonance_state.zones[zone_idx].api.load() != nullptr) {
        zone_idx++;
    }
    if (zone_idx == RESONANCE_ZONE_MAX) {
        Msg("! Too many reverb zones, zone '%s' has no reverb", zone->name.c_str());
        zone->reverb_id = 0;
        return;
    }

    vraudio::ResonanceAudioApi* api = vraudio::CreateResonanceAudioApi(SND_CHANNEL_COUNT, SND_BLOCKSIZE, SND_SAMPLERATE);
    api->EnableRoomEffects(true);
    api->SetReverbProperties(reverb);
    api->SetReflectionProperties(reflection);

    resonance_zone_state* state = &resonance_state.zones[zone_idx];
    state->buffer = api->CreateSoundObjectSource(vraudio::RenderingMode::kStereoPanning);
    memset(state->compressor_envelope, 0, sizeof(state->compressor_envelope));
    for (u32 envelope_idx = 0; envelope_idx < SND_CHANNEL_COUNT * 2; envelope_idx++) {
        (&state->compressor_envelope[0][0])[envelope_idx] = FLT_EPSILON;
    }

    state->api.store(api, std::memory_order_release);
    zone->reverb_id = zone_idx + 1;
}

void
Resonance_ReleaseZone(sound_zone_desc* zone)
{
    if (zone->reverb_id != 0 && zone->reverb_id <= RESONANCE_ZONE_MAX) {
        delete resonance_state.zones[zone->reverb_id - 1].api.exchange(nullptr);
    }

    zone->reverb_id = 0;
}

void
Resonance_ProcessZone(sound_zone_desc* zone, float** reverb_buffer, float** process_buffer, float** bus_buffer)
{
    if (zone->reverb_id == 0 || zone->reverb_id > RESONANCE_ZONE_MAX) {
        return;
    }

    resonance_zone_state* state = &resonance_state.zones[zone->reverb_id - 1];
    vraudio::ResonanceAudioApi* api = state->api.load(std::memory_order_acquire);
    if (api == nullptr) {
        return;
    }

    DSP_Compressor(0.0001f, 0.100f, -20.0f, 2.0f, reverb_buffer, 1.0f, SND_BLOCKSIZE, state->compressor_envelope[0]);
    api->SetPlanarBuffer(state->buffer, reverb_buffer, SND_CHANNEL_COUNT, SND_BLOCKSIZE);
    api->FillPlanarOutputBuffer(SND_CHANNEL_COUNT, SND_BLOCKSIZE, process_buffer);
    DSP_Compressor(0.0001f, 0.100f, -20.0f, 2.0f, bus_buffer, 1.0f, SND_BLOCKSIZE, state->compressor_envelope[1]);
}

void
Resonance_ResetHrtfSlot(u32 slot_idx)
{
    if (slot_idx >= resonance_state.hrtf_slots.size() || resonance_state.hrtf_slots[slot_idx].api != NULL) {
        return;
    }

    resonance_hrtf_state* slot = &resonance_state.hrtf_slots[slot_idx];
    if (!resonance_state.hrtf_free.empty()) {
        *slot = resonance_state.hrtf_free.back();
        resonance_state.hrtf_free.pop_back();
        return;
    }

    slot->api = vraudio::CreateResonanceAudioApi(SND_CHANNEL_COUNT, SND_BLOCKSIZE, SND_SAMPLERATE);
    R_ASSERT(slot->api != NULL);

    slot->source_id = slot->api->CreateSoundObjectSource(vraudio::RenderingMode::kBinauralHighQuality);
    R_ASSERT(slot->source_id != vraudio::ResonanceAudioApi::kInvalidSourceId);

    slot->api->EnableRoomEffects(false);
    slot->api->SetSourceDistanceModel(slot->source_id, vraudio::DistanceRolloffModel::kNone, 0.0f, 0.0f);
    slot->api->SetSourceDistanceAttenuation(slot->source_id, 1.0f);
    slot->api->SetHeadPosition(0.0f, 0.0f, 0.0f);
    slot->api->SetHeadRotation(0.0f, 0.0f, 0.0f, 1.0f);
}

void
Resonance_FreeHrtfSlot(u32 slot_idx)
{
    if (slot_idx >= resonance_state.hrtf_slots.size() || resonance_state.hrtf_slots[slot_idx].api == NULL) {
        return;
    }

    resonance_state.hrtf_free.push_back(resonance_state.hrtf_slots[slot_idx]);
    resonance_state.hrtf_slots[slot_idx] = resonance_hrtf_state();
}

void
Resonance_ProcessHrtf(u32 slot_idx, float** data, const Fvector* relative_direction)
{
    if (slot_idx >= resonance_state.hrtf_slots.size()) {
        return;
    }

    resonance_hrtf_state* state = &resonance_state.hrtf_slots[slot_idx];
    if (state->api == NULL || state->source_id == vraudio::ResonanceAudioApi::kInvalidSourceId) {
        return;
    }

    Fvector direction = *relative_direction;
    float length = direction.magnitude();
    if (length < EPS) {
        direction.set(0.0f, 0.0f, 1.0f);
    } else {
        direction.mul(1.0f / length);
    }

    state->api->SetSourcePosition(state->source_id, direction.x, direction.y, direction.z);

    float buffer[SND_BLOCKSIZE];
	memcpy(buffer, data[0], sizeof(buffer));
	const float* mono_input[1] = {buffer};
	state->api->SetPlanarBuffer(state->source_id, mono_input, 1, SND_BLOCKSIZE);
	state->api->FillPlanarOutputBuffer(SND_CHANNEL_COUNT, SND_BLOCKSIZE, data);
}
