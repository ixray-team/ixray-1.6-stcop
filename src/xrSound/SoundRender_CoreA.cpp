#include "stdafx.h"
#pragma hdrstop

#include "SoundRender_Core.h"
#include "SoundRender_CoreA.h"
#include "SoundRender_TargetA.h"
#include "OpenALDeviceList.h"
#include "SoundRender_Environment.h"

CSoundRender_CoreA*	SoundRenderA = nullptr;

CSoundRender_CoreA::CSoundRender_CoreA	() {
	pDevice						= 0;
	pDeviceList					= 0;
	pContext					= 0;
}

bool CSoundRender_CoreA::initialized()
{
    return m_is_supported;
}

int CSoundRender_CoreA::load_chorus(ALuint effect)
{
    ALenum err;

    // Prepare the effect for chorus.
    alEffecti(effect, AL_EFFECT_TYPE, AL_EFFECT_DISTORTION);
    if ((err = alGetError()) != AL_NO_ERROR)
    {
        Msg("Failed to load chorus effect: %s (0x%04x)\n", alGetString(err), err);
        return 0;
    }

    // Load the chorus effect properties
    A_CHK(alEffecti(effect, AL_CHORUS_WAVEFORM, AL_CHORUS_DEFAULT_WAVEFORM));
    A_CHK(alEffecti(effect, AL_CHORUS_PHASE, AL_CHORUS_DEFAULT_PHASE));
    A_CHK(alEffectf(effect, AL_CHORUS_RATE, AL_CHORUS_DEFAULT_RATE));
    A_CHK(alEffectf(effect, AL_CHORUS_DEPTH, AL_CHORUS_DEFAULT_DEPTH));
    A_CHK(alEffectf(effect, AL_CHORUS_FEEDBACK, AL_CHORUS_DEFAULT_FEEDBACK));
    A_CHK(alEffectf(effect, AL_CHORUS_DELAY, AL_CHORUS_DEFAULT_DELAY));

    // Check if an error occured, and return failure if so.
    if ((err = alGetError()) != AL_NO_ERROR)
    {
        Msg("Error setting up chorus effect: %s\n", alGetString(err));
        return 0;
    }

    return 1;
}

int CSoundRender_CoreA::load_distortion(ALuint effect)
{
    ALenum err;

    // Prepare the effect for distortion.
    alEffecti(effect, AL_EFFECT_TYPE, AL_EFFECT_DISTORTION);
    if ((err = alGetError()) != AL_NO_ERROR)
    {
        Msg("Failed to load distortion effect: %s (0x%04x)\n", alGetString(err), err);
        return 0;
    }

    // Load the distortion effect parameters
    A_CHK(alEffectf(effect, AL_DISTORTION_EDGE, AL_DISTORTION_DEFAULT_EDGE));
    A_CHK(alEffectf(effect, AL_DISTORTION_GAIN, AL_DISTORTION_DEFAULT_GAIN));
    A_CHK(alEffectf(effect, AL_DISTORTION_LOWPASS_CUTOFF, AL_DISTORTION_DEFAULT_LOWPASS_CUTOFF));
    A_CHK(alEffectf(effect, AL_DISTORTION_EQCENTER, AL_DISTORTION_DEFAULT_EQCENTER));
    A_CHK(alEffectf(effect, AL_DISTORTION_EQBANDWIDTH, AL_DISTORTION_DEFAULT_EQBANDWIDTH));

    // Check if an error occured, and return failure if so.
    if ((err = alGetError()) != AL_NO_ERROR)
    {
        Msg("Error setting up distortion effect: %s\n", alGetString(err));
        return 0;
    }

    return 1;
}

int CSoundRender_CoreA::load_echo(ALuint effect)
{
    ALenum err;

    // Prepare the effect for echo.
    alEffecti(effect, AL_EFFECT_TYPE, AL_EFFECT_ECHO);
    if ((err = alGetError()) != AL_NO_ERROR)
    {
        Msg("Failed to load echo effect: %s (0x%04x)\n", alGetString(err), err);
        return 0;
    }

    // Load the echo effect parameters
    A_CHK(alEffectf(effect, AL_ECHO_DELAY, AL_ECHO_DEFAULT_DELAY));
    A_CHK(alEffectf(effect, AL_ECHO_LRDELAY, AL_ECHO_DEFAULT_LRDELAY));
    A_CHK(alEffectf(effect, AL_ECHO_DAMPING, AL_ECHO_DEFAULT_DAMPING));
    A_CHK(alEffectf(effect, AL_ECHO_FEEDBACK, AL_ECHO_DEFAULT_FEEDBACK));
    A_CHK(alEffectf(effect, AL_ECHO_SPREAD, AL_ECHO_DEFAULT_SPREAD));

    // Check if an error occured, and return failure if so.
    if ((err = alGetError()) != AL_NO_ERROR)
    {
        Msg("Error setting up echo effect: %s\n", alGetString(err));
        return 0;
    }

    return 1;
}

int CSoundRender_CoreA::load_flanger(ALuint effect)
{
    ALenum err;

    // Prepare the effect for flanger.
    alEffecti(effect, AL_EFFECT_TYPE, AL_EFFECT_FLANGER);
    if ((err = alGetError()) != AL_NO_ERROR)
    {
        Msg("Failed to load flanger effect: %s (0x%04x)\n", alGetString(err), err);
        return 0;
    }

    // Load the flanger effect parameters 
    A_CHK(alEffecti(effect, AL_FLANGER_WAVEFORM, AL_FLANGER_DEFAULT_WAVEFORM));
    A_CHK(alEffecti(effect, AL_FLANGER_PHASE, AL_FLANGER_DEFAULT_PHASE));
    A_CHK(alEffectf(effect, AL_FLANGER_RATE, AL_FLANGER_DEFAULT_RATE));
    A_CHK(alEffectf(effect, AL_FLANGER_DEPTH, AL_FLANGER_DEFAULT_DEPTH));
    A_CHK(alEffectf(effect, AL_FLANGER_FEEDBACK, AL_FLANGER_DEFAULT_FEEDBACK));
    A_CHK(alEffectf(effect, AL_FLANGER_DELAY, AL_FLANGER_DEFAULT_DELAY));

    // Check if an error occured, and return failure if so.
    if ((err = alGetError()) != AL_NO_ERROR)
    {
        Msg("Error setting up echo effect: %s\n", alGetString(err));
        return 0;
    }

    return 1;
}

int CSoundRender_CoreA::load_vocal_morpher(ALuint effect)
{
    ALenum err;

    // Prepare the effect for vocal morpher.
    alEffecti(effect, AL_EFFECT_TYPE, AL_EFFECT_VOCAL_MORPHER);
    if ((err = alGetError()) != AL_NO_ERROR)
    {
        Msg("Failed to load vocal morpher effect: %s (0x%04x)\n", alGetString(err), err);
        return 0;
    }

    // Load the frequency vocal morpher effect parameters
    A_CHK(alEffectf(effect, AL_VOCAL_MORPHER_PHONEMEA, AL_VOCAL_MORPHER_DEFAULT_PHONEMEA));
    A_CHK(alEffectf(effect, AL_VOCAL_MORPHER_PHONEMEA_COARSE_TUNING, AL_VOCAL_MORPHER_DEFAULT_PHONEMEA_COARSE_TUNING));
    A_CHK(alEffectf(effect, AL_VOCAL_MORPHER_PHONEMEB, AL_VOCAL_MORPHER_DEFAULT_PHONEMEB));
    A_CHK(alEffectf(effect, AL_VOCAL_MORPHER_PHONEMEB_COARSE_TUNING, AL_VOCAL_MORPHER_DEFAULT_PHONEMEA_COARSE_TUNING));
    A_CHK(alEffectf(effect, AL_VOCAL_MORPHER_WAVEFORM, AL_VOCAL_MORPHER_DEFAULT_WAVEFORM));
    A_CHK(alEffectf(effect, AL_VOCAL_MORPHER_RATE, AL_VOCAL_MORPHER_DEFAULT_RATE));

    // Check if an error occured, and return failure if so.
    if ((err = alGetError()) != AL_NO_ERROR)
    {
        Msg("Error setting up vocal morpher effect: %s\n", alGetString(err));
        return 0;
    }

    return 1;
}

int CSoundRender_CoreA::load_pitch_shifter(ALuint effect)
{
    ALenum err;

    // Prepare the effect for pitch shifter.
    alEffecti(effect, AL_EFFECT_TYPE, AL_EFFECT_PITCH_SHIFTER);
    if ((err = alGetError()) != AL_NO_ERROR)
    {
        Msg("Failed to load pitch shifter effect: %s (0x%04x)\n", alGetString(err), err);
        return 0;
    }

    // Load the pitchshifter effect parameters
    A_CHK(alEffectf(effect, AL_PITCH_SHIFTER_COARSE_TUNE, AL_PITCH_SHIFTER_DEFAULT_COARSE_TUNE));
    A_CHK(alEffectf(effect, AL_PITCH_SHIFTER_FINE_TUNE, AL_PITCH_SHIFTER_DEFAULT_FINE_TUNE));

    // Check if an error occured, and return failure if so.
    if ((err = alGetError()) != AL_NO_ERROR)
    {
        Msg("Error setting up pitch shifter effect: %s\n", alGetString(err));
        return 0;
    }

    return 1;
}

int CSoundRender_CoreA::load_ring_modulator(ALuint effect)
{
    ALenum err;

    // Prepare the effect for ring modulation.
    alEffecti(effect, AL_EFFECT_TYPE, AL_EFFECT_RING_MODULATOR);
    if ((err = alGetError()) != AL_NO_ERROR)
    {
        Msg("Failed to prepare ring modulation effect: %s (0x%04x)\n", alGetString(err), err);
        return 0;
    }

    // Load the ringmodulator effect parameters
    A_CHK(alEffectf(effect, AL_RING_MODULATOR_FREQUENCY, AL_RING_MODULATOR_DEFAULT_FREQUENCY));
    A_CHK(alEffectf(effect, AL_RING_MODULATOR_HIGHPASS_CUTOFF, AL_RING_MODULATOR_DEFAULT_HIGHPASS_CUTOFF));
    A_CHK(alEffectf(effect, AL_RING_MODULATOR_WAVEFORM, AL_RING_MODULATOR_DEFAULT_WAVEFORM));

    // Check if an error occured, and return failure if so.
    if ((err = alGetError()) != AL_NO_ERROR)
    {
        Msg("Error setting up ring modulator effect: %s\n", alGetString(err));
        return 0;
    }

    return 1;
}

int CSoundRender_CoreA::load_autowah(ALuint effect)
{
    ALenum err;

    // Prepare the effect for autowah.
    alEffecti(effect, AL_EFFECT_TYPE, AL_EFFECT_AUTOWAH);
    if ((err = alGetError()) != AL_NO_ERROR)
    {
        Msg("Failed to prepare autowah effect: %s (0x%04x)\n", alGetString(err), err);
        return 0;
    }

    // Load the autowah effect parameters
    A_CHK(alEffectf(effect, AL_AUTOWAH_ATTACK_TIME, AL_AUTOWAH_DEFAULT_ATTACK_TIME));
    A_CHK(alEffectf(effect, AL_AUTOWAH_RELEASE_TIME, AL_AUTOWAH_DEFAULT_RELEASE_TIME));
    A_CHK(alEffectf(effect, AL_AUTOWAH_RESONANCE, AL_AUTOWAH_DEFAULT_RESONANCE));
    A_CHK(alEffectf(effect, AL_AUTOWAH_PEAK_GAIN, AL_AUTOWAH_DEFAULT_PEAK_GAIN));

    // Check if an error occured, and return failure if so.
    if ((err = alGetError()) != AL_NO_ERROR)
    {
        Msg("Error setting up autowah effect: %s\n", alGetString(err));
        return 0;
    }

    return 1;
}

int CSoundRender_CoreA::load_equalizer(ALuint effect)
{
    ALenum err;

    // Prepare the effect for equalizer.
    alEffecti(effect, AL_EFFECT_TYPE, AL_EFFECT_EQUALIZER);
    if ((err = alGetError()) != AL_NO_ERROR)
    {
        Msg("Failed to load equalizer effect type: %s (0x%04x)\n", alGetString(err), err);
        return 0;
    }

    // Equalizer effect parameters
    A_CHK(alEffectf(effect, AL_EQUALIZER_LOW_GAIN, AL_EQUALIZER_DEFAULT_LOW_GAIN));
    A_CHK(alEffectf(effect, AL_EQUALIZER_LOW_CUTOFF, AL_EQUALIZER_DEFAULT_LOW_CUTOFF));
    A_CHK(alEffectf(effect, AL_EQUALIZER_MID1_GAIN, AL_EQUALIZER_DEFAULT_MID1_GAIN));
    A_CHK(alEffectf(effect, AL_EQUALIZER_MID1_CENTER, AL_EQUALIZER_DEFAULT_MID1_CENTER));
    A_CHK(alEffectf(effect, AL_EQUALIZER_MID1_WIDTH, AL_EQUALIZER_DEFAULT_MID1_WIDTH));
    A_CHK(alEffectf(effect, AL_EQUALIZER_MID2_GAIN, AL_EQUALIZER_DEFAULT_MID2_GAIN));
    A_CHK(alEffectf(effect, AL_EQUALIZER_MID2_CENTER, AL_EQUALIZER_DEFAULT_MID2_CENTER));
    A_CHK(alEffectf(effect, AL_EQUALIZER_MID2_WIDTH, AL_EQUALIZER_DEFAULT_MID2_WIDTH));
    A_CHK(alEffectf(effect, AL_EQUALIZER_HIGH_GAIN, AL_EQUALIZER_DEFAULT_HIGH_GAIN));
    A_CHK(alEffectf(effect, AL_EQUALIZER_HIGH_CUTOFF, AL_EQUALIZER_DEFAULT_HIGH_CUTOFF));

    // Check if an error occured, and return failure if so.
    if ((err = alGetError()) != AL_NO_ERROR)
    {
        Msg("Error setting up equalizer effect: %s\n", alGetString(err));
        return 0;
    }

    return 1;
}

int CSoundRender_CoreA::load_compressor(ALuint effect)
{
    ALenum err;


    // Prepare the effect for compressor.
    alEffecti(effect, AL_EFFECT_TYPE, AL_EFFECT_COMPRESSOR);
    if ((err = alGetError()) != AL_NO_ERROR)
    {
        Msg("Failed to load compressor effect type: %s (0x%04x)\n", alGetString(err), err);
        return 0;
    }

    // Load the compressor effect parameters
    A_CHK(alEffecti(effect, AL_COMPRESSOR_ONOFF, AL_COMPRESSOR_DEFAULT_ONOFF));

    // Check if an error occured, and return failure if so.
    if ((err = alGetError()) != AL_NO_ERROR)
    {
        Msg("Error setting up compressor effect: %s\n", alGetString(err));
        return 0;
    }

    return 1;
}

// load_reverb loads the given initial reverb properties into the given OpenAL
//  effect object, and returns non-zero on success.
int CSoundRender_CoreA::load_reverb(ALuint effect, const EFXEAXREVERBPROPERTIES* reverb)
{
    ALenum err;

    // Prepare the effect for EAX Reverb.
    alEffecti(effect, AL_EFFECT_TYPE, AL_EFFECT_EAXREVERB);
    if ((err = alGetError()) != AL_NO_ERROR)
    {
        Msg("Failed to load the EAX reverb effect: %s (0x%04x)\n", alGetString(err), err);
        return 0;
    }

    // Load the reverb properties.
    A_CHK(alEffectf(effect, AL_EAXREVERB_DENSITY, reverb->flDensity));
    A_CHK(alEffectf(effect, AL_EAXREVERB_DIFFUSION, reverb->flDiffusion));
    A_CHK(alEffectf(effect, AL_EAXREVERB_GAIN, reverb->flGain));
    A_CHK(alEffectf(effect, AL_EAXREVERB_GAINHF, reverb->flGainHF));
    A_CHK(alEffectf(effect, AL_EAXREVERB_GAINLF, reverb->flGainLF));
    A_CHK(alEffectf(effect, AL_EAXREVERB_DECAY_TIME, reverb->flDecayTime));
    A_CHK(alEffectf(effect, AL_EAXREVERB_DECAY_HFRATIO, reverb->flDecayHFRatio));
    A_CHK(alEffectf(effect, AL_EAXREVERB_DECAY_LFRATIO, reverb->flDecayLFRatio));
    A_CHK(alEffectf(effect, AL_EAXREVERB_REFLECTIONS_GAIN, reverb->flReflectionsGain));
    A_CHK(alEffectf(effect, AL_EAXREVERB_REFLECTIONS_DELAY, reverb->flReflectionsDelay));
    A_CHK(alEffectfv(effect, AL_EAXREVERB_REFLECTIONS_PAN, reverb->flReflectionsPan));
    A_CHK(alEffectf(effect, AL_EAXREVERB_LATE_REVERB_GAIN, reverb->flLateReverbGain));
    A_CHK(alEffectf(effect, AL_EAXREVERB_LATE_REVERB_DELAY, reverb->flLateReverbDelay));
    A_CHK(alEffectfv(effect, AL_EAXREVERB_LATE_REVERB_PAN, reverb->flLateReverbPan));
    A_CHK(alEffectf(effect, AL_EAXREVERB_ECHO_TIME, reverb->flEchoTime));
    A_CHK(alEffectf(effect, AL_EAXREVERB_ECHO_DEPTH, reverb->flEchoDepth));
    A_CHK(alEffectf(effect, AL_EAXREVERB_MODULATION_TIME, reverb->flModulationTime));
    A_CHK(alEffectf(effect, AL_EAXREVERB_MODULATION_DEPTH, reverb->flModulationDepth));
    A_CHK(alEffectf(effect, AL_EAXREVERB_AIR_ABSORPTION_GAINHF, reverb->flAirAbsorptionGainHF));
    A_CHK(alEffectf(effect, AL_EAXREVERB_HFREFERENCE, reverb->flHFReference));
    A_CHK(alEffectf(effect, AL_EAXREVERB_LFREFERENCE, reverb->flLFReference));
    A_CHK(alEffectf(effect, AL_EAXREVERB_ROOM_ROLLOFF_FACTOR, reverb->flRoomRolloffFactor));
    A_CHK(alEffecti(effect, AL_EAXREVERB_DECAY_HFLIMIT, reverb->iDecayHFLimit));

    // Check if an error occured, and return failure if so.
    if ((err = alGetError()) != AL_NO_ERROR)
    {
        Msg("Error setting up reverb effect: %s\n", alGetString(err));
        return 0;
    }

    return 1;
}

void CSoundRender_CoreA::set_listener(const CSoundRender_Environment& env)
{
    A_CHK(alEffectf(effect, AL_EAXREVERB_DENSITY, env.Density));
    A_CHK(alEffectf(effect, AL_EAXREVERB_DIFFUSION, env.EnvironmentDiffusion));
    A_CHK(alEffectf(effect, AL_EAXREVERB_GAIN, env.Room));
    A_CHK(alEffectf(effect, AL_EAXREVERB_GAINHF, env.RoomHF));
    A_CHK(alEffectf(effect, AL_EAXREVERB_GAINHF, env.RoomLF));
    A_CHK(alEffectf(effect, AL_EAXREVERB_DECAY_TIME, env.DecayTime));
    A_CHK(alEffectf(effect, AL_EAXREVERB_DECAY_HFRATIO, env.DecayHFRatio));
    A_CHK(alEffectf(effect, AL_EAXREVERB_DECAY_LFRATIO, env.DecayLFRatio));
    A_CHK(alEffectf(effect, AL_EAXREVERB_REFLECTIONS_GAIN, env.Reflections));
    A_CHK(alEffectf(effect, AL_EAXREVERB_REFLECTIONS_DELAY, env.ReflectionsDelay));
    A_CHK(alEffectf(effect, AL_EAXREVERB_REFLECTIONS_PAN, *env.ReflectionsPan));
    A_CHK(alEffectf(effect, AL_EAXREVERB_LATE_REVERB_GAIN, env.Reverb));
    A_CHK(alEffectf(effect, AL_EAXREVERB_LATE_REVERB_DELAY, env.ReverbDelay));
    A_CHK(alEffectf(effect, AL_EAXREVERB_ECHO_TIME, env.EchoTime));
    A_CHK(alEffectf(effect, AL_EAXREVERB_ECHO_DEPTH, env.EchoDepth));
    A_CHK(alEffectf(effect, AL_EAXREVERB_LATE_REVERB_PAN, *env.ReverbPan));
    A_CHK(alEffectf(effect, AL_EAXREVERB_MODULATION_TIME, env.ModulationTime));
    A_CHK(alEffectf(effect, AL_EAXREVERB_MODULATION_DEPTH, env.ModulationDepth));
    A_CHK(alEffectf(effect, AL_EAXREVERB_AIR_ABSORPTION_GAINHF, env.AirAbsorptionHF));
    A_CHK(alEffectf(effect, AL_EAXREVERB_HFREFERENCE, env.HFReference));
    A_CHK(alEffectf(effect, AL_EAXREVERB_LFREFERENCE, env.LFReference));
    A_CHK(alEffectf(effect, AL_EAXREVERB_ROOM_ROLLOFF_FACTOR, env.RoomRolloffFactor));
    A_CHK(alEffecti(effect, AL_EAXREVERB_DECAY_HFLIMIT, env.DecayHFLimit));
}

void CSoundRender_CoreA::get_listener(CSoundRender_Environment& env)
{
    A_CHK(alGetEffectf(effect, AL_EAXREVERB_DENSITY, &env.Density));
    A_CHK(alGetEffectf(effect, AL_EAXREVERB_DIFFUSION, &env.EnvironmentDiffusion));
    A_CHK(alGetEffectf(effect, AL_EAXREVERB_GAIN, &env.Room));
    A_CHK(alGetEffectf(effect, AL_EAXREVERB_GAINHF, &env.RoomHF));
    A_CHK(alGetEffectf(effect, AL_EAXREVERB_GAINHF, &env.RoomLF));
    A_CHK(alGetEffectf(effect, AL_EAXREVERB_DECAY_TIME, &env.DecayTime));
    A_CHK(alGetEffectf(effect, AL_EAXREVERB_DECAY_HFRATIO, &env.DecayHFRatio));
    A_CHK(alGetEffectf(effect, AL_EAXREVERB_DECAY_LFRATIO, &env.DecayLFRatio));
    A_CHK(alGetEffectf(effect, AL_EAXREVERB_REFLECTIONS_GAIN, &env.Reflections));
    A_CHK(alGetEffectf(effect, AL_EAXREVERB_REFLECTIONS_DELAY, &env.ReflectionsDelay));
    A_CHK(alGetEffectf(effect, AL_EAXREVERB_LATE_REVERB_GAIN, &env.Reverb));
    A_CHK(alGetEffectf(effect, AL_EAXREVERB_REFLECTIONS_PAN, env.ReflectionsPan));
    A_CHK(alGetEffectf(effect, AL_EAXREVERB_LATE_REVERB_DELAY, &env.ReverbDelay));
    A_CHK(alGetEffectf(effect, AL_EAXREVERB_ECHO_TIME, &env.EchoTime));
    A_CHK(alGetEffectf(effect, AL_EAXREVERB_ECHO_DEPTH, &env.EchoDepth));
    A_CHK(alGetEffectf(effect, AL_EAXREVERB_LATE_REVERB_PAN, env.ReverbPan));
    A_CHK(alGetEffectf(effect, AL_EAXREVERB_MODULATION_TIME, &env.ModulationTime));
    A_CHK(alGetEffectf(effect, AL_EAXREVERB_MODULATION_DEPTH, &env.ModulationDepth));
    A_CHK(alGetEffectf(effect, AL_EAXREVERB_AIR_ABSORPTION_GAINHF, &env.AirAbsorptionHF));
    A_CHK(alGetEffectf(effect, AL_EAXREVERB_HFREFERENCE, &env.HFReference));
    A_CHK(alGetEffectf(effect, AL_EAXREVERB_LFREFERENCE, &env.LFReference));
    A_CHK(alGetEffectf(effect, AL_EAXREVERB_ROOM_ROLLOFF_FACTOR, &env.RoomRolloffFactor));
    A_CHK(alEffecti(effect, AL_EAXREVERB_DECAY_HFLIMIT, (ALint)env.DecayHFLimit));
}

void CSoundRender_CoreA::commit()
{
    // Tell the effect slot to use the loaded effect object. Note that this
    // effectively copies the effect properties. You can modify or delete the
    // effect object afterward without affecting the effect slot.
    A_CHK(alAuxiliaryEffectSlotf(slot, AL_EFFECTSLOT_GAIN, 1.f));
    A_CHK(alAuxiliaryEffectSloti(slot, AL_EFFECTSLOT_AUXILIARY_SEND_AUTO, false));
    A_CHK(alAuxiliaryEffectSloti(slot, AL_EFFECTSLOT_EFFECT, effect));
}

CSoundRender_CoreA::~CSoundRender_CoreA	()
{
    if (m_is_supported)
    {
        alDeleteEffects(1, &effect);
        if (alIsAuxiliaryEffectSlot(slot))
            alDeleteAuxiliaryEffectSlots(1, &slot);
    }
}

void  CSoundRender_CoreA::_restart()
{
	inherited::_restart();
}

void CSoundRender_CoreA::_initialize(int stage)
{
	if(stage==0)
	{
		pDeviceList					= xr_new<ALDeviceList>();

		if (0==pDeviceList->GetNumDevices())
		{ 
			CHECK_OR_EXIT			(0,"OpenAL: Can't create sound device.");
			xr_delete				(pDeviceList);
		}
		return;
	}
	
	pDeviceList->SelectBestDevice	();
	R_ASSERT						(snd_device_id>=0 && snd_device_id<pDeviceList->GetNumDevices());
	const ALDeviceDesc& deviceDesc	= pDeviceList->GetDeviceDesc(snd_device_id);
    // OpenAL device
    pDevice						= alcOpenDevice		(deviceDesc.name);
    if (pDevice == nullptr)
	{
		CHECK_OR_EXIT			(0,"SOUND: OpenAL: Failed to create device.");
        bPresent = false;
		return;
	}

    // Get the device specifier.
    const ALCchar*		        deviceSpecifier;
    deviceSpecifier         	= alcGetString		(pDevice, ALC_DEVICE_SPECIFIER);

    // Create context
    pContext = alcCreateContext(pDevice, nullptr);
	if (0==pContext){
		CHECK_OR_EXIT			(0,"SOUND: OpenAL: Failed to create context.");
        bPresent = false;
		alcCloseDevice			(pDevice); pDevice = 0;
		return;
	}
    
    // clear errors
	alGetError					();
	alcGetError					(pDevice);
    
    // Set active context
    AC_CHK				        (alcMakeContextCurrent(pContext));

    // initialize listener
    A_CHK				        (alListener3f		(AL_POSITION,0.f,0.f,0.f));
    A_CHK				        (alListener3f		(AL_VELOCITY,0.f,0.f,0.f));
    Fvector	orient[2]	        = {{0.f,0.f,1.f},{0.f,1.f,0.f}};
    A_CHK				        (alListenerfv		(AL_ORIENTATION,&orient[0].x));
    A_CHK				        (alListenerf		(AL_GAIN,1.f));

     // Define a macro to help load the function pointers.
#define LOAD_PROC(T, x)  ((x) = FUNCTION_CAST(T, alGetProcAddress(#x)))
    // Filter object functions
    LOAD_PROC(LPALGENFILTERS, alGenFilters);
    LOAD_PROC(LPALDELETEFILTERS, alDeleteFilters);
    LOAD_PROC(LPALISFILTER, alIsFilter);
    LOAD_PROC(LPALFILTERI, alFilteri);
    LOAD_PROC(LPALFILTERIV, alFilteriv);
    LOAD_PROC(LPALFILTERF, alFilterf);
    LOAD_PROC(LPALFILTERFV, alFilterfv);
    LOAD_PROC(LPALGETFILTERI, alGetFilteri);
    LOAD_PROC(LPALGETFILTERIV, alGetFilteriv);
    LOAD_PROC(LPALGETFILTERF, alGetFilterf);
    LOAD_PROC(LPALGETFILTERFV, alGetFilterfv);

    // Effect object functions
    LOAD_PROC(LPALGENEFFECTS, alGenEffects);
    LOAD_PROC(LPALDELETEEFFECTS, alDeleteEffects);
    LOAD_PROC(LPALISEFFECT, alIsEffect);
    LOAD_PROC(LPALEFFECTI, alEffecti);
    LOAD_PROC(LPALEFFECTIV, alEffectiv);
    LOAD_PROC(LPALEFFECTF, alEffectf);
    LOAD_PROC(LPALEFFECTFV, alEffectfv);
    LOAD_PROC(LPALGETEFFECTI, alGetEffecti);
    LOAD_PROC(LPALGETEFFECTIV, alGetEffectiv);
    LOAD_PROC(LPALGETEFFECTF, alGetEffectf);
    LOAD_PROC(LPALGETEFFECTFV, alGetEffectfv); alEffecti(effect, AL_EFFECT_TYPE, AL_EFFECT_EAXREVERB);
   
    // Auxiliary effect slot object functions
    LOAD_PROC(LPALGENAUXILIARYEFFECTSLOTS, alGenAuxiliaryEffectSlots);
    LOAD_PROC(LPALDELETEAUXILIARYEFFECTSLOTS, alDeleteAuxiliaryEffectSlots);
    LOAD_PROC(LPALISAUXILIARYEFFECTSLOT, alIsAuxiliaryEffectSlot);
    LOAD_PROC(LPALAUXILIARYEFFECTSLOTI, alAuxiliaryEffectSloti);
    LOAD_PROC(LPALAUXILIARYEFFECTSLOTIV, alAuxiliaryEffectSlotiv);
    LOAD_PROC(LPALAUXILIARYEFFECTSLOTF, alAuxiliaryEffectSlotf);
    LOAD_PROC(LPALAUXILIARYEFFECTSLOTFV, alAuxiliaryEffectSlotfv);
    LOAD_PROC(LPALGETAUXILIARYEFFECTSLOTI, alGetAuxiliaryEffectSloti);
    LOAD_PROC(LPALGETAUXILIARYEFFECTSLOTIV, alGetAuxiliaryEffectSlotiv);
    LOAD_PROC(LPALGETAUXILIARYEFFECTSLOTF, alGetAuxiliaryEffectSlotf);
    LOAD_PROC(LPALGETAUXILIARYEFFECTSLOTFV, alGetAuxiliaryEffectSlotfv);
#undef LOAD_PROC

    alGenEffects(1, &effect);

    load_reverb(effect, &reverbs[0]);
   
    // Check if an error occured, and clean up if so.
    ALenum err = alGetError();
    if (err == AL_NO_ERROR)
        m_is_supported = true;
    else
    {
        Log("SOUND: OpenAL: Failed to init EFX:", alGetString(err));
        if (alIsEffect(effect))
            alDeleteEffects(1, &effect);
    }

    alGenAuxiliaryEffectSlots(1, &slot);

    err = alGetError();
    if (err != AL_NO_ERROR)
        Log("! SOUND: OpenAL: failed to generate auxiliary slot:", alGetString(err));

    Log("* SOUND: EFX extension:", m_is_supported ? "present" : "absent");

    auto auxSlot = ALuint(-1);

    if (deviceDesc.props.efx) {
        if (initialized())
            auxSlot = get_slot();
        else {
            Log("SOUND: OpenAL: Failed to initialize EFX.");
        }
    }

    inherited::_initialize		(stage);

	if(stage==1)//first initialize
	{
		// Pre-create targets
		CSoundRender_Target*	T	= nullptr;
		for (u32 tit=0; tit<u32(psSoundTargets); tit++)
		{
			T = xr_new<CSoundRender_TargetA>(auxSlot);
			if (T->_initialize())
			{
				s_targets.push_back	(T);
			}else
			{
        		Log					("! SOUND: OpenAL: Max targets - ",tit);
				T->_destroy			();
        		xr_delete			(T);
        		break;
			}
		}
	}
}

void CSoundRender_CoreA::set_master_volume(float f )
{
	if (bPresent)				{
		A_CHK				    (alListenerf	(AL_GAIN,f));
	}
}

void CSoundRender_CoreA::_clear	()
{
	inherited::_clear			();
    // remove targets
	CSoundRender_Target*	T	= 0;
	for (u32 tit=0; tit<s_targets.size(); tit++)
	{
		T						= s_targets[tit];
		T->_destroy				();
        xr_delete				(T);
	}
    // Reset the current context to nullptr.
    alcMakeContextCurrent(nullptr);
    // Release the context and the device.
    alcDestroyContext			(pContext);		pContext	= 0;
    alcCloseDevice				(pDevice);		pDevice		= 0;
	xr_delete					(pDeviceList);
}

void CSoundRender_CoreA::update_listener		( const Fvector& P, const Fvector& D, const Fvector& N, float dt )
{
	inherited::update_listener(P,D,N,dt);

	if (!Listener.position.similar(P)){
		Listener.position.set	(P);
        bListenerMoved = true;
	}
	Listener.orientation[0].set	(D.x,D.y,-D.z);
	Listener.orientation[1].set	(N.x,N.y,-N.z);

	A_CHK						(alListener3f	(AL_POSITION,Listener.position.x,Listener.position.y,-Listener.position.z));
	A_CHK						(alListener3f	(AL_VELOCITY,0.f,0.f,0.f));
	A_CHK						(alListenerfv	(AL_ORIENTATION,&Listener.orientation[0].x));
} 