#ifndef SoundRender_CoreAH
#define SoundRender_CoreAH
#pragma once

#include "SoundRender_Core.h"            
#include "OpenALDeviceList.h"
#include <AL/efx.h>

#ifdef DEBUG
#	define A_CHK(expr)		{ alGetError(); 		expr; ALenum error=alGetError(); 			VERIFY2(error==AL_NO_ERROR, (LPCSTR)alGetString(error)); }
#	define AC_CHK(expr)		{ alcGetError(pDevice); expr; ALCenum error=alcGetError(pDevice); 	VERIFY2(error==ALC_NO_ERROR,(LPCSTR)alcGetString(pDevice,error)); }
#else
#	define A_CHK(expr)		{ expr; }
#	define AC_CHK(expr)		{ expr; }
#endif

#define FUNCTION_CAST(T, ptr) reinterpret_cast<T>(ptr)

class CSoundRender_CoreA: public CSoundRender_Core
{
	typedef CSoundRender_Core inherited;
	ALCdevice* 				pDevice;
    ALCcontext*				pContext;
	ALDeviceList*			pDeviceList;

	struct SListener{
		Fvector				position;
        Fvector velocity{};
        Fvector curVelocity{};
        Fvector prevVelocity{};
        Fvector accVelocity{};
        Fvector	orientation[2]{};
	};
	SListener				Listener;

    /* Filter object functions */
    LPALGENFILTERS alGenFilters{};
    LPALDELETEFILTERS alDeleteFilters{};
    LPALISFILTER alIsFilter{};
    LPALFILTERI alFilteri{};
    LPALFILTERIV alFilteriv{};
    LPALFILTERF alFilterf{};
    LPALFILTERFV alFilterfv{};
    LPALGETFILTERI alGetFilteri{};
    LPALGETFILTERIV alGetFilteriv{};
    LPALGETFILTERF alGetFilterf{};
    LPALGETFILTERFV alGetFilterfv{};

    /* Effect object functions */
    LPALGENEFFECTS alGenEffects{};
    LPALDELETEEFFECTS alDeleteEffects{};
    LPALISEFFECT alIsEffect{};
    LPALEFFECTI alEffecti{};
    LPALEFFECTIV alEffectiv{};
    LPALEFFECTF alEffectf{};
    LPALEFFECTFV alEffectfv{};
    LPALGETEFFECTI alGetEffecti{};
    LPALGETEFFECTIV alGetEffectiv{};
    LPALGETEFFECTF alGetEffectf;
    LPALGETEFFECTFV alGetEffectfv;

    /* Auxiliary Effect Slot object functions */
    LPALGENAUXILIARYEFFECTSLOTS alGenAuxiliaryEffectSlots{};
    LPALDELETEAUXILIARYEFFECTSLOTS alDeleteAuxiliaryEffectSlots{};
    LPALISAUXILIARYEFFECTSLOT alIsAuxiliaryEffectSlot{};
    LPALAUXILIARYEFFECTSLOTI alAuxiliaryEffectSloti{};
    LPALAUXILIARYEFFECTSLOTIV alAuxiliaryEffectSlotiv{};
    LPALAUXILIARYEFFECTSLOTF alAuxiliaryEffectSlotf{};
    LPALAUXILIARYEFFECTSLOTFV alAuxiliaryEffectSlotfv{};
    LPALGETAUXILIARYEFFECTSLOTI alGetAuxiliaryEffectSloti{};
    LPALGETAUXILIARYEFFECTSLOTIV alGetAuxiliaryEffectSlotiv{};
    LPALGETAUXILIARYEFFECTSLOTF alGetAuxiliaryEffectSlotf{};
    LPALGETAUXILIARYEFFECTSLOTFV alGetAuxiliaryEffectSlotfv{};

    ALuint effect{};
    ALuint effectfv{};
    ALuint slot{};

    bool m_is_supported{}; // Boolean variable to indicate presence of EFX Extension

protected:
	virtual void			update_listener			( const Fvector& P, const Fvector& D, const Fvector& N, float dt );
public:	
						    CSoundRender_CoreA		();
    virtual					~CSoundRender_CoreA		();

    auto get_slot() const {
        return slot;
    }

    virtual int load_effect(ALuint effect, const EFXEAXREVERBPROPERTIES* reverb);
    virtual int load_chorus(ALuint effect);
    virtual int load_flanger(ALuint effect);
    virtual int load_vocal_morpher(ALuint effect);
    virtual int load_pitch_shifter(ALuint effect);
    virtual int load_ring_modulator(ALuint effect);
    virtual int load_autowah(ALuint effect);
    virtual int load_equalizer(ALuint effect);
    virtual int load_distortion(ALuint effect);
    virtual int load_compressor(ALuint effect);
    virtual int load_echo(ALuint effect);

    virtual void set_listener(const CSoundRender_Environment& env);
    virtual void get_listener(CSoundRender_Environment& env);
    virtual bool initialized();
    virtual void commit();

    virtual void _initialize(int stage) override;
    virtual void _clear() override;
    virtual void _restart() override;
    
	virtual void			set_master_volume		( float f		);

	virtual const Fvector&	listener_position		( ){return Listener.position;}
};
extern CSoundRender_CoreA* SoundRenderA;
#endif
