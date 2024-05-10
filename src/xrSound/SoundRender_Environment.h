#pragma once

#include <AL/efx-presets.h>

constexpr EFXEAXREVERBPROPERTIES reverbs[] = 
{
    // Default presets
    EFX_REVERB_PRESET_GENERIC,
    EFX_REVERB_PRESET_PADDEDCELL,
    EFX_REVERB_PRESET_ROOM,
    EFX_REVERB_PRESET_BATHROOM,
    EFX_REVERB_PRESET_LIVINGROOM,
    EFX_REVERB_PRESET_STONEROOM,
    EFX_REVERB_PRESET_AUDITORIUM,
    EFX_REVERB_PRESET_CONCERTHALL,
    EFX_REVERB_PRESET_CAVE,
    EFX_REVERB_PRESET_ARENA,
    EFX_REVERB_PRESET_HANGAR,
    EFX_REVERB_PRESET_CARPETEDHALLWAY,
    EFX_REVERB_PRESET_HALLWAY,
    EFX_REVERB_PRESET_STONECORRIDOR,
    EFX_REVERB_PRESET_ALLEY,
    EFX_REVERB_PRESET_FOREST,
    EFX_REVERB_PRESET_CITY,
    EFX_REVERB_PRESET_MOUNTAINS,
    EFX_REVERB_PRESET_QUARRY,
    EFX_REVERB_PRESET_PLAIN,
    EFX_REVERB_PRESET_PARKINGLOT,
    EFX_REVERB_PRESET_SEWERPIPE,
    EFX_REVERB_PRESET_UNDERWATER,
    EFX_REVERB_PRESET_DRUGGED,
    EFX_REVERB_PRESET_DIZZY,
    EFX_REVERB_PRESET_PSYCHOTIC,
    // Castle Presets
    EFX_REVERB_PRESET_CASTLE_SMALLROOM,
    EFX_REVERB_PRESET_CASTLE_SHORTPASSAGE,
    EFX_REVERB_PRESET_CASTLE_MEDIUMROOM,
    EFX_REVERB_PRESET_CASTLE_LARGEROOM,
    EFX_REVERB_PRESET_CASTLE_LONGPASSAGE,
    EFX_REVERB_PRESET_CASTLE_HALL,
    EFX_REVERB_PRESET_CASTLE_CUPBOARD,
    EFX_REVERB_PRESET_CASTLE_COURTYARD,
    EFX_REVERB_PRESET_CASTLE_ALCOVE,
    // Factory Presets
    EFX_REVERB_PRESET_FACTORY_SMALLROOM,
    EFX_REVERB_PRESET_FACTORY_SHORTPASSAGE,
    EFX_REVERB_PRESET_FACTORY_MEDIUMROOM,
    EFX_REVERB_PRESET_FACTORY_LARGEROOM,
    EFX_REVERB_PRESET_FACTORY_LONGPASSAGE,
    EFX_REVERB_PRESET_FACTORY_HALL,
    EFX_REVERB_PRESET_FACTORY_CUPBOARD,
    EFX_REVERB_PRESET_FACTORY_COURTYARD,
    EFX_REVERB_PRESET_FACTORY_ALCOVE,
    // Ice Palace Presets
    EFX_REVERB_PRESET_ICEPALACE_SMALLROOM,
    EFX_REVERB_PRESET_ICEPALACE_SHORTPASSAGE,
    EFX_REVERB_PRESET_ICEPALACE_MEDIUMROOM,
    EFX_REVERB_PRESET_ICEPALACE_LARGEROOM,
    EFX_REVERB_PRESET_ICEPALACE_LONGPASSAGE,
    EFX_REVERB_PRESET_ICEPALACE_HALL,
    EFX_REVERB_PRESET_ICEPALACE_CUPBOARD,
    EFX_REVERB_PRESET_ICEPALACE_COURTYARD,
    EFX_REVERB_PRESET_ICEPALACE_ALCOVE,
    // Space Station Presets
    EFX_REVERB_PRESET_SPACESTATION_SMALLROOM,
    EFX_REVERB_PRESET_SPACESTATION_SHORTPASSAGE,
    EFX_REVERB_PRESET_SPACESTATION_MEDIUMROOM,
    EFX_REVERB_PRESET_SPACESTATION_LARGEROOM,
    EFX_REVERB_PRESET_SPACESTATION_LONGPASSAGE,
    EFX_REVERB_PRESET_SPACESTATION_HALL,
    EFX_REVERB_PRESET_SPACESTATION_CUPBOARD,
    EFX_REVERB_PRESET_SPACESTATION_ALCOVE,
    // Wooden Galleon Presets
    EFX_REVERB_PRESET_WOODEN_SMALLROOM,
    EFX_REVERB_PRESET_WOODEN_SHORTPASSAGE,
    EFX_REVERB_PRESET_WOODEN_MEDIUMROOM,
    EFX_REVERB_PRESET_WOODEN_LARGEROOM,
    EFX_REVERB_PRESET_WOODEN_LONGPASSAGE,
    EFX_REVERB_PRESET_WOODEN_HALL,
    EFX_REVERB_PRESET_WOODEN_CUPBOARD,
    EFX_REVERB_PRESET_WOODEN_COURTYARD,
    EFX_REVERB_PRESET_WOODEN_ALCOVE,
    // Sports Presets
    EFX_REVERB_PRESET_SPORT_EMPTYSTADIUM,
    EFX_REVERB_PRESET_SPORT_SQUASHCOURT,
    EFX_REVERB_PRESET_SPORT_SMALLSWIMMINGPOOL,
    EFX_REVERB_PRESET_SPORT_LARGESWIMMINGPOOL,
    EFX_REVERB_PRESET_SPORT_GYMNASIUM,
    EFX_REVERB_PRESET_SPORT_FULLSTADIUM,
    EFX_REVERB_PRESET_SPORT_STADIUMTANNOY,
    // Prefab Presets
    EFX_REVERB_PRESET_PREFAB_WORKSHOP,
    EFX_REVERB_PRESET_PREFAB_SCHOOLROOM,
    EFX_REVERB_PRESET_PREFAB_PRACTISEROOM,
    EFX_REVERB_PRESET_PREFAB_OUTHOUSE,
    EFX_REVERB_PRESET_PREFAB_CARAVAN,
    // Dome and Pipe Presets
    EFX_REVERB_PRESET_DOME_TOMB,
    EFX_REVERB_PRESET_PIPE_SMALL,
    EFX_REVERB_PRESET_DOME_SAINTPAULS,
    EFX_REVERB_PRESET_PIPE_LONGTHIN,
    EFX_REVERB_PRESET_PIPE_LARGE,
    EFX_REVERB_PRESET_PIPE_RESONANT,
    // Outdoors Presets
    EFX_REVERB_PRESET_OUTDOORS_BACKYARD,
    EFX_REVERB_PRESET_OUTDOORS_ROLLINGPLAINS,
    EFX_REVERB_PRESET_OUTDOORS_DEEPCANYON,
    EFX_REVERB_PRESET_OUTDOORS_CREEK,
    EFX_REVERB_PRESET_OUTDOORS_VALLEY,
    // Mood Presets
    EFX_REVERB_PRESET_MOOD_HEAVEN,
    EFX_REVERB_PRESET_MOOD_HELL,
    EFX_REVERB_PRESET_MOOD_MEMORY,
    // Driving Presets
    EFX_REVERB_PRESET_DRIVING_COMMENTATOR,
    EFX_REVERB_PRESET_DRIVING_PITGARAGE,
    EFX_REVERB_PRESET_DRIVING_INCAR_RACER,
    EFX_REVERB_PRESET_DRIVING_INCAR_SPORTS,
    EFX_REVERB_PRESET_DRIVING_INCAR_LUXURY,
    EFX_REVERB_PRESET_DRIVING_FULLGRANDSTAND,
    EFX_REVERB_PRESET_DRIVING_EMPTYGRANDSTAND,
    EFX_REVERB_PRESET_DRIVING_TUNNEL,
    // City Presets
    EFX_REVERB_PRESET_CITY_STREETS,
    EFX_REVERB_PRESET_CITY_SUBWAY,
    EFX_REVERB_PRESET_CITY_MUSEUM,
    EFX_REVERB_PRESET_CITY_LIBRARY,
    EFX_REVERB_PRESET_CITY_UNDERPASS,
    EFX_REVERB_PRESET_CITY_ABANDONED,
    // Misc. Presets
    EFX_REVERB_PRESET_DUSTYROOM,
    EFX_REVERB_PRESET_CHAPEL,
    EFX_REVERB_PRESET_SMALLWATERROOM
};

#define SNDENV_VER_COP 4
#define SNDENV_VER_IXR 5

// refs
class XRSOUND_API CSoundRender_Environment		: public CSound_environment
{
public:
	u32				version;
	shared_str			name;

    u32				Environment;				// source environment
    float           Room;                       // room effect level at low frequencies
    float           RoomHF;                     // room effect high-frequency level re. low frequency level
    float RoomLF; // room effect high-frequency level re. low frequency level
    float           RoomRolloffFactor;          // like DS3D flRolloffFactor but for room effect
    float           DecayTime;                  // reverberation decay time at low frequencies
    float           DecayHFRatio;               // high-frequency to low-frequency decay time ratio
    float DecayLFRatio; // low-frequency to high-frequency decay time ratio
    int DecayHFLimit; // high- frequency decay time limit set by air absorbtion gain
    float           Reflections;                // early reflections level relative to room effect
    float           ReflectionsDelay;           // initial reflection delay time
    float ReflectionsPan[3]; // reflections panning xyz
    float EchoTime; // delay between the original sound and echo instance
    float EchoDepth; // controls the amount of echo feedback signal in the loop
    float ReverbPan[3]; // reverb pan xyz
    float           Reverb;                     // late reverberation level relative to room effect
    float           ReverbDelay;                // late reverberation delay time relative to initial reflection
    float           EnvironmentSize;            // environment size in meters
    float           EnvironmentDiffusion;       // controls the echo density in the reverberation decay
    float           AirAbsorptionHF;            // change in level per meter at 5 kHz
    float ModulationTime; // modulation time for sound blending
    float ModulationDepth; // modulation depth for sound blending
    float Density; // controls the coloration of the late reverb
    float HFReference; // frequency at which low frequency effects are measured
    float LFReference; // frequency at which high frequency effects are measured

public:
                    CSoundRender_Environment	(void);
                    ~CSoundRender_Environment	(void);
	void			set_identity	            ();
	void			set_default		            ();
	void			clamp			            ();
	void			lerp			            (CSoundRender_Environment& A, CSoundRender_Environment& B, float f);
	bool			load			            (IReader* fs);
	void			save			            (IWriter* fs);
};

class XRSOUND_API SoundEnvironment_LIB
{
public:
	using SE_VEC = xr_vector<CSoundRender_Environment*>;
	using SE_IT = SE_VEC::iterator;
private:
	SE_VEC						library;
public:
	void						Load	(LPCSTR name);
	bool						Save	(LPCSTR name);
	void						Unload	();
	int							GetID	(LPCSTR name);
	CSoundRender_Environment*	Get		(LPCSTR name);
	CSoundRender_Environment*	Get		(int id);
	CSoundRender_Environment*	Append	(CSoundRender_Environment* parent=0);
	void						Remove	(LPCSTR name);
	void						Remove	(int id);
	SE_VEC&						Library	();
};