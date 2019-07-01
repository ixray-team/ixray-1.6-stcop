/*$ preserve start $*/

/* ============================================================================================ */
/* FMOD Ex - Main C/C++ event/data driven system header file.                                   */
/* Copyright (c), Firelight Technologies Pty, Ltd. 2004-2008.                                   */
/*                                                                                              */
/* This header is the base header for all other FMOD EventSystem headers. If you are            */
/* programming in C use this exclusively, or if you are programming C++ use this in             */
/* conjunction with FMOD_EVENT.HPP                                                              */
/* ============================================================================================ */

#ifndef __FMOD_EVENT_H__
#define __FMOD_EVENT_H__

#ifndef _FMOD_H
#include "fmod.h"
#endif

/*
    FMOD EventSystem version number.  Check this against FMOD::EventSystem::getVersion.
    0xaaaabbcc -> aaaa = major version number.  bb = minor version number.  cc = development version number.
*/

#define FMOD_EVENT_VERSION 0x00041708

/*
    FMOD event types
*/

typedef struct FMOD_EVENTSYSTEM    FMOD_EVENTSYSTEM;
typedef struct FMOD_EVENTPROJECT   FMOD_EVENTPROJECT;
typedef struct FMOD_EVENTGROUP     FMOD_EVENTGROUP;
typedef struct FMOD_EVENTCATEGORY  FMOD_EVENTCATEGORY;
typedef struct FMOD_EVENT          FMOD_EVENT;
typedef struct FMOD_EVENTPARAMETER FMOD_EVENTPARAMETER;
typedef struct FMOD_EVENTREVERB    FMOD_EVENTREVERB;
typedef struct FMOD_MUSICPROMPT    FMOD_MUSICPROMPT;
typedef struct FMOD_MUSICSYSTEM    FMOD_MUSICSYSTEM;
typedef unsigned int               FMOD_EVENT_INITFLAGS;
typedef unsigned int               FMOD_EVENT_MODE;
typedef unsigned int               FMOD_EVENT_STATE;
typedef unsigned int               FMOD_MUSIC_CUE_ID;
typedef unsigned int               FMOD_MUSIC_PARAM_ID;


/*
[DEFINE]
[
    [NAME]
    FMOD_EVENT_INITFLAGS

    [DESCRIPTION]   
    Initialization flags.  Use them with EventSystem::init in the eventflags parameter to change various behaviour.

    [REMARKS]

    [PLATFORMS]
    Win32, Win64, Linux, Linux64, Macintosh, Xbox, Xbox360, PlayStation 2, GameCube, PlayStation Portable, PlayStation 3

    [SEE_ALSO]
    EventSystem::init
]
*/
#define FMOD_EVENT_INIT_NORMAL              0x00000000 /* All platforms - Initialize normally */
#define FMOD_EVENT_INIT_USER_ASSETMANAGER   0x00000001 /* All platforms - All wave data loading/freeing will be referred back to the user through the event callback */
#define FMOD_EVENT_INIT_FAIL_ON_MAXSTREAMS  0x00000002 /* All platforms - Events will fail if "Max streams" was reached when playing streamed banks, instead of going virtual. */
#define FMOD_EVENT_INIT_DONTUSENAMES        0x00000004 /* All platforms - All event/eventgroup/eventparameter/eventcategory/eventreverb names will be discarded on load. Use getXXXByIndex to access them. This may potentially save a lot of memory at runtime. */
#define FMOD_EVENT_INIT_UPPERCASE_FILENAMES 0x00000008 /* All platforms - All FSB filenames will be translated to upper case before being used. */
#define FMOD_EVENT_INIT_SEARCH_PLUGINS      0x00000010 /* All platforms - Search the current directory for dsp/codec plugins on EventSystem::init. */

/* [DEFINE_END] */


/*
[DEFINE]
[
    [NAME] 
    FMOD_EVENT_MODE

    [DESCRIPTION]   
    Event data loading bitfields. Bitwise OR them together for controlling how event data is loaded.

    [REMARKS]

    [PLATFORMS]
    Win32, Win64, Linux, Macintosh, Xbox, Xbox360, PlayStation 2, GameCube, PlayStation Portable, PlayStation 3, Wii

    [SEE_ALSO]
    EventGroup::loadEventData
    EventGroup::getEvent
    EventGroup::getEventByIndex
]
*/
#define FMOD_EVENT_DEFAULT               0x00000000  /* FMOD_EVENT_DEFAULT specifies default loading behaviour i.e. event data for the whole group is NOT cached and the function that initiated the loading process will block until loading is complete. */
#define FMOD_EVENT_NONBLOCKING           0x00000001  /* For loading event data asynchronously. FMOD will use a thread to load the data.  Use Event::getState to find out when loading is complete. */
#define FMOD_EVENT_ERROR_ON_DISKACCESS   0x00000002  /* For EventGroup::getEvent / EventGroup::getEventByIndex.  If EventGroup::loadEventData has accidently been forgotten this flag will return an FMOD_ERR_FILE_UNWANTED if the getEvent function tries to load data. */
#define FMOD_EVENT_INFOONLY              0x00000004  /* For EventGroup::getEvent / EventGroup::getEventByIndex.  Don't allocate instances or load data, just get a handle to allow user to get information from the event. */
/* [DEFINE_END] */


/*
[DEFINE]
[
    [NAME] 
    FMOD_EVENT_STATE

    [DESCRIPTION]   
    These values describe what state an event is in.

    [REMARKS]    
    The flags below can be combined to set multiple states at once.  Use bitwise AND operations to test for these.
    An example of a combined flag set would be FMOD_EVENT_STATE_READY | FMOD_EVENT_STATE_PLAYING.

    [PLATFORMS]
    Win32, Win64, Linux, Macintosh, Xbox, Xbox360, PlayStation 2, GameCube, PlayStation Portable, PlayStation 3, Wii

    [SEE_ALSO]
    Event::getState
    FMOD_EVENT_MODE
]
*/
#define FMOD_EVENT_STATE_READY           0x00000001  /* Event is ready to play. */
#define FMOD_EVENT_STATE_LOADING         0x00000002  /* Loading in progress. */
#define FMOD_EVENT_STATE_ERROR           0x00000004  /* Failed to open - file not found, out of memory etc.  See return value of Event::getState for what happened. */
#define FMOD_EVENT_STATE_PLAYING         0x00000008  /* Event has been started.  This will still be true even if there are no sounds active.  Event::stop must be called or the event must stop itself using a 'one shot and stop event' parameter mode. */
#define FMOD_EVENT_STATE_CHANNELSACTIVE  0x00000010  /* Event has active voices.  Use this if you want to detect if sounds are playing in the event or not. */
#define FMOD_EVENT_STATE_INFOONLY        0x00000020  /* Event was loaded with the FMOD_EVENT_INFOONLY flag. */
#define FMOD_EVENT_STATE_STARVING        0x00000040  /* Event is streaming but not being fed data in time, so may be stuttering. */
/* [DEFINE_END] */


/*
[ENUM]
[
	[DESCRIPTION]
    Property indices for Event::getPropertyByIndex.
    
	[REMARKS]        

    [PLATFORMS]
    Win32, Win64, Linux, Macintosh, Xbox, Xbox360, PlayStation 2, GameCube, PlayStation Portable, PlayStation 3, Wii

	[SEE_ALSO]
    Event::getPropertyByIndex
]
*/
typedef enum
{
    FMOD_EVENTPROPERTY_NAME = 0,                        /* Type : char *    - Name of event. */
    FMOD_EVENTPROPERTY_VOLUME,                          /* Type : float     - Relative volume of event. */
    FMOD_EVENTPROPERTY_VOLUMERANDOMIZATION,             /* Type : float     - Random deviation in volume of event. */
    FMOD_EVENTPROPERTY_PITCH,                           /* Type : float     - Relative pitch of event in raw underlying units. */
    FMOD_EVENTPROPERTY_PITCH_OCTAVES,                   /* Type : float     - Relative pitch of event in octaves. */
    FMOD_EVENTPROPERTY_PITCH_SEMITONES,                 /* Type : float     - Relative pitch of event in semitones. */
    FMOD_EVENTPROPERTY_PITCH_TONES,                     /* Type : float     - Relative pitch of event in tones. */
    FMOD_EVENTPROPERTY_PITCHRANDOMIZATION,              /* Type : float     - Random deviation in pitch of event in raw underlying units. */
    FMOD_EVENTPROPERTY_PITCHRANDOMIZATION_OCTAVES,      /* Type : float     - Random deviation in pitch of event in octaves. */
    FMOD_EVENTPROPERTY_PITCHRANDOMIZATION_SEMITONES,    /* Type : float     - Random deviation in pitch of event in semitones. */
    FMOD_EVENTPROPERTY_PITCHRANDOMIZATION_TONES,        /* Type : float     - Random deviation in pitch of event in tones. */
    FMOD_EVENTPROPERTY_PRIORITY,                        /* Type : int       - Playback priority of event. */
    FMOD_EVENTPROPERTY_MAX_PLAYBACKS,                   /* Type : int       - Maximum simultaneous playbacks of event. */
    FMOD_EVENTPROPERTY_MAX_PLAYBACKS_BEHAVIOR,          /* Type : int       - 1 = steal oldest, 2 = steal newest, 3 = steal quietest, 4 = just fail, 5 = just fail if quietest. */
    FMOD_EVENTPROPERTY_MODE,                            /* Type : FMOD_MODE - Either FMOD_3D or FMOD_2D. */
    FMOD_EVENTPROPERTY_3D_ROLLOFF,                      /* Type : FMOD_MODE - Either FMOD_3D_LOGROLLOFF, FMOD_3D_LINEARROLLOFF, or none for custom rolloff. */
    FMOD_EVENTPROPERTY_3D_MINDISTANCE,                  /* Type : float     - Minimum 3d distance of event. */
    FMOD_EVENTPROPERTY_3D_MAXDISTANCE,                  /* Type : float     - Maximum 3d distance of event.  Means different things depending on EVENTPROPERTY_3D_ROLLOFF. If event has custom rolloff, setting FMOD_EVENTPROPERTY_3D_MAXDISTANCE will scale the range of all distance parameters in this event e.g. set this property to 2.0 to double the range of all distance parameters, set it to 0.5 to halve the range of all distance parameters. */
    FMOD_EVENTPROPERTY_3D_POSITION,                     /* Type : FMOD_MODE - Either FMOD_3D_HEADRELATIVE or FMOD_3D_WORLDRELATIVE. */
    FMOD_EVENTPROPERTY_3D_CONEINSIDEANGLE,              /* Type : float     - Event cone inside angle.  0 to 360. */
    FMOD_EVENTPROPERTY_3D_CONEOUTSIDEANGLE,             /* Type : float     - Event cone outside angle.  0 to 360. */
    FMOD_EVENTPROPERTY_3D_CONEOUTSIDEVOLUME,            /* Type : float     - Event cone outside volume.  0 to 1.0. */
    FMOD_EVENTPROPERTY_3D_DOPPLERSCALE,                 /* Type : float     - Doppler scale where 0 = no doppler, 1.0 = normal doppler, 2.0 = double doppler etc. */
    FMOD_EVENTPROPERTY_3D_SPEAKERSPREAD,                /* Type : float     - Angle of spread for stereo/mutlichannel source. 0 to 360. */
    FMOD_EVENTPROPERTY_3D_PANLEVEL,                     /* Type : float     - 0 = sound pans according to speaker levels, 1 = sound pans according to 3D position. */
    FMOD_EVENTPROPERTY_SPEAKER_L,                       /* Type : float     - 2D event volume for front left speaker. */
    FMOD_EVENTPROPERTY_SPEAKER_C,                       /* Type : float     - 2D event volume for front center speaker. */
    FMOD_EVENTPROPERTY_SPEAKER_R,                       /* Type : float     - 2D event volume for front right speaker. */
    FMOD_EVENTPROPERTY_SPEAKER_LS,                      /* Type : float     - 2D event volume for side left speaker. */
    FMOD_EVENTPROPERTY_SPEAKER_RS,                      /* Type : float     - 2D event volume for side right speaker. */
    FMOD_EVENTPROPERTY_SPEAKER_LR,                      /* Type : float     - 2D event volume for back left speaker. */
    FMOD_EVENTPROPERTY_SPEAKER_RR,                      /* Type : float     - 2D event volume for back right speaker. */
    FMOD_EVENTPROPERTY_SPEAKER_LFE,                     /* Type : float     - 2D event volume for low frequency speaker. */
    FMOD_EVENTPROPERTY_REVERBWETLEVEL,                  /* Type : float     - Reverb gain for this event where 0 = full reverb, -60 = no reverb. */
    FMOD_EVENTPROPERTY_ONESHOT,                         /* Type : int       - Oneshot event - stops when no channels playing */
    FMOD_EVENTPROPERTY_FADEIN,                          /* Type : int       - Time in milliseconds over which to fade this event in when programmer starts it. 0 = no fade in. */
    FMOD_EVENTPROPERTY_FADEOUT,                         /* Type : int       - Time in milliseconds over which to fade this event out when programmer stops it. 0 = no fade out. */
    FMOD_EVENTPROPERTY_REVERBDRYLEVEL,                  /* Type : float     - Dry reverb gain for this event where 0 = full dry, -60 = no dry. */
    FMOD_EVENTPROPERTY_TIMEOFFSET,                      /* Type : float     - Time offset of sound start in seconds. */
    FMOD_EVENTPROPERTY_SPAWNINTENSITY,                  /* Type : float     - Multiplier for spawn frequency of all sounds in this event. */
    FMOD_EVENTPROPERTY_SPAWNINTENSITY_RANDOMIZATION,    /* Type : float     - Random deviation in spawn intensity of event. */
    FMOD_EVENTPROPERTY_WII_CONTROLLERSPEAKER,           /* Type : int       - Wii only. Use 0 to 3 to specify a Wii controller speaker to play this event on, -1 to play on normal Wii speakers. */
	FMOD_EVENTPROPERTY_3D_POSRANDOMIZATION,             /* Type : unsigned int   - Radius of random deviation in the 3D position of event. */
    FMOD_EVENTPROPERTY_USER_BASE                        /* User created events start from here onwards. */
} FMOD_EVENT_PROPERTY;


/*
[ENUM]
[
	[DESCRIPTION]
    Pitch units for Event::setPitch and EventCategory::setPitch.
    
	[REMARKS]        

    [PLATFORMS]
    Win32, Win64, Linux, Macintosh, Xbox, Xbox360, PlayStation 2, GameCube, PlayStation Portable, PlayStation 3, Wii

	[SEE_ALSO]
    Event::setPitch
    EventCategory::setPitch
]
*/
typedef enum
{
    FMOD_EVENT_PITCHUNITS_RAW = 0,      /* Pitch is specified in raw underlying units. */
    FMOD_EVENT_PITCHUNITS_OCTAVES,      /* Pitch is specified in units of octaves. */
    FMOD_EVENT_PITCHUNITS_SEMITONES,    /* Pitch is specified in units of semitones. */
    FMOD_EVENT_PITCHUNITS_TONES,        /* Pitch is specified in units of tones. */
} FMOD_EVENT_PITCHUNITS;


/*
[ENUM]
[
	[DESCRIPTION]
    Flags to pass to EventGroup::loadEventData to determine what to load at the time of calling.
    
	[REMARKS]

    [PLATFORMS]
    Win32, Win64, Linux, Macintosh, Xbox, Xbox360, PlayStation 2, GameCube, PlayStation Portable, PlayStation 3, Wii

	[SEE_ALSO]
    EventGroup::loadEventData
]
*/
typedef enum
{
    FMOD_EVENT_RESOURCE_STREAMS_AND_SAMPLES,  /* Open all streams and load all banks into memory, under this group (recursive) */
    FMOD_EVENT_RESOURCE_STREAMS,              /* Open all streams under this group (recursive).  No samples are loaded. */
    FMOD_EVENT_RESOURCE_SAMPLES               /* Load all banks into memory, under this group (recursive).  No streams are opened. */
} FMOD_EVENT_RESOURCE;


/*
[ENUM]
[
    [DESCRIPTION]   
    These callback types are used with FMOD_EVENT_CALLBACK.

    [REMARKS]
    <b>Note!</b>  Currently the user must call EventSystem::update for these callbacks to trigger!
    <br>
    An FMOD_EVENT_CALLBACKTYPE_SYNCPOINT callback is generated from 'markers' embedded in .wav files.
    These can be created by placing 'markers' in the original source wavs using a tool such as Sound Forge or Cooledit.<br>
    The wavs are then compiled into .FSB files when compiling the audio data using the FMOD designer tool.<br>
    Callbacks will be automatically generated at the correct place in the timeline when these markers are encountered
    which makes it useful for synchronization, lip syncing etc.<br>
    <br>
    An FMOD_EVENT_CALLBACKTYPE_SOUNDDEF_START callback is generated each time a sound definition is played in an event.<br>
    This happens every time a sound definition starts due to the event parameter entering the region specified in the 
    layer created by the sound designer..<br>
    This also happens when sounds are randomly respawned using the random respawn feature in the sound definition 
    properties in FMOD designer.<br>
    <br>
    An FMOD_EVENT_CALLBACKTYPE_SOUNDDEF_END callback is generated when a one-shot sound definition inside an event ends, 
    or when a looping sound definition stops due to the event parameter leaving the region specified in the layer created 
    by the sound designer.<br>
    <br>
    An FMOD_EVENT_CALLBACKTYPE_STOLEN callback is generated when a getEventXXX call needs to steal a playing event because 
    the event's "Max playbacks" has been exceeded. This callback is called before the event is stolen and before the event 
    is stopped. An FMOD_EVENT_CALLBACKTYPE_EVENTFINISHED callback will be generated when the stolen event is stopped i.e. <b>after</b>
    the FMOD_EVENT_CALLBACKTYPE_STOLEN. If the callback function returns FMOD_ERR_EVENT_FAILED, the event will <b>not</b>
    be stolen, and the returned value will be passed back as the return value of the getEventXXX call that triggered the steal attempt.<br>
    <br>
    An FMOD_EVENT_CALLBACKTYPE_EVENTFINISHED callback is generated whenever an event is stopped for any reason including when the user
    calls Event::stop().<br>
    <br>
    An FMOD_EVENT_CALLBACKTYPE_NET_MODIFIED callback is generated when someone has connected to your running application with 
    FMOD Designer and changed a property within this event, for example volume or pitch.<br>
    <br>
    An FMOD_EVENT_CALLBACKTYPE_SOUNDDEF_CREATE callback is generated when a "programmer" sound needs to be loaded.<br>
    <br>
    An FMOD_EVENT_CALLBACKTYPE_SOUNDDEF_RELEASE callback is generated when a "programmer" sound needs to be unloaded.<br>
    <br>
    An FMOD_EVENT_CALLBACKTYPE_SOUNDDEF_INFO callback is generated when a sound definition is loaded. It can be used to find
    information about the specific sound that will be played.<br>
    <br>
    An FMOD_EVENT_CALLBACKTYPE_EVENTSTARTED callback is generated whenever an event is started. This callback will be called before any sounds in the event have begun to play.<br>
    <br>
    An FMOD_EVENT_CALLBACKTYPE_SOUNDDEF_SELECTINDEX callback is generated when a sound definition entry needs to be chosen from a "ProgrammerSelected" sound definition.<br>
    <br>

    [PLATFORMS]
    Win32, Win64, Linux, Macintosh, Xbox, Xbox360, PlayStation 2, GameCube, PlayStation Portable, PlayStation 3, Wii, Wii

    [SEE_ALSO]      
    Event::setCallback
    FMOD_EVENT_CALLBACK
    EventSystem::update
]
*/
typedef enum
{
    FMOD_EVENT_CALLBACKTYPE_SYNCPOINT,            /* Called when a syncpoint is encountered. Can be from wav file markers. */
    FMOD_EVENT_CALLBACKTYPE_SOUNDDEF_START,       /* Called when a sound definition inside an event is triggered. */
    FMOD_EVENT_CALLBACKTYPE_SOUNDDEF_END,         /* Called when a sound definition inside an event ends or is stopped. */
    FMOD_EVENT_CALLBACKTYPE_STOLEN,               /* Called when a getEventXXX call steals a playing event instance. */
    FMOD_EVENT_CALLBACKTYPE_EVENTFINISHED,        /* Called when an event is stopped for any reason. */
    FMOD_EVENT_CALLBACKTYPE_NET_MODIFIED,         /* Called when a property of the event has been modified by a network-connected host. */
    FMOD_EVENT_CALLBACKTYPE_SOUNDDEF_CREATE,      /* Called when a programmer sound definition entry is loaded. */
    FMOD_EVENT_CALLBACKTYPE_SOUNDDEF_RELEASE,     /* Called when a programmer sound definition entry is unloaded. */
    FMOD_EVENT_CALLBACKTYPE_SOUNDDEF_INFO,        /* Called when a sound definition entry is loaded. */
    FMOD_EVENT_CALLBACKTYPE_EVENTSTARTED,         /* Called when an event is started. */
    FMOD_EVENT_CALLBACKTYPE_SOUNDDEF_SELECTINDEX  /* Called when a sound definition entry needs to be chosen from a "ProgrammerSelected" sound definition. */
} FMOD_EVENT_CALLBACKTYPE;


/*
[STRUCTURE]
[
    [DESCRIPTION]
    Structure containing realtime information about a wavebank.

    [REMARKS]

    [PLATFORMS]
    Win32, Win64, Linux, Linux64, Macintosh, Xbox, Xbox360, PlayStation 2, GameCube, PlayStation Portable, PlayStation 3

    [SEE_ALSO]      
    EventSystem::getInfo
    FMOD_EVENT_SYSTEMINFO
]
*/
typedef struct FMOD_EVENT_WAVEBANKINFO
{
    char         *name;                  /* [out] Name of this wave bank. */
    int           streamrefcnt;          /* [out] Number of stream references to this wave bank made by events in this event system. */
    int           samplerefcnt;          /* [out] Number of sample references to this wave bank made by events in this event system. */
    int           numstreams;            /* [out] Number of times this wave bank has been opened for streaming. */
    int           maxstreams;            /* [out] Maximum number of times this wave bank will be opened for streaming. */
    int           streamsinuse;          /* [out] Number of streams currently in use. */
    unsigned int  streammemory;          /* [out] Amount of memory (in bytes) used by streams. */
    unsigned int  samplememory;          /* [out] Amount of memory (in bytes) used by samples. */
    int           type;                  /* [out] 0 = stream from disk, 1 = load into memory, 2 = decompress into memory. */

} FMOD_EVENT_WAVEBANKINFO;


/*
[STRUCTURE]
[
    [DESCRIPTION]
    Structure containing realtime information about an event system.

    [REMARKS]
    On entry, numplayingevents should be set to the number of elements in the playingevents array. If the actual
    number of playing events is greater than numplayingevents then the playingevents array will be filled with
    numplayingevents entries and numplayingevents will be set to the actual number of playing events on exit.
    In short, if numplayingevents on exit > numplayingevents on entry then the playingevents array wasn't large
    enough and some events were unable to be added to the array.

    [PLATFORMS]
    Win32, Win64, Linux, Linux64, Macintosh, Xbox, Xbox360, PlayStation 2, GameCube, PlayStation Portable, PlayStation 3

    [SEE_ALSO]      
    EventSystem::getInfo
    FMOD_EVENT_WAVEBANKINFO
]
*/
typedef struct FMOD_EVENT_SYSTEMINFO
{
    int                      numevents;        /* [out] Total number of events in all event groups in this event system. */
    int                      eventmemory;      /* [out] Amount of memory (in bytes) used by event hierarchy classes. (Logging version only) */
    int                      numinstances;     /* [out] Total number of event instances in all event groups in this event system. */
    int                      instancememory;   /* [out] Amount of memory (in bytes) used by all event instances in this event system. DISABLED.  Not working currently until further notice.  Use FMOD::Memory_GetStats instead. */
    int                      dspmemory;        /* [out] Amount of memory (in bytes) used by event dsp networks. DISABLED.  Not working currently until further notice.  Use FMOD::Memory_GetStats instead. */
    int                      numwavebanks;     /* [out] Number of wave banks known to this event system. */
    FMOD_EVENT_WAVEBANKINFO *wavebankinfo;     /* [out] Array of detailed information on each wave bank. */
    int                      numplayingevents; /* [in/out] On entry, maximum number of entries in playingevents array. On exit, actual number of entries in playingevents array, or if playingevents is null, then it is just the number of currently playing events. Optional. */
    FMOD_EVENT             **playingevents;    /* [in/out] Pointer to an array that will be filled with the event handles of all playing events. Optional. Specify 0 if not needed. Must be used in conjunction with numplayingevents. */

} FMOD_EVENT_SYSTEMINFO;


/*
[STRUCTURE]
[
    [DESCRIPTION]
    Structure containing extended information about an event.

    [REMARKS]
    This structure is optional!  Specify 0 or NULL in Event::getInfo if you don't need it!<br>
    This structure has members that need to be initialized before Event::getInfo is called. Always initialize this structure before calling Event::getInfo!

    [PLATFORMS]
    Win32, Win64, Linux, Linux64, Macintosh, Xbox, Xbox360, PlayStation 2, GameCube, PlayStation Portable, PlayStation 3

    [SEE_ALSO]      
    Event::getInfo
]
*/
typedef struct FMOD_EVENT_INFO
{
    int           memoryused;             /* [out] Amount of memory (in bytes) used by this event. (Logging version only) */
    int           positionms;             /* [out] Time passed in playback of this event instance in milliseconds. */
    int           lengthms;               /* [out] Length in milliseconds of this event. Note: lengthms will be -1 if the length of the event can't be determined i.e. if it has looping sounds. */
    int           channelsplaying;        /* [out] Number of channels currently playing in this event instance. */
    int           instancesactive;        /* [out] Number of event instances currently in use. */
    char        **wavebanknames;          /* [out] An array containing the names of all wave banks that are referenced by this event. */
    unsigned int  projectid;              /* [out] The runtime 'EventProject' wide unique identifier for this event. */
    unsigned int  systemid;               /* [out] The runtime 'EventSystem' wide unique identifier for this event.  This is calculated when single or multiple projects are loaded. */
    float         audibility;             /* [out] current audibility of event. */
    int           numinstances;           /* [in/out] On entry, maximum number of entries in instances array. On exit, actual number of entries in instances array, or if instances is null, then it is just the number of instances of this event. Optional. */
    FMOD_EVENT  **instances;              /* [in/out] Pointer to an array that will be filled with the current reference-counted event handles of all instances of this event. Optional. Specify 0 if not needed. Must be used in conjunction with numinstances. Note: Due to reference counting, the event instance handles returned here may be different between subsequent calls to this function. If you use these event handles, make sure your code is prepared for them to be invalid! */

} FMOD_EVENT_INFO;


/*
[STRUCTURE]
[
    [DESCRIPTION]
    Use this structure with EventSystem::load when more control is needed over loading.

    [REMARKS]
    This structure is optional!  Specify 0 or NULL in EventSystem::load if you don't need it!<br>
    <br>
    Members marked with [in] mean the user sets the value before passing it to the function.<br>
    Members marked with [out] mean FMOD sets the value to be used after the function exits.<br>
    Use sounddefentrylimit to limit the number of sound definition entries - and therefore the amount of wave data - loaded for each sound definition. This feature allows the programmer to implement a "low detail" setting at runtime without needing a seperate "low detail" set of assets.

    [PLATFORMS]
    Win32, Win64, Linux, Linux64, Macintosh, Xbox, Xbox360, PlayStation 2, GameCube, PlayStation Portable, PlayStation 3

    [SEE_ALSO]      
    EventSystem::load
]
*/
typedef struct FMOD_EVENT_LOADINFO
{
    unsigned int  size;                   /* [in] Size of this structure.  This is used so the structure can be expanded in the future and still work on older versions of FMOD Ex. */
    char         *encryptionkey;          /* [in] Optional. Specify 0 to ignore. Key, or 'password' to decrypt a bank.  A sound designer may have encrypted the audio data to protect their sound data from 'rippers'. */
    float         sounddefentrylimit;     /* [in] Optional. Specify 0 to ignore. A value between 0 -> 1 that is multiplied with the number of sound definition entries in each sound definition in the project being loaded in order to programmatically reduce the number of sound definition entries used at runtime. */
    unsigned int  loadfrommemory_length;  /* [in] Optional. Specify 0 to ignore. Length of memory buffer pointed to by name_or_data parameter passed to EventSystem::load. If this field is non-zero then the name_or_data parameter passed to EventSystem::load will be interpreted as a pointer to a memory buffer containing the .fev data to load. If this field is zero the name_or_data parameter is interpreted as the filename of the .fev file to load. */
    bool          override_category_vals; /* [in] Optional. If this member is set to true, newly-loaded categories will impart their properties (volume, pitch etc.) to existing categories of the same name. */

} FMOD_EVENT_LOADINFO;


typedef FMOD_RESULT (F_CALLBACK *FMOD_EVENT_CALLBACK) (FMOD_EVENT *event, FMOD_EVENT_CALLBACKTYPE type, void *param1, void *param2, void *userdata);


/*
[STRUCTURE]
[
    [DESCRIPTION]
    Structure containing realtime information about the music system.

    [REMARKS]

    [PLATFORMS]
    Win32, Win64, Linux, Linux64, Macintosh, Xbox, Xbox360, PlayStation 2, GameCube, PlayStation Portable, PlayStation 3

    [SEE_ALSO]      
    MusicSystem::getInfo
]
*/
typedef struct FMOD_MUSIC_INFO
{
    FMOD_BOOL               starving;        /* [out] True if any streams in the music system are starving. */
} FMOD_MUSIC_INFO;

/*
[ENUM]
[
    [DESCRIPTION]   
    These callback types are used with FMOD_MUSIC_CALLBACK.

    [REMARKS]
    <b>Note!</b>  Currently the user must call EventSystem::update for these callbacks to trigger!<br />

    [PLATFORMS]
    Win32, Win64, Linux, Macintosh, Xbox, Xbox360, PlayStation 2, GameCube, PlayStation Portable, PlayStation 3, Wii, Wii

    [SEE_ALSO]      
    MusicSystem::setCallback
    FMOD_MUSIC_CALLBACK
    EventSystem::update
]
*/
typedef enum
{
    FMOD_MUSIC_CALLBACKTYPE_SEGMENT_START,       /* Called when a segment is started. */
    FMOD_MUSIC_CALLBACKTYPE_SEGMENT_END,         /* Called when a segment ends. */
    FMOD_MUSIC_CALLBACKTYPE_RESET,               /* Called when the system is reset */
} FMOD_MUSIC_CALLBACKTYPE;


typedef FMOD_RESULT (F_CALLBACK *FMOD_MUSIC_CALLBACK) (FMOD_MUSIC_CALLBACKTYPE type, void *param1, void *param2, void *userdata);


/* ========================================================================================== */
/* FUNCTION PROTOTYPES                                                                        */
/* ========================================================================================== */

#ifdef __cplusplus
extern "C" 
{
#endif

/*
    FMOD EventSystem factory functions.  Use this to create an FMOD EventSystem Instance.  Below you will see FMOD_EventSystem_Init/Release to get started.
*/

FMOD_RESULT F_API FMOD_EventSystem_Create(FMOD_EVENTSYSTEM **eventsystem);

/*$ preserve end $*/

/*
    'EventSystem' API
*/

/*
     Initialization / system functions.
*/

FMOD_RESULT F_API FMOD_EventSystem_Init              (FMOD_EVENTSYSTEM *eventsystem, int maxchannels, FMOD_INITFLAGS flags, void *extradriverdata, FMOD_EVENT_INITFLAGS eventflags);
FMOD_RESULT F_API FMOD_EventSystem_Release           (FMOD_EVENTSYSTEM *eventsystem);
FMOD_RESULT F_API FMOD_EventSystem_Update            (FMOD_EVENTSYSTEM *eventsystem);
FMOD_RESULT F_API FMOD_EventSystem_SetMediaPath      (FMOD_EVENTSYSTEM *eventsystem, const char *path);
FMOD_RESULT F_API FMOD_EventSystem_SetPluginPath     (FMOD_EVENTSYSTEM *eventsystem, const char *path);
FMOD_RESULT F_API FMOD_EventSystem_GetVersion        (FMOD_EVENTSYSTEM *eventsystem, unsigned int *version);
FMOD_RESULT F_API FMOD_EventSystem_GetInfo           (FMOD_EVENTSYSTEM *eventsystem, FMOD_EVENT_SYSTEMINFO *info);
FMOD_RESULT F_API FMOD_EventSystem_GetSystemObject   (FMOD_EVENTSYSTEM *eventsystem, FMOD_SYSTEM **system);
FMOD_RESULT F_API FMOD_EventSystem_GetMusicSystem    (FMOD_EVENTSYSTEM *eventsystem, FMOD_MUSICSYSTEM **musicsystem);

/*
     FEV load/unload.                                 
*/

FMOD_RESULT F_API FMOD_EventSystem_Load              (FMOD_EVENTSYSTEM *eventsystem, const char *name_or_data, FMOD_EVENT_LOADINFO *loadinfo, FMOD_EVENTPROJECT **project);
FMOD_RESULT F_API FMOD_EventSystem_Unload            (FMOD_EVENTSYSTEM *eventsystem);

/*
     Event,EventGroup,EventCategory Retrieval.        
*/

FMOD_RESULT F_API FMOD_EventSystem_GetProject        (FMOD_EVENTSYSTEM *eventsystem, const char *name, FMOD_EVENTPROJECT **project);
FMOD_RESULT F_API FMOD_EventSystem_GetProjectByIndex (FMOD_EVENTSYSTEM *eventsystem, int index, FMOD_EVENTPROJECT **project);
FMOD_RESULT F_API FMOD_EventSystem_GetNumProjects    (FMOD_EVENTSYSTEM *eventsystem, int *numprojects);
FMOD_RESULT F_API FMOD_EventSystem_GetCategory       (FMOD_EVENTSYSTEM *eventsystem, const char *name, FMOD_EVENTCATEGORY **category);
FMOD_RESULT F_API FMOD_EventSystem_GetCategoryByIndex(FMOD_EVENTSYSTEM *eventsystem, int index, FMOD_EVENTCATEGORY **category);
FMOD_RESULT F_API FMOD_EventSystem_GetNumCategories  (FMOD_EVENTSYSTEM *eventsystem, int *numcategories);
FMOD_RESULT F_API FMOD_EventSystem_GetGroup          (FMOD_EVENTSYSTEM *eventsystem, const char *name, FMOD_BOOL cacheevents, FMOD_EVENTGROUP **group);
FMOD_RESULT F_API FMOD_EventSystem_GetEvent          (FMOD_EVENTSYSTEM *eventsystem, const char *name, FMOD_EVENT_MODE mode, FMOD_EVENT **event);
FMOD_RESULT F_API FMOD_EventSystem_GetEventBySystemID(FMOD_EVENTSYSTEM *eventsystem, unsigned int systemid, FMOD_EVENT_MODE mode, FMOD_EVENT **event);
FMOD_RESULT F_API FMOD_EventSystem_GetNumEvents      (FMOD_EVENTSYSTEM *eventsystem, int *numevents);

/*
     Reverb interfaces.
*/

FMOD_RESULT F_API FMOD_EventSystem_SetReverbProperties(FMOD_EVENTSYSTEM *eventsystem, const FMOD_REVERB_PROPERTIES *prop);
FMOD_RESULT F_API FMOD_EventSystem_GetReverbProperties(FMOD_EVENTSYSTEM *eventsystem, FMOD_REVERB_PROPERTIES *prop);

FMOD_RESULT F_API FMOD_EventSystem_GetReverbPreset   (FMOD_EVENTSYSTEM *eventsystem, const char *name, FMOD_REVERB_PROPERTIES *prop, int *index);
FMOD_RESULT F_API FMOD_EventSystem_GetReverbPresetByIndex(FMOD_EVENTSYSTEM *eventsystem, const int index, FMOD_REVERB_PROPERTIES *prop, char **name);
FMOD_RESULT F_API FMOD_EventSystem_GetNumReverbPresets(FMOD_EVENTSYSTEM *eventsystem, int *numpresets);

FMOD_RESULT F_API FMOD_EventSystem_CreateReverb      (FMOD_EVENTSYSTEM *eventsystem, FMOD_EVENTREVERB **reverb);
FMOD_RESULT F_API FMOD_EventSystem_SetReverbAmbientProperties(FMOD_EVENTSYSTEM *eventsystem, FMOD_REVERB_PROPERTIES *prop);
FMOD_RESULT F_API FMOD_EventSystem_GetReverbAmbientProperties(FMOD_EVENTSYSTEM *eventsystem, FMOD_REVERB_PROPERTIES *prop);

/*
     3D Listener interface.
*/

FMOD_RESULT F_API FMOD_EventSystem_Set3DNumListeners (FMOD_EVENTSYSTEM *eventsystem, int numlisteners);
FMOD_RESULT F_API FMOD_EventSystem_Get3DNumListeners (FMOD_EVENTSYSTEM *eventsystem, int *numlisteners);
FMOD_RESULT F_API FMOD_EventSystem_Set3DListenerAttributes(FMOD_EVENTSYSTEM *eventsystem, int listener, const FMOD_VECTOR *pos, const FMOD_VECTOR *vel, const FMOD_VECTOR *forward, const FMOD_VECTOR *up);
FMOD_RESULT F_API FMOD_EventSystem_Get3DListenerAttributes(FMOD_EVENTSYSTEM *eventsystem, int listener, FMOD_VECTOR *pos, FMOD_VECTOR *vel, FMOD_VECTOR *forward, FMOD_VECTOR *up);

/*
     Get/set user data
*/

FMOD_RESULT F_API FMOD_EventSystem_SetUserData       (FMOD_EVENTSYSTEM *eventsystem, void *userdata);
FMOD_RESULT F_API FMOD_EventSystem_GetUserData       (FMOD_EVENTSYSTEM *eventsystem, void **userdata);

/*
     In memory FSB registration.
*/

FMOD_RESULT F_API FMOD_EventSystem_RegisterMemoryFSB (FMOD_EVENTSYSTEM *eventsystem, const char *filename, void *fsbdata, unsigned int fsbdatalen, FMOD_BOOL loadintorsx);
FMOD_RESULT F_API FMOD_EventSystem_UnregisterMemoryFSB(FMOD_EVENTSYSTEM *eventsystem, const char *filename);

/*
    'EventProject' API
*/

FMOD_RESULT F_API FMOD_EventProject_Release          (FMOD_EVENTPROJECT *eventproject);
FMOD_RESULT F_API FMOD_EventProject_GetInfo          (FMOD_EVENTPROJECT *eventproject, int *index, char **name);
FMOD_RESULT F_API FMOD_EventProject_GetGroup         (FMOD_EVENTPROJECT *eventproject, const char *name, FMOD_BOOL cacheevents, FMOD_EVENTGROUP **group);
FMOD_RESULT F_API FMOD_EventProject_GetGroupByIndex  (FMOD_EVENTPROJECT *eventproject, int index, FMOD_BOOL cacheevents, FMOD_EVENTGROUP **group);
FMOD_RESULT F_API FMOD_EventProject_GetNumGroups     (FMOD_EVENTPROJECT *eventproject, int *numgroups);
FMOD_RESULT F_API FMOD_EventProject_GetEvent         (FMOD_EVENTPROJECT *eventproject, const char *name, FMOD_EVENT_MODE mode, FMOD_EVENT **event);
FMOD_RESULT F_API FMOD_EventProject_GetEventByProjectID(FMOD_EVENTPROJECT *eventproject, unsigned int projectid, FMOD_EVENT_MODE mode, FMOD_EVENT **event);
FMOD_RESULT F_API FMOD_EventProject_GetNumEvents     (FMOD_EVENTPROJECT *eventproject, int *numevents);
FMOD_RESULT F_API FMOD_EventProject_SetUserData      (FMOD_EVENTPROJECT *eventproject, void *userdata);
FMOD_RESULT F_API FMOD_EventProject_GetUserData      (FMOD_EVENTPROJECT *eventproject, void **userdata);

FMOD_RESULT F_API FMOD_EventProject_StopAllEvents    (FMOD_EVENTPROJECT *eventproject, FMOD_BOOL immediate);

/*
    'EventGroup' API
*/

FMOD_RESULT F_API FMOD_EventGroup_GetInfo            (FMOD_EVENTGROUP *eventgroup, int *index, char **name);
FMOD_RESULT F_API FMOD_EventGroup_LoadEventData      (FMOD_EVENTGROUP *eventgroup, FMOD_EVENT_RESOURCE resource, FMOD_EVENT_MODE mode);
FMOD_RESULT F_API FMOD_EventGroup_FreeEventData      (FMOD_EVENTGROUP *eventgroup, FMOD_EVENT *event, FMOD_BOOL waituntilready);
FMOD_RESULT F_API FMOD_EventGroup_GetGroup           (FMOD_EVENTGROUP *eventgroup, const char *name, FMOD_BOOL cacheevents, FMOD_EVENTGROUP **group);
FMOD_RESULT F_API FMOD_EventGroup_GetGroupByIndex    (FMOD_EVENTGROUP *eventgroup, int index, FMOD_BOOL cacheevents, FMOD_EVENTGROUP **group);
FMOD_RESULT F_API FMOD_EventGroup_GetParentGroup     (FMOD_EVENTGROUP *eventgroup, FMOD_EVENTGROUP **group);
FMOD_RESULT F_API FMOD_EventGroup_GetParentProject   (FMOD_EVENTGROUP *eventgroup, FMOD_EVENTPROJECT **project);
FMOD_RESULT F_API FMOD_EventGroup_GetNumGroups       (FMOD_EVENTGROUP *eventgroup, int *numgroups);
FMOD_RESULT F_API FMOD_EventGroup_GetEvent           (FMOD_EVENTGROUP *eventgroup, const char *name, FMOD_EVENT_MODE mode, FMOD_EVENT **event);
FMOD_RESULT F_API FMOD_EventGroup_GetEventByIndex    (FMOD_EVENTGROUP *eventgroup, int index, FMOD_EVENT_MODE mode, FMOD_EVENT **event);
FMOD_RESULT F_API FMOD_EventGroup_GetNumEvents       (FMOD_EVENTGROUP *eventgroup, int *numevents);
FMOD_RESULT F_API FMOD_EventGroup_GetProperty        (FMOD_EVENTGROUP *eventgroup, const char *propertyname, void *value);
FMOD_RESULT F_API FMOD_EventGroup_GetPropertyByIndex (FMOD_EVENTGROUP *eventgroup, int propertyindex, void *value);
FMOD_RESULT F_API FMOD_EventGroup_GetNumProperties   (FMOD_EVENTGROUP *eventgroup, int *numproperties);
FMOD_RESULT F_API FMOD_EventGroup_GetState           (FMOD_EVENTGROUP *eventgroup, FMOD_EVENT_STATE *state);
FMOD_RESULT F_API FMOD_EventGroup_SetUserData        (FMOD_EVENTGROUP *eventgroup, void *userdata);
FMOD_RESULT F_API FMOD_EventGroup_GetUserData        (FMOD_EVENTGROUP *eventgroup, void **userdata);

/*
    'EventCategory' API
*/

FMOD_RESULT F_API FMOD_EventCategory_GetInfo         (FMOD_EVENTCATEGORY *eventcategory, int *index, char **name);
FMOD_RESULT F_API FMOD_EventCategory_GetCategory     (FMOD_EVENTCATEGORY *eventcategory, const char *name, FMOD_EVENTCATEGORY **category);
FMOD_RESULT F_API FMOD_EventCategory_GetCategoryByIndex(FMOD_EVENTCATEGORY *eventcategory, int index, FMOD_EVENTCATEGORY **category);
FMOD_RESULT F_API FMOD_EventCategory_GetNumCategories(FMOD_EVENTCATEGORY *eventcategory, int *numcategories);
FMOD_RESULT F_API FMOD_EventCategory_GetEventByIndex (FMOD_EVENTCATEGORY *eventcategory, int index, FMOD_EVENT_MODE mode, FMOD_EVENT **event);
FMOD_RESULT F_API FMOD_EventCategory_GetNumEvents    (FMOD_EVENTCATEGORY *eventcategory, int *numevents);

FMOD_RESULT F_API FMOD_EventCategory_StopAllEvents   (FMOD_EVENTCATEGORY *eventcategory);
FMOD_RESULT F_API FMOD_EventCategory_SetVolume       (FMOD_EVENTCATEGORY *eventcategory, float volume);
FMOD_RESULT F_API FMOD_EventCategory_GetVolume       (FMOD_EVENTCATEGORY *eventcategory, float *volume);
FMOD_RESULT F_API FMOD_EventCategory_SetPitch        (FMOD_EVENTCATEGORY *eventcategory, float pitch, FMOD_EVENT_PITCHUNITS units);
FMOD_RESULT F_API FMOD_EventCategory_GetPitch        (FMOD_EVENTCATEGORY *eventcategory, float *pitch, FMOD_EVENT_PITCHUNITS units);
FMOD_RESULT F_API FMOD_EventCategory_SetPaused       (FMOD_EVENTCATEGORY *eventcategory, FMOD_BOOL paused);
FMOD_RESULT F_API FMOD_EventCategory_GetPaused       (FMOD_EVENTCATEGORY *eventcategory, FMOD_BOOL *paused);
FMOD_RESULT F_API FMOD_EventCategory_SetMute         (FMOD_EVENTCATEGORY *eventcategory, FMOD_BOOL mute);
FMOD_RESULT F_API FMOD_EventCategory_GetMute         (FMOD_EVENTCATEGORY *eventcategory, FMOD_BOOL *mute);
FMOD_RESULT F_API FMOD_EventCategory_GetChannelGroup (FMOD_EVENTCATEGORY *eventcategory, FMOD_CHANNELGROUP **channelgroup);
FMOD_RESULT F_API FMOD_EventCategory_SetUserData     (FMOD_EVENTCATEGORY *eventcategory, void *userdata);
FMOD_RESULT F_API FMOD_EventCategory_GetUserData     (FMOD_EVENTCATEGORY *eventcategory, void **userdata);

/*
    'Event' API
*/

FMOD_RESULT F_API FMOD_Event_Start                   (FMOD_EVENT *event);
FMOD_RESULT F_API FMOD_Event_Stop                    (FMOD_EVENT *event, FMOD_BOOL immediate);

FMOD_RESULT F_API FMOD_Event_GetInfo                 (FMOD_EVENT *event, int *index, char **name, FMOD_EVENT_INFO *info);
FMOD_RESULT F_API FMOD_Event_GetState                (FMOD_EVENT *event, FMOD_EVENT_STATE *state);
FMOD_RESULT F_API FMOD_Event_GetParentGroup          (FMOD_EVENT *event, FMOD_EVENTGROUP **group);
FMOD_RESULT F_API FMOD_Event_GetChannelGroup         (FMOD_EVENT *event, FMOD_CHANNELGROUP **channelgroup);
FMOD_RESULT F_API FMOD_Event_SetCallback             (FMOD_EVENT *event, FMOD_EVENT_CALLBACK callback, void *userdata);

FMOD_RESULT F_API FMOD_Event_GetParameter            (FMOD_EVENT *event, const char *name, FMOD_EVENTPARAMETER **parameter);
FMOD_RESULT F_API FMOD_Event_GetParameterByIndex     (FMOD_EVENT *event, int index, FMOD_EVENTPARAMETER **parameter);
FMOD_RESULT F_API FMOD_Event_GetNumParameters        (FMOD_EVENT *event, int *numparameters);
FMOD_RESULT F_API FMOD_Event_GetProperty             (FMOD_EVENT *event, const char *propertyname, void *value, FMOD_BOOL this_instance);
FMOD_RESULT F_API FMOD_Event_GetPropertyByIndex      (FMOD_EVENT *event, int propertyindex, void *value, FMOD_BOOL this_instance);
FMOD_RESULT F_API FMOD_Event_SetProperty             (FMOD_EVENT *event, const char *propertyname, void *value, FMOD_BOOL this_instance);
FMOD_RESULT F_API FMOD_Event_SetPropertyByIndex      (FMOD_EVENT *event, int propertyindex, void *value, FMOD_BOOL this_instance);
FMOD_RESULT F_API FMOD_Event_GetNumProperties        (FMOD_EVENT *event, int *numproperties);
FMOD_RESULT F_API FMOD_Event_GetCategory             (FMOD_EVENT *event, FMOD_EVENTCATEGORY **category);

FMOD_RESULT F_API FMOD_Event_SetVolume               (FMOD_EVENT *event, float volume);
FMOD_RESULT F_API FMOD_Event_GetVolume               (FMOD_EVENT *event, float *volume);
FMOD_RESULT F_API FMOD_Event_SetPitch                (FMOD_EVENT *event, float pitch, FMOD_EVENT_PITCHUNITS units);
FMOD_RESULT F_API FMOD_Event_GetPitch                (FMOD_EVENT *event, float *pitch, FMOD_EVENT_PITCHUNITS units);
FMOD_RESULT F_API FMOD_Event_SetPaused               (FMOD_EVENT *event, FMOD_BOOL paused);
FMOD_RESULT F_API FMOD_Event_GetPaused               (FMOD_EVENT *event, FMOD_BOOL *paused);
FMOD_RESULT F_API FMOD_Event_SetMute                 (FMOD_EVENT *event, FMOD_BOOL mute);
FMOD_RESULT F_API FMOD_Event_GetMute                 (FMOD_EVENT *event, FMOD_BOOL *mute);
FMOD_RESULT F_API FMOD_Event_Set3DAttributes         (FMOD_EVENT *event, const FMOD_VECTOR *position, const FMOD_VECTOR *velocity, const FMOD_VECTOR *orientation);
FMOD_RESULT F_API FMOD_Event_Get3DAttributes         (FMOD_EVENT *event, FMOD_VECTOR *position, FMOD_VECTOR *velocity, FMOD_VECTOR *orientation);
FMOD_RESULT F_API FMOD_Event_Set3DOcclusion          (FMOD_EVENT *event, float directocclusion, float reverbocclusion);
FMOD_RESULT F_API FMOD_Event_Get3DOcclusion          (FMOD_EVENT *event, float *directocclusion, float *reverbocclusion);
FMOD_RESULT F_API FMOD_Event_SetReverbProperties     (FMOD_EVENT *event, const FMOD_REVERB_CHANNELPROPERTIES *prop);
FMOD_RESULT F_API FMOD_Event_GetReverbProperties     (FMOD_EVENT *event, FMOD_REVERB_CHANNELPROPERTIES *prop);
FMOD_RESULT F_API FMOD_Event_SetUserData             (FMOD_EVENT *event, void *userdata);
FMOD_RESULT F_API FMOD_Event_GetUserData             (FMOD_EVENT *event, void **userdata);

/*
    'EventParameter' API
*/

FMOD_RESULT F_API FMOD_EventParameter_GetInfo        (FMOD_EVENTPARAMETER *eventparameter, int *index, char **name);
FMOD_RESULT F_API FMOD_EventParameter_GetRange       (FMOD_EVENTPARAMETER *eventparameter, float *rangemin, float *rangemax);
FMOD_RESULT F_API FMOD_EventParameter_SetValue       (FMOD_EVENTPARAMETER *eventparameter, float value);
FMOD_RESULT F_API FMOD_EventParameter_GetValue       (FMOD_EVENTPARAMETER *eventparameter, float *value);
FMOD_RESULT F_API FMOD_EventParameter_SetVelocity    (FMOD_EVENTPARAMETER *eventparameter, float value);
FMOD_RESULT F_API FMOD_EventParameter_GetVelocity    (FMOD_EVENTPARAMETER *eventparameter, float *value);
FMOD_RESULT F_API FMOD_EventParameter_SetSeekSpeed   (FMOD_EVENTPARAMETER *eventparameter, float value);
FMOD_RESULT F_API FMOD_EventParameter_GetSeekSpeed   (FMOD_EVENTPARAMETER *eventparameter, float *value);
FMOD_RESULT F_API FMOD_EventParameter_SetUserData    (FMOD_EVENTPARAMETER *eventparameter, void *userdata);
FMOD_RESULT F_API FMOD_EventParameter_GetUserData    (FMOD_EVENTPARAMETER *eventparameter, void **userdata);
FMOD_RESULT F_API FMOD_EventParameter_KeyOff         (FMOD_EVENTPARAMETER *eventparameter);

/*
    'EventReverb' API
*/

FMOD_RESULT F_API FMOD_EventReverb_Release           (FMOD_EVENTREVERB *eventreverb);
FMOD_RESULT F_API FMOD_EventReverb_Set3DAttributes   (FMOD_EVENTREVERB *eventreverb, const FMOD_VECTOR *position, float mindistance, float maxdistance);
FMOD_RESULT F_API FMOD_EventReverb_Get3DAttributes   (FMOD_EVENTREVERB *eventreverb, FMOD_VECTOR *position, float *mindistance, float *maxdistance);
FMOD_RESULT F_API FMOD_EventReverb_SetProperties     (FMOD_EVENTREVERB *eventreverb, const FMOD_REVERB_PROPERTIES *props);
FMOD_RESULT F_API FMOD_EventReverb_GetProperties     (FMOD_EVENTREVERB *eventreverb, FMOD_REVERB_PROPERTIES *props);
FMOD_RESULT F_API FMOD_EventReverb_SetActive         (FMOD_EVENTREVERB *eventreverb, FMOD_BOOL active);
FMOD_RESULT F_API FMOD_EventReverb_GetActive         (FMOD_EVENTREVERB *eventreverb, FMOD_BOOL *active);
FMOD_RESULT F_API FMOD_EventReverb_SetUserData       (FMOD_EVENTREVERB *eventreverb, void *userdata);
FMOD_RESULT F_API FMOD_EventReverb_GetUserData       (FMOD_EVENTREVERB *eventreverb, void **userdata);

/*
    'MusicSystem' API
*/

FMOD_RESULT F_API FMOD_MusicSystem_Reset             (FMOD_MUSICSYSTEM *musicsystem);
FMOD_RESULT F_API FMOD_MusicSystem_SetVolume         (FMOD_MUSICSYSTEM *musicsystem, float volume);
FMOD_RESULT F_API FMOD_MusicSystem_GetVolume         (FMOD_MUSICSYSTEM *musicsystem, float *volume);
FMOD_RESULT F_API FMOD_MusicSystem_SetPaused         (FMOD_MUSICSYSTEM *musicsystem, FMOD_BOOL paused);
FMOD_RESULT F_API FMOD_MusicSystem_GetPaused         (FMOD_MUSICSYSTEM *musicsystem, FMOD_BOOL *paused);
FMOD_RESULT F_API FMOD_MusicSystem_SetMute           (FMOD_MUSICSYSTEM *musicsystem, FMOD_BOOL mute);
FMOD_RESULT F_API FMOD_MusicSystem_GetMute           (FMOD_MUSICSYSTEM *musicsystem, FMOD_BOOL *mute);
FMOD_RESULT F_API FMOD_MusicSystem_GetInfo           (FMOD_MUSICSYSTEM *musicsystem, FMOD_MUSIC_INFO *info);
FMOD_RESULT F_API FMOD_MusicSystem_PromptCue         (FMOD_MUSICSYSTEM *musicsystem, FMOD_MUSIC_CUE_ID id);
FMOD_RESULT F_API FMOD_MusicSystem_PrepareCue        (FMOD_MUSICSYSTEM *musicsystem, FMOD_MUSIC_CUE_ID id, FMOD_MUSICPROMPT **prompt);
FMOD_RESULT F_API FMOD_MusicSystem_GetParameter      (FMOD_MUSICSYSTEM *musicsystem, FMOD_MUSIC_PARAM_ID id, float *parameter);
FMOD_RESULT F_API FMOD_MusicSystem_SetParameter      (FMOD_MUSICSYSTEM *musicsystem, FMOD_MUSIC_PARAM_ID id, float parameter);

FMOD_RESULT F_API FMOD_MusicSystem_LoadSoundData     (FMOD_MUSICSYSTEM *musicsystem, FMOD_EVENT_RESOURCE resource, FMOD_EVENT_MODE mode);
FMOD_RESULT F_API FMOD_MusicSystem_FreeSoundData     (FMOD_MUSICSYSTEM *musicsystem, FMOD_BOOL waituntilready);

FMOD_RESULT F_API FMOD_MusicSystem_SetCallback       (FMOD_MUSICSYSTEM *musicsystem, FMOD_MUSIC_CALLBACK callback, void *userdata);

/*
    'MusicPrompt' API
*/

FMOD_RESULT F_API FMOD_MusicPrompt_Release           (FMOD_MUSICPROMPT *musicprompt);
FMOD_RESULT F_API FMOD_MusicPrompt_Begin             (FMOD_MUSICPROMPT *musicprompt);
FMOD_RESULT F_API FMOD_MusicPrompt_End               (FMOD_MUSICPROMPT *musicprompt);
FMOD_RESULT F_API FMOD_MusicPrompt_IsActive          (FMOD_MUSICPROMPT *musicprompt, FMOD_BOOL *active);
/*$ preserve start $*/

#ifdef __cplusplus
}
#endif


#endif

/*$ preserve end $*/
