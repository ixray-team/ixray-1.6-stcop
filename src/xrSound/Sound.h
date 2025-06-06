#pragma once
#ifdef XRSOUND_EXPORTS
#define XRSOUND_API __declspec(dllexport)
#else
#define XRSOUND_API __declspec(dllimport)
#endif

#include "New/SoundMixer.h"

#define SNDENV_FILENAME				"sEnvironment.xr"
#define OGG_COMMENT_VERSION 		0x0003 

// refs
class	CObject;
class	XRSOUND_API					CSound_params;
class	XRSOUND_API					CSound_stream_interface;
class	XRSOUND_API					CSound_environment;
class	XRSOUND_API					ISoundVoiceChat;

XRSOUND_API extern u32				psSoundModel			;
XRSOUND_API extern float			psSoundVEffects			;
XRSOUND_API extern float			psSoundVFactor			;
XRSOUND_API extern float            psSoundCompression      ;
XRSOUND_API extern float			psSoundVMusic			;
XRSOUND_API extern float			psSoundVRecorder		;
XRSOUND_API extern float			psSoundVShooting		;
XRSOUND_API extern int				psSoundRecorderMode		;
XRSOUND_API extern int				psSoundRecorderDenoise	;
XRSOUND_API extern float			psSoundRolloff			;
XRSOUND_API extern float			psSoundOcclusionScale	;
XRSOUND_API extern Flags32			psSoundFlags			;
XRSOUND_API extern float			psSoundVPlayers			;
XRSOUND_API extern int				psSoundTargets			;
XRSOUND_API extern int				psSoundCacheSizeMB		;
XRSOUND_API extern xr_token*		snd_devices_token		;
XRSOUND_API extern u32				snd_device_id			;

// Flags
enum {
	ss_Hardware			= (1ul<<1ul),	//!< Use hardware mixing only
    ss_EFX				= (1ul<<2ul),	//!< Use eax
    ss_HRTF				= (1ul<<3ul),	//!< Use eax
	ss_forcedword		= u32(-1)
};

enum {
	sq_DEFAULT,
	sq_NOVIRT,
	sq_LIGHT,
	sq_HIGH,
	sq_forcedword = u32(-1)
};
enum {
	sg_Undefined		= 0,
	sg_SourceType		= u32(-1),
	sg_forcedword		= u32(-1),
};
enum {
	sm_Looped			= (1ul<<0ul),	//!< Looped
	sm_2D				= (1ul<<1ul),	//!< 2D mode
	sm_Intro			= (1ul<<2ul),	//!< Only for music and video
	sm_NoFeedback       = (1ul<<3ul),
	sm_forcedword		= u32(-1),
};
enum esound_type
{
	st_Effect,
	st_Music,
	st_Shooting,
	st_forcedword		= u32(-1),
};

class CSound_UserDataVisitor;

/// definition (Sound Params)
class XRSOUND_API CSound_params
{
public:
	Fvector position;
	float	base_volume;
	float	volume;
	float	freq;
	float	min_distance;
	float	max_distance;
	float	max_ai_distance;
};

class CSound_UserData :
	public xr_resource
{
public:
	virtual							~CSound_UserData() {}
	virtual void					accept(CSound_UserDataVisitor*) = 0;
	virtual void					invalidate() = 0;
};
typedef resptr_core<CSound_UserData,resptr_base<CSound_UserData> >	CSound_UserDataPtr;

class ref_sound_data : 
	public xr_resource
{
public:
//	shared_str						nm;
	u32                             slot;
	esound_type						s_type;
	int								g_type;			//!< Sound type, usually for AI
	CObject*						g_object;		//!< Game object that emitts ref_sound
	CSound_UserDataPtr				g_userdata;
	shared_str						fn_attached[2];

	float							fTimeTotal;
	bool                            dont_destroy_slot = false;
public:

	IC CSound_params get_params()
	{
		Fvector* params = XRay::Sound::Mixer::GetParameters(slot);
		if (params) {
			CSound_params out_params = {};
			out_params.position = params[(u32)XRay::Sound::Mixer::ParameterId::Position];
			out_params.base_volume = params[(u32)XRay::Sound::Mixer::ParameterId::VolumePerChannel].x;
			out_params.volume = params[(u32)XRay::Sound::Mixer::ParameterId::VolumePerChannel].y;
			out_params.freq = params[(u32)XRay::Sound::Mixer::ParameterId::Pitch].x;
			out_params.min_distance = params[(u32)XRay::Sound::Mixer::ParameterId::DistanceRange].x;
			out_params.max_distance = params[(u32)XRay::Sound::Mixer::ParameterId::DistanceRange].y;
			out_params.max_ai_distance = params[(u32)XRay::Sound::Mixer::ParameterId::DistanceRange].z;
			return out_params;
		}

		return {};
	}

	IC bool is_2d() { return ((XRay::Sound::Mixer::GetFlags(slot) & (u32)XRay::Sound::Mixer::Flags::Spatial) == 0); }
									ref_sound_data		();
									ref_sound_data		(const char* fName, esound_type sound_type, int game_type);
	virtual							~ref_sound_data		();
	float							get_length_sec		() const {return fTimeTotal;};
};
typedef resptr_core<ref_sound_data,resptr_base<ref_sound_data> >	ref_sound_data_ptr;
/*! \class ref_sound
\brief Sound source + control

The main class respresenting source/emitter interface
This class infact just hides internals and redirect calls to 
specific sub-systems
*/

struct XRSOUND_API ref_sound
{
	u32 _unique_id;
	ref_sound_data_ptr		_p;
	float					TimeToPropagade = 0.0f;	//!< timer for periodic AI sound-event propagation
public:
							ref_sound				();
							~ref_sound				();

	IC u32		            slot				    ()						{return _p?_p->slot:0;}
	IC CObject*				_g_object				()						{ if (_p == nullptr) return nullptr; return _p->g_object;}
	IC int					_g_type					()						{ if (_p == nullptr) return 0; return _p->g_type;}
	IC esound_type			_sound_type				()						{ if (_p == nullptr) return esound_type::st_Effect; return _p->s_type;}
	IC CSound_UserDataPtr _g_userdata()
	{
		if (_p == nullptr)
		{
			return nullptr;
		}
		return _p->g_userdata;
	}
	IC u32                  unique_id               ()                      {return _unique_id;}

	IC void					create					( const char* name, esound_type sound_type,	int	game_type);
	IC void					attach_tail				( const char* name);

	IC void					clone					( const ref_sound& from, esound_type sound_type, int game_type);

	IC void					destroy					( );

	IC bool                 handle()                { if (_p) { return _p->fn_attached[0].size() != 0; } else { return false ;} }
	IC bool                 is_playing()			{ if (_p) { return (XRay::Sound::Mixer::GetState(_p->slot) == XRay::Sound::Mixer::State::Playing); } else return false; }
	IC void					play					( CObject* O, u32 flags=0, float delay=0.f);
	IC void					play_at_pos				( CObject* O, const Fvector &pos ,	u32 flags=0, float delay=0.f);
	IC void					play_no_feedback		( CObject* O, u32 flags=0, float delay=0.f, Fvector* pos=0, float* vol=0, float* freq=0, Fvector2* range=0);
	IC void					set_panning				( double left, double right);

	IC void					stop 					( );
	IC void					stop_deffered			( );
	IC void					set_position			( const Fvector &pos);
	IC void					set_frequency			( float freq);
	IC void					set_range				( float min, float max );
	IC void					set_volume				( float vol );
	IC void					set_priority			( float vol );

	IC CSound_params		get_params				( );
    IC void					set_params				( CSound_params* p );
	IC float				get_length_sec			() const						{return _p?_p->get_length_sec():0.0f;};
};

/// definition (Sound Source)
class XRSOUND_API			CSound_environment
{
public:
};

/// definition (Sound Stream Interface)
class XRSOUND_API			CSound_stream_interface
{
public:
};

/// definition (Sound Stream Interface)
class XRSOUND_API			CSound_stats
{
public:
	u32						_rendered;
	u32						_simulated;
	u32						_cache_hits;
	u32						_cache_misses;
	u32						_events;
};

class XRSOUND_API			CSound_stats_ext
{
public:
	struct SItem{
		shared_str			name;
		CSound_params		params;
		float				volume;
		esound_type			type;
		int					game_type;
		CObject*			game_object;
		struct  {
			u32				_3D			:1;
			u32				_rendered	:1;
		};
	};

	using item_vec = xr_vector<SItem>;
	using item_vec_it = item_vec::iterator;

	item_vec				items;
public:
void						clear							()					{items.clear();}
void						append							(const SItem& itm)	{items.push_back(itm);}
};

/// definition (Sound Callback)
typedef void sound_event (ref_sound_data_ptr S, float range);

/// definition (Sound Manager Interface)
class SoundEnvironment_LIB;
class XRSOUND_API	CSound_manager_interface
{
	virtual void	  				_initialize				(int stage)																			= 0;
	virtual void					_clear					( )																						= 0;

protected:
	friend class 					ref_sound_data;
	virtual void					_create_data			( ref_sound_data& S, const char* fName, esound_type sound_type, int	game_type)				= 0;
	virtual void					_destroy_data			( ref_sound_data& S)																	= 0;
public:
	virtual							~CSound_manager_interface(){}

	static void						_create					(int stage);
	static void						_destroy				( );

	virtual void					_restart				( )																						= 0;

	virtual void					create					( ref_sound& S, const char* fName,				esound_type sound_type, int		game_type)	= 0;
	virtual void					attach_tail				( ref_sound& S, const char* fName)															= 0;
	virtual void					clone					( ref_sound& S, const ref_sound& from,		esound_type sound_type, int		game_type)	= 0;
	virtual void					destroy					( ref_sound& S)																			= 0;
	virtual void					stop_emitters			( )																						= 0;	
	virtual int						pause_emitters			( bool val )																			= 0;

	virtual void					play					( ref_sound& S, CObject* O,						u32 flags=0, float delay=0.f)			= 0;
	virtual void					play_at_pos				( ref_sound& S, CObject* O,	const Fvector &pos,	u32 flags=0, float delay=0.f)			= 0;
	virtual void					play_no_feedback		( ref_sound& S, CObject* O,						u32 flags=0, float delay=0.f, Fvector* pos=0, float* vol=0, float* freq=0, Fvector2* range=0)= 0;

	virtual void					set_master_volume		( float f=1.f )																			= 0;
	virtual void					set_geometry_env		( IReader* I )																			= 0;
	virtual void					set_geometry_som		( IReader* I )																			= 0;
	virtual void					set_geometry_occ		( CDB::MODEL* M )																		= 0;
	virtual void					set_handler				( sound_event* E )																		= 0;
	
	virtual CDB::COLLIDER*			get_geometry_db         ()                                                                                      = 0;
	virtual CDB::MODEL*				get_geometry_env		()																						= 0;
	virtual CDB::MODEL*				get_geometry_som		()																						= 0;
	virtual CDB::MODEL*				get_geometry_occ		()																						= 0;

	virtual void                    debug_draw              ()                                                                                      = 0;
	virtual void					update					( const Fmatrix& m_V, const Fvector& P, const Fvector& D, const Fvector& N)				= 0;
	virtual void					statistic				( CSound_stats*  s0, CSound_stats_ext* s1 )												= 0;
	virtual void					time_factor				(float time_factor)																		= 0;
	virtual float					get_occlusion(Fvector& P, float R, Fvector* occ)                                                                = 0;
	virtual float					get_occlusion_to		( const Fvector& hear_pt, const Fvector& snd_pt, float dispersion=0.2f)					= 0;

	virtual void					objects_relcase			( CObject** objects, int count)															= 0;
	virtual const Fvector&			listener_position		()																						= 0;
	virtual ISoundVoiceChat*		GetSoundVoiceChat		()																						= 0;
	virtual SoundEnvironment_LIB*	get_env_library			()																						= 0;
	virtual void					refresh_env_library		()																						= 0;
	virtual void					set_user_env			(CSound_environment* E)																	= 0;
	virtual void					refresh_sources			()																						= 0;
	virtual void					SwitchAuidoDevice		(const xr_string& Name) = 0;

public:
	xr_delegate<float(u16)> OcclusionMaterialCallback;
};

extern XRSOUND_API CSound_manager_interface*		Sound;

/// ********* Sound ********* (utils, accessors, helpers)
IC ref_sound_data::ref_sound_data				()																{	slot=0;g_type=0;g_object=0;s_type=st_Effect;			}
IC ref_sound_data::ref_sound_data				( const char* fName, esound_type sound_type, int	game_type )			{	::Sound->_create_data			(*this,fName, sound_type, game_type);							}
IC ref_sound_data::~ref_sound_data				()																{	::Sound->_destroy_data			(*this);																}

IC void	ref_sound::create						( const char* name, esound_type sound_type, int	game_type)	{	 		::Sound->create		(*this,name,sound_type,game_type);							}
IC void	ref_sound::attach_tail					( const char* name)											{	 		::Sound->attach_tail(*this,name);							}

IC void	ref_sound::clone						( const ref_sound& from,esound_type sound_type, int	game_type)	{	::Sound->clone		(*this,from,sound_type,game_type);					}
IC void	ref_sound::destroy						( )														{	 		::Sound->destroy	(*this);													}
IC void	ref_sound::play							( CObject* O,						u32 flags, float d)	{	 		::Sound->play		(*this,O,flags,d);											}
IC void	ref_sound::play_at_pos					( CObject* O, const Fvector &pos,	u32 flags, float d)	{	 		::Sound->play_at_pos(*this,O,pos,flags,d);										}
IC void	ref_sound::play_no_feedback				( CObject* O, u32 flags, float d, Fvector* pos, float* vol, float* freq, Fvector2* range){	 ::Sound->play_no_feedback(*this,O,flags,d,pos,vol,freq,range);	}
IC void	ref_sound::set_panning					( double left, double right )									
{	
	if (slot())
	{
		XRay::Sound::Mixer::SetPanning(slot(), left, right);
	}
}

IC void	ref_sound::set_position(const Fvector& pos) 
{ 
	if (slot()) {
		XRay::Sound::Mixer::UpdateParameter(slot(), XRay::Sound::Mixer::ParameterId::Position, pos);
	}
}

IC void	ref_sound::set_frequency(float freq)
{
	if (slot()) {
		XRay::Sound::Mixer::UpdateParameter(slot(), XRay::Sound::Mixer::ParameterId::Pitch, Fvector{ freq , freq , freq });
	}
}

IC void	ref_sound::set_range(float min, float max)
{
	if (slot()) {
		XRay::Sound::Mixer::UpdateParameter(slot(), XRay::Sound::Mixer::ParameterId::Pitch, Fvector{ min, max, 1.f });
	}
}

IC void	ref_sound::set_volume(float vol)
{
	if (slot()) {
		XRay::Sound::Mixer::SetVolume(slot(), vol);
	}
}

IC void	ref_sound::stop()
{
	if (slot()) {
		XRay::Sound::Mixer::Stop(slot(), false);
	}
}

IC void	ref_sound::stop_deffered()
{
	if (slot()) {
		XRay::Sound::Mixer::Stop(slot(), true);
	}
}

IC CSound_params ref_sound::get_params()
{
	Fvector* params = XRay::Sound::Mixer::GetParameters(slot());
	if (slot()) {
		CSound_params out_params = {};
		out_params.position = params[(u32)XRay::Sound::Mixer::ParameterId::Position];
		out_params.volume = params[(u32)XRay::Sound::Mixer::ParameterId::VolumePerChannel].x;
		out_params.freq = params[(u32)XRay::Sound::Mixer::ParameterId::Pitch].x;
		out_params.min_distance = params[(u32)XRay::Sound::Mixer::ParameterId::DistanceRange].x;
		out_params.max_distance = params[(u32)XRay::Sound::Mixer::ParameterId::DistanceRange].y;
		out_params.max_ai_distance = params[(u32)XRay::Sound::Mixer::ParameterId::DistanceRange].z;
		return out_params;
	}

	return {};
}

IC void	ref_sound::set_params(CSound_params* p)
{
	if (slot()) {
		set_position(p->position);
		set_frequency(p->freq);
		set_range(p->min_distance, p->max_distance);
		set_volume(p->volume);
	}
}