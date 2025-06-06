#pragma once
#include "SoundRender.h"
#include "SoundRender_Environment.h"
class SoundVoiceChat;

class CSoundRender_Core	: 
	public CSound_manager_interface
{
protected:
	Fvector								listenerPos;
	bool								bListenerMoved;
	float								master_volume;
	CSoundRender_Environment			e_current;
	CSoundRender_Environment			e_target;

public:
	typedef	std::pair<ref_sound_data_ptr,float>	event;                                               
	xr_vector<event>					s_events;

	bool								bPresent;
	bool								bUserEnvironment;
	bool								bReady;

	SoundVoiceChat* pSoundVoiceChat = nullptr;

	bool m_is_supported; // Boolean variable to indicate presence of EFX Extension

	sound_event*						Handler;

	xr_string_map<xr_string, u32>		SoundDevices;

protected:
	// Collider
	CDB::MODEL*							geom_SOM;
	CDB::MODEL*							geom_MODEL;
	CDB::MODEL*							geom_ENV;

	// Containers
	SoundEnvironment_LIB*				s_environment;
	CSoundRender_Environment			s_user_environment;

	int									m_iPauseCounter;

protected:
	virtual void						_create_data			( ref_sound_data& S, const char* fName,	esound_type sound_type, int game_type); 
	virtual void						_destroy_data			( ref_sound_data& S);
			void						GenerateDevicesToken	();
	virtual void						SwitchAuidoDevice		(const xr_string& Name) override;
public:
										CSoundRender_Core		();
	virtual								~CSoundRender_Core		();

	// General
	virtual void  						_initialize				(int stage);
	virtual void						_clear					( );
	virtual void						_restart				( );

	// Sound interface
	virtual void						create					( ref_sound& S, const char* fName,			esound_type sound_type, int	game_type);
	virtual void						attach_tail				( ref_sound& S, const char* fName);

	virtual void						clone					( ref_sound& S, const ref_sound& from,	esound_type sound_type, int	game_type);
	virtual void						destroy					( ref_sound& S);
	virtual void						stop_emitters			( );
	virtual int							pause_emitters			( bool val );

			u32							GetMixedFlags			(u32 flags, ref_sound& S);
	virtual void						play					( ref_sound& S, CObject* O,								u32 flags=0, float delay=0.f);
	virtual void						play_at_pos				( ref_sound& S, CObject* O,		const Fvector &pos,		u32 flags=0, float delay=0.f);
	virtual void						play_no_feedback		( ref_sound& S, CObject* O,	u32 flags=0, float delay=0.f, Fvector* pos=0, float* vol=0, float* freq=0, Fvector2* range=0);
	virtual void						set_master_volume		( float			f );
	virtual void						set_geometry_env		( IReader*		I );
	virtual void						set_geometry_som		( IReader*		I );
	virtual void						set_geometry_occ		( CDB::MODEL*	M );
	virtual void						set_handler				( sound_event*	E );
	
	virtual CDB::COLLIDER*				get_geometry_db         ( );
	virtual CDB::MODEL*					get_geometry_env        ( );
	virtual CDB::MODEL*					get_geometry_som        ( );
	virtual CDB::MODEL*					get_geometry_occ        ( );

	virtual void                        debug_draw              ();
	virtual void						update					( const Fmatrix& m_V, const Fvector& P, const Fvector& D, const Fvector& N );
	virtual void						update_events			( );
	virtual void						statistic				( CSound_stats*  dest, CSound_stats_ext*  ext );

	virtual void						time_factor				(float time_factor);
	virtual float						get_occlusion			(Fvector& P, float R, Fvector* occ);
	virtual float						get_occlusion_to		( const Fvector& hear_pt, const Fvector& snd_pt, float dispersion=0.2f);

	virtual SoundEnvironment_LIB*		get_env_library			()																{ return s_environment; }
	virtual void						refresh_env_library		();
	virtual void						set_user_env			(CSound_environment* E);
	virtual void						refresh_sources			();

	virtual void						objects_relcase			( CObject** objects, int count);

	CSoundRender_Environment*			get_environment			( const Fvector& P );
	virtual const Fvector&				listener_position		();

	void								env_load				();
	void								env_unload				();
	void								env_apply				();
	virtual ISoundVoiceChat*			GetSoundVoiceChat		() override;
};
extern CSoundRender_Core* SoundRender;