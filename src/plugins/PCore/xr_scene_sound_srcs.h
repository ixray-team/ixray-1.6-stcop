#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_SCENE_SOUND_SRCS_H__
#define __XR_SCENE_SOUND_SRCS_H__

#include "xr_vector2.h"
#include "xr_vector3.h"
#include "xr_scene_objects.h"

namespace xray_re {

// ESoundSource
const uint16_t SOUNDSRC_VERSION = 0x14;

enum {
	SOUNDSRC_TYPE_0	= 0,
};

enum {
	SOUNDSRC_CHUNK_VERSION		= 0x1001,
	SOUNDSRC_CHUNK_TYPE		= 0x1002,
	SOUNDSRC_CHUNK_SOURCE_NAME	= 0x1003,
	SOUNDSRC_CHUNK_PARAMS_0		= 0x1004,
	SOUNDSRC_CHUNK_FLAGS		= 0x1005,	// really?
	SOUNDSRC_CHUNK_PARAMS_1		= 0x1006,
	SOUNDSRC_CHUNK_PARAMS_2		= 0x1007,
	SOUNDSRC_CHUNK_TIME		= 0x1008,
};

class xr_sound_src_object: public xr_custom_object {
public:
			xr_sound_src_object(xr_scene& scene);
	virtual		~xr_sound_src_object();
	virtual void	load(xr_reader& r);
	virtual void	save(xr_writer& w) const;

	virtual void		save_v12(xr_ini_writer* w) const;

	uint32_t&	flags();
	uint8_t&	type();
	std::string&	source_name();
	fvector3&	sound_pos();
	float&		volume();
	float&		frequency();
	fvector2&	pause_time();
	fvector2&	active_time();
	fvector2&	play_time();

private:
	uint32_t	m_flags;
	uint8_t		m_type;

	std::string	m_source_name;

	fvector3	m_sound_pos;	// not sure. it might duplicate the xr_custom_object::m_co_position.
	float		m_volume;
	float		m_frequency;
	float		m_min_dist;
	float		m_max_dist;
	float		m_max_ai_dist;

	fvector2	m_pause_time;
	fvector2	m_active_time;
	fvector2	m_play_time;
};

inline uint32_t& xr_sound_src_object::flags() { return m_flags; }
inline uint8_t& xr_sound_src_object::type() { return m_type; }
inline std::string& xr_sound_src_object::source_name() { return m_source_name; }
inline fvector3& xr_sound_src_object::sound_pos() { return m_sound_pos; }
inline float& xr_sound_src_object::volume() { return m_volume; }
inline float& xr_sound_src_object::frequency() { return m_frequency; }
inline fvector2& xr_sound_src_object::pause_time() { return m_pause_time; }
inline fvector2& xr_sound_src_object::active_time() { return m_active_time; }
inline fvector2& xr_sound_src_object::play_time() { return m_play_time; }


// ESceneSoundSrcTools
class xr_scene_sound_srcs: public xr_scene_objects {
public:
			xr_scene_sound_srcs(xr_scene& scene);
	virtual		~xr_scene_sound_srcs();
	virtual void	load(xr_reader& r);
	virtual void	save(xr_writer& w) const;

	virtual void		save_v12(xr_ini_writer* w) const;
};

} // end of namespace xray_re

#endif
