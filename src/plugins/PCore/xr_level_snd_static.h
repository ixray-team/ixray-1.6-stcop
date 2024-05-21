#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_LEVEL_SND_STATIC_H__
#define __XR_LEVEL_SND_STATIC_H__

#include <string>
#include <vector>
#include "xr_vector2.h"
#include "xr_vector3.h"

namespace xray_re {

struct sound_src_data {
	std::string	name;
	fvector3	position;
	float		volume;
	float		freq;
	i32vector2	active_time;
	i32vector2	play_time;
	i32vector2	pause_time;
};

TYPEDEF_STD_VECTOR_PTR(sound_src_data)

class xr_reader;
class xr_writer;
class xr_ini_file;

class xr_level_snd_static {
public:
			xr_level_snd_static();
			xr_level_snd_static(xr_reader& r);
			xr_level_snd_static(const xr_ini_file& ini);
	virtual		~xr_level_snd_static();

	void		load(const xr_ini_file& ini);
	void		load(xr_reader& r);
	void		save(xr_writer& w) const;

	const sound_src_data_vec&	sound_srcs() const;

private:
	sound_src_data_vec	m_sound_srcs;
};

inline xr_level_snd_static::xr_level_snd_static() {}
inline xr_level_snd_static::xr_level_snd_static(xr_reader& r) { load(r); }
inline xr_level_snd_static::xr_level_snd_static(const xr_ini_file& ini) { load(ini); }
inline const sound_src_data_vec& xr_level_snd_static::sound_srcs() const { return m_sound_srcs; }

} // end of namespace xray_re

#endif
