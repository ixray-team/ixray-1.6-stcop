#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_SCENE_SOUND_ENVS_H__
#define __XR_SCENE_SOUND_ENVS_H__

#include "xr_scene_shapes.h"

namespace xray_re {

// ESoundEnvironment
const uint16_t SOUNDENV_VERSION = 0x12;

enum {
	SOUNDENV_CHUNK_VERSION	= 0x1001,
	SOUNDENV_CHUNK_ENV_REFS	= 0x1003,
};

class xr_sound_env_object: public xr_shape_object {
public:
			xr_sound_env_object(xr_scene& scene);
	virtual		~xr_sound_env_object();
	virtual void	load(xr_reader& r);
	virtual void	save(xr_writer& w) const;

	virtual void		save_v12(xr_ini_writer* w) const;

	std::string&	inner();
	std::string&	outer();

private:
	std::string	m_inner;
	std::string	m_outer;
};

inline std::string& xr_sound_env_object::inner() { return m_inner; }
inline std::string& xr_sound_env_object::outer() { return m_outer; }


// ESceneSoundEnvTools
class xr_scene_sound_envs: public xr_scene_objects {
public:
			xr_scene_sound_envs(xr_scene& scene);
	virtual		~xr_scene_sound_envs();
	virtual void	load(xr_reader& r);
	virtual void	save(xr_writer& w) const;

	virtual void		save_v12(xr_ini_writer* w) const;
};

} // end of namespace xray_re

#endif
