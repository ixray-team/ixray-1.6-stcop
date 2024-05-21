#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_SCENE_PARTICLES_H__
#define __XR_SCENE_PARTICLES_H__

#include "xr_scene_objects.h"

namespace xray_re {

// EParticlesObject
const uint16_t CPSOBJECT_VERSION = 0x11;
const uint16_t CPSOBJECT_VERSION_V12 = 0x13;

enum {
	CPSOBJECT_CHUNK_VERSION		= 0x0001,
	CPSOBJECT_CHUNK_REFERENCE	= 0x0002,
};

class xr_particle_object: public xr_custom_object {
public:
			xr_particle_object(xr_scene& scene);
	virtual		~xr_particle_object();
	virtual void	load(xr_reader& r);
	virtual void	save(xr_writer& w) const;

	virtual void		save_v12(xr_ini_writer* w) const;

	std::string&	reference();

private:
	std::string	m_reference;
};

inline std::string& xr_particle_object::reference() { return m_reference; }

// EScenePSTools
class xr_scene_particles: public xr_scene_objects {
public:
			xr_scene_particles(xr_scene& scene);
	virtual		~xr_scene_particles();
	virtual void	load(xr_reader& r);
	virtual void	save(xr_writer& w) const;

	virtual void		save_v12(xr_ini_writer* w) const;
};

} // end of namespace xray_re

#endif
