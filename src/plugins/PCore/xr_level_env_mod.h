#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_LEVEL_ENV_MOD_H__
#define __XR_LEVEL_ENV_MOD_H__

#include <vector>
#include "xr_vector3.h"

namespace xray_re {

const uint32_t ENV_MOD_VERSION_0 = 0;		// implicit
const uint32_t ENV_MOD_VERSION_23 = 23;		// introduced in CS

enum {
	ENV_MOD_CHUNK_VERSION	= 0x0000,	// CS only
};

struct env_mod_data {
	fvector3	position;
	float		radius;
	float		power;
	float		far_plane;
	fvector3	fog_color;
	float		fog_density;
	fvector3	ambient_color;
	fvector3	sky_color;
	fvector3	hemi_color;
	uint16_t	extra;		// introduced in CS
};

TYPEDEF_STD_VECTOR_PTR(env_mod_data)

class xr_reader;
class xr_writer;

class xr_level_env_mod {
public:
				xr_level_env_mod(xr_reader& r);
				xr_level_env_mod();
	virtual			~xr_level_env_mod();

	bool			load(const char* path, const char* name);
	bool			save(const char* path, const char* name);
	void			load(xr_reader& r);
	void			save(xr_writer& w) const;

	uint32_t		version() const;
	uint32_t&		version();
	const env_mod_data_vec&	env_mods() const;

private:
	uint32_t		m_version;
	env_mod_data_vec	m_env_mods;
};

inline xr_level_env_mod::xr_level_env_mod(xr_reader& r) { load(r); }
inline xr_level_env_mod::xr_level_env_mod(): m_version(ENV_MOD_VERSION_0) {}
inline uint32_t xr_level_env_mod::version() const { return m_version; }
inline uint32_t& xr_level_env_mod::version() { return m_version; }
inline const env_mod_data_vec& xr_level_env_mod::env_mods() const { return m_env_mods; }

} // end of namespace xray_re

#endif
