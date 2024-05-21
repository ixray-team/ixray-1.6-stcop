#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_LEVEL_PS_STATIC_H__
#define __XR_LEVEL_PS_STATIC_H__

#include <string>
#include <vector>
#include "xr_matrix.h"

namespace xray_re {

const uint32_t PS_VERSION_0 = 0;	// implicit
const uint32_t PS_VERSION_1 = 1;	// introduced in Clear Sky

enum {
	PS_CHUNK_VERSION	= 0x0000,	// CS only
};

struct particle_data {
	uint16_t	extra;
	std::string	reference;
	fmatrix		xform;
};

TYPEDEF_STD_VECTOR_PTR(particle_data);

class xr_reader;
class xr_writer;

class xr_level_ps_static {
public:
				xr_level_ps_static(xr_reader& r);
				xr_level_ps_static();
	virtual			~xr_level_ps_static();

	bool			load(const char* path, const char* name);
	bool			save(const char* path, const char* name);
	void			load(xr_reader& r);
	void			save(xr_writer& w) const;

	uint32_t		version() const;
	uint32_t&		version();
	const particle_data_vec&particles() const;

private:
	uint32_t		m_version;
	particle_data_vec	m_particles;
};

inline xr_level_ps_static::xr_level_ps_static(xr_reader& r) { load(r); }
inline xr_level_ps_static::xr_level_ps_static(): m_version(PS_VERSION_0) {}
inline uint32_t xr_level_ps_static::version() const { return m_version; }
inline uint32_t& xr_level_ps_static::version() { return m_version; }
inline const particle_data_vec& xr_level_ps_static::particles() const { return m_particles; }

} // end of namespace xray_re

#endif
