#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_SHADERS_XRLC_LIB_H__
#define __XR_SHADERS_XRLC_LIB_H__

#include <string>
#include <vector>
#include "xr_types.h"

namespace xray_re {

class xr_reader;

const size_t SHADER_XRLC_SIZE = 128 + 16;

struct xr_shader_xrlc {
	std::string	name;
	union {
		uint32_t	dummy;
		struct {
			uint32_t	collision:1;
			uint32_t	rendering:1;
			uint32_t	optimize_uv:1;
			uint32_t	light_vertex:1;
			uint32_t	light_cast_shadow:1;
			uint32_t	light_sharp:1;
			uint32_t	unused:26;
		};
	};
	float		vert_translucency;
	float		vert_ambient;
	float		lm_density;
};

TYPEDEF_STD_VECTOR_PTR(xr_shader_xrlc)

class xr_shaders_xrlc_lib {
public:
	virtual		~xr_shaders_xrlc_lib();

	bool		load(const char* path, const char* name);
	void		load(xr_reader& r);

	const xr_shader_xrlc_vec&	shaders() const;

private:
	xr_shader_xrlc_vec	m_shaders;
};

inline const xr_shader_xrlc_vec& xr_shaders_xrlc_lib::shaders() const { return m_shaders; }

} // end of namespace xray_re

#endif
