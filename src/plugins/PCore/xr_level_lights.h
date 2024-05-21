#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_LEVEL_LIGHTS_H__
#define __XR_LEVEL_LIGHTS_H__

#include <vector>
#include "xr_d3d_light.h"

namespace xray_re {

// in-game format definitions

// xrLC v5 (1098)
struct fsl_light_v5 {
	d3d_light	d3d_params;
	uint32_t	unk[5];
};

// xrLC v8 (1114, 1154)
const size_t B_LIGHT_V8_NAME_LEN = 64;

struct fsl_light_v8 {
	d3d_light	d3d_params;
	uint32_t	unk1;
	uint32_t	unk2;
	char		name[B_LIGHT_V8_NAME_LEN];
};

// xrLC v13, v14 (2215+)
struct fsl_light_v13 {
	uint32_t	controller_id;	// 1 for directional, 2 for point in v13
	d3d_light	d3d_params;
};

struct light_data {
	uint32_t	controller_id;
	d3d_light	d3d_params;
};

TYPEDEF_STD_VECTOR_PTR(light_data)

class xr_reader;
class xr_writer;

class xr_level_lights {
public:
			xr_level_lights(uint32_t xrlc_version, xr_reader& r);
	virtual		~xr_level_lights();

	void		load(uint32_t xrlc_version, xr_reader& r);
	void		save(xr_writer& w) const;

	const light_data_vec&	lights() const;

private:
	void		load_v5(xr_reader& r);
	void		load_v8(xr_reader& r);
	void		load_v13(xr_reader& r);

private:
	light_data_vec	m_lights;
};

inline xr_level_lights::xr_level_lights(uint32_t xrlc_version, xr_reader& r)
{
	load(xrlc_version, r);
}

inline const light_data_vec& xr_level_lights::lights() const { return m_lights; }

} // end of namespace xray_re

#endif
