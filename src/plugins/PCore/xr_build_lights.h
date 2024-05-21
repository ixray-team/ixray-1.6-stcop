#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_BUILD_LIGHTS_H__
#define __XR_BUILD_LIGHTS_H__

#include <vector>
#include "xr_vector3.h"

namespace xray_re {

enum {
	BL_CHUNK_RGB	= 0,
	BL_CHUNK_HEMI	= 1,
	BL_CHUNK_SUN	= 2,
};

const uint32_t R_LIGHT_SIZE_SOC = 0x64;
const uint32_t R_LIGHT_SIZE_CS = 0x68;

struct r_light {
	uint16_t	type;
	uint16_t	level;
	fvector3	diffuse;
	fvector3	position;
	fvector3	direction;
	float		range;
	float		range2;
	float		attenuation0;
	float		attenuation1;
	float		attenuation2;
	float		energy;
	fvector3	tri[3];
	uint32_t	unknown;	// clear sky
};

TYPEDEF_STD_VECTOR_PTR(r_light)

class xr_reader;
class xr_writer;

class xr_build_lights {
public:
				xr_build_lights(xr_reader& r);
	virtual			~xr_build_lights();

	void			load(xr_reader& r);
	void			save(xr_writer& w) const;

	const r_light_vec&	rgb() const;
	const r_light_vec&	hemi() const;
	const r_light_vec&	sun() const;

private:
	void			read_lights(xr_reader& r, uint32_t id, r_light_vec& lights, bool cs);

private:
	r_light_vec		m_rgb;
	r_light_vec		m_hemi;
	r_light_vec		m_sun;
};

inline xr_build_lights::xr_build_lights(xr_reader& r) { load(r); }
inline const r_light_vec& xr_build_lights::rgb() const { return m_rgb; }
inline const r_light_vec& xr_build_lights::hemi() const { return m_hemi; }
inline const r_light_vec& xr_build_lights::sun() const { return m_sun; }

} // end of namespace xray_re

#endif
