#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_LEVEL_GLOWS_H__
#define __XR_LEVEL_GLOWS_H__

#include <vector>
#include "xr_vector3.h"

namespace xray_re {

// xrLC v5, v8
struct fsl_glow_v5 {
	fvector3	position;
	float		radius;
	uint32_t	texture_id;
	uint32_t	shader_id;
};

// xrLC v13+
#pragma pack(push, 1)
struct fsl_glow_v13 {
	fvector3	position;
	float		radius;
	uint16_t	shader_texture_id;
};
#pragma pack(pop)

// in-memory structure
struct glow_data {
	fvector3	position;
	float		radius;
	uint16_t	shader_id;
	uint16_t	texture_id;
};

TYPEDEF_STD_VECTOR_PTR(glow_data)

class xr_reader;
class xr_writer;

class xr_level_glows {
public:
			xr_level_glows(uint32_t xrlc_version, xr_reader& r);
	virtual		~xr_level_glows();

	void		load(uint32_t xrlc_version, xr_reader& r);
	void		save(xr_writer& w) const;

	const glow_data_vec&	glows() const;

private:
	void		load_v5(xr_reader& r);
	void		load_v13(xr_reader& r);

private:
	glow_data_vec	m_glows;
};

inline xr_level_glows::xr_level_glows(uint32_t xrlc_version, xr_reader& r)
{
	load(xrlc_version, r);
}

inline const glow_data_vec& xr_level_glows::glows() const { return m_glows; }

} // end of namespace xray_re

#endif
