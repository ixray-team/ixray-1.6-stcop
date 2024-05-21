#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_D3D_LIGHT_H__
#define __XR_D3D_LIGHT_H__

#include "xr_vector3.h"
#include "xr_color.h"

namespace xray_re {

// common definitions (D3D actually)
enum {
	D3D_LIGHT_POINT		= 1,
	D3D_LIGHT_SPOT		= 2,
	D3D_LIGHT_DIRECTIONAL	= 3,
};

enum {
	LT_DIRECT	= 0,
	LT_POINT	= 1,
	LT_SPOT		= 2,
	LT_OMNIPART	= 3,
	LT_REFLECTED	= 4,
};

class xr_reader;
class xr_writer;

struct d3d_light {
	void		load(xr_reader& r);
	void		save(xr_writer& w) const;

	uint32_t	type;
	fcolor		diffuse;
	fcolor		specular;
	fcolor		ambient;
	fvector3	position;
	fvector3	direction;
	float		range;
	float		falloff;
	float		attenuation0;
	float		attenuation1;
	float		attenuation2;
	float		theta;
	float		phi;
};

} // end of namespace xray_re

#endif
