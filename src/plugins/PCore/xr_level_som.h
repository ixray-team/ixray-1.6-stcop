#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_LEVEL_SOM_H__
#define __XR_LEVEL_SOM_H__

#include "xr_vector3.h"

namespace xray_re {

const uint32_t SOM_VERSION = 0;

enum {
	SOM_CHUNK_VERSION	= 0,
	SOM_CHUNK_POLYGONS	= 1,
};

struct som_poly {
	union {
		struct {
			fvector3	v0, v1, v2;
		};
		fvector3		v[3];
	};
	uint32_t			b2sided;
	float				occ;
};

class xr_reader;
class xr_writer;

class xr_level_som {
public:
			xr_level_som();
			xr_level_som(xr_reader& r);
	virtual		~xr_level_som();

	void		load(xr_reader& r);
	void		save(xr_writer& w) const;

	uint32_t	num_polys() const;
	const som_poly*	polys() const;

private:
	uint32_t	m_num_polys;
	som_poly*	m_polys;
};

inline xr_level_som::xr_level_som(): m_num_polys(0), m_polys(0) {}
inline xr_level_som::xr_level_som(xr_reader& r) { load(r); }
inline uint32_t xr_level_som::num_polys() const { return m_num_polys; }
inline const som_poly* xr_level_som::polys() const { return m_polys; }

} // end of namespace xray_re

#endif
