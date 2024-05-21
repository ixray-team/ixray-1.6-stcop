#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_LEVEL_HOM_H__
#define __XR_LEVEL_HOM_H__

#include "xr_vector3.h"

namespace xray_re {

const uint32_t HOM_VERSION = 0;

enum {
	HOM_CHUNK_VERSION	= 0,
	HOM_CHUNK_POLYGONS	= 1,
};

const uint32_t HOM_FLAG_2SIDED = 0x1;

struct hom_poly {
	union {
		struct {
			fvector3	v0, v1, v2;
		};
		fvector3		v[3];
	};
	uint32_t			flags;
};

class xr_reader;
class xr_writer;

class xr_level_hom {
public:
			xr_level_hom();
			xr_level_hom(xr_reader& r);
	virtual		~xr_level_hom();

	void		load(xr_reader& r);
	void		save(xr_writer& w) const;

	uint32_t	num_polys() const;
	const hom_poly*	polys() const;

private:
	uint32_t	m_num_polys;
	hom_poly*	m_polys;
};

inline xr_level_hom::xr_level_hom(): m_num_polys(0), m_polys(0) {}
inline xr_level_hom::xr_level_hom(xr_reader& r) { load(r); }
inline uint32_t xr_level_hom::num_polys() const { return m_num_polys; }
inline const hom_poly* xr_level_hom::polys() const { return m_polys; }

} // end of namespace xray_re

#endif
