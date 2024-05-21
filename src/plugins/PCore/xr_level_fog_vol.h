#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_LEVEL_FOG_VOL_H__
#define __XR_LEVEL_FOG_VOL_H__

#include <string>
#include <vector>
#include "xr_matrix.h"

namespace xray_re {

const uint16_t FOG_VOL_VERSION_2 = 2;
const uint16_t FOG_VOL_VERSION_3 = 3;

struct fog_vol_data {
			fog_vol_data();
			~fog_vol_data();
	std::string	ltx;
	fmatrix		xform;
	uint32_t	num_particles;
	fmatrix*	particles;
};

inline fog_vol_data::fog_vol_data(): particles(0) {}
inline fog_vol_data::~fog_vol_data() { delete[] particles; }

TYPEDEF_STD_VECTOR_PTR(fog_vol_data)

class xr_reader;
class xr_writer;

class xr_level_fog_vol {
public:
				xr_level_fog_vol(xr_reader& r);
	virtual			~xr_level_fog_vol();

	void			load(xr_reader& r);
	void			save(xr_writer& w) const;

	uint16_t		version() const;
	const fog_vol_data_vec&	fog_vols() const;

private:
	uint16_t		m_version;
	fog_vol_data_vec	m_fog_vols;
};

inline xr_level_fog_vol::xr_level_fog_vol(xr_reader& r) { load(r); }
inline uint16_t xr_level_fog_vol::version() const { return m_version; }
inline const fog_vol_data_vec& xr_level_fog_vol::fog_vols() const { return m_fog_vols; }

} // end of namespace xray_re

#endif
