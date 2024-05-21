#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_LEVEL_GEOM_H__
#define __XR_LEVEL_GEOM_H__

#include "xr_geom_buf.h"

namespace xray_re {

class xr_level_geom {
public:
				xr_level_geom(xr_reader& r);
				xr_level_geom(uint32_t xrlc_version, xr_reader& r);
	virtual			~xr_level_geom();

	void			load(uint32_t xrlc_version, xr_reader& r);
	void			save(xr_writer& w) const;

	const xr_vbuf_vec&	vbufs() const;
	const xr_ibuf_vec&	ibufs() const;
	const xr_swibuf_vec&	swibufs() const;

private:
	void			load_d3d7(xr_reader& r);
	void			load_d3d9(xr_reader& r);
	void			load_1865(xr_reader& r);
	void			load_v9(xr_reader& r);
private:
	xr_vbuf_vec		m_vbufs;
	xr_ibuf_vec		m_ibufs;
	xr_swibuf_vec		m_swibufs;
};

inline xr_level_geom::xr_level_geom(xr_reader& r) { load(UINT32_MAX, r); }

inline xr_level_geom::xr_level_geom(uint32_t xrlc_version, xr_reader& r)
{
	load(xrlc_version, r);
}

inline const xr_vbuf_vec& xr_level_geom::vbufs() const { return m_vbufs; }
inline const xr_ibuf_vec& xr_level_geom::ibufs() const { return m_ibufs; }
inline const xr_swibuf_vec& xr_level_geom::swibufs() const { return m_swibufs; }

} // end of namespace xray_re

#endif
