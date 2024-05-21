#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_LEVEL_VISUALS_H__
#define __XR_LEVEL_VISUALS_H__

#include <vector>
#include "xr_types.h"

namespace xray_re {

class xr_ogf;
class xr_level_geom;
class xr_image;
class xr_reader;
class xr_writer;

class xr_level_visuals {
public:
				xr_level_visuals(uint32_t xrlc_version, xr_reader& r,
						const xr_level_geom* geom);
	virtual			~xr_level_visuals();

	void			load(uint32_t xrlc_version, xr_reader& r, const xr_level_geom* geom);

	std::vector<xr_ogf*>&		ogfs();
	const std::vector<xr_ogf*>&	ogfs() const;
	const xr_image*			lods() const;
	const xr_image*			lods_nm() const;

private:
//	void			load_d3d7(xr_reader& r, const xr_level_geom* geom);
//	void			load_d3d9(xr_reader& r, const xr_level_geom* geom);
//	void			load_1865(xr_reader& r, const xr_level_geom* geom);
	void load_ogfs(xr_reader& r, const xr_level_geom* geom);
private:
	std::vector<xr_ogf*>	m_ogfs;
	xr_image*		m_lods;
	xr_image*		m_lods_nm;
};

inline xr_level_visuals::xr_level_visuals(uint32_t xrlc_version, xr_reader& r,
		const xr_level_geom* geom)
{
	load(xrlc_version, r, geom);
}

inline std::vector<xr_ogf*>& xr_level_visuals::ogfs() { return m_ogfs; }
inline const std::vector<xr_ogf*>& xr_level_visuals::ogfs() const { return m_ogfs; }

} // end of namespace xray_re

#endif
