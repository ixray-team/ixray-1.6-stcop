#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_LEVEL_SHADERS_H__
#define __XR_LEVEL_SHADERS_H__

#include <string>
#include <vector>
#include "xr_types.h"

namespace xray_re {

class xr_reader;
class xr_writer;

class xr_level_shaders {
public:
			xr_level_shaders(uint32_t xrlc_version, xr_reader& r);
	virtual		~xr_level_shaders();

	void		load(uint32_t xrlc_version, xr_reader& r);
	void		save(xr_writer& w) const;

	std::vector<std::string>&	shaders();
	const std::vector<std::string>&	shaders() const;
	std::vector<std::string>&	textures();
	const std::vector<std::string>&	textures() const;
	std::vector<std::string>&	lightmaps0();
	const std::vector<std::string>&	lightmaps0() const;
	std::vector<std::string>&	lightmaps1();
	const std::vector<std::string>&	lightmaps1() const;

private:
	void		load_shaders_v5(xr_reader& r);
	void		load_textures_v5(xr_reader& r);
	void		load_v8(xr_reader& r);
	void		load_v13(xr_reader& r);

private:
	std::vector<std::string>	m_shaders;
	std::vector<std::string>	m_textures;
	std::vector<std::string>	m_lightmaps0;
	std::vector<std::string>	m_lightmaps1;
};

inline xr_level_shaders::xr_level_shaders(uint32_t xrlc_version, xr_reader& r)
{
	load(xrlc_version, r);
}

inline std::vector<std::string>& xr_level_shaders::shaders() { return m_shaders; }
inline const std::vector<std::string>& xr_level_shaders::shaders() const { return m_shaders; }
inline std::vector<std::string>& xr_level_shaders::textures() { return m_textures; }
inline const std::vector<std::string>& xr_level_shaders::textures() const { return m_textures; }
inline std::vector<std::string>& xr_level_shaders::lightmaps0() { return m_lightmaps0; }
inline const std::vector<std::string>& xr_level_shaders::lightmaps0() const { return m_lightmaps0; }
inline std::vector<std::string>& xr_level_shaders::lightmaps1() { return m_lightmaps1; }
inline const std::vector<std::string>& xr_level_shaders::lightmaps1() const { return m_lightmaps1; }

} // end of namespace xray_re

#endif
