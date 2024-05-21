#include <cstring>
#include "xr_level_version.h"
#include "xr_level_shaders.h"
#include "xr_reader.h"

using namespace xray_re;

xr_level_shaders::~xr_level_shaders() {}

void xr_level_shaders::load_shaders_v5(xr_reader& r)
{
	r.r_seq(r.r_u32(), m_shaders, xr_reader::f_r_sz());
}

void xr_level_shaders::load_textures_v5(xr_reader& r)
{
	size_t n = r.r_u32();
	m_textures.resize(n);
	m_lightmaps0.resize(n);
	for (std::vector<std::string>::iterator it = m_textures.begin(),
			lm0_it = m_lightmaps0.begin(), end = m_textures.end();
			it != end; ++it, ++lm0_it) {
		const char* raw = r.skip_sz();
		if (const char* comma = std::strchr(raw, ',')) {
			lm0_it->assign(raw, comma - raw);
			*it = comma + 1;
		} else {
			*it = raw;
		}
	}
}

void xr_level_shaders::load_v8(xr_reader& r)
{
	size_t n = r.r_u32();
	m_textures.resize(n);
	m_shaders.resize(n);
	m_lightmaps0.resize(n);
	for (std::vector<std::string>::iterator it = m_textures.begin(),
			s_it = m_shaders.begin(), lm0_it = m_lightmaps0.begin(),
			end = m_textures.end(); it != end; ++it, ++s_it, ++lm0_it) {
		const char* raw = r.skip_sz();
		*s_it = raw;
		if (const char* comma = std::strchr(raw, ',')) {
			it->assign(raw, comma - raw);
			*lm0_it = comma + 1;
		} else {
			*it = raw;
		}
	}
}

void xr_level_shaders::load_v13(xr_reader& r)
{
	size_t n = r.r_u32();
	m_textures.resize(n);
	m_shaders.resize(n);
	m_lightmaps0.resize(n);
	m_lightmaps1.resize(n);
	for (std::vector<std::string>::iterator it = m_textures.begin(),
			s_it = m_shaders.begin(), lm0_it = m_lightmaps0.begin(),
			lm1_it = m_lightmaps0.begin(), end = m_textures.end();
			it != end; ++it, ++s_it, ++lm0_it, ++lm1_it) {
		const char* raw = r.skip_sz();
		const char* sep = std::strchr(raw, '/');
		if (sep == 0) {
			xr_assert(raw[0] == '\0');
			continue;
		}
		s_it->assign(raw, sep);
		if ((sep = std::strchr(raw = sep + 1, ','))) {
			it->assign(raw, sep);
			if ((sep = std::strchr(raw = sep + 1, ','))) {
				lm0_it->assign(raw, sep); 
				*lm1_it = sep + 1;
			} else {
				*lm0_it = raw + 1;
			}
		} else {
			*it = raw;
		}
	}
}

void xr_level_shaders::load(uint32_t xrlc_version, xr_reader& r)
{
	xr_reader* s = 0;
	if (xrlc_version == XRLC_VERSION_5) {
		s = r.open_chunk(FSL5_SHADERS);
		xr_assert(s);
		load_shaders_v5(*s);
		r.close_chunk(s);
		s = r.open_chunk(FSL5_TEXTURES);
		xr_assert(s);
		load_textures_v5(*s);
	} else if (xrlc_version >= XRLC_VERSION_8 && xrlc_version <= XRLC_VERSION_11) {
		s = r.open_chunk(FSL8_SHADERS);
		xr_assert(s);
		load_v8(*s);
	} else if (xrlc_version >= XRLC_VERSION_12) {
		s = r.open_chunk(FSL13_SHADERS);
		xr_assert(s);
		load_v13(*s);
	}
	r.close_chunk(s);
}
