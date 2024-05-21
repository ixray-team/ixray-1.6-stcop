#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_SCENE_GLOWS_H__
#define __XR_SCENE_GLOWS_H__

#include "xr_scene_objects.h"

namespace xray_re {

// CGlow

const uint16_t GLOW_VERSION_17 = 0x11;
const uint16_t GLOW_VERSION_18 = 0x12;
const uint16_t GLOW_VERSION = GLOW_VERSION_18;

enum {
	GLOW_CHUNK_VERSION	= 0xc411,
	GLOW_CHUNK_PARAMS	= 0xc413,
	GLOW_CHUNK_SHADER	= 0xc414,
	GLOW_CHUNK_TEXTURE	= 0xc415,
	GLOW_CHUNK_FLAGS	= 0xc416,	// FIXME
};

class xr_glow_object: public xr_custom_object {
public:
			xr_glow_object(xr_scene& scene);
	virtual		~xr_glow_object();
	virtual	void	load(xr_reader& r);
	virtual	void	save(xr_writer& w) const;

	virtual void		save_v12(xr_ini_writer* w) const;

	std::string&	shader();
	std::string&	texture();
	float&		radius();
	uint16_t&	flags();

private:
	std::string	m_shader;	// GLOW_CHUNK_SHADER
	std::string	m_texture;	// GLOW_CHUNK_TEXTURE
	float		m_radius;	// GLOW_CHUNK_PARAMS
	uint16_t	m_flags;	// GLOW_CHUNK_FLAGS
};

inline std::string& xr_glow_object::shader() { return m_shader; }
inline std::string& xr_glow_object::texture() { return m_texture; }
inline float& xr_glow_object::radius() { return m_radius; }
inline uint16_t& xr_glow_object::flags() { return m_flags; }

// ESceneGlowTools
enum {
	GLOWS_CHUNK_COMMON_FLAGS	= 0x1002,
};

enum {
	GLOWS_FLAG_DRAW_CROSS		= 0x40000000,
	GLOWS_FLAG_TEST_VISIBILITY	= 0x80000000,
};

class xr_scene_glows: public xr_scene_objects {
public:
			xr_scene_glows(xr_scene& scene);
	virtual		~xr_scene_glows();
	virtual void	load(xr_reader& r);
	virtual void	save(xr_writer& w) const;

	virtual void		save_v12(xr_ini_writer* w) const;

	uint32_t&	flags();
	uint32_t	flags() const;

private:
	uint32_t	m_flags;	// GLOWS_CHUNK_COMMON_FLAGS
};

inline uint32_t& xr_scene_glows::flags() { return m_flags; }
inline uint32_t xr_scene_glows::flags() const { return m_flags; }

} // end of namespace xray_re

#endif
