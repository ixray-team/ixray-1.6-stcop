#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_SCENE_WALLMARKS_H__
#define __XR_SCENE_WALLMARKS_H__

#include "xr_scene_part.h"
#include "xr_wallmarks.h"
#include "xr_aabb.h"
#include "xr_shape.h"

namespace xray_re {

// ESceneWallmarkTools
const uint16_t WALLMARK_VERSION = 0x3;

enum {
	WM_FLAG_DRAW_WALLMARKS	= 0x1,
	WM_FLAG_ALIGN_BY_WORLD	= 0x2,		// vs. align by camera
};

enum {
	WM_CHUNK_VERSION	= 0x0001,
	WM_CHUNK_FLAGS		= 0x0002,
	WM_CHUNK_PARAMS		= 0x0003,
	WM_CHUNK_WALLMARKS_0	= 0x0004,	// ???
	WM_CHUNK_WALLMARKS_1	= 0x0005,
};

struct wm_object {
	bool		selected;
	fbox		bbox;
	fsphere		bsphere;
	float		width;
	float		height;
	float		rotate;
	wm_vertex_vec	vertices;
};

TYPEDEF_STD_VECTOR(wm_object)

struct wm_slot_le {
	std::string	shader;
	std::string	texture;
	wm_object_vec	wallmarks;
};

TYPEDEF_STD_VECTOR_PTR(wm_slot_le)

class xr_scene_wallmarks: public xr_scene_part {
public:
			xr_scene_wallmarks(xr_scene& scene);
	virtual		~xr_scene_wallmarks();
	virtual void	load(xr_reader& r);
	virtual void	save(xr_writer& w) const;

	virtual void		save_v12(xr_ini_writer* w) const;

	uint32_t&	flags();
	float&		height();
	float&		width();
	std::string&	shader();
	std::string&	texture();
	wm_slot_le_vec&	slots();

private:
	uint32_t	m_flags;	// WM_CHUNK_FLAGS

	float		m_width;	// WM_CHUNK_PARAMS
	float		m_height;
	float		m_rotate;
	std::string	m_shader;
	std::string	m_texture;

	wm_slot_le_vec	m_slots;	// WM_CHUNK_WALLMARKS_1
};

inline uint32_t& xr_scene_wallmarks::flags() { return m_flags; }
inline float& xr_scene_wallmarks::height() { return m_height; }
inline float& xr_scene_wallmarks::width() { return m_width; }
inline std::string& xr_scene_wallmarks::shader() { return m_shader; }
inline std::string& xr_scene_wallmarks::texture() { return m_texture; }
inline wm_slot_le_vec& xr_scene_wallmarks::slots() { return m_slots; }

} // end of namespace xray_re

#endif
