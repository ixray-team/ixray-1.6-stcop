#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_SCENE_DETAILS_H__
#define __XR_SCENE_DETAILS_H__

#include <string>
#include <vector>
#include "xr_scene_part.h"
#include "xr_aabb.h"
#include "xr_color.h"
#include "xr_details.h"

namespace xray_re {

// EDetail
const uint32_t DETOBJ_VERSION = 0x1;

enum {
	DETOBJ_CHUNK_VERSION		= 0x1000,
	DETOBJ_CHUNK_REFERENCE		= 0x0101,
	DETOBJ_CHUNK_SCALE_LIMITS	= 0x0102,
	DETOBJ_CHUNK_DENSITY		= 0x0103,
	DETOBJ_CHUNK_FLAGS		= 0x0104,
};

// EDetailManager
enum {
	DETMGR_FLAG_DRAW_OBJECTS		= 0x10000000,
	DETMGR_FLAG_DRAW_SLOT_BOXES		= 0x20000000,
	DETMGR_FLAG_BASE_TEXTURE_BLENDED	= 0x40000000,
	DETMGR_FLAG_DRAW_BASE_TEXTURE		= 0x80000000,
};

const uint32_t DETMGR_VERSION = 3;

enum {
	DETMGR_CHUNK_HEADER		= 0x0000,
	DETMGR_CHUNK_OBJECTS		= 0x0001,
	DETMGR_CHUNK_SLOTS		= 0x0002,
	DETMGR_CHUNK_VERSION		= 0x1000,
	DETMGR_CHUNK_BBOX		= 0x1001,
	DETMGR_CHUNK_TEXTURE		= 0x1002,
	DETMGR_CHUNK_COLOR_INDEX	= 0x1003,
	DETMGR_CHUNK_SNAP_OBJECTS	= 0x1004,
	DETMGR_CHUNK_DENSITY		= 0x1005,
	DETMGR_CHUNK_FLAGS		= 0x1006,
};

struct detail_object {
			detail_object();

	std::string	reference;	// DETOBJ_CHUNK_REFERENCE
	float		min_scale;	// DETOBJ_CHUNK_SCALE_LIMITS
	float		max_scale;
	float		density;	// DETOBJ_CHUNK_DENSITY
	uint32_t	flags;		// DETOBJ_CHUNK_FLAGS
};

TYPEDEF_STD_VECTOR_PTR(detail_object);

struct color_index {
	rgba32				color;
	std::vector<std::string>	references;
};

TYPEDEF_STD_VECTOR_PTR(color_index);

class xr_scene_details: public xr_scene_part {
public:
				xr_scene_details(xr_scene& scene);
	virtual			~xr_scene_details();
	virtual void		load(xr_reader& r);
	virtual void		save(xr_writer& w) const;

	virtual void		save_v12(xr_ini_writer* w) const;

	uint32_t&		flags();
	details_header&		header();
	uint32_t&		num_slots();
	detail_slot_v3*&	slots();
	detail_object_vec&	objects();
	color_index_vec&	indices();
	fbox&			bbox();
	float&			density();
	std::string&		texture();
	std::vector<std::string>&	snap_objects();

private:
	uint32_t		m_flags;	// DETMGR_CHUNK_FLAGS
	details_header		m_header;	// DETMGR_CHUNK_HEADER
	uint32_t		m_num_slots;	// DETMGR_CHUNK_SLOTS
	detail_slot_v3*		m_slots;
	detail_object_vec	m_objects;	// DETMGR_CHUNK_OBJECTS
	color_index_vec		m_indices;	// DETMGR_CHUNK_COLOR_INDEX
	fbox			m_bbox;		// DETMGR_CHUNK_BBOX
	float			m_density;	// DETMGR_CHUNK_DENSITY
	std::string		m_texture;	// DETMGR_CHUNK_TEXTURE
	std::vector<std::string>m_snap_objects;	// DETMGR_CHUNK_SNAP_OBJECTS
};

inline uint32_t& xr_scene_details::flags() { return m_flags; }
inline details_header& xr_scene_details::header() { return m_header; }
inline uint32_t& xr_scene_details::num_slots() { return m_num_slots; }
inline detail_slot_v3*& xr_scene_details::slots() { return m_slots; }
inline detail_object_vec& xr_scene_details::objects() { return m_objects; }
inline color_index_vec& xr_scene_details::indices() { return m_indices; }
inline fbox& xr_scene_details::bbox() { return m_bbox; }
inline float& xr_scene_details::density() { return m_density; }
inline std::string& xr_scene_details::texture() { return m_texture; }
inline std::vector<std::string>& xr_scene_details::snap_objects() { return m_snap_objects; }

} // end of namespace xray_re

#endif
