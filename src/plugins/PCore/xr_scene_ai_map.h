#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_SCENE_AI_MAP_H__
#define __XR_SCENE_AI_MAP_H__

#include "xr_scene_part.h"
#include "xr_aabb.h"

namespace xray_re {

// ESceneAIMapTools

// flags chunk
enum {
	AIMAP_FLAG_DRAW_NODES		= 0x2,
	AIMAP_FLAG_SLOW_CALCULATE_MODE	= 0x4,
};

const uint16_t AIMAP_VERSION = 2;

enum {
	AIMAP_CHUNK_VERSION		= 0x0001,
	AIMAP_CHUNK_FLAGS		= 0x0002,
	AIMAP_CHUNK_BOX			= 0x0003,
	AIMAP_CHUNK_PARAMS		= 0x0004,
	AIMAP_CHUNK_NODES		= 0x0006,
	AIMAP_CHUNK_SNAP_OBJECTS	= 0x0007,
	AIMAP_CHUNK_PREFS		= 0x0008,
	AIMAP_CHUNK_SMOOTH_HEIGHT	= 0x0009,
};

struct ai_node_le {
	uint32_t	link[4];
	uint16_t	plane;
	int16_t		packed_x;
	uint16_t	packed_y;
	int16_t		packed_z;
	bool		selected;
};

class xr_scene_ai_map: public xr_scene_part {
public:
				xr_scene_ai_map(xr_scene& scene);
	virtual			~xr_scene_ai_map();
	virtual void		load(xr_reader& r);
	virtual void		save(xr_writer& w) const;

	virtual void		save_v12(xr_ini_writer* w) const;

	uint32_t&		flags();
	fbox&			bbox();
	float&			cell_size();
	float&			factor_y();
	uint32_t&		num_nodes();
	ai_node_le*&		nodes();

private:
	uint32_t		m_flags;		// AIMAP_CHUNK_FLAGS
	fbox			m_bbox;			// AIMAP_CHUNK_BOX

	float			m_cell_size;		// AIMAP_CHUNK_PARAMS;
	float			m_factor_y;
	float			m_can_up;
	float			m_can_down;

	uint32_t		m_num_nodes;
	ai_node_le*		m_nodes;		// AIMAP_CHUNK_NODES

	float			m_visible_radius;	// AIMAP_CHUNK_PREFS
	unsigned		m_brush_size;

	float			m_smooth_height;	// AIMAP_CHUNK_SMOOTH_HEIGHT
	std::vector<std::string>m_snap_objects;		// AIMAP_CHUNK_SNAP_OBJECTS
};

inline uint32_t& xr_scene_ai_map::flags() { return m_flags; }
inline fbox& xr_scene_ai_map::bbox() { return m_bbox; }
inline float& xr_scene_ai_map::cell_size() { return m_cell_size; }
inline float& xr_scene_ai_map::factor_y() { return m_factor_y; }
inline uint32_t& xr_scene_ai_map::num_nodes() { return m_num_nodes; }
inline ai_node_le*& xr_scene_ai_map::nodes() { return m_nodes; }

} // end of namespace xray_re

#endif
