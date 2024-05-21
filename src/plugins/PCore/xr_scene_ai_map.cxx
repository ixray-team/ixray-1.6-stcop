#include "xr_scene_ai_map.h"
#include "xr_reader.h"
#include "xr_writer.h"

using namespace xray_re;

xr_scene_ai_map::xr_scene_ai_map(xr_scene& scene):
	xr_scene_part(scene, "ai_map.part", SCENE_CHUNK_AI_MAP),
	m_flags(0),
	m_cell_size(0.7f), m_factor_y(1.f), m_can_up(1.5f), m_can_down(4.f),
	m_num_nodes(0), m_nodes(0),
	m_visible_radius(30.f), m_brush_size(1), m_smooth_height(0.3f)
{
	m_bbox.null();
}

xr_scene_ai_map::~xr_scene_ai_map()
{
	delete m_nodes;
}

struct read_node_le { void operator()(ai_node_le& node, xr_reader& r) const {
	for (uint_fast32_t i = 0; i != 4; ++i)
		node.link[i] = r.r_u24();
	node.plane = r.r_u16();
	node.packed_x = r.r_s16();
	node.packed_y = r.r_u16();
	node.packed_z = r.r_s16();
	node.selected = r.r_bool();
}};

void xr_scene_ai_map::load(xr_reader& r)
{
	revision().load(r);

	uint16_t version;
	if (!r.r_chunk(AIMAP_CHUNK_VERSION, version))
		xr_not_expected();
	xr_assert(version == AIMAP_VERSION);

	if (!r.r_chunk(AIMAP_CHUNK_FLAGS, m_flags))
		xr_not_expected();

	if (!r.r_chunk(AIMAP_CHUNK_BOX, m_bbox))
		xr_not_expected();

	if (!r.find_chunk(AIMAP_CHUNK_PARAMS))
		xr_not_expected();
	m_cell_size = r.r_float();
	m_factor_y = r.r_float();
	m_can_up = r.r_float();
	m_can_down = r.r_float();
	r.debug_find_chunk();

	if (!r.find_chunk(AIMAP_CHUNK_NODES))
		xr_not_expected();
	m_num_nodes = r.r_u32();
	r.r_cseq(m_num_nodes, m_nodes = new ai_node_le[m_num_nodes], read_node_le());
	r.debug_find_chunk();

	if (r.find_chunk(AIMAP_CHUNK_PREFS)) {
		m_visible_radius = r.r_float();
		m_brush_size = r.r_u32();
		r.debug_find_chunk();
	}

	r.r_chunk<float>(AIMAP_CHUNK_SMOOTH_HEIGHT, m_smooth_height);

	if (r.find_chunk(AIMAP_CHUNK_SNAP_OBJECTS))
		r.r_seq(r.r_u32(), m_snap_objects, xr_reader::f_r_sz());
}

struct write_node_le { void operator()(const ai_node_le& node, xr_writer& w) const {
	for (uint_fast32_t i = 0; i != 4; ++i)
		w.w_u24(node.link[i]);
	w.w_u16(node.plane);
	w.w_s16(node.packed_x);
	w.w_u16(node.packed_y);
	w.w_s16(node.packed_z);
	w.w_bool(node.selected);
}};

void xr_scene_ai_map::save(xr_writer& w) const
{
	revision().save(w);

	w.w_chunk(AIMAP_CHUNK_VERSION, AIMAP_VERSION);
	w.w_chunk(AIMAP_CHUNK_FLAGS, m_flags);
	w.w_chunk(AIMAP_CHUNK_BOX, m_bbox);

	w.open_chunk(AIMAP_CHUNK_PARAMS);
	w.w_float(m_cell_size);
	w.w_float(m_factor_y);
	w.w_float(m_can_up);
	w.w_float(m_can_down);
	w.close_chunk();

	w.open_chunk(AIMAP_CHUNK_NODES);
	w.w_u32(m_num_nodes);
	w.w_cseq(m_num_nodes, m_nodes, write_node_le());
	w.close_chunk();

	w.open_chunk(AIMAP_CHUNK_PREFS);
	w.w_float(m_visible_radius);
	w.w_u32(m_brush_size);
	w.close_chunk();

	w.w_chunk<float>(AIMAP_CHUNK_SMOOTH_HEIGHT, m_smooth_height);

	w.open_chunk(AIMAP_CHUNK_SNAP_OBJECTS);
	w.w_size_u32(m_snap_objects.size());
	w.w_seq(m_snap_objects, xr_writer::f_w_sz());
	w.close_chunk();
}


void xr_scene_ai_map::save_v12(xr_ini_writer* w) const
{
}

//void xr_scene_ai_map::save
