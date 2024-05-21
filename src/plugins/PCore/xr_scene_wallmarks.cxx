#include "xr_scene_wallmarks.h"
#include "xr_reader.h"
#include "xr_writer.h"
#include "xr_utils.h"

using namespace xray_re;

xr_scene_wallmarks::xr_scene_wallmarks(xr_scene& scene):
	xr_scene_part(scene, "wallmark.part", SCENE_CHUNK_WALLMARKS),
	m_flags(WM_FLAG_DRAW_WALLMARKS),
	m_width(1.f),
	m_height(1.f),
	m_rotate(0),
	m_shader("effects\\wallmarkblend") {}

xr_scene_wallmarks::~xr_scene_wallmarks()
{
	delete_elements(m_slots);
}

struct read_wallmark { void operator()(wm_object& wm, xr_reader& r) const {
	wm.selected = r.r_bool();
	r.r_fvector3(wm.bbox.min);
	r.r_fvector3(wm.bbox.max);
	r.r_fvector3(wm.bsphere.p);
	wm.bsphere.r = r.r_float();
	wm.width = r.r_float();
	wm.height = r.r_float();
	wm.rotate = r.r_float();
	r.r_seq(r.r_u32(), wm.vertices);
}};

struct read_slot_1 { void operator()(wm_slot_le*& _slot, xr_reader& r) const {
	wm_slot_le* slot = new wm_slot_le;
	_slot = slot;
	size_t n = r.r_u32();
	r.r_sz(slot->shader);
	r.r_sz(slot->texture);
	r.r_seq(n, slot->wallmarks, read_wallmark());
}};

struct read_slot_0 { void operator()(wm_slot_le*& slot, xr_reader& r) const {
	xr_not_implemented();
}};

void xr_scene_wallmarks::load(xr_reader& r)
{
	revision().load(r);

	uint16_t version;
	if (!r.r_chunk<uint16_t>(WM_CHUNK_VERSION, version))
		xr_not_expected();
	xr_assert(version == WALLMARK_VERSION);

	if (!r.find_chunk(WM_CHUNK_PARAMS))
		xr_not_expected();
	m_width = r.r_float();
	m_height = r.r_float();
	m_rotate = r.r_float();
	r.r_sz(m_shader);
	r.r_sz(m_texture);
	r.debug_find_chunk();

	xr_reader* s;
	if ((s = r.open_chunk(WM_CHUNK_WALLMARKS_1))) {
		s->r_chunks(m_slots, read_slot_1());
		r.close_chunk(s);
	} else if ((s = r.open_chunk(WM_CHUNK_WALLMARKS_0))) {
		s->r_chunks(m_slots, read_slot_0());
		r.close_chunk(s);
	}
}

struct write_wallmark { void operator()(const wm_object& wm, xr_writer& w) const {
	w.w_bool(wm.selected);
	w.w_fvector3(wm.bbox.min);
	w.w_fvector3(wm.bbox.max);
	w.w_fvector3(wm.bsphere.p);
	w.w_float(wm.bsphere.r);
	w.w_float(wm.width);
	w.w_float(wm.height);
	w.w_float(wm.rotate);
	w.w_size_u32(wm.vertices.size());
	w.w_seq(wm.vertices);
}};

struct write_slot { void operator()(const wm_slot_le* slot, xr_writer& w) const {
	w.w_size_u32(slot->wallmarks.size());
	w.w_sz(slot->shader);
	w.w_sz(slot->texture);
	w.w_seq(slot->wallmarks, write_wallmark());
}};

void xr_scene_wallmarks::save(xr_writer& w) const
{
	revision().save(w);

	w.w_chunk<uint16_t>(WM_CHUNK_VERSION, WALLMARK_VERSION);
	w.w_chunk<uint32_t>(WM_CHUNK_FLAGS, m_flags);

	w.open_chunk(WM_CHUNK_PARAMS);
	w.w_float(m_width);
	w.w_float(m_height);
	w.w_float(m_rotate);
	w.w_sz(m_shader);
	w.w_sz(m_texture);
	w.close_chunk();

	w.open_chunk(WM_CHUNK_WALLMARKS_1);
	w.w_chunks(m_slots, write_slot());
	w.close_chunk();
}

void xr_scene_wallmarks::save_v12(xr_ini_writer* w) const
{
}
