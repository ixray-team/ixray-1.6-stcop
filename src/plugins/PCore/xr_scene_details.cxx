// detail_object.part (EDetailManager, EDetail)
#include "xr_scene_details.h"
#include "xr_reader.h"
#include "xr_writer.h"
#include "xr_utils.h"

using namespace xray_re;

detail_object::detail_object():
	min_scale(0.5f), max_scale(2.f), density(1.f), flags(0) {}

xr_scene_details::xr_scene_details(xr_scene& scene):
	xr_scene_part(scene, "detail_object.part", SCENE_CHUNK_DETAIL_OBJECTS),
	m_flags(DETMGR_FLAG_DRAW_OBJECTS), m_num_slots(0), m_slots(0),
	m_density(0.2f)
{
	m_header.version = DETMGR_VERSION;
	m_header.object_count = 0;
	m_header.offs_x = 0;
	m_header.offs_z = 0;
	m_header.size_x = 0;
	m_header.size_z = 0;
}

xr_scene_details::~xr_scene_details()
{
	delete[] m_slots;
	delete_elements(m_objects);
	delete_elements(m_indices);
}

struct read_detail { void operator()(detail_object*& _detail, xr_reader& r) const {
	detail_object* detail = new detail_object;
	_detail = detail;
	uint32_t version;
	if (!r.r_chunk(DETOBJ_CHUNK_VERSION, version))
		xr_not_expected();
	xr_assert(version == DETOBJ_VERSION);

	if (!r.r_chunk(DETOBJ_CHUNK_REFERENCE, detail->reference))
		xr_not_expected();

	if (!r.find_chunk(DETOBJ_CHUNK_SCALE_LIMITS))
		xr_not_expected();
	detail->min_scale = r.r_float();
	detail->max_scale = r.r_float();
	r.debug_find_chunk();

	r.r_chunk<float>(DETOBJ_CHUNK_DENSITY, detail->density);
	r.r_chunk<uint32_t>(DETOBJ_CHUNK_FLAGS, detail->flags);
}};

struct read_color_index { void operator()(color_index*& _ci, xr_reader& r) {
	color_index* ci = new color_index;
	_ci = ci;
	ci->color = r.r_u32();
	r.r_seq(r.r_u8(), ci->references, xr_reader::f_r_sz());
}};

void xr_scene_details::load(xr_reader& r)
{
	revision().load(r);

	uint32_t version;
	if (!r.r_chunk<uint32_t>(DETMGR_CHUNK_VERSION, version))
		xr_not_expected();
	xr_assert(version == DETMGR_VERSION);

	r.r_chunk<uint32_t>(DETMGR_CHUNK_FLAGS, m_flags);
	if (!r.find_chunk(DETMGR_CHUNK_HEADER))
		xr_not_expected();
	m_header.load(r);
	r.debug_find_chunk();

	if (!r.find_chunk(DETMGR_CHUNK_SLOTS))
		xr_not_expected();
	if ((m_num_slots = r.r_u32())) {
		xr_assert(m_num_slots == m_header.size_x*m_header.size_z);
		m_slots = new detail_slot_v3[m_num_slots];
		r.r_cseq(m_num_slots, m_slots);
	}
	r.debug_find_chunk();

	xr_reader* s = r.open_chunk(DETMGR_CHUNK_OBJECTS);
	if (s) {
		s->r_chunks(m_objects, read_detail());
		r.close_chunk(s);
	}

	if (!r.find_chunk(DETMGR_CHUNK_COLOR_INDEX))
		xr_not_expected();
	r.r_seq(r.r_u8(), m_indices, read_color_index());
	r.debug_find_chunk();

	if (!r.r_chunk(DETMGR_CHUNK_BBOX, m_bbox))
		xr_not_expected();

	if (r.find_chunk(DETMGR_CHUNK_SNAP_OBJECTS))
		r.r_seq(r.r_u32(), m_snap_objects, xr_reader::f_r_sz());

	r.r_chunk(DETMGR_CHUNK_DENSITY, m_density);

	if (r.find_chunk(DETMGR_CHUNK_TEXTURE)) {
		r.r_sz(m_texture);
		r.debug_find_chunk();
	}
}

struct write_detail {void operator()(const detail_object* detail, xr_writer& w) const {
	w.w_chunk(DETOBJ_CHUNK_VERSION, DETOBJ_VERSION);
	w.w_chunk(DETOBJ_CHUNK_REFERENCE, detail->reference);
	w.open_chunk(DETOBJ_CHUNK_SCALE_LIMITS);
	w.w_float(detail->min_scale);
	w.w_float(detail->max_scale);
	w.close_chunk();
	w.w_chunk(DETOBJ_CHUNK_DENSITY, detail->density);
	w.w_chunk(DETOBJ_CHUNK_FLAGS, detail->flags);
}};

struct write_color_index { void operator()(const color_index* ci, xr_writer& w) const {
	w.w_u32(ci->color);
	w.w_size_u8(ci->references.size());
	w.w_seq(ci->references, xr_writer::f_w_sz());
}};

void xr_scene_details::save(xr_writer& w) const
{
	revision().save(w);
	w.w_chunk<uint32_t>(DETMGR_CHUNK_VERSION, DETMGR_VERSION);
	w.w_chunk<uint32_t>(DETMGR_CHUNK_FLAGS, m_flags);

// this is ignored in LE
//	m_header.object_count = uint32_t(m_objects.size() & UINT32_MAX);
	w.open_chunk(DETMGR_CHUNK_HEADER);
	m_header.save(w);
	w.close_chunk();

	w.open_chunk(DETMGR_CHUNK_OBJECTS);
	w.w_chunks(m_objects, write_detail());
	w.close_chunk();

	w.open_chunk(DETMGR_CHUNK_COLOR_INDEX);
	w.w_size_u8(m_indices.size());
	w.w_seq(m_indices, write_color_index());
	w.close_chunk();

	w.open_chunk(DETMGR_CHUNK_SLOTS);
	w.w_u32(m_num_slots);
	w.w_cseq(m_num_slots, m_slots);
	w.close_chunk();

	w.w_chunk(DETMGR_CHUNK_BBOX, m_bbox);

	if (!m_texture.empty())
		w.w_chunk(DETMGR_CHUNK_TEXTURE, m_texture);

	w.w_chunk(DETMGR_CHUNK_DENSITY, m_density);

	w.open_chunk(DETMGR_CHUNK_SNAP_OBJECTS);
	w.w_size_u32(m_snap_objects.size());
	w.w_seq(m_snap_objects, xr_writer::f_w_sz());
	w.close_chunk();
}

void xr_scene_details::save_v12(xr_ini_writer* w) const
{
}
