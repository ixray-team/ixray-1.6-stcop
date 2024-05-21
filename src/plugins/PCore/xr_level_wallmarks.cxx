#include "xr_level_wallmarks.h"
#include "xr_reader.h"
#include "xr_writer.h"
#include "xr_utils.h"

using namespace xray_re;

xr_level_wallmarks::~xr_level_wallmarks()
{
	delete_elements(m_slots);
}

struct read_wm_data { void operator()(wm_data& wm, xr_reader& r) const {
	r.r_fvector3(wm.bounds.p);
	wm.bounds.r = r.r_float();
	r.r_seq(r.r_u32(), wm.vertices);
}};

struct read_wm_slot { void operator()(wm_slot*& _slot, xr_reader& r) const {
	wm_slot* slot = new wm_slot;
	_slot = slot;
	size_t n = r.r_u32();
	r.r_sz(slot->shader);
	r.r_sz(slot->texture);
	r.r_seq(n, slot->wallmarks, read_wm_data());
}};

void xr_level_wallmarks::load(xr_reader& r)
{
	xr_reader* s = r.open_chunk(1);
	
	//before 1935 chunk_id = 0
	if (s == 0)
		s = r.open_chunk(0);

	s->r_seq(s->r_u32(), m_slots, read_wm_slot());
	r.close_chunk(s);
}

struct write_wm_data { void operator()(const wm_data& wm, xr_writer& w) const {
	w.w_fvector3(wm.bounds.p);
	w.w_float(wm.bounds.r);
	w.w_size_u32(wm.vertices.size());
	w.w_seq(wm.vertices);
}};

struct write_wm_slot { void operator()(const wm_slot* slot, xr_writer& w) const {
	w.w_size_u32(slot->wallmarks.size());
	w.w_sz(slot->shader);
	w.w_sz(slot->texture);
	w.w_seq(slot->wallmarks, write_wm_data());
}};

void xr_level_wallmarks::save(xr_writer& w) const
{
	w.open_chunk(1);
	w.w_size_u32(m_slots.size());
	w.w_seq(m_slots, write_wm_slot());
	w.close_chunk();
}
