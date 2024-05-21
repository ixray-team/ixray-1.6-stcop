#include <algorithm>
#include "xr_ai_version.h"
#include "xr_game_spawn.h"
#include "xr_level_game.h"
#include "xr_entity.h"
#include "xr_entity_factory.h"
#include "xr_utils.h"
#include "xr_file_system.h"

using namespace xray_re;

xr_game_spawn::xr_game_spawn(): m_version(AI_VERSION_8),
	m_num_levels(0), m_num_af_slots(0), m_af_slots(0)
{
	m_guid.reset();
	m_graph_guid.reset();
}

xr_game_spawn::~xr_game_spawn()
{
	delete_elements(m_spawns);
	delete[] m_af_slots;
}

void xr_game_spawn::load_spawns(xr_reader& r)
{
	if (!r.find_chunk(0))
		xr_not_expected();
	size_t num_spawns = r.r_u32();
	r.debug_find_chunk();

	xr_assert(num_spawns < 65536);
	m_spawns.reserve(num_spawns);

	xr_reader* f = r.open_chunk(1);
	xr_assert(f);
	for (uint32_t id = 0; id != num_spawns; ++id) {
		xr_reader* s = f->open_chunk(id);
		uint16_t obj_id;
		if (!s->r_chunk(0, obj_id))
			xr_not_expected();
		xr_assert(id == obj_id);

		xr_reader* o = s->open_chunk(1);
		xr_assert(o);

		size_t size = o->find_chunk(0);
		xr_assert(size);

		size_t size16 = o->r_u16();
		xr_assert(size16 + 2 == size);
		xr_packet packet;
		o->r_packet(packet, size16);
		uint16_t pkt_id;
		packet.r_begin(pkt_id);
		xr_assert(pkt_id == M_SPAWN);
		const char* name = packet.skip_sz();
		packet.r_seek(0);
		cse_abstract* entity = create_entity(name);
		xr_assert(entity);
		entity->spawn_read(packet);

		size = o->find_chunk(1);
		xr_assert(size);

		size16 = o->r_u16();
		xr_assert(size16 + 2 == size);
		o->r_packet(packet, size16);
		packet.r_begin(pkt_id);
		xr_assert(pkt_id == M_UPDATE);
		entity->update_read(packet);

		m_spawns.push_back(entity);

		s->close_chunk(o);
		f->close_chunk(s);
	}
	r.close_chunk(f);
	if (size_t size = r.find_chunk(2)) {
		// FIXME: what is this?
		r.advance(size);
		r.debug_find_chunk();
	}
}

void xr_game_spawn::save_spawns(xr_writer& w)
{
	w.open_chunk(1);
	w.open_chunk(0);
	w.w_size_u32(m_spawns.size());
	w.close_chunk();

	w.open_chunk(1);
	uint16_t id = 0;
	for (xr_entity_vec_it it = m_spawns.begin(), end = m_spawns.end();
			it != end; ++it, ++id) {
		w.open_chunk(id);

		w.open_chunk(0);
		w.w_u16(id);
		w.close_chunk();

		w.open_chunk(1);
		w.open_chunk(0);
		xr_packet packet;
		(*it)->spawn_write(packet, true);
		w.w_size_u16(packet.w_tell());
		w.w_packet(packet);
		w.close_chunk();

		w.open_chunk(1);
		packet.clear();
		(*it)->update_write(packet);
		w.w_size_u16(packet.w_tell() + 2);
		w.w_u16(M_UPDATE);
		w.w_packet(packet);
		w.close_chunk();

		w.close_chunk();

		w.close_chunk();
	}
	w.close_chunk();

	// FIXME: what is this?
	w.open_chunk(2);
	w.close_chunk();

	w.close_chunk();
}

void xr_game_spawn::load_af_slots(xr_reader& r)
{
	if ((m_num_af_slots = r.r_u32())) {
		m_af_slots = new gg_level_point[m_num_af_slots];
		r.r_cseq(m_num_af_slots, m_af_slots, gg_level_point_io());
	}
}

void xr_game_spawn::save_af_slots(xr_writer& w) const
{
	w.open_chunk(2);
	w.w_u32(m_num_af_slots);
	w.w_cseq(m_num_af_slots, m_af_slots, gg_level_point_io());
	w.close_chunk();
}

struct read_point_gs { void operator()(way_point_gs& point, xr_reader& r) const {
	if (!r.find_chunk(1))
		xr_not_expected();
	r.r_sz(point.name);
	r.r_fvector3(point.position);
	point.flags = r.r_u32();
	point.node_id = r.r_u32();
	point.graph_id = r.r_u16();
	r.debug_find_chunk();
}};

struct read_path_gs { void operator()(way_path_gs*& _path, xr_reader& r) const {
	way_path_gs* path = new way_path_gs;
	_path = path;
	if (!r.r_chunk(0, path->name))
		xr_not_expected();

	xr_reader* s = r.open_chunk(1);
	xr_assert(s);

	uint32_t num_points = 0;
	if (!s->r_chunk(0, num_points))
		xr_not_expected();
	xr_reader* f = s->open_chunk(1);
	xr_assert(f);
	f->r_chunks(path->points, read_point_gs());
	s->close_chunk(f);

	for (size_t size = s->find_chunk(2); size > 0; ) {
		xr_assert(size >= 8);
		size -= 8;
		uint16_t from = uint16_t(s->r_u32() & UINT16_MAX);
		unsigned n = s->r_u32();
		while (n--) {
			xr_assert(size >= 8);
			size -= 8;
			path->links.push_back(way_link());
			way_link& link = path->links.back();
			link.from = from;
			link.to = uint16_t(s->r_u32() & UINT16_MAX);
			link.weight = s->r_float();
		}
	}
	r.close_chunk(s);
}};

void xr_game_spawn::load_paths(xr_reader& r)
{
	if (!r.find_chunk(0))
		xr_not_expected();
	size_t num_paths = r.r_u32();
	r.debug_find_chunk();

	xr_reader* s = r.open_chunk(1);
	xr_assert(s);
	m_paths.reserve(num_paths);
	s->r_chunks(m_paths, read_path_gs());
	r.close_chunk(s);
}

struct link_pred { bool operator()(const way_link& l, const way_link& r) const {
	return l.from < r.from || (l.from == r.from && (l.to < r.to || (l.to == r.to && l.weight < r.weight)));
}};

void xr_game_spawn::save_paths(xr_writer& w)
{
	w.open_chunk(3);
	w.open_chunk(0);
	w.w_size_u32(m_paths.size());
	w.close_chunk();

	w.open_chunk(1);
	unsigned path_idx = 0;
	for (way_path_gs_vec_it it = m_paths.begin(), end = m_paths.end(); it != end; ++it, ++path_idx) {
		way_path_gs* path = *it;
		w.open_chunk(path_idx);
		w.w_chunk(0, path->name);
		w.open_chunk(1);

		w.open_chunk(0);
		w.w_size_u32(path->points.size());
		w.close_chunk();

		w.open_chunk(1);
		uint32_t point_idx = 0;
		for (way_point_gs_vec_it it1 = path->points.begin(), end1 = path->points.end();
				it1 != end1; ++it1, ++point_idx) {
			w.open_chunk(point_idx);
			w.w_chunk(0, point_idx);
			w.open_chunk(1);
			w.w_sz(it1->name);
			w.w_fvector3(it1->position);
			w.w_u32(it1->flags);
			w.w_u32(it1->node_id);
			w.w_u16(it1->graph_id);
			w.close_chunk();
			w.close_chunk();
		}
		w.close_chunk();

		w.open_chunk(2);
		way_link_vec_it link = path->links.begin(), end1 = path->links.end();
		std::sort(link, end1, link_pred());
		for (; link != end1;) {
			way_link_vec_it it1 = link;
			for (; ++it1 != end1 && it1->from == link->from;) {}
			xr_assert(link != it1);
			w.w_u32(link->from);
			w.w_size_u32(it1 - link);
			for (; link != it1; ++link) {
				w.w_u32(link->to);
				w.w_float(link->weight);
			}
		}
		w.close_chunk();

		w.close_chunk();
		w.close_chunk();
	}
	w.close_chunk();
	w.close_chunk();
}

void xr_game_spawn::load(xr_reader& r)
{
	if (!r.find_chunk(0))
		xr_not_expected();
	m_version = r.r_u32();
	xr_assert(m_version >= AI_VERSION_8 && m_version <= AI_VERSION_10);
	m_guid.load(r);
	m_graph_guid.load(r);
	size_t num_spawns = r.r_u32();
	m_num_levels = r.r_u32();
	r.debug_find_chunk();

	xr_reader* s = r.open_chunk(1);
	xr_assert(s);
	load_spawns(*s);
	xr_assert(num_spawns == m_spawns.size());
	r.close_chunk(s);

	s = r.open_chunk(2);
	xr_assert(s);
	load_af_slots(*s);
	r.close_chunk(s);

	s = r.open_chunk(3);
	xr_assert(s);
	load_paths(*s);
	r.close_chunk(s);

	if (m_version >= AI_VERSION_9) {
		s = r.open_chunk(4);
		xr_assert(s);
		m_graph.load(*s);
		r.close_chunk(s);
	}
}

void xr_game_spawn::save(xr_writer& w)
{
	w.open_chunk(0);
	w.w_u32(m_version);
	m_guid.save(w);
	m_graph_guid.save(w);
	w.w_size_u32(m_spawns.size());
	w.w_u32(m_num_levels);
	w.close_chunk();

	save_spawns(w);
	save_af_slots(w);
	save_paths(w);

	if (m_version >= AI_VERSION_9) {
		w.open_chunk(4);
		m_graph.save(w);
		w.close_chunk();
	}
}

bool xr_game_spawn::load(const char* path, const char* name)
{
	xr_file_system& fs = xr_file_system::instance();
	xr_reader* r = fs.r_open(path, name);
	if (r == 0)
		return false;
	load(*r);
	fs.r_close(r);
	return true;
}

bool xr_game_spawn::save(const char* path, const char* name)
{
	xr_memory_writer* w = new xr_memory_writer();
	save(*w);
	bool status = w->save_to(path, name);
	delete w;
	return status;
}

bool xr_game_spawn::load_graph(const char* path, const char* name)
{
	if (m_graph.load(path, name)) {
		xr_assert(m_graph.num_levels() >= m_num_levels);
		xr_assert(m_graph.version() == m_version);
		xr_assert(m_graph.guid() == m_graph_guid);
		return true;
	}
	return false;
}

bool xr_game_spawn::save_graph(const char* path, const char* name) const
{
	return m_graph.save(path, name);
}
