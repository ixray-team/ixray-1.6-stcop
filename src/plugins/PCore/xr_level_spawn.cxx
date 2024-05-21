#include "xr_level_spawn.h"
#include "xr_entity.h"
#include "xr_entity_factory.h"
#include "xr_file_system.h"
#include "xr_utils.h"

using namespace xray_re;

xr_level_spawn::~xr_level_spawn()
{
	delete_elements(m_spawns);
}

void xr_level_spawn::load(xr_reader& r)
{
	xr_reader* s;
	for (uint32_t id = 0; (s = r.open_chunk(id)); ++id) {
		xr_packet packet;
		s->r_packet(packet, s->size());
		uint16_t pkt_id;
		packet.r_begin(pkt_id);
		xr_assert(pkt_id == M_SPAWN);
		const char* name = packet.skip_sz();
		packet.r_seek(0);
		// just skip the spawn record if we can't create it.
		if (cse_abstract* entity = create_entity(name)) {
			entity->spawn_read(packet);
			if (!packet.r_eof()) {
				msg("reading %s (%s): %u <> %u", entity->name_replace().c_str(),
						name, packet.r_tell(), packet.w_tell());
			}
			m_spawns.push_back(entity);
		}
		r.close_chunk(s);
	}
}

void xr_level_spawn::save(xr_writer& w)
{
	uint32_t id = 0;
	for (xr_entity_vec_it it = m_spawns.begin(), end = m_spawns.end(); it != end; ++it, ++id) {
		xr_packet packet;
		(*it)->spawn_write(packet, true);
		w.open_chunk(id);
		w.w_packet(packet);
		w.close_chunk();
	}
}

bool xr_level_spawn::load(const char* path, const char* name)
{
	xr_file_system& fs = xr_file_system::instance();
	xr_reader* r = fs.r_open(path, name);
	if (r == 0)
		return false;
	load(*r);
	fs.r_close(r);
	return true;
}

bool xr_level_spawn::save(const char* path, const char* name)
{
	xr_file_system& fs = xr_file_system::instance();
	xr_writer* w = fs.w_open(path, name);
	if (w == 0)
		return false;
	save(*w);
	fs.w_close(w);
	return true;
}
