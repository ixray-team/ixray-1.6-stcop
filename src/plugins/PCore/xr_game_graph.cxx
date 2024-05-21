#include "xr_ai_version.h"
#include "xr_game_graph.h"
#include "xr_file_system.h"
#include "xr_utils.h"

using namespace xray_re;

xr_game_graph::~xr_game_graph()
{
	delete_elements(m_cross_tables);
}

void xr_game_graph::load(xr_reader& r)
{
	xr_level_graph::load(r);
	if (m_version == AI_VERSION_8) {
		xr_assert(m_num_death_points);
		xr_assert(r.elapsed() == 4*m_num_levels);
		r.advance(r.elapsed());
	} else if (m_version == AI_VERSION_9 || m_version == AI_VERSION_10) {
		m_cross_tables.reserve(m_num_levels);
		for (gg_level *it = m_levels, *end = it + m_num_levels; it != end; ++it) {
			xr_level_gct* gct = new xr_level_gct;
			gct->load_v9(r);
			xr_assert(gct->version() == m_version);
			xr_assert(gct->level_guid() == it->guid);
			xr_assert(gct->game_guid() == m_guid);
			m_cross_tables.push_back(gct);
		}
		xr_assert(r.elapsed() == 0);
	}
}

void xr_game_graph::save(xr_writer& w) const
{
	xr_level_graph::save(w);
	if (m_version == AI_VERSION_8) {
		xr_assert(m_num_death_points);
		for (uint_fast32_t i = m_num_levels; i != 0; --i)
			w.w_u32(sizeof(uint32_t));
	} else if (m_version == AI_VERSION_9 || m_version == AI_VERSION_10) {
		for (xr_level_gct_vec_cit it = m_cross_tables.begin(),
				end = m_cross_tables.end(); it != end; ++it) {
			if (const xr_level_gct* gct = *it)
				gct->save_v9(w);
			else
				w.w_u32(sizeof(uint32_t));
		}
	}
}

bool xr_game_graph::load(const char* path, const char* name)
{
	xr_file_system& fs = xr_file_system::instance();
	xr_reader* r = fs.r_open(path, name);
	if (r == 0)
		return false;
	load(*r);
	fs.r_close(r);
	return true;
}

bool xr_game_graph::save(const char* path, const char* name) const
{
	xr_file_system& fs = xr_file_system::instance();
	xr_writer* w = fs.w_open(path, name);
	if (w == 0)
		return false;
	save(*w);
	fs.w_close(w);
	return true;
}
