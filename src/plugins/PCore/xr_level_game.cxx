#include "xr_level_game.h"
#include "xr_file_system.h"
#include "xr_utils.h"

using namespace xray_re;

xr_level_game::~xr_level_game()
{
	delete_elements(m_paths);
	delete_elements(m_rpoints);
}

void xr_level_game::load(xr_reader& r)
{
	xr_reader* s = r.open_chunk(GAME_CHUNK_WAYS);
	if (s) {
		s->r_chunks(m_paths, way_path_io());
		r.close_chunk(s);
	}
	s = r.open_chunk(GAME_CHUNK_RPOINTS);
	if (s) {
		s->r_chunks(m_rpoints, mp_rpoint_io());
		r.close_chunk(s);
	}
}

void xr_level_game::save(xr_writer& w) const
{
	w.open_chunk(GAME_CHUNK_WAYS);
	w.w_chunks(m_paths, way_path_io());
	w.close_chunk();

	w.open_chunk(GAME_CHUNK_RPOINTS);
	w.w_chunks(m_rpoints, mp_rpoint_io());
	w.close_chunk();
}

bool xr_level_game::load(const char* path, const char* name)
{
	xr_file_system& fs = xr_file_system::instance();
	xr_reader* r = fs.r_open(path, name);
	if (r == 0)
		return false;
	load(*r);
	fs.r_close(r);
	return true;
}

bool xr_level_game::save(const char* path, const char* name) const
{
	xr_file_system& fs = xr_file_system::instance();
	xr_writer* w = fs.w_open(path, name);
	if (w == 0)
		return false;
	save(*w);
	fs.w_close(w);
	return true;
}
