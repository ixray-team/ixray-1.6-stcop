#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_LEVEL_GAME_H__
#define __XR_LEVEL_GAME_H__

#include "xr_ai_way.h"

namespace xray_re {

// in-game format defs.
enum {
	GAME_CHUNK_WAYS		= 0x1000,
	GAME_CHUNK_RPOINTS	= 0x2000,
};

class xr_level_game {
public:
				xr_level_game();
				xr_level_game(xr_reader& r);
	virtual			~xr_level_game();

	void			load(xr_reader& r);
	void			save(xr_writer& w) const;
	bool			load(const char* path, const char* name);
	bool			save(const char* path, const char* name) const;

	way_path_vec&		paths();
	const way_path_vec&	paths() const;
	mp_rpoint_vec&		rpoints();
	const mp_rpoint_vec&	rpoints() const;

private:
	way_path_vec		m_paths;
	mp_rpoint_vec		m_rpoints;
};

inline xr_level_game::xr_level_game() {}
inline xr_level_game::xr_level_game(xr_reader& r) { load(r); }

inline way_path_vec& xr_level_game::paths() { return m_paths; }
inline const way_path_vec& xr_level_game::paths() const { return m_paths; }
inline mp_rpoint_vec& xr_level_game::rpoints() { return m_rpoints; }
inline const mp_rpoint_vec& xr_level_game::rpoints() const { return m_rpoints; }

} // end of namespace xray_re

#endif
