#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_LEVEL_SPAWN_H__
#define __XR_LEVEL_SPAWN_H__

#include <vector>
#include "xr_types.h"

namespace xray_re {

class cse_abstract;
class xr_reader;
class xr_writer;

class xr_level_spawn {
public:
			xr_level_spawn();
			xr_level_spawn(xr_reader& r);
	virtual		~xr_level_spawn();

	bool		load(const char* path, const char* name);
	bool		save(const char* path, const char* name);
	void		load(xr_reader& r);
	void		save(xr_writer& w);

	std::vector<cse_abstract*>&	spawns();

private:
	std::vector<cse_abstract*>	m_spawns;
};

inline xr_level_spawn::xr_level_spawn() {}
inline xr_level_spawn::xr_level_spawn(xr_reader& r) { load(r); }

inline std::vector<cse_abstract*>& xr_level_spawn::spawns() { return m_spawns; }

} // end of namespace xray_re

#endif
