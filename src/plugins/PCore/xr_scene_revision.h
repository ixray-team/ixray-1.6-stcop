#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_SCENE_REVISION_H__
#define __XR_SCENE_REVISION_H__

#include <string>
#include "xr_types.h"

namespace xray_re {

class xr_reader;
class xr_writer;
class xr_ini_writer;

class xr_scene_revision {
public:
			xr_scene_revision();

	void		load(xr_reader& r);
	void		save(xr_writer& w) const;

	void		save_v12(xr_ini_writer *w, bool scene_part = false);

private:
	std::string	m_modifier;
	uint32_t	m_modified_time;
};

} // end of namespace xray_re

#endif
