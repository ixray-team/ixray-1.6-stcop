#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_SHADERS_LIB_H__
#define __XR_SHADERS_LIB_H__

#include <string>
#include <vector>
#include "xr_blender.h"

namespace xray_re {

class xr_reader;

class xr_shaders_lib {
public:
	virtual		~xr_shaders_lib();

	bool		load(const char* path, const char* name);
	void		load(xr_reader& r);

	const std::vector<std::string>&	names() const;

private:
	std::vector<std::string>	m_names;
};

inline const std::vector<std::string>& xr_shaders_lib::names() const { return m_names; }

} // end of namespace xray_re

#endif
