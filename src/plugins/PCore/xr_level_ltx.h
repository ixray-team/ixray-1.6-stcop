#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_LEVEL_LTX_H__
#define __XR_LEVEL_LTX_H__

#include <string>

namespace xray_re {

class xr_ini_file;
class xr_reader;
class xr_writer;

class xr_level_ltx {
public:
				xr_level_ltx(xr_reader& r);
	virtual			~xr_level_ltx();

	void			load(xr_reader& r);
	void			save(xr_writer& w) const;

	const xr_ini_file*	ini() const;
	const std::string&	data() const;

private:
	xr_ini_file*		m_ini;
	std::string		m_data;
};

inline xr_level_ltx::xr_level_ltx(xr_reader& r) { load(r); }
inline const xr_ini_file* xr_level_ltx::ini() const { return m_ini; }
inline const std::string& xr_level_ltx::data() const { return m_data; }

} // end of namespace xray_re

#endif
