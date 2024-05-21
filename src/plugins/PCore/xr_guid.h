#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_GUID_H__
#define __XR_GUID_H__

#include <cstring>
#include "xr_types.h"

namespace xray_re {

class xr_reader;
class xr_writer;

struct xr_guid {
	void		reset();
	bool		operator==(const xr_guid& right) const;
	bool		operator!=(const xr_guid& right) const;

	void		load(xr_reader& r);
	void		save(xr_writer& w) const;

	uint32_t	g[4];
};

inline void xr_guid::reset() { std::memset(g, 0x55, sizeof(g)); }
inline bool xr_guid::operator==(const xr_guid& right) const { return std::memcmp(g, right.g, sizeof(g)) == 0; }
inline bool xr_guid::operator!=(const xr_guid& right) const { return !(*this == right); }

} // end of namespace xray_re

#endif
