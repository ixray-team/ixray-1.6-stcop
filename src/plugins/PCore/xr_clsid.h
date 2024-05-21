#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_CLSID_H__
#define __XR_CLSID_H__

#include "xr_types.h"

namespace xray_re {

struct xr_clsid {
			xr_clsid();
			xr_clsid(const char* clsid);
			xr_clsid(uint64_t clsid);
	bool		operator<(const xr_clsid& right) const;
	bool		operator==(const char* right) const;
	bool		operator==(const xr_clsid& right) const;
	bool		operator!=(const xr_clsid& right) const;
	void		get(char s[]) const;

	static uint64_t	to_quad(const char* s);

	union {
		char		bytes[8];
		uint32_t	words[2];
		uint64_t	quad;
	};
};

inline xr_clsid::xr_clsid(): quad(0) {}
inline xr_clsid::xr_clsid(const char* clsid): quad(to_quad(clsid)) {}
inline xr_clsid::xr_clsid(uint64_t clsid): quad(clsid) {}
inline bool xr_clsid::operator<(const xr_clsid& right) const { return quad < right.quad; }
inline bool xr_clsid::operator==(const char* right) const { return quad == to_quad(right); }
inline bool xr_clsid::operator==(const xr_clsid& right) const { return quad == right.quad; }
inline bool xr_clsid::operator!=(const xr_clsid& right) const { return quad != right.quad; }

} // end of namespace xray_re

#endif
