#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_STRING_UTILS_H__
#define __XR_STRING_UTILS_H__

#include <string>
#include <cstring>
#include <cctype>

namespace xray_re {

static inline void xr_strlwr(std::string& s)
{
	for (std::string::iterator it = s.begin(), end = s.end(); it != end; ++it)
		*it = char(std::tolower(*it));
}

static inline void xr_strlwr(char* s, size_t n)
{
#if defined(_MSC_VER) && _MSC_VER >= 1400
	_strlwr_s(s, n);
#else
	for (int c; (c = *s) != 0;)
		*s++ = std::tolower(c);
#endif
}

static inline int xr_stricmp(const char* s1, const char* s2)
{
	return _stricmp(s1, s2);
}

#if defined(_MSC_VER) && _MSC_VER >= 1400
#define xr_snprintf	sprintf_s
#else
// FIXME: snprintf has different semantics really!!!
#define xr_snprintf	snprintf
#endif

/*
static char* xr_strdup(const char* string)
{
	xr_assert(string);
	uint32_t len = uint32_t(strlen(string)) + 1;
	
	char* mem = (char*)malloc(len);
	CopyMemory(mem, string, len);

	return mem;
}
*/

} // end of namespace xray_re

#endif
