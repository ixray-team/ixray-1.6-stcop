#include <algorithm>
#include <cstring>
#include "xr_clsid.h"

using namespace xray_re;

void xr_clsid::get(char* s) const
{
	for (const char *p = bytes, *end = p + 8; p != end; ++p) {
		int c = *p;
		if (c == ' ')
			break;
		*s++ = c;
	}
	*s = '\0';
}

uint64_t xr_clsid::to_quad(const char* s)
{
	union {
		char		temp[8];
		uint64_t	temp64;
	};
	char* p = temp;
	for (char* end = p + std::min(std::strlen(s), sizeof(temp)); p != end; ++p)
		*p = *s++;
	for (char* end = temp + sizeof(temp); p != end;)
		*p++ = ' ';
	return temp64;
}
