#pragma once
#include <wctype.h>
#include <vector>

namespace Platform
{
	XRCORE_API const char* ANSI_TO_TCHAR(const char* C);
	XRCORE_API const char* ANSI_TO_TCHAR_U8(const char* C);
	//XRCORE_API xr_string ANSI_TO_UTF8(const xr_string& C);
	XRCORE_API size_t CP1251_PATH_TO_UTF8(const char* In, char* Out, size_t OutSize);

	// HACK: TCHAR is char on this platform, so font code walking `wideStr[i]` would step
	// UTF-8 BYTES instead of codepoints. Decode once, iterate glyphs (same as the
	// Windows wchar_t path).
	inline void DecodeUtf8CodePoints(const char* s, std::vector<int>& out)
	{
		while (*s)
		{
			unsigned char c = (unsigned char)*s;
			unsigned int cp = c;
			int extra = 0;
			if ((c & 0xE0) == 0xC0) { cp = c & 0x1Fu; extra = 1; }
			else if ((c & 0xF0) == 0xE0) { cp = c & 0x0Fu; extra = 2; }
			else if ((c & 0xF8) == 0xF0) { cp = c & 0x07u; extra = 3; }
			++s;
			for (int k = 0; k < extra && (*s & 0xC0) == 0x80; ++k)
			{
				cp = (cp << 6) | (unsigned char)(*s & 0x3F);
				++s;
			}
			out.push_back((int)cp);
		}
	}
}