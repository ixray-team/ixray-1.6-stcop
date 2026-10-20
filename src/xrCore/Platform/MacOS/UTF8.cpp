#include "stdafx.h"

// HACK: the engine feeds windows-1251 text through ANSI_TO_UTF8/UTF8_to_CP1251 and assumes
// real conversion (the Windows layer does). Returning the input unchanged made
// CGameFont::WidthOf recurse forever on Russian text (stack overflow in the main menu).
// Tables generated from the standard cp1251 codec.

static const unsigned short CP1251_to_UCS2[128] =
{
    0x0402, 0x0403, 0x201A, 0x0453, 0x201E, 0x2026, 0x2020, 0x2021,
    0x20AC, 0x2030, 0x0409, 0x2039, 0x040A, 0x040C, 0x040B, 0x040F,
    0x0452, 0x2018, 0x2019, 0x201C, 0x201D, 0x2022, 0x2013, 0x2014,
    0x0098, 0x2122, 0x0459, 0x203A, 0x045A, 0x045C, 0x045B, 0x045F,
    0x00A0, 0x040E, 0x045E, 0x0408, 0x00A4, 0x0490, 0x00A6, 0x00A7,
    0x0401, 0x00A9, 0x0404, 0x00AB, 0x00AC, 0x00AD, 0x00AE, 0x0407,
    0x00B0, 0x00B1, 0x0406, 0x0456, 0x0491, 0x00B5, 0x00B6, 0x00B7,
    0x0451, 0x2116, 0x0454, 0x00BB, 0x0458, 0x0405, 0x0455, 0x0457,
    0x0410, 0x0411, 0x0412, 0x0413, 0x0414, 0x0415, 0x0416, 0x0417,
    0x0418, 0x0419, 0x041A, 0x041B, 0x041C, 0x041D, 0x041E, 0x041F,
    0x0420, 0x0421, 0x0422, 0x0423, 0x0424, 0x0425, 0x0426, 0x0427,
    0x0428, 0x0429, 0x042A, 0x042B, 0x042C, 0x042D, 0x042E, 0x042F,
    0x0430, 0x0431, 0x0432, 0x0433, 0x0434, 0x0435, 0x0436, 0x0437,
    0x0438, 0x0439, 0x043A, 0x043B, 0x043C, 0x043D, 0x043E, 0x043F,
    0x0440, 0x0441, 0x0442, 0x0443, 0x0444, 0x0445, 0x0446, 0x0447,
    0x0448, 0x0449, 0x044A, 0x044B, 0x044C, 0x044D, 0x044E, 0x044F,
};

static unsigned short UCS2_to_CP1251(unsigned short u)
{
    if (u < 0x80)
        return (unsigned short)u;
    for (int i = 0; i < 128; ++i)
        if (CP1251_to_UCS2[i] == u)
            return (unsigned short)(0x80 + i);
    return '?';
}

XRCORE_API const char* Platform::ANSI_TO_TCHAR(const char* C)
{
	return C;
}

XRCORE_API const char* Platform::ANSI_TO_TCHAR_U8(const char* C)
{
	return C;
}

XRCORE_API xr_string Platform::ANSI_TO_UTF8(const xr_string& ansi)
{
	xr_string utf8;
	utf8.reserve(ansi.size() * 2);
	for (char ch : ansi)
	{
		unsigned char c = (unsigned char)ch;
		if (c < 0x80)
		{
			utf8 += (char)c;
			continue;
		}
		unsigned short u = CP1251_to_UCS2[c - 0x80];
		utf8 += (char)(0xC0 | (u >> 6));
		utf8 += (char)(0x80 | (u & 0x3F));
	}
	return utf8;
}

XRCORE_API size_t Platform::CP1251_PATH_TO_UTF8(const char* In, char* Out, size_t OutSize)
{
	// Same conversion as ANSI_TO_UTF8 but with C types: usable from OSFile.h,
	// which sits in the include chain before xrstring.h.
	size_t o = 0;
	for (const unsigned char* p = (const unsigned char*)In; *p && o + 4 < OutSize; ++p)
	{
		unsigned char c = *p;
		if (c < 0x80)
		{
			Out[o++] = (char)c;
			continue;
		}
		unsigned short u = CP1251_to_UCS2[c - 0x80];
		if (u < 0x80)
		{
			Out[o++] = (char)u;
		}
		else if (u < 0x800)
		{
			Out[o++] = (char)(0xC0 | (u >> 6));
			Out[o++] = (char)(0x80 | (u & 0x3F));
		}
		else
		{
			Out[o++] = (char)(0xE0 | (u >> 12));
			Out[o++] = (char)(0x80 | ((u >> 6) & 0x3F));
			Out[o++] = (char)(0x80 | (u & 0x3F));
		}
	}
	Out[o] = 0;
	return o;
}

namespace Platform
{
	XRCORE_API xr_string TCHAR_TO_ANSI_U8(const char* C)
	{
		return C;
	}

	XRCORE_API xr_string UTF8_to_CP1251(xr_string const& utf8)
	{
		xr_string ansi;
		ansi.reserve(utf8.size());
		for (size_t i = 0; i < utf8.size();)
		{
			unsigned char c = (unsigned char)utf8[i];
			if (c < 0x80)
			{
				ansi += (char)c;
				i += 1;
				continue;
			}
			if ((c & 0xE0) == 0xC0 && i + 1 < utf8.size())
			{
				unsigned short u = (unsigned short)(((c & 0x1F) << 6) | (utf8[i + 1] & 0x3F));
				ansi += (char)UCS2_to_CP1251(u);
				i += 2;
				continue;
			}
			if ((c & 0xF0) == 0xE0 && i + 2 < utf8.size())
			{
				unsigned short u = (unsigned short)(((c & 0x0F) << 12) | ((utf8[i + 1] & 0x3F) << 6) | (utf8[i + 2] & 0x3F));
				ansi += (char)UCS2_to_CP1251(u);
				i += 3;
				continue;
			}
			ansi += '?';
			i += 1;
		}
		return ansi;
	}
}
