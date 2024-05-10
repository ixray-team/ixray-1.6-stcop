#include "stdafx.h"

XRCORE_API const char* Platform::ANSI_TO_TCHAR(const char* C)
{
	return C;
}

XRCORE_API const char* Platform::ANSI_TO_TCHAR_U8(const char* C)
{
	return C;
}

XRCORE_API xr_string ANSI_TO_UTF8(const xr_string& ansi)
{
	return ansi;
}

namespace Platform
{
	XRCORE_API xr_string TCHAR_TO_ANSI_U8(const char* C)
	{
		return C;
	}

	XRCORE_API xr_string UTF8_to_CP1251(xr_string const& utf8)
	{
		return utf8;
	}
}