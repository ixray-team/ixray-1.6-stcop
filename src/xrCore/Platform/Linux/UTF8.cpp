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
