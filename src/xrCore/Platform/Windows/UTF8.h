#pragma once

namespace Platform
{
	XRCORE_API wchar_t* ANSI_TO_TCHAR(const char* C);
	XRCORE_API wchar_t* ANSI_TO_TCHAR_U8(const char* C);
	//XRCORE_API xr_string ANSI_TO_UTF8(const xr_string& C);
}