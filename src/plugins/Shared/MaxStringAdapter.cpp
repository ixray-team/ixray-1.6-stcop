#include "stdafx.h"

#include "MaxStringAdapter.h"

#ifdef MCHAR_IS_WCHAR
#include <locale>
#include <codecvt>
#endif


std::string NormalizeFromMaxString(const raw_char* data)
{

#ifdef MCHAR_IS_WCHAR

	std::wstring_convert<std::codecvt_utf8_utf16<wchar_t>> converter;
	std::string utf8str = converter.to_bytes(data);
	return utf8str;

#else

	return std::string(data);

#endif

}

const raw_char* NormalizeToMaxString(const char* data)
{

#ifdef MCHAR_IS_WCHAR

	std::wstring_convert<std::codecvt_utf8_utf16<wchar_t>> converter;
	std::wstring wstr = converter.from_bytes(data);
	return wstr.c_str();

#else

	return data;

#endif

}