#pragma once
#include "stdafx.h"

XRCORE_API wchar_t* Platform::ANSI_TO_TCHAR(const char* C)
{
	int len = (int)strlen(C);
	static thread_local wchar_t WName[4096];
	RtlZeroMemory(&WName, sizeof(WName));

	// Converts the path to wide characters
	[[maybe_unused]]int needed = MultiByteToWideChar(CP_UTF8, 0, C, len + 1, WName, len + 1);
	return WName;
}

XRCORE_API xr_string Platform::ANSI_TO_UTF8(const xr_string& ansi)
{
	wchar_t* wcs = nullptr;
	int need_length = MultiByteToWideChar(1251, 0, ansi.c_str(), (int)ansi.size(), wcs, 0);
	wcs = new wchar_t[need_length + 1];
	MultiByteToWideChar(1251, 0, ansi.c_str(), (int)ansi.size(), wcs, need_length);
	wcs[need_length] = L'\0';

	char* u8s = nullptr;
	need_length = WideCharToMultiByte(CP_UTF8, 0, wcs, (int)std::wcslen(wcs), u8s, 0, nullptr, nullptr);
	u8s = new char[need_length + 1];
	WideCharToMultiByte(CP_UTF8, 0, wcs, (int)std::wcslen(wcs), u8s, need_length, nullptr, nullptr);
	u8s[need_length] = '\0';

	xr_string result(u8s);
	delete[] wcs;
	delete[] u8s;
	return result;
}

XRCORE_API xr_string Platform::UTF8_to_CP1251(xr_string const& utf8)
{
	if (!utf8.empty() && IsUTF8(utf8.data()))
	{
		int wchlen = MultiByteToWideChar(CP_UTF8, 0, utf8.c_str(), utf8.size(), nullptr, 0);
		if (wchlen > 0 && wchlen != 0xFFFD)
		{
			xr_vector<wchar_t> wbuf(wchlen);
			MultiByteToWideChar(CP_UTF8, 0, utf8.c_str(), utf8.size(), &wbuf[0], wchlen);
			xr_vector<char> buf(wchlen);
			WideCharToMultiByte(1251, 0, &wbuf[0], wchlen, &buf[0], wchlen, 0, 0);

			return xr_string(&buf[0], wchlen);
		}
	}

	return utf8;
}

XRCORE_API wchar_t* Platform::ANSI_TO_TCHAR_U8(const char* C)
{
	if (IsUTF8(C))
		return ANSI_TO_TCHAR(C);

	return ANSI_TO_TCHAR(ANSI_TO_UTF8(C).c_str());
}

XRCORE_API xr_string Platform::TCHAR_TO_ANSI_U8(const wchar_t* input)
{
	int size = WideCharToMultiByte(CP_UTF8, 0, input, (int)wcslen(input), 0, 0,
		nullptr, nullptr);

	static thread_local char buf[256] = {};
	std::memset(&buf, 0, 256);

	WideCharToMultiByte(1251, 0, input, (int)wcslen(input), buf, size, nullptr, nullptr);
	return buf;
}
