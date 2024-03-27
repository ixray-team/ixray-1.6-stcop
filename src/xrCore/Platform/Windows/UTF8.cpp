#pragma once
#include "stdafx.h"

XRCORE_API wchar_t* Platform::ANSI_TO_TCHAR(const char* C)
{
	int len = (int)strlen(C);
	static wchar_t WName[4096];
	RtlZeroMemory(&WName, sizeof(WName));

	// Converts the path to wide characters
	[[maybe_unused]]int needed = MultiByteToWideChar(CP_UTF8, 0, C, len + 1, WName, len + 1);
	return WName;
}

XRCORE_API xr_string ANSI_TO_UTF8(const xr_string& ansi)
{
	wchar_t* wcs = nullptr;
	int need_length = MultiByteToWideChar(CP_ACP, 0, ansi.c_str(), (int)ansi.size(), wcs, 0);
	wcs = new wchar_t[need_length + 1];
	MultiByteToWideChar(CP_ACP, 0, ansi.c_str(), (int)ansi.size(), wcs, need_length);
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

XRCORE_API wchar_t* Platform::ANSI_TO_TCHAR_U8(const char* C)
{
	return ANSI_TO_TCHAR(ANSI_TO_UTF8(C).c_str());
}