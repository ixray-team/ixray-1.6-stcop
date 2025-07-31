#pragma once

#include "../../stack_string.h"

namespace Platform
{
	template<xr_ssnt_t InSize, xr_ssnt_t OutSize>
	inline bool WCHAR_TO_CHAR(xr_stack_tstring<InSize>& input, xr_stack_string<OutSize>& output)
	{
		if constexpr (!std::is_same_v<xr_stack_tstring<InSize>, xr_stack_wstring<InSize>>)
			return true;

#ifdef IXR_WINDOWS
		bool result = true;
		int size_needed = WideCharToMultiByte(CP_UTF8, 0, input.c_str(), InSize, NULL, 0,
			NULL, NULL);

		if (size_needed > OutSize)
		{
			result = false;
			return result;
		}


		size_needed = WideCharToMultiByte(CP_UTF8, 0, input.c_str(), InSize, output.data(), size_needed, NULL, NULL);

		result = size_needed != 0;

		return result;
#else
		return false;
#endif
	}

	XRCORE_API wchar_t* ANSI_TO_TCHAR(const char* C);
	XRCORE_API wchar_t* ANSI_TO_TCHAR_U8(const char* C);
}