#pragma once

#include <string>

#ifdef MCHAR_IS_WCHAR
typedef wchar_t raw_char;
#else
typedef char raw_char;
#endif

std::string NormalizeFromMaxString(const raw_char* data);
const raw_char* NormalizeToMaxString(const char* data);