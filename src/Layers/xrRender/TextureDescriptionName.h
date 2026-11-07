#pragma once

#include "../../xrCore/xrCore.h"

namespace TextureDescription
{
// Compiled material names use backslashes, independently of host filesystem paths.
inline void NormalizeSeparators(char* Name)
{
	for (char* Character = Name; *Character; ++Character)
	{
		if (*Character == '/')
		{
			*Character = '\\';
		}
	}
}
} // namespace TextureDescription

inline void fix_texture_thm_name(LPSTR Name)
{
	TextureDescription::NormalizeSeparators(Name);
	LPSTR Extension = strext(Name);
	if (Extension &&
		(0 == _stricmp(Extension, ".tga") ||
			0 == _stricmp(Extension, ".thm") ||
			0 == _stricmp(Extension, ".dds") ||
			0 == _stricmp(Extension, ".bmp") ||
			0 == _stricmp(Extension, ".ogm")))
	{
		*Extension = 0;
	}
}

