#pragma once

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

