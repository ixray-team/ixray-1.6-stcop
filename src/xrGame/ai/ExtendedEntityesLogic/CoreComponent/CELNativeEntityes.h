#pragma once

namespace ExtendedEntityesClasses
{
inline constexpr CLASS_ID Komar = MK_CLSID('E', 'L', 'K', 'O', 'M', 'A', 'R', ' ');
inline constexpr CLASS_ID Duten = MK_CLSID('E', 'L', 'D', 'U', 'T', 'E', 'N', ' ');
inline constexpr CLASS_ID Bat = MK_CLSID('E', 'L', 'B', 'A', 'T', ' ', ' ', ' ');
inline constexpr CLASS_ID Strekoza = MK_CLSID('E', 'L', 'S', 'T', 'R', 'E', 'K', ' ');
inline constexpr CLASS_ID Crow = MK_CLSID('A', 'I', '_', 'S', 'C', 'R', 'O', 'W');
inline bool IsFlying(CLASS_ID ClassIdentifier)
{
	return ClassIdentifier == Komar || ClassIdentifier == Duten || ClassIdentifier == Bat || ClassIdentifier == Strekoza || ClassIdentifier == Crow;
}
} // namespace ExtendedEntityesClasses
