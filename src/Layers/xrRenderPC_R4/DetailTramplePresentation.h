#pragma once
#include <array>
#include <bit>
#include <cstdint>

namespace DetailTramplePresentation
{
using Key = std::array<std::uint32_t, 4>;
inline Key MakeKey(float X, float Y, float Z, float Scale)
{
	return {std::bit_cast<std::uint32_t>(X), std::bit_cast<std::uint32_t>(Y), std::bit_cast<std::uint32_t>(Z), std::bit_cast<std::uint32_t>(Scale)};
}
inline std::uint32_t Hash(const Key& Identity)
{
	std::uint32_t Value = 2166136261u;
	for (auto Part : Identity)
	{
		Value = (Value ^ Part) * 16777619u;
	}
	Value ^= Value >> 16;
	Value *= 0x7feb352du;
	Value ^= Value >> 15;
	Value *= 0x846ca68bu;
	return Value ^ (Value >> 16);
}
} // namespace DetailTramplePresentation
