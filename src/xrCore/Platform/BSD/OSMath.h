#pragma once
#include <cmath>

inline
unsigned char _bittest64(std::int64_t *a, std::int64_t b)
{
    auto const value{ *a };
    auto const mask{ 1LL << (b&63) };
    auto const masked_value{ value & mask };
    return (unsigned char){ masked_value != 0 };
}

namespace Platform
{
}