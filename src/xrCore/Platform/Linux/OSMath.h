#pragma once
#include <cmath>

struct RECT
{
    int left;
    int top;
    int right;
    int bottom;
};

inline
unsigned char _bittest64(std::int64_t *a, std::int64_t b)
{
    auto const value{ *a };
    auto const mask{ 1LL << (b&63) };
    auto const masked_value{ value & mask };
    return (unsigned char){ masked_value != 0 };
}

inline struct tm* _localtime64(const __time64_t* timer)
{
    static struct tm tm_storage;
    time_t t = static_cast<time_t>(*timer);
    localtime_r(&t, &tm_storage);
    return &tm_storage;
}

namespace Platform
{
}