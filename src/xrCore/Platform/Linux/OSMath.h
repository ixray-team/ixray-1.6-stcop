#pragma once
#include <cmath>

#ifndef XR_USE_DXVK_NATIVE
struct RECT
{
    int left;
    int top;
    int right;
    int bottom;
};
#endif

inline unsigned char _bittest64(std::int64_t *a, std::int64_t b)
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

struct SYSTEMTIME 
{
  std::uint16_t wYear;         // год (1601-30827)
  std::uint16_t wMonth;        // месяц (1-12)
  std::uint16_t wDayOfWeek;    // день недели (0=воскресенье, 1=понедельник...)
  std::uint16_t wDay;          // день месяца (1-31)
  std::uint16_t wHour;         // час (0-23)
  std::uint16_t wMinute;       // минуты (0-59)
  std::uint16_t wSecond;       // секунды (0-59)
  std::uint16_t wMilliseconds; // миллисекунды (0-999)
};

namespace Platform
{
}