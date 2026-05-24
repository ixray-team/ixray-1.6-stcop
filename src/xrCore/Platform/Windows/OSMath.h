#pragma once
#include <float.h>

namespace Platform
{
    inline void GetCurrentUnixTime32(__time32_t* InVar)
    {
        _tzset();
        _time32(InVar);
    }
}