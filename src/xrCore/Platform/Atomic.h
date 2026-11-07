#pragma once

namespace Platform
{
XRCORE_API u32 AtomicCompareExchange(u32* Destination, u32 Expected, u32 Desired);
}

