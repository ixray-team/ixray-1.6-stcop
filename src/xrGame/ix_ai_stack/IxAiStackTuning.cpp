#include "StdAfx.h"

#include "IxAiStackTuning.h"

IxAiRuntimeTuning g_ixAiRuntimeTuning{};
xrCriticalSection g_ixAiRuntimeTuningCs{};

void IxAiRuntimeTuningResetDefaults()
{
    xrCriticalSectionGuard guard(g_ixAiRuntimeTuningCs);
    g_ixAiRuntimeTuning = IxAiRuntimeTuning{};
}
