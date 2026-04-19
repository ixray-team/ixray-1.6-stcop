#include "StdAfx.h"

#include "IxAiStackScriptBridge.h"
#include "IxAiStackApi.h"
#include "IxAiStackTuning.h"

void IxAiStackScriptReloadRuntimeConfig()
{
    IxAiStackApi::ReloadRuntimeConfig();
}

void IxAiStackScriptSetBridgeEnabled(bool enabled)
{
    xrCriticalSectionGuard guard(g_ixAiRuntimeTuningCs);
    g_ixAiRuntimeTuning.bridgeEnabled = enabled;
}

void IxAiStackScriptSetMemoryAuthoritative(bool enabled)
{
    xrCriticalSectionGuard guard(g_ixAiRuntimeTuningCs);
    g_ixAiRuntimeTuning.memoryAuthoritative = enabled;
}

void IxAiStackScriptSetTacticsFeedMovementHint(bool enabled)
{
    xrCriticalSectionGuard guard(g_ixAiRuntimeTuningCs);
    g_ixAiRuntimeTuning.tacticsFeedMovementHint = enabled;
}

void IxAiStackScriptSetCoverFeedDangerHint(bool enabled)
{
    xrCriticalSectionGuard guard(g_ixAiRuntimeTuningCs);
    g_ixAiRuntimeTuning.coverFeedDangerHint = enabled;
}

void IxAiStackScriptSetLocalityActorAttenuationEnabled(bool enabled)
{
    xrCriticalSectionGuard guard(g_ixAiRuntimeTuningCs);
    g_ixAiRuntimeTuning.localityActorAttenuationEnabled = enabled;
}
