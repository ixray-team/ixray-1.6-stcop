#include "StdAfx.h"

#include "IxAiStackScriptBridge.h"
#include "IxAiStackApi.h"

void IxAiStackScriptReloadRuntimeConfig()
{
    IxAiStackApi::ReloadRuntimeConfig();
}

void IxAiStackScriptResetRuntimeOverrides()
{
    IxAiStackApi::ResetRuntimeOverrides();
}

bool IxAiStackScriptSetControlMode(pcstr modeName)
{
    IxAiControlMode mode = IxAiControlMode::LegacyAssist;

    if (!IxAiControlModeTryParse(modeName, mode))
    {
        return false;
    }

    IxAiStackScriptSetControlMode(mode);
    return true;
}

void IxAiStackScriptSetControlMode(IxAiControlMode mode)
{
    IxAiStackApi::SetControlMode(mode);
}

void IxAiStackScriptSetBridgeEnabled(bool enabled)
{
    IxAiStackApi::SetFeatureEnabled(IxAiFeatureGate::LegacyBridge, enabled);
}

void IxAiStackScriptSetMemoryAuthoritative(bool enabled)
{
    IxAiStackApi::SetFeatureEnabled(IxAiFeatureGate::MemoryAuthoritative, enabled);
}

void IxAiStackScriptSetTacticsFeedMovementHint(bool enabled)
{
    IxAiStackApi::SetFeatureEnabled(IxAiFeatureGate::TacticsFeedMovementHint, enabled);
}

void IxAiStackScriptSetCoverFeedDangerHint(bool enabled)
{
    IxAiStackApi::SetFeatureEnabled(IxAiFeatureGate::CoverFeedDangerHint, enabled);
}

void IxAiStackScriptSetLocalityActorAttenuationEnabled(bool enabled)
{
    IxAiStackApi::SetFeatureEnabled(IxAiFeatureGate::LocalityActorAttenuation, enabled);
}

bool IxAiStackScriptSetFeature(pcstr featureName, bool enabled)
{
    IxAiFeatureGate feature = IxAiFeatureGate::LegacyBridge;

    if (!IxAiFeatureGateTryParse(featureName, feature))
    {
        return false;
    }

    IxAiStackApi::SetFeatureEnabled(feature, enabled);
    return true;
}
