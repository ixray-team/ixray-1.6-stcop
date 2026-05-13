#pragma once

#include "IxAiTypes.h"

void IxAiStackScriptReloadRuntimeConfig();
void IxAiStackScriptResetRuntimeOverrides();

bool IxAiStackScriptSetControlMode(pcstr modeName);
void IxAiStackScriptSetControlMode(IxAiControlMode mode);

void IxAiStackScriptSetBridgeEnabled(bool enabled);
void IxAiStackScriptSetMemoryAuthoritative(bool enabled);
void IxAiStackScriptSetTacticsFeedMovementHint(bool enabled);
void IxAiStackScriptSetCoverFeedDangerHint(bool enabled);
void IxAiStackScriptSetLocalityActorAttenuationEnabled(bool enabled);
bool IxAiStackScriptSetFeature(pcstr featureName, bool enabled);
