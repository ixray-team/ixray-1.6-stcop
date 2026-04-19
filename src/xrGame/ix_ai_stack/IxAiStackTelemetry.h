#pragma once

#include "../../xrCore/_types.h"

void IxAiStackTelemetry_ResetFrame();
void IxAiStackTelemetry_AddBridgePush(u32 count);
void IxAiStackTelemetry_AddSoundIngest(u32 count);
void IxAiStackTelemetry_AddTacticHintPush(u32 count);
void IxAiStackTelemetry_AddCoverHintPush(u32 count);

u32 IxAiStackTelemetry_GetBridgePushCountLastFrame();
u32 IxAiStackTelemetry_GetSoundIngestCountLastFrame();
u32 IxAiStackTelemetry_GetTacticHintPushCountLastFrame();
u32 IxAiStackTelemetry_GetCoverHintPushCountLastFrame();
