#include "StdAfx.h"

#include "IxAiStackTelemetry.h"

static u32 g_ixAiTelemetryBridgePushes{};
static u32 g_ixAiTelemetrySoundIngests{};
static u32 g_ixAiTelemetryTacticHintPushes{};
static u32 g_ixAiTelemetryCoverHintPushes{};

void IxAiStackTelemetry_ResetFrame()
{
    g_ixAiTelemetryBridgePushes = 0;
    g_ixAiTelemetrySoundIngests = 0;
    g_ixAiTelemetryTacticHintPushes = 0;
    g_ixAiTelemetryCoverHintPushes = 0;
}

void IxAiStackTelemetry_AddBridgePush(u32 count)
{
    g_ixAiTelemetryBridgePushes += count;
}

void IxAiStackTelemetry_AddSoundIngest(u32 count)
{
    g_ixAiTelemetrySoundIngests += count;
}

void IxAiStackTelemetry_AddTacticHintPush(u32 count)
{
    g_ixAiTelemetryTacticHintPushes += count;
}

void IxAiStackTelemetry_AddCoverHintPush(u32 count)
{
    g_ixAiTelemetryCoverHintPushes += count;
}

u32 IxAiStackTelemetry_GetBridgePushCountLastFrame()
{
    return g_ixAiTelemetryBridgePushes;
}

u32 IxAiStackTelemetry_GetSoundIngestCountLastFrame()
{
    return g_ixAiTelemetrySoundIngests;
}

u32 IxAiStackTelemetry_GetTacticHintPushCountLastFrame()
{
    return g_ixAiTelemetryTacticHintPushes;
}

u32 IxAiStackTelemetry_GetCoverHintPushCountLastFrame()
{
    return g_ixAiTelemetryCoverHintPushes;
}
