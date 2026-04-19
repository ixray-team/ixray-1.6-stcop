#pragma once

#include "../../xrCore/_types.h"

namespace IxAiConstants
{
    constexpr f32 kSuspicionScoreMax = 16.f;
    constexpr u32 kMemorySlotCap = 16u;
    constexpr f32 kMemoryMergeDistanceSq = 9.f;
    constexpr f32 kMemoryPushThreshold = 0.15f;
    constexpr f32 kMemoryStrengthCap = 4.f;

    constexpr u32 kWorkingBeliefCap = 8u;
    constexpr f32 kWorkingBeliefMergeDistanceSq = 25.f;
    constexpr f32 kWorkingBeliefDecayPerSecond = 0.55f;
    constexpr f32 kBeliefPromoteStrengthThreshold = 0.18f;
    constexpr f32 kBeliefPromoteStrengthToConfidence = 0.22f;
    constexpr f32 kWorkingBeliefEpsilon = 0.025f;
    constexpr f32 kBeliefConfidenceCap = 1.f;
    constexpr f32 kWorkingBeliefHintThreshold = 0.12f;

    constexpr f32 kLodReferenceDistance = 200.f;
    constexpr f32 kLodDistanceSq = kLodReferenceDistance * kLodReferenceDistance;
    constexpr f32 kCorpseIngestEventRadius = 3.f;

    constexpr f32 kTacticsCoverLateralScale = 1.75f;
    constexpr f32 kTacticsCoverBackAlongThreatScale = -1.25f;

    constexpr u32 kPerceptionGlobalEventCap = 512u;
    constexpr f32 kPerceptionEventRetentionSeconds = 15.f;
    constexpr u32 kPerceptionMaxEventsNearQuery = 32u;
    constexpr f32 kPerceptionSpatialCellSize = 30.f;

    constexpr u32 kAgentSyncIntervalFrames = 10u;

    constexpr f32 kDefaultProfileSuspicionDecayRate = 0.4f;
    constexpr f32 kDefaultProfileAlertRadius = 45.f;
    constexpr f32 kDefaultProfileSilencedGunHearingMultiplier = 0.65f;
    constexpr f32 kDefaultProfileFlankRange = 25.f;

    constexpr f32 kLodAlertRadiusScale = 0.5f;

    constexpr u32 kCorpseProbeMaxPerFrame = 20u;

    constexpr f32 kTacticsGuardHoldClampMin = 0.5f;
    constexpr f32 kTacticsGuardHoldClampMax = 12.f;
    constexpr f32 kTacticsGuardBackDistanceFraction = 0.2f;
    constexpr f32 kTacticsGuardBackDistanceMin = 1.f;

    constexpr f32 kInvestigateMemoryMinStrength = 0.11f;
    constexpr u32 kBoltIxThrottleMs = 140u;
    constexpr u32 kInvestigateDangerHintCooldownMs = 1250u;
}

