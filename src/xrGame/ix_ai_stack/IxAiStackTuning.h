#pragma once

#include "../../xrCore/_types.h"
#include "../../xrCore/xrSyncronize.h"

struct IxAiRuntimeTuning final
{
    f32 silencedShotPowerCutoff{0.25f};
    f32 suspicionToSuspicious{0.35f};
    f32 suspicionToSearch{1.2f};
    f32 suspicionToCombat{2.8f};
    f32 globalSuspicionDecayScale{1.f};

    bool bridgeEnabled{true};
    bool memoryAuthoritative{false};
    f32 bridgeSuspiciousCooldownSeconds{4.f};
    f32 bridgeSearchCooldownSeconds{2.5f};
    f32 bridgeCombatCooldownSeconds{1.5f};

    f32 memoryDecayPerSecond{1.25f};
    f32 memorySuspicionLeakScale{0.12f};
    f32 memorySampleWeightScale{0.25f};
    f32 memoryStrengthEpsilon{0.02f};

    u32 visualProbeIntervalFrames{4u};
    f32 visualProbeIntensity{1.15f};
    f32 visualProbeRadius{2.5f};
    u32 maxVisualProbesPerFrame{24u};

    f32 visionHalfFovDegreesFallback{55.f};
    f32 visionRayRangeExtra{0.65f};
    f32 visionOcclusionDepthEpsilon{0.08f};
    f32 visionTargetChestHeight{0.95f};
    f32 visionMaxDistanceScale{1.08f};

    bool squadChannelEnabled{true};
    f32 squadChannelMaxDistance{0.f};
    f32 squadAllyWoundIntensity{1.35f};
    f32 squadAllyWoundRadius{8.f};
    f32 squadCombatEngagedIntensity{1.1f};
    f32 squadCombatEngagedRadius{12.f};
    f32 squadChannelSuspicionScale{0.42f};
    f32 squadChannelFocusIntensityMin{0.25f};

    bool squadFanoutStealthHitHandlingEnabled{true};
    bool squadFanoutClearAttackerIdOnStealthHit{true};
    f32 squadFanoutStealthVictimPositionWeight{0.32f};
    bool squadFanoutSuppressDirectFocusOnStealthHit{true};
    f32 squadFanoutStealthSuspicionScale{0.55f};

    f32 tacticsGuardHoldDistance{4.f};
    f32 tacticsFlankSideScale{0.35f};
    bool tacticsFeedMovementHint{true};
    u32 tacticHintDangerCooldownMs{900u};

    bool coverFeedDangerHint{true};
    u32 coverHintDangerCooldownMs{1100u};
    u32 coverHintIntervalFrames{14u};

    u32 corpseProbeIntervalFrames{18u};
    f32 corpseProbeRadius{42.f};
    f32 corpseEventIntensity{0.55f};

    bool localityActorAttenuationEnabled{true};
    f32 localityActorSoftBeginDistance{95.f};
    f32 localityActorHardCutoffDistance{260.f};
    f32 localityActorMinIntensityScale{0.04f};
    f32 localityActorAnchorRadius{6.f};
};

extern IxAiRuntimeTuning g_ixAiRuntimeTuning;
extern xrCriticalSection g_ixAiRuntimeTuningCs;

void IxAiRuntimeTuningResetDefaults();
