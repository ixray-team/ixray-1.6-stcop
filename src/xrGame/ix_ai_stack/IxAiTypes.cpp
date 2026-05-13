#include "StdAfx.h"

#include "IxAiTypes.h"

namespace
{
    struct IxAiControlModeNameRow final
    {
        IxAiControlMode mode;
        LPCSTR configName;
        LPCSTR displayName;
    };

    static const IxAiControlModeNameRow kControlModeNames[] = {
        {IxAiControlMode::LegacyOnly, "legacy_only", "LegacyOnly"},
        {IxAiControlMode::ObserveOnly, "observe_only", "ObserveOnly"},
        {IxAiControlMode::LegacyAssist, "legacy_assist", "LegacyAssist"},
        {IxAiControlMode::IxAuthoritative, "ix_authoritative", "IxAuthoritative"},
    };

    struct IxAiFeatureGateNameRow final
    {
        IxAiFeatureGate feature;
        LPCSTR configName;
    };

    static const IxAiFeatureGateNameRow kFeatureGateNames[] = {
        {IxAiFeatureGate::LegacyBridge, "bridge"},
        {IxAiFeatureGate::MemoryAuthoritative, "memory_authoritative"},
        {IxAiFeatureGate::TacticsFeedMovementHint, "tactics_feed_movement_hint"},
        {IxAiFeatureGate::CoverFeedDangerHint, "cover_feed_danger_hint"},
        {IxAiFeatureGate::LocalityActorAttenuation, "locality_actor_attenuation"},
        {IxAiFeatureGate::SquadChannel, "squad_channel"},
        {IxAiFeatureGate::SquadStealthFanout, "squad_stealth_fanout"},
        {IxAiFeatureGate::SquadClearAttackerOnStealthHit, "squad_clear_attacker_on_stealth_hit"},
        {IxAiFeatureGate::SquadSuppressDirectFocusOnStealthHit, "squad_suppress_direct_focus_on_stealth_hit"},
    };
} // namespace

LPCSTR IxAiControlModeToConfigName(IxAiControlMode mode)
{
    for (const IxAiControlModeNameRow& row : kControlModeNames)
    {
        if (row.mode == mode)
        {
            return row.configName;
        }
    }

    return "legacy_assist";
}

LPCSTR IxAiControlModeToDisplayName(IxAiControlMode mode)
{
    for (const IxAiControlModeNameRow& row : kControlModeNames)
    {
        if (row.mode == mode)
        {
            return row.displayName;
        }
    }

    return "LegacyAssist";
}

bool IxAiControlModeTryParse(pcstr value, IxAiControlMode& outMode)
{
    if (value == nullptr || value[0] == '\0')
    {
        return false;
    }

    for (const IxAiControlModeNameRow& row : kControlModeNames)
    {
        if (_stricmp(value, row.configName) == 0 || _stricmp(value, row.displayName) == 0)
        {
            outMode = row.mode;
            return true;
        }
    }

    return false;
}

LPCSTR IxAiFeatureGateToConfigName(IxAiFeatureGate feature)
{
    for (const IxAiFeatureGateNameRow& row : kFeatureGateNames)
    {
        if (row.feature == feature)
        {
            return row.configName;
        }
    }

    return "unknown";
}

bool IxAiFeatureGateTryParse(pcstr value, IxAiFeatureGate& outFeature)
{
    if (value == nullptr || value[0] == '\0')
    {
        return false;
    }

    for (const IxAiFeatureGateNameRow& row : kFeatureGateNames)
    {
        if (_stricmp(value, row.configName) == 0)
        {
            outFeature = row.feature;
            return true;
        }
    }

    return false;
}

IxAiPerceptionEvent::IxAiPerceptionEvent() = default;

IxAiPerceptionEvent::~IxAiPerceptionEvent() = default;

IxAiBehaviourProfile::IxAiBehaviourProfile() = default;

IxAiBehaviourProfile::~IxAiBehaviourProfile() = default;
