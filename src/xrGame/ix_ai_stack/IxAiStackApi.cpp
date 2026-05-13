#include "StdAfx.h"

#include <new>

#include "../../xrCore/EngineExternal.h"
#include "../../xrEngine/IGame_Level.h"
#include "../Level.h"
#include "IxAiManager.h"
#include "IxAiSoundDispatch.h"
#include "IxAiStackApi.h"
#include "IxAiStackConfigLoad.h"
#include "IxAiStackTuning.h"

static IxAiManager* g_ixAiStackManager = nullptr;

namespace
{
    bool IxAiControlModeAllowsLegacyOutput(IxAiControlMode mode)
    {
        return mode == IxAiControlMode::LegacyAssist || mode == IxAiControlMode::IxAuthoritative;
    }

    bool IxAiFeatureEnabledRaw(const IxAiRuntimeTuning& tuning, IxAiFeatureGate feature)
    {
        const bool legacyOutputAllowedByMode = IxAiControlModeAllowsLegacyOutput(tuning.controlMode);

        switch (feature)
        {
        case IxAiFeatureGate::LegacyBridge:
            return tuning.bridgeEnabled && legacyOutputAllowedByMode;
        case IxAiFeatureGate::MemoryAuthoritative:
            return tuning.memoryAuthoritative && tuning.bridgeEnabled && legacyOutputAllowedByMode;
        case IxAiFeatureGate::TacticsFeedMovementHint:
            return tuning.tacticsFeedMovementHint && tuning.bridgeEnabled && legacyOutputAllowedByMode;
        case IxAiFeatureGate::CoverFeedDangerHint:
            return tuning.coverFeedDangerHint && tuning.bridgeEnabled && legacyOutputAllowedByMode;
        case IxAiFeatureGate::LocalityActorAttenuation:
            return tuning.localityActorAttenuationEnabled;
        case IxAiFeatureGate::SquadChannel:
            return tuning.squadChannelEnabled;
        case IxAiFeatureGate::SquadStealthFanout:
            return tuning.squadFanoutStealthHitHandlingEnabled;
        case IxAiFeatureGate::SquadClearAttackerOnStealthHit:
            return tuning.squadFanoutClearAttackerIdOnStealthHit;
        case IxAiFeatureGate::SquadSuppressDirectFocusOnStealthHit:
            return tuning.squadFanoutSuppressDirectFocusOnStealthHit;
        default:
            return false;
        }
    }

    void IxAiSetFeatureEnabledRaw(IxAiRuntimeTuning& tuning, IxAiFeatureGate feature, bool enabled)
    {
        switch (feature)
        {
        case IxAiFeatureGate::LegacyBridge:
            tuning.bridgeEnabled = enabled;
            return;
        case IxAiFeatureGate::MemoryAuthoritative:
            tuning.memoryAuthoritative = enabled;
            return;
        case IxAiFeatureGate::TacticsFeedMovementHint:
            tuning.tacticsFeedMovementHint = enabled;
            return;
        case IxAiFeatureGate::CoverFeedDangerHint:
            tuning.coverFeedDangerHint = enabled;
            return;
        case IxAiFeatureGate::LocalityActorAttenuation:
            tuning.localityActorAttenuationEnabled = enabled;
            return;
        case IxAiFeatureGate::SquadChannel:
            tuning.squadChannelEnabled = enabled;
            return;
        case IxAiFeatureGate::SquadStealthFanout:
            tuning.squadFanoutStealthHitHandlingEnabled = enabled;
            return;
        case IxAiFeatureGate::SquadClearAttackerOnStealthHit:
            tuning.squadFanoutClearAttackerIdOnStealthHit = enabled;
            return;
        case IxAiFeatureGate::SquadSuppressDirectFocusOnStealthHit:
            tuning.squadFanoutSuppressDirectFocusOnStealthHit = enabled;
            return;
        default:
            return;
        }
    }
} // namespace

void IxAiStackApi::Initialize()
{
    if (g_dedicated_server)
    {
        return;
    }

    if (!EngineExternal().CallOfPripyatMode())
    {
        return;
    }

    if (!EngineExternal()[EEngineExternalGame::EnableIxAiStack])
    {
        Msg("* [IX AI]: EnableIxAiStack is disabled");
        return;
    }

    if (g_pGameLevel == nullptr)
    {
        return;
    }

    if (!IsGameTypeSingle())
    {
        return;
    }

    if (g_ixAiStackManager != nullptr)
    {
        return;
    }

    IxAiStackRuntimeTuningReloadFromDefaultsAndOptionalFile();

    IxAiManager* const manager = new (std::nothrow) IxAiManager();

    if (manager == nullptr || !manager->HasValidSubsystems())
    {
        Msg("! [IX AI]: Stack init failed (out of memory or subsystem allocation failure)");
        xr_delete(manager);
        return;
    }

    g_ixAiStackManager = manager;
    IxAiStackRegisterSoundDispatchHook();
    Msg("* [IX AI]: Stack initialized");

    bool bridgeEnabledForLog = false;
    IxAiControlMode controlModeForLog = IxAiControlMode::LegacyAssist;
    {
        xrCriticalSectionGuard guard(g_ixAiRuntimeTuningCs);
        controlModeForLog = g_ixAiRuntimeTuning.controlMode;
        bridgeEnabledForLog = IxAiFeatureEnabledRaw(g_ixAiRuntimeTuning, IxAiFeatureGate::LegacyBridge);
    }

    Msg("* [IX AI]: Control mode: %s", IxAiControlModeToDisplayName(controlModeForLog));

    if (bridgeEnabledForLog)
    {
        Msg("* [IX AI]: Legacy behaviour bridge is ENABLED (runtime tuning)");
    }
}

void IxAiStackApi::Shutdown()
{
    IxAiStackUnregisterSoundDispatchHook();
    xr_delete(g_ixAiStackManager);
    Msg("* [IX AI]: Stack shutdown");
}

void IxAiStackApi::Update(f32 deltaTime)
{
    if (g_dedicated_server || !EngineExternal().CallOfPripyatMode() || !EngineExternal()[EEngineExternalGame::EnableIxAiStack])
    {
        return;
    }

    if (g_ixAiStackManager == nullptr)
    {
        return;
    }

    if (g_pGameLevel == nullptr || !g_pGameLevel->bReady)
    {
        return;
    }

    if (!IsGameTypeSingle())
    {
        return;
    }

    g_ixAiStackManager->Update(deltaTime);
}

bool IxAiStackApi::IsActive()
{
    return g_ixAiStackManager != nullptr;
}

IxAiManager* IxAiStackApi::Manager()
{
    return g_ixAiStackManager;
}

void IxAiStackApi::ReloadRuntimeConfig()
{
    if (!IxAiStackRuntimeTuningReloadFromDefaultsAndOptionalFile())
    {
        Msg("* [IX AI]: ReloadRuntimeConfig: misc\\ix_ai_stack.ltx not applied (using code defaults)");
    }
}

void IxAiStackApi::ResetRuntimeOverrides()
{
    ReloadRuntimeConfig();
}

IxAiControlMode IxAiStackApi::GetControlMode()
{
    xrCriticalSectionGuard guard(g_ixAiRuntimeTuningCs);
    return g_ixAiRuntimeTuning.controlMode;
}

void IxAiStackApi::SetControlMode(IxAiControlMode mode)
{
    xrCriticalSectionGuard guard(g_ixAiRuntimeTuningCs);
    g_ixAiRuntimeTuning.controlMode = mode;
}

bool IxAiStackApi::IsLegacyOutputAllowed()
{
    xrCriticalSectionGuard guard(g_ixAiRuntimeTuningCs);
    return IxAiFeatureEnabledRaw(g_ixAiRuntimeTuning, IxAiFeatureGate::LegacyBridge);
}

bool IxAiStackApi::IsFeatureEnabled(IxAiFeatureGate feature)
{
    xrCriticalSectionGuard guard(g_ixAiRuntimeTuningCs);
    return IxAiFeatureEnabledRaw(g_ixAiRuntimeTuning, feature);
}

void IxAiStackApi::SetFeatureEnabled(IxAiFeatureGate feature, bool enabled)
{
    xrCriticalSectionGuard guard(g_ixAiRuntimeTuningCs);
    IxAiSetFeatureEnabledRaw(g_ixAiRuntimeTuning, feature, enabled);
}
