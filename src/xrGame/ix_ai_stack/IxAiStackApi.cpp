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
    {
        xrCriticalSectionGuard guard(g_ixAiRuntimeTuningCs);
        bridgeEnabledForLog = g_ixAiRuntimeTuning.bridgeEnabled;
    }

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
