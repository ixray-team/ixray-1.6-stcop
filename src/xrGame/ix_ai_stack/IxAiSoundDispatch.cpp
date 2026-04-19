#include "StdAfx.h"

#include "../../xrCore/EngineExternal.h"
#include "../../xrEngine/IxAiStackSoundBridge.h"
#include "../../xrEngine/IGame_Level.h"
#include "../../xrEngine/xr_object.h"
#include "../../xrSound/Sound.h"
#include "IxAiManager.h"
#include "IxAiPerceptionSystem.h"
#include "IxAiSoundClassification.h"
#include "IxAiStackApi.h"
#include "IxAiStackTuning.h"

static void IxAiSoundDispatchCallback(const xr_vector<IGame_Level::_esound_delegate>& events)
{
    if (!EngineExternal()[EEngineExternalGame::EnableIxAiStack])
    {
        return;
    }

    xrCriticalSectionGuard tuningGuard(g_ixAiRuntimeTuningCs);

    IxAiManager* manager = IxAiStackApi::Manager();
    if (manager == nullptr)
    {
        return;
    }

    manager->Perception().IngestEngineSoundDelegates(events);
}

void IxAiStackRegisterSoundDispatchHook()
{
    IxAiStackSoundBridge_Register(&IxAiSoundDispatchCallback);
}

void IxAiStackUnregisterSoundDispatchHook()
{
    IxAiStackSoundBridge_Unregister();
}
