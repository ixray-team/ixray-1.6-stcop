#include "StdAfx.h"

#include "../../xrCore/EngineExternal.h"
#include "../Level.h"
#include "IxAiStackApi.h"

#include "../../xrEngine/device.h"

void CLevel::IxAiStackParallelUpdate()
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
        return;
    }

    if (!IsGameTypeSingle())
    {
        return;
    }

    IxAiStackApi::Update(Device.fTimeDelta);
}
