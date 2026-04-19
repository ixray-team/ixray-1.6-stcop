#include "stdafx.h"

#include "IxAiStackSoundBridge.h"

static IxAiStackSoundDispatchFn g_ixAiStackSoundDispatchFn = nullptr;

void IxAiStackSoundBridge_Register(IxAiStackSoundDispatchFn fn)
{
    g_ixAiStackSoundDispatchFn = fn;
}

void IxAiStackSoundBridge_Unregister()
{
    g_ixAiStackSoundDispatchFn = nullptr;
}

void IxAiStackSoundBridge_DispatchTap(const xr_vector<IGame_Level::_esound_delegate>& events)
{
    if (g_ixAiStackSoundDispatchFn == nullptr)
    {
        return;
    }

    g_ixAiStackSoundDispatchFn(events);
}
