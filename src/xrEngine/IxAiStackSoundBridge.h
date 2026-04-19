#pragma once

#include "IGame_Level.h"

using IxAiStackSoundDispatchFn = void (*)(const xr_vector<IGame_Level::_esound_delegate>& events);

ENGINE_API void IxAiStackSoundBridge_Register(IxAiStackSoundDispatchFn fn);
ENGINE_API void IxAiStackSoundBridge_Unregister();
ENGINE_API void IxAiStackSoundBridge_DispatchTap(const xr_vector<IGame_Level::_esound_delegate>& events);
