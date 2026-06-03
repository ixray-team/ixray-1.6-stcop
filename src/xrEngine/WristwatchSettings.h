#pragma once

#include "ExportDefines.h"
#include "WristwatchTypes.h"

ENGINE_API void ReloadWristwatchRuntimeSettings();
ENGINE_API const SWristwatchRuntimeSettings& GetWristwatchRuntimeSettings();
ENGINE_API bool IsWristwatchContentConfigured();

ENGINE_API void SetWristwatchSurgeState(u8 mode, u32 countdownSeconds, u32 untilSurgeSeconds);
ENGINE_API const SWristwatchSurgeState& GetWristwatchSurgeState();

ENGINE_API void SetWristwatchHudSessionActive(bool active);
ENGINE_API bool IsWristwatchHudSessionActive();
ENGINE_API bool IsWristwatchReplaceSurgeActive();
