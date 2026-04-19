#pragma once

#include "../../xrCore/_types.h"

class IxAiManager;

class IxAiStackApi final
{
public:
    static void Initialize();
    static void Shutdown();
    static void Update(f32 deltaTime);
    static bool IsActive();
    static IxAiManager* Manager();

    static void ReloadRuntimeConfig();

private:
    IxAiStackApi() = delete;
};
